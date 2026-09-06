-- | The shell's end of the pipe.
--
-- Starts the kernel, sends it requests, and hands each reply to whoever asked
-- for it.  The one rule the whole module is built around is that none of it
-- ever blocks the thread holding the window: a window that stops drawing
-- because another process is thinking has given away the only thing the split
-- was for.
--
-- The Guile shell did that by polling a @select@ once a frame.  Here a reader
-- thread blocks on the pipe and drops whole messages into an inbox, and the
-- main loop empties the inbox whenever it likes.  With the threaded runtime a
-- blocked read costs nothing and wakes the moment bytes arrive, so the answer
-- is delivered sooner and the frame that delivers it does no reading at all.
--
-- Nothing here knows about GTK.  The shell owns a main loop and calls 'pump'
-- from it; this module owns a process and a table of promises.  That is
-- deliberate: it keeps the one piece of genuinely fiddly bookkeeping -- which
-- requests are outstanding, and for how long -- somewhere it can be tested
-- without a display.
module Cellar.Client
  ( Kernel
  , startKernel
  , kernelAlive
  , call
  , pump
  , outstanding
  , waitingFor
  , waitingOp
  , stalled
  , markReady
  , stopKernel
  , restartKernel
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as M
import GHC.Clock (getMonotonicTime)
import System.IO
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process

import Cellar.Protocol
import Cellar.Sexp

-- | A request that has been sent and not yet answered.
data Pending = Pending
  { pendingContinue :: Sexp -> IO ()
  , pendingFail :: String -> IO ()
  , pendingSent :: Double
  , pendingOp :: String
  }

data Running = Running
  { runningProcess :: ProcessHandle
  , runningStdin :: Handle
  , runningReader :: ThreadId
  }

data Kernel = Kernel
  { kernelCommand :: (FilePath, [String])
  , kernelRunning :: IORef (Maybe Running)
  , kernelPending :: IORef (M.Map Int Pending)
  , kernelNextId :: IORef Int
    -- | Whole messages the reader thread has taken off the pipe, newest first.
    -- The main loop swaps this out; nothing else touches it.
  , kernelInbox :: IORef [Message]
  }

-- Starting and stopping

-- | Start the kernel a command names and return a handle on it.
startKernel :: FilePath -> [String] -> IO Kernel
startKernel program arguments = do
  kernel <- Kernel (program, arguments)
    <$> newIORef Nothing
    <*> newIORef M.empty
    <*> newIORef 0
    <*> newIORef []
  spawnInto kernel
  pure kernel

spawnInto :: Kernel -> IO ()
spawnInto kernel = do
  let (program, arguments) = kernelCommand kernel
  (Just input, Just output, _, handle) <- createProcess (proc program arguments)
    { std_in = CreatePipe
    , std_out = CreatePipe
      -- The kernel's stderr is left alone: warnings and backtraces belong on
      -- the terminal the shell was started from, and nothing here reads them.
    , std_err = Inherit
    }
  hSetBinaryMode input True
  hSetBinaryMode output True
  hSetBuffering input (BlockBuffering Nothing)
  writeIORef (kernelInbox kernel) []
  reader <- forkIO (readLoop kernel output)
  writeIORef (kernelRunning kernel) (Just (Running handle input reader))

-- | Read the pipe until it ends, dropping whole messages into the inbox.
--
-- This blocks, and is supposed to: it is its own thread, and a thread blocked
-- on a file descriptor is free.  What it must never do is touch anything the
-- main loop is also touching, which is why it only ever conses onto the inbox.
readLoop :: Kernel -> Handle -> IO ()
readLoop kernel output = go newDecoder
  where
    go decoder = do
      chunk <- try (B.hGetSome output 8192) :: IO (Either SomeException B.ByteString)
      case chunk of
        Left _ -> deliverEnd
        Right bytes
          | B.null bytes -> deliverEnd
          | otherwise -> do
              let (decoder', messages) = feed decoder bytes
              unless (null messages) $
                atomicModifyIORef' (kernelInbox kernel)
                  (\queued -> (reverse messages ++ queued, ()))
              go decoder'

    -- End of file: the kernel has gone.  Said as a message so that the main
    -- loop learns about it in the same place it learns about everything else,
    -- rather than by inspecting the process from a timer.
    deliverEnd = atomicModifyIORef' (kernelInbox kernel)
      (\queued -> (Garbled "\0end" : queued, ()))

kernelAlive :: Kernel -> IO Bool
kernelAlive kernel = maybe False (const True) <$> readIORef (kernelRunning kernel)

-- | Kill the kernel and reap it.
--
-- A kill rather than a polite word, because the case this exists for is a
-- kernel that is not listening: a cell that will not finish is in a loop, and
-- a process in a loop does not read its pipe, notice that it has closed, or
-- act on anything it is asked.  Every request still outstanding is failed on
-- the way out, so that nothing in the shell is left waiting on an answer that
-- can no longer come.
stopKernel :: Kernel -> IO ()
stopKernel kernel = do
  running <- readIORef (kernelRunning kernel)
  writeIORef (kernelRunning kernel) Nothing
  forM_ running $ \r -> do
    ignore (killThread (runningReader r))
    pid <- getPid (runningProcess r)
    forM_ pid $ \p -> ignore (signalProcess sigKILL p)
    ignore (void (waitForProcess (runningProcess r)))
    ignore (hClose (runningStdin r))
  failEverything kernel "the kernel was stopped"

-- | Stop the kernel and start another.  The handle stays the same one, so
-- everything holding it goes on holding it; what it is attached to is new, and
-- knows nothing -- the shell has to open its sheets again.
restartKernel :: Kernel -> IO ()
restartKernel kernel = do
  stopKernel kernel
  spawnInto kernel

failEverything :: Kernel -> String -> IO ()
failEverything kernel why = do
  waiting <- atomicModifyIORef' (kernelPending kernel) (\m -> (M.empty, m))
  forM_ (M.elems waiting) $ \p -> ignore (pendingFail p why)

-- | Run something for its effect and swallow whatever it throws.  Used only
-- for tearing a dead kernel down, where every step is allowed to have already
-- happened.
ignore :: IO a -> IO ()
ignore action = do
  outcome <- try action
  case outcome of
    Left e -> let _ = (e :: SomeException) in pure ()
    Right _ -> pure ()

-- Asking

-- | Ask the kernel to do something.  Returns at once, having sent nothing but
-- bytes: the first continuation is called with the reply's payload when it
-- comes back, and the second with a message if the kernel refuses or dies
-- first.
call :: Kernel -> String -> [Sexp] -> (Sexp -> IO ()) -> (String -> IO ()) -> IO ()
call kernel op arguments continue onFail = do
  running <- readIORef (kernelRunning kernel)
  case running of
    Nothing -> onFail "the kernel is not running"
    Just r -> do
      requestId <- atomicModifyIORef' (kernelNextId kernel) (\n -> (n + 1, n + 1))
      now <- getMonotonicTime
      atomicModifyIORef' (kernelPending kernel)
        (\m -> (M.insert requestId (Pending continue onFail now op) m, ()))
      sent <- try (do B.hPut (runningStdin r) (requestBytes requestId op arguments)
                      hFlush (runningStdin r))
                :: IO (Either SomeException ())
      case sent of
        Right () -> pure ()
        -- Writing to a kernel that has gone is a broken pipe, which is news
        -- about the kernel rather than an error in the shell.
        Left _ -> do
          writeIORef (kernelRunning kernel) Nothing
          failEverything kernel "the kernel stopped answering"

outstanding :: Kernel -> IO Int
outstanding kernel = M.size <$> readIORef (kernelPending kernel)

-- | How long the kernel has kept the oldest unanswered request waiting.
waitingFor :: Kernel -> IO (Maybe Double)
waitingFor kernel = do
  waiting <- readIORef (kernelPending kernel)
  case oldest waiting of
    Nothing -> pure Nothing
    Just p -> do
      now <- getMonotonicTime
      pure (Just (now - pendingSent p))

-- | What the oldest unanswered request asked for.
waitingOp :: Kernel -> IO (Maybe String)
waitingOp kernel = fmap pendingOp . oldest <$> readIORef (kernelPending kernel)

oldest :: M.Map Int Pending -> Maybe Pending
oldest waiting = case M.elems waiting of
  [] -> Nothing
  ps -> Just (foldr1 earlier ps)
  where earlier a b = if pendingSent a <= pendingSent b then a else b

-- | Has the kernel been sitting on a request for longer than this?
--
-- This is what a cell that will not finish looks like from out here.  There is
-- no way to tell it apart from one that is merely slow -- that is the halting
-- problem, and Cellar is not going to solve it on a timer -- which is why the
-- shell asks the person rather than deciding by itself.
stalled :: Kernel -> Double -> IO Bool
stalled kernel seconds = maybe False (> seconds) <$> waitingFor kernel

-- | Treat everything outstanding as though it had just been sent.
--
-- Called when the kernel first answers anything.  Starting a process and
-- loading Guile into it takes a noticeable moment, and a request sent during
-- that has been waiting for reasons that have nothing to do with the cell it
-- carries.  Timing those from here is what stops a slow start -- or a
-- restart -- from being reported as a cell that will not finish.
markReady :: Kernel -> IO ()
markReady kernel = do
  now <- getMonotonicTime
  atomicModifyIORef' (kernelPending kernel)
    (\m -> (M.map (\p -> p { pendingSent = now }) m, ()))

-- Listening

-- | Hand out whatever the kernel has said.  Returns whether anything happened.
-- Called from the main loop, and does no reading itself: the reader thread has
-- already done that, so this is a swap of an 'IORef' and a few calls.
pump :: Kernel -> IO Bool
pump kernel = do
  queued <- atomicModifyIORef' (kernelInbox kernel) (\q -> ([], reverse q))
  forM_ queued (deliver kernel)
  pure (not (null queued))

deliver :: Kernel -> Message -> IO ()
deliver kernel message = case message of
  Reply requestId payload -> answer kernel requestId (Right payload)
  Failed requestId why -> answer kernel requestId (Left why)
  -- The reader thread's way of saying the pipe ended.
  Garbled "\0end" -> do
    alive <- kernelAlive kernel
    when alive $ do
      running <- readIORef (kernelRunning kernel)
      writeIORef (kernelRunning kernel) Nothing
      forM_ running $ \r -> ignore (void (waitForProcess (runningProcess r)))
      failEverything kernel "the kernel stopped without saying why"
  Garbled why -> failEverything kernel ("the kernel said something unreadable: " ++ why)

-- | Hand the answer to a request to whoever asked, and forget it.  An id that
-- is not outstanding is dropped: a reply to a request that was abandoned when
-- the kernel restarted is not a reason to do anything.
answer :: Kernel -> Int -> Either String Sexp -> IO ()
answer kernel requestId outcome = do
  found <- atomicModifyIORef' (kernelPending kernel) $ \waiting ->
    (M.delete requestId waiting, M.lookup requestId waiting)
  forM_ found $ \p -> case outcome of
    Left why -> pendingFail p why
    Right payload -> pendingContinue p payload
