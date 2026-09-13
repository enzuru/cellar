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
  , Reply (..)
  , startKernel
  , kernelAlive
  , call
  , reserve
  , sendRequest
  , takeReplies
  , awaitReplies
  , outstanding
  , waitingFor
  , waitingOp
  , stalled
  , markReady
  , stopKernel
  , restartKernel
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import qualified Data.ByteString as B
import Data.IORef
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import GHC.Clock (getMonotonicTime)
import System.IO
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process

import Cellar.Protocol
import Cellar.Sexp

-- | What the kernel said about a request.
--
-- Which request is a number rather than a continuation: the shell holds what
-- it asked for and why, and this says only what came back.  That is what lets
-- the answers arrive as events rather than as calls into whatever was in scope
-- when the question was asked.
data Reply
  = Answered Int Sexp     -- ^ The reply to this request.
  | Refused Int String    -- ^ Why this request will not be answered.
  deriving (Eq, Show)

-- | A request that has been sent and not yet answered.
data Pending = Pending
  { pendingSent :: Double
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
    -- | Rung whenever something goes in the inbox, so that a thread can wait
    -- on the kernel rather than ask after it on a timer.  One ring stands for
    -- any number of messages: what it says is "there is something", and the
    -- taker finds out how much.
  , kernelDoorbell :: MVar ()
    -- | Requests that will not be answered: ones sent to a kernel that was not
    -- running, and ones abandoned when it stopped.  They are replies like any
    -- other, but they come from this side of the pipe.
  , kernelRefused :: IORef [Reply]
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
    <*> newEmptyMVar
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
              unless (null messages) $ do
                atomicModifyIORef' (kernelInbox kernel)
                  (\queued -> (reverse messages ++ queued, ()))
                ring kernel
              go decoder'

    -- End of file: the kernel has gone.  Said as a message so that the main
    -- loop learns about it in the same place it learns about everything else,
    -- rather than by inspecting the process from a timer.
    deliverEnd = do
      atomicModifyIORef' (kernelInbox kernel)
        (\queued -> (Garbled "\0end" : queued, ()))
      ring kernel

-- | Say that there is something to take.  Never blocks: a bell already rung
-- says the same thing as one rung twice.
ring :: Kernel -> IO ()
ring kernel = void (tryPutMVar (kernelDoorbell kernel) ())

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

-- | Refuse everything outstanding, because the kernel will not be answering
-- it.  The refusals queue like any other reply.
-- | Refuse everything outstanding, because the kernel will not be answering
-- it.  Nothing is waiting afterwards, and the refusals are on their way.
failEverything :: Kernel -> String -> IO ()
failEverything kernel why = do
  waiting <- atomicModifyIORef' (kernelPending kernel) (\m -> (M.empty, m))
  forM_ (M.keys waiting) $ \requestId -> refuse kernel requestId why

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

-- | Ask the kernel to do something, and answer with the number of the
-- request.  Returns at once, having sent nothing but bytes.
--
-- What comes back comes back through 'takeReplies', under that number.  A
-- kernel that is not running is not an error here: the request is refused, and
-- the refusal arrives the same way an answer would, so the caller has one
-- place to hear about it rather than two.
call :: Kernel -> String -> [Sexp] -> IO Int
call kernel op arguments = do
  requestId <- reserve kernel
  sendRequest kernel requestId op arguments
  pure requestId

-- | Take the next request number without sending anything.
--
-- For a caller that has to write down what a request is for before the answer
-- can arrive: the kernel is quick and another thread is reading the pipe, so
-- an answer can be in hand before a caller that numbered and sent in one step
-- has had a chance to say what it asked.
reserve :: Kernel -> IO Int
reserve kernel = atomicModifyIORef' (kernelNextId kernel) (\n -> (n + 1, n + 1))

-- | Send a request that has already been given a number.
sendRequest :: Kernel -> Int -> String -> [Sexp] -> IO ()
sendRequest kernel requestId op arguments = do
  running <- readIORef (kernelRunning kernel)
  case running of
    Nothing -> refuse kernel requestId "the kernel is not running"
    Just r -> do
      now <- getMonotonicTime
      atomicModifyIORef' (kernelPending kernel)
        (\m -> (M.insert requestId (Pending now op) m, ()))
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

-- | Put a refusal in the queue, for a request the kernel will not answer.
refuse :: Kernel -> Int -> String -> IO ()
refuse kernel requestId why = do
  atomicModifyIORef' (kernelRefused kernel)
    (\queued -> (Refused requestId why : queued, ()))
  ring kernel

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

-- | Take whatever the kernel has said, and say nothing if it has said nothing.
--
-- The reader thread has already done the reading, so this is a swap of an
-- 'IORef' and a little bookkeeping.  A reply to a request that is no longer
-- outstanding is dropped: an answer to something abandoned when the kernel
-- restarted is not a reason to do anything.
takeReplies :: Kernel -> IO [Reply]
takeReplies kernel = do
  refused <- atomicModifyIORef' (kernelRefused kernel) (\q -> ([], reverse q))
  queued <- atomicModifyIORef' (kernelInbox kernel) (\q -> ([], reverse q))
  answers <- concat <$> mapM (interpret kernel) queued
  pure (refused ++ answers)

-- | Wait until the kernel has said something, and take it.
--
-- This is what a thread of its own does with a kernel: block here, hand on
-- what comes back, and block again.  Nothing is polled and nothing is timed.
awaitReplies :: Kernel -> IO [Reply]
awaitReplies kernel = do
  takeMVar (kernelDoorbell kernel)
  replies <- takeReplies kernel
  -- One ring can stand for messages that a previous take already carried off,
  -- so an empty handful means wait again rather than hand back nothing.
  if null replies then awaitReplies kernel else pure replies

interpret :: Kernel -> Message -> IO [Reply]
interpret kernel message = case message of
  Reply requestId payload -> forget requestId (Answered requestId payload)
  Failed requestId why -> forget requestId (Refused requestId why)
  -- The reader thread's way of saying the pipe ended.
  Garbled "\0end" -> do
    alive <- kernelAlive kernel
    if not alive then pure [] else do
      running <- readIORef (kernelRunning kernel)
      writeIORef (kernelRunning kernel) Nothing
      forM_ running $ \r -> ignore (void (waitForProcess (runningProcess r)))
      refusals "the kernel stopped without saying why"
  Garbled why -> refusals ("the kernel said something unreadable: " ++ why)
  where
    -- A reply is worth handing on only if somebody is still waiting for it.
    forget requestId reply = do
      found <- atomicModifyIORef' (kernelPending kernel) $ \waiting ->
        (M.delete requestId waiting, M.lookup requestId waiting)
      pure [reply | isJust found]
    refusals why = do
      waiting <- atomicModifyIORef' (kernelPending kernel) (\m -> (M.empty, m))
      pure [Refused requestId why | requestId <- M.keys waiting]
