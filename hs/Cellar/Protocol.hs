-- | What the two halves of Cellar say to each other.
--
-- A message is its byte count, a newline, and then that many bytes of UTF-8:
--
-- >  47
-- >  (request 12 set-cell "Summary" "D6" "(* 6 7)")
--
-- The count is what makes the shell able to read without ever blocking.
-- Reading a datum straight off a pipe means waiting until a whole one has
-- arrived, and a window that does that has handed its responsiveness to
-- another process -- which is the one thing this split exists to prevent.
-- With a count in front, the shell can look at what has turned up, decide
-- whether a whole message is there, and go back to drawing if it is not.
--
-- > shell -> kernel   (request <id> <op> <argument> ...)
-- > kernel -> shell   (reply <id> <alist>)
-- >                   (fail <id> "what went wrong")
module Cellar.Protocol
  ( Message (..)
  , Decoder
  , newDecoder
  , feed
  , encode
  , requestBytes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Cellar.Sexp

-- | What the kernel says back.  Anything that is neither a reply nor a
-- refusal is a 'Garbled', which the caller reports rather than crashes on: the
-- kernel is a separate program and a version of it that spoke nonsense would
-- otherwise take the window down with it.
data Message
  = Reply Int Sexp
  | Failed Int String
  | Garbled String
  deriving (Eq, Show)

-- | Bytes that have arrived and not yet made a whole message.
newtype Decoder = Decoder B.ByteString

newDecoder :: Decoder
newDecoder = Decoder B.empty

-- | Add whatever just came off the pipe, and take back the messages that
-- completed -- which may be none, and usually is.
feed :: Decoder -> B.ByteString -> (Decoder, [Message])
feed (Decoder pending) more = go (pending <> more) []
  where
    go buffer acc = case takeOne buffer of
      Nothing -> (Decoder buffer, reverse acc)
      Just (message, rest) -> go rest (message : acc)

-- | The first whole message in a buffer, and what is left.
takeOne :: B.ByteString -> Maybe (Message, B.ByteString)
takeOne buffer = do
  newline <- C.elemIndex '\n' buffer
  let (header, afterHeader) = B.splitAt newline buffer
      body = B.drop 1 afterHeader
      digits = C.unpack (C.filter (not . (`elem` (" \r" :: String))) header)
  count <- if not (null digits) && all isDigit digits
             then Just (read digits :: Int)
             else Just (-1)  -- a length we cannot read; reported below
  if count < 0
    then Just (Garbled ("a message whose length was " ++ show digits), body)
    else if B.length body < count
      then Nothing
      else
        let (payload, rest) = B.splitAt count body
        in Just (interpret payload, rest)

interpret :: B.ByteString -> Message
interpret payload =
  case TE.decodeUtf8' payload of
    Left _ -> Garbled "a message that was not UTF-8"
    Right text -> case parseSexp (T.unpack text) of
      Left why -> Garbled why
      Right value -> case toList value of
        Just [Sym "reply", Num n, body] -> Reply (fromIntegral n) body
        Just [Sym "fail", Num n, Str why] -> Failed (fromIntegral n) why
        _ -> Garbled ("a message that is neither a reply nor a failure: "
                      ++ take 120 (writeSexp value))

-- | Frame a datum for the wire.
encode :: Sexp -> B.ByteString
encode value =
  let payload = TE.encodeUtf8 (T.pack (writeSexp value))
  in C.pack (show (B.length payload)) <> C.singleton '\n' <> payload

-- | A request, framed and ready to write.
requestBytes :: Int -> String -> [Sexp] -> B.ByteString
requestBytes requestId op arguments =
  encode (list (Sym "request" : Num (fromIntegral requestId) : Sym op : arguments))
