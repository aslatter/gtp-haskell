
{-| A client for the Go Text Protocol.


 -}

import qualified Data.ByteString as B
import Data.Int

data Color = White | Black
data Location = Loc Char Int
data Vertex = Pass | Coord Location
data Move  = Move Color Vertex
-- no whitespace, entirely printable
newtype GTPString = GS B.ByteString

data Request = Req (Maybe Int32) GTPString
data Response = Res (Maybe Int32)

-- Client half of the protocol

data Connection = Connection

-- | Version of the GTP protocol
protocolVersion :: Connection -> IO Int
protocolVersion = undefined

-- | Name of the engine
name :: Connection -> IO [B.ByteString]
name = undefined

-- | Version of the engine
version :: Conncetion -> IO [B.ByteString]
version = undefined

knownCommand :: Connection -> B.ByteString -> IO Bool
knownCommand = undefined

listCommands :: Connection -> IO [B.ByteString]
listCommands = undefined

quit :: Connection -> IO [B.ByteString]
quit = undefined

boardsize :: Connection -> Int32 -> IO ()
boardsize = undefined

clearBoard :: Connection -> IO ()
clearBoard = undefined

komi :: Connection -> Float -> IO ()
komi = undefined

fixedHandicap :: Connection -> Int32 -> IO [Location]
fixedHandicap = undefined

placeFreeHandicap :: Connection -> Int32 -> IO [Location]
placeFreeHandicap = undefined

setFreeHandicap :: Connection -> [Location] -> IO ()
setFreeHandicap = undefined

play :: Connection -> Move -> IO ()
play = undefined

-- | Resignation is indicated by 'Nothing'
type MoveResult = Maybe Move

genmove :: Conection -> Color -> IO MoveResult
genmove = undefined


undo :: Connection -> IO ()
undo = undefined

-- timeSettings
-- timeLeft
-- finalScore
-- finalStatusList


loadsgf :: Connection -> String -> Int32 -> IO ()
loadsgf = undefined

regGenmove :: Connection -> Color -> IO MoveResult
regGenmove = undefined

showboard :: Connection -> IO [[B.ByteString]]
showboard = undefined

data GtpException
    = IllegalMove
    | Undo
    | SgfLoad
    | Unknown B.ByteString
 deriving (Data, Typeable, Show)

instance Exception GtpException where
  toException IllegalMove = toException IllegalMoveException
  toException Undo = toException UndoException
  toException SgfLoad = toException SgfLoadException
  toException (Unknown msg) = toException (GtpUnknownException msg)

  fromException (fromException -> Just IllegalMoveException)
      = Just IllegalMove
  fromException (fromException -> Just UndoException)
      = Just Undo
  fromException (fromException -> Just SgfLoadException)
      = Just SgfLoad
  fromException (fromException -> Just (GtpUnknownException msg))
      = Just (Unknown msg)

data IllegalMoveException = IllegalMoveException
 deriving (Data, Typeable, Show)
instance Excpetion IllegalMoveException

data UndoException = UndoException
 deriving (Data, Typeable, Show)
instance Exception UndoException

data SgfLoadException = SgfLoadException
 deriving (Data, Typeable, Show)
instance Exception SgfLoadException

data GtpUnknownException = GtpUnknownException B.ByteString
 deriving (Data, Typeable, Show)
instance Exception GtpUnknownException

toGtpEx :: B.ByteString -> GtpException
toGtpEx "illegal move" = IllegalMove
toGtpEx "cannot undo" = Undo
toGtpEx "cannot load file" = SgfLoad
toGtpEx msg = Unknown msg

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
mySkipWhile1 :: (Word8 -> Bool) -> Parser B.ByteString
mySkipWhile1 p = (takeWhile1 p *> pure ()) <|> fail "mySkipWhile1"

opt :: Parser a -> Parser (Maybe a)
opt p = (Just <$> p) <|> pure Nothing

-- | A less raw view of results coming back from the
-- engine
data ResultFrame
    = Error (Maybe Int) [B.ByteString]
    | Response (Maybe Int) [[B.ByteString]] -- multi words in multi lines

resultParser :: Parser ResultFrame
resultParser = parseSuccess <|> parseError

rejectChar '\127' = True
-- rejectChar '\13'  = False
rejectChar '\10'  = False
rejectChar '\9'   = False
rejectChar x
    | x <= '\31'  = True

convToSpace '\t' = True
convToSpace _    = False

-- This could be much lower level.
-- In addition, we could assume the other end
-- sends us optimally conformant data, and only
-- allocate new buffers when it is not.
fixChunk :: B.ByteString -> B.ByteString
fixChunk chunk =
    case B.unfoldrN len upd chunk of (chunk',_) -> chunk'
 where
   upd xs =
       case B.uncons xs of
         Nothing -> Nothing
         result@(Just (char, xs'))
             | rejectChar char
                 -> upd xs'
             | convToSpace char
                 -> Just (' ', xs')
             | otherwise
                 -> result
             
   len = B.length chunk


-- Horizontal tabs or a space charecter
gtpSpace :: Char -> Bool
gtpSpace ' '  = True
gtpSpace '\t' = True
gtpSpace _    = False

-- Skip one or more space or tab charecters
gtpSpaces1 :: Parser ()
gtpSpaces1 = mySkipWhile1 gtpSpace

parseSuccess :: Parser ResultFrame
parseSuccess =
    Response
      <$> (char '=' *> opt decimal <* gtpSpaces1)
      <*> parseResponseBody

parseError :: Parser ResultFrame
parseError =
    Error
      <$> (char '?' *> opt decimal <* gtpSpaces1)
      <*> parseErrorBody

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (::) <$> p <*> many (sep *> p)

parseErrorBody :: Parser [B.ByteString]
parseErrorBody = 

resultConduit :: Monad m => Conduit B.ByteString m ResultFrame
resultConduit = C.sequence (sinkParser resultParser)

-- | This is a weird sink - we sink the result frames into a queue of
-- MVars waiting for results. This is where we assume:
--  - The server delivers results in FIFO order
--  - The server delivers exactly one response per request
resultSink :: Chan PendingResponse -> Sink ResultFrame IO ()
resultSink responseQ =
    sinkState
      () -- no pure state
      (\_ frame -> do
         -- pull an mvar from the queue, write to it
         mvar <- readChan responseQ
         putMVar mvar $
           case frame of
             Error _ wds = Left $ toGtpEx $ B8.unwords wds
             Response _ bytes = Right bytes
         return $ StateProcessing ()
       )
      (\() -> return ()) -- no close
           

-- Caller is responsible for parsing on Right
type PendingResponse = MVar (Either GtpException [[B.ByteString]])
type PendingRequest = (B.ByteString, PendingResponse)

-- Quasi-blocking. This call does not return until a response comes
-- back from the server. However another thread may send a request
-- to the server while this request is in progress.
sendRequest :: Connection -> B.ByteString -> IO [[B.ByteString]]
sendRequest c req
    = do
  respVar <- newEmptyMVar
  writeChan (reqQueue c) (req, respVar)
  resp <- takeMVar respVar
  case resp of
    Left err -> throwIO err
    Right bytes -> return bytes

data Connection
    = C
      { reqTid  :: ThreadId, -- request loop
        respTid  :: ThreadId, -- response loop
        isClosed :: IORef Bool,
        reqQueue :: Chan PendingRequest
      }

-- | Returns the previous value of the closed flag
setClosedFlag :: Connection -> IO Bool
setClosedFlag c =
    atomicModifyIORef (isClosed c) $ \prev -> (True, prev)

closeConnection :: Connection -> IO ()
closeConnection c =
    wasClosed <- setClosedFlag c
    when (not wasClosed) $ do
      killThread $ respTid c
      killThread $ reqTid c

