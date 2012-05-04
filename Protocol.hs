
{-| A client for the Go Text Protocol.


 -}

import qualified Data.ByteString as B
import Data.Int

import Types

data Color = White | Black
data Location = Loc Char Int
data Vertex = Pass | Coord Location
data Move  = Move Color Vertex
-- no whitespace, entirely printable
newtype GTPString = GS B.ByteString

data Request = Req (Maybe Int32) GTPString
data Response = Res (Maybe Int32)

-- Client half of the protocol

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
           


