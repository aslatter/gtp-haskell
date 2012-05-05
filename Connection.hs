
module Connection
    ( Connection(..)
    , senRequest
    , closeConnection
    ) where

import Types
import Parser

-- | Quasi-blocking. This call does not return until a response comes
-- back from the server. However another thread may send a request
-- to the server while this request is in progress.
-- The caller supplies the exact message we should put on the wire.
-- Any errors returned from the server are thrown.
-- Responses from the server are tokenized into lines and words per
-- the protocol spec. Further parsing is at the discretion of the caller.
sendRequest :: Connection -> B.ByteString -> IO [[B.ByteString]]
sendRequest c req
    = do
  respVar <- newEmptyMVar
  writeChan (reqQueue c) (req, respVar)
  resp <- takeMVar respVar
  case resp of
    Left err -> throwIO err
    Right bytes -> return bytes


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

data RawConnection = RC
    { rcSendAll :: B.ByteString -> IO ()
    , rcClose   :: IO ()
    , rcReceive :: IO B.ByteString 
    }

openConnectionRaw :: RawConnection -> IO Connection
openConnectionRaw rc = do
  -- we have a thread which sends requests and
  -- a thread which receives responses.
  -- they communicate with a channel of sent requests.
  -- respQueue :: Chan PendingResponse
  respQueue <- newChan
  respTid <- forkIO $ runResourceT $
               -- register close action to fill MVars with errors?
               rcSource rc $= resultConduit $$ resultSink respQueue
  reqTid <- forkIO undefined
  isClosed <- newIORef False
  return $ C reqTid respTid isClosed respQueue
  
rcSource :: RawConnection -> Source (ResourceT IO) B.ByteString
rcSource rc =
    sourceIO
      (return ())
      (\_ -> rcClose rc)
      (\_ -> IOOpen <$> rcReceive rc)

resultConduit :: Monad m => Conduit B.ByteString m ResultFrame
resultConduit = C.sequence (sinkParser resultParser)

-- | This is a weird sink - we sink the result frames into a queue of
-- MVars waiting for results. This is where we assume:
--  - The server delivers results in FIFO order
--  - The server delivers exactly one response per request
resultSink :: Chan PendingResponse -> Sink ResultFrame (ResourceT IO) ()
resultSink respQueue =
    sinkState
      () -- no pure state
      (\_ frame -> liftIO $ do
         -- pull an mvar from the queue, write to it
         mvar <- readChan respQueue
         putMVar mvar $
           case frame of
             Error _ wds = Left $ toGtpEx $ B8.unwords wds
             Response _ bytes = Right bytes
         return $ StateProcessing ()
       )
      (\() -> return ()) -- no close
           
