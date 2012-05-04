
module Connection
    ( Connection(..)
    , senRequest
    , closeConnection
    ) where


-- | Quasi-blocking. This call does not return until a response comes
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

