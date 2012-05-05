
module Types

-- | Caller is responsible for parsing on Right
type PendingResponse = MVar (Either GtpException [[B.ByteString]])
type PendingRequest = (B.ByteString, PendingResponse)

data Connection
    = C
      { reqTid  :: ThreadId, -- request loop
        respTid  :: ThreadId, -- response loop
        isClosed :: IORef Bool,
        reqQueue :: Chan PendingRequest
      }

-- | A less raw view of results coming back from the
-- engine
data ResultFrame
    = Error (Maybe Int) GtpException
    | Response (Maybe Int) [[B.ByteString]] -- multi words in multi lines

-- | Exceptions that may be thrown by this library. Exceptions
-- may be caught either as a 'GtpException' or as any of the
-- more specific exception types.
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

