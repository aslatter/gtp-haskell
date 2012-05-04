
module Parser where

import Types

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
parseErrorBody = undefined

