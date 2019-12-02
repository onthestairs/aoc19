module Parsing where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = Parsec Void Text a

runParseFile :: String -> Parser a -> IO (Maybe a)
runParseFile filename parser = do
  input <- readFileText filename
  let result = runParser parser filename input
  pure $ rightToMaybe result

parseInteger :: Parser Int
parseInteger = L.decimal
