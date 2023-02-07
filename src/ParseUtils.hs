module ParseUtils where

import Text.Parsec.String (Parser)
import Text.Parsec (many, many1, parse)
import Text.Parsec.Char (oneOf, noneOf, spaces, char, string)

import Data.Rewriting.Term (Term (..))
import qualified Data.Rewriting.Term.Parse as Tp

term :: Parser String
term = do
  spaces
  t <- many1 $ noneOf " \t\n\r\f\v=[]"
  spaces
  return t

vpParser :: Parser ([String],String,String)
vpParser = do
  char '['
  spaces
  string "VAR"
  vs <- many term
  char ']'
  char '['
  s1 <- term
  string "=="
  s2 <- term
  char ']'
  return (vs,s1,s2)

parseTermPair :: String -> Either String (Term String String, Term String String)
parseTermPair s = case parse vpParser "" s of
  Left err -> Left $ show err
  Right (vs,s1,s2) -> case Tp.fromString vs s1 of
    Left err -> Left $ show err
    Right t1 -> case Tp.fromString vs s2 of
      Left err -> Left $ show err
      Right t2 -> Right (t1,t2)
