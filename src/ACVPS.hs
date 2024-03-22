module Main where

import System.Environment
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)

import Text.PrettyPrint.ANSI.Leijen (putDoc, text)

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rules.Rewrite (fullRewrite)

import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Problem as P

import ParseUtils (parseTermPair)
import Utils (SimpRel(..))
import qualified Utils as U
import Rewriting
import RewritingAC

data Options = Options
  { simpRel         :: SimpRel
  , mTermPair       :: Maybe (Term String String, Term String String)
  , inputFile       :: String
  } deriving Show

parseSimpRel :: ReadM SimpRel
parseSimpRel = eitherReader $ \s -> case s of
  "R"    -> Right Standard
  "R/AC" -> Right RSlashAC
  _      -> Left "parse error; use R or R/AC"

parseVPOption ::ReadM (Maybe (Term String String, Term String String))
parseVPOption = eitherReader $ \s -> case parseTermPair s of
    Left err -> Left err
    Right pair -> Right $ Just pair

optionsParser :: Parser Options
optionsParser = Options
      <$> option parseSimpRel
          ( long "simp"
         <> short 's'
         <> metavar "R | R/AC"
         <> help "rewrite relation for simplification"
         <> showDefault
         <> value Standard )
      <*> option (parseVPOption) -- ['x' : show i | i <- [1..]])
          ( long "vp"
         <> metavar "\"[VAR x y][s == t]\""
         <> help "validity problem"
         <> value Nothing)
      <*> argument str
          ( metavar "FILE"
         <> help "input file in WST format" )

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc (
              "Validity Problem Solver for AC complete systems"
              )
  <> header "acvps" )

main :: IO ()
main = do
  args <- execParser opts
  p <- P.parseFileIO (inputFile args)
  let rs = P.allRules (P.rules p)
  let acss = U.acSymbols (P.theory p)
  let prob = U.problem rs acss
  let rw = case simpRel args of
        Standard -> normalRewriting
        RSlashAC -> rewriteSlashAC acss
  case mTermPair args of
    Nothing -> do
      putStrLn "No equation given."
      putStrLn "TRS:"
      putDoc $ P.prettyWST text text prob
    Just (s,t) -> do
          let (valid,s',t') = solveValidityProblem acss rs s t rw
          if valid then putStrLn "YES" else putStrLn "NO"
          putStrLn ""
          putDoc $ T.prettyTerm text text s
          putStr " ->^! "
          putDoc $ T.prettyTerm text text s'
          putStrLn ""
          putDoc $ T.prettyTerm text text t
          putStr " ->^! "
          putDoc $ T.prettyTerm text text t'
          putStrLn ""
          putStrLn "TRS:"
          putDoc $ P.prettyWST text text prob
