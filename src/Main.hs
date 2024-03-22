module Main where

import System.Environment
import System.IO
import System.Process
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)
import Control.Concurrent.Async (race)

import Text.PrettyPrint.ANSI.Leijen (putDoc, text)

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rules.Rewrite (fullRewrite)

import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Problem as P

import ParseUtils (parseTermPair)
import Utils
import Rewriting
import RewritingAC
import TTInterface
import Completion

parseSimpRel :: ReadM SimpRel
parseSimpRel = eitherReader $ \s -> case s of
  "R"    -> Right Standard
  "R,AC" -> Right RCommaAC
  "R/AC" -> Right RSlashAC
  _      -> Left "parse error; use R R,AC or R/AC"

parseVPOption ::ReadM (Maybe (Term String String, Term String String))
parseVPOption = eitherReader $ \s -> case parseTermPair s of
    Left err -> Left err
    Right pair -> Right $ Just pair

optionsParser :: Parser Options
optionsParser = Options
      <$> option parseSimpRel
          ( long "simp"
         <> short 's'
         <> metavar "R | R,AC | R/AC"
         <> help "rewrite relation for simplification"
         <> showDefault
         <> value Standard )
      <*> strOption
          ( long "tt"
         <> metavar "PATH"
         <> help "path to termination tool executable"
         <> showDefault
         <> value "./callNaTT.sh" )
      <*> option auto
          ( long "ti"
         <> metavar "NaTTXML | WST"
         <> help "input format for termination tool"
         <> showDefault
         <> value NaTTXML )
      <*> option (parseVPOption) -- ['x' : show i | i <- [1..]])
          ( long "vp"
         <> metavar "\"[VAR x y][s == t]\""
         <> help "validity problem (optional)"
         <> value Nothing)
      <*> switch
          ( long "interactive"
         <> short 'i'
         <> help "enable interactive termination tool mode" )
      <*> switch
          ( long "pcp"
         <> short 'p'
         <> help "use prime critical pairs" )
      <*> argument str
          ( metavar "FILE"
         <> help "input file in WST format" )
      <*> switch
          ( long "debug"
         <> short 'd'
         <> help "enable debug printing" )
      <*> option auto
         ( long "dt"
        <> metavar "LeftToRight | RightToLeft"
        <> help "which thread to debug"
        <> showDefault
        <> value LeftToRight )
      <*> option auto
          ( long "verb"
         <> short 'v'
         <> help "verbosity of debug output"
         <> showDefault
         <> value 1 )

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc (
              "Performs completion and outputs either YES + complete TRS or MAYBE. " ++
              "If an equation is given via the -vp option, the tool attempts to solve " ++
              "the corresponding validity problem which adds NO as a possible answer."
              )
  <> header "accompll - a tool performing AC-completion for left-linear TRSs" )

runCompletion :: TTInfo -> [String] -> Options -> RewriteFunction -> [Equation] -> OrientationPreference ->
                 IO (Maybe CompletionQuadruple)
runCompletion ttinfo acss opt rw es o = do
  ttinfo <- if (interactive ttinfo) then startTTProcess ttinfo else return ttinfo
  res <- complete ttinfo acss ["x","y","z"] opt rw o (es,[],[],[])
  when (interactive ttinfo) $ do
    hClose (hin ttinfo)
    -- waitForProcess (hproc ttinfo) this does not work due to TTT2 interactive mode restrictions
    terminateProcess (hproc ttinfo)
    return ()
  return res

main :: IO ()
main = do
  args <- execParser opts
  p <- P.parseFileIO (inputFile args)
  let es = P.allRules (P.rules p)
  let acss = acSymbols (P.theory p)
  let rw = case simpRel args of
        Standard -> normalRewriting
        RCommaAC -> rewriteCommaAC acss
        RSlashAC -> rewriteSlashAC acss
  let ttinfo = TTInfo { path = terminationTool args
                      , inputFormat = ttInputFormat args
                      , interactive = interactiveMode args
                      , hin = undefined
                      , hout = undefined
                      , herr = undefined
                      , hproc = undefined
                      }
  let complete = runCompletion ttinfo acss args rw es
  eitherRes <- case debug args of
    False -> race (complete LeftToRight) (complete RightToLeft)
    True -> case debugThread args of
      LeftToRight -> Left <$> complete LeftToRight
      RightToLeft -> Right <$> complete RightToLeft
  case fromEither eitherRes of
    Nothing -> putStrLn "MAYBE"
    Just (_,_,trs,_) -> let
        res = problem trs acss
      in case mTermPair args of
        Nothing -> do
          putStrLn "YES"
          putDoc $ P.prettyWST text text res
        Just (s,t) -> do
          let (valid,s',t') = solveValidityProblem acss trs s t normalRewriting
          if valid then putStrLn "YES" else putStrLn "NO"
          putStrLn ""
          putDoc $ T.prettyTerm text text s
          putStr " ->^! "
          putDoc $ T.prettyTerm text text s'
          putStrLn ""
          putDoc $ T.prettyTerm text text t
          putStr " ->^! "
          putDoc $ T.prettyTerm text text t'
          putStrLn "\n"
          putStrLn "Complete TRS:"
          putDoc $ P.prettyWST text text res
