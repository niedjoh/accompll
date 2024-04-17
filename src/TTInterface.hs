module TTInterface (TTInfo (..), TTAnswer (..), startTTProcess, invokeTT) where

import System.IO
import System.Process
import Data.List (isPrefixOf)
import Text.PrettyPrint.ANSI.Leijen (hPutDoc, text)
import Control.Monad (when)

import qualified Data.Rewriting.Problem as P
import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Rules.Ops as RsOs
import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rule (Rule (..))

import Utils

data TTInfo = TTInfo { path :: String
                     , inputFormat :: TTInputFormat
                     , interactive :: Bool
                     , hin :: Handle
                     , hout :: Handle
                     , herr :: Handle
                     , hproc :: ProcessHandle
                     }

data TTAnswer = Yes | No | Error String deriving Show

encodeVarsXML :: Show v => [v] -> String
encodeVarsXML = foldr ((++) . (\v -> "<var>" ++ show v ++ "</var>")) []

encodeACSymbolsXML :: Show f => [f] -> String
encodeACSymbolsXML = foldr ((++) . (\f -> "<fun theory=\"AC\">" ++ show f ++ "</fun>")) []

encodeTermXML :: (Show f, Show v) => Term f v -> String
encodeTermXML = T.fold (\v -> "<sym>" ++ show v ++ "</sym>")
                   (\f xs -> "<app><sym>" ++ show f ++ "</sym>" ++ concat xs ++ "</app>") 

encodeTrsXML :: (Show f, Show v) => [Rule f v] -> String
encodeTrsXML =
  foldr ((++) . (\r -> "<rule>" ++ encodeTermXML (R.lhs r) ++ encodeTermXML (R.rhs r) ++ "</rule>")) []

encodeXML :: (Show f, Show v) => [Rule f v] -> [f] -> String
encodeXML trs acss =
  "<trs>" ++ encodeVarsXML (RsOs.vars trs) ++ encodeACSymbolsXML acss ++ encodeTrsXML trs ++ "</trs>"

answerAvailable :: String -> Bool
answerAvailable out =
  isPrefixOf "YES" out || isPrefixOf "NO" out || isPrefixOf "MAYBE" out || isPrefixOf "TIMEOUT" out

decode :: String -> String -> TTAnswer
decode out err = if isPrefixOf "YES" out then Yes else
                 if isPrefixOf "NO" out || isPrefixOf "MAYBE" out || isPrefixOf "TIMEOUT" out || out == "" then No
                 else Error ("Termination Tool: " ++ out ++ err)

startTTProcess :: TTInfo -> IO TTInfo
startTTProcess ttinfo = do
  (Just hin, Just hout, Just herr, hproc) <-
    createProcess (proc (path ttinfo) []){ std_in = CreatePipe
                                         , std_out = CreatePipe
                                         , std_err = CreatePipe
                                         }
  return ttinfo{hin = hin, hout = hout, herr = herr, hproc = hproc}

invokeTT :: TTInfo -> [String] -> [Rule String String] -> IO TTAnswer
invokeTT ttinfo acss trs = do
  let i = interactive ttinfo
  ttinfo <- if i then return ttinfo else startTTProcess ttinfo
  case inputFormat ttinfo of
    NaTTXML -> hPutStrLn (hin ttinfo) (encodeXML trs acss)
    WST     -> hPutDoc (hin ttinfo) (P.prettyWST text text (problem trs acss))
  if i then do {hPutStrLn (hin ttinfo) "(RUN)"; hFlush (hin ttinfo)} else hClose (hin ttinfo)
  out <- if i then hGetLine (hout ttinfo) else hGetContents (hout ttinfo)
  when (not i) $ do {_ <- waitForProcess (hproc ttinfo); return ()}
  eofReached <- if i then return False else hIsEOF (herr ttinfo)
  errAvail <- if eofReached then return False else hReady (herr ttinfo)
  err <- if errAvail then hGetLine (herr ttinfo) else return ""
  return $ decode out err
