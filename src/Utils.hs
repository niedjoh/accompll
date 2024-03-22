module Utils where

import Data.List (nub)
import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Problem (Problem (..))

import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Rules as Rs
import qualified Data.Rewriting.Problem as P

import Text.PrettyPrint.ANSI.Leijen (Doc,text,vsep,line,(<>))

import ParseUtils

data SimpRel = Standard | RCommaAC | RSlashAC deriving Eq

instance Show SimpRel where
  show Standard = "R"
  show RCommaAC = "R,AC"
  show RSlashAC = "R/AC"

data TTInputFormat = NaTTXML | WST deriving (Eq, Show, Read)

data OrientationPreference = LeftToRight | RightToLeft deriving (Eq, Show, Read)

data Options = Options
  { simpRel         :: SimpRel
  , terminationTool :: String
  , ttInputFormat   :: TTInputFormat
  , mTermPair       :: Maybe (Term String String, Term String String)
  , interactiveMode :: Bool
  , enablePCP       :: Bool
  , inputFile       :: String
  , debug           :: Bool
  , debugThread     :: OrientationPreference
  , verbosity       :: Int
  } deriving Show

prettyRs :: String -> [Rule String String] -> Doc
prettyRs sep rs = (vsep $ map (R.prettyRule (text sep) text text) rs) <> line

acSymbols :: Maybe [P.Theory f v] -> [f]
acSymbols (Just ths) = concat $ go ths where
  go (P.SymbolProperty "AC" symbs : ths) = symbs : go ths
  go (_ : ths) = go ths
  go [] = []
acSymbols _ = []

problem :: Eq v => [Rule f v] -> [f] -> P.Problem f v
problem trs acss = Problem
  { P.startTerms = P.AllTerms
  , P.strategy = P.Full
  , P.theory = Just [P.SymbolProperty "AC" acss]
  , P.rules = P.RulesPair
    { P.strictRules = trs
    , P.weakRules = []
    }
  , P.variables = nub $ Rs.vars trs
  , P.symbols = []
  , P.signature = Nothing
  , P.comment = Nothing
  }

splitTRS :: (Rule f v -> Bool) -> [Rule f v] -> ([Rule f v], [Rule f v])
splitTRS p trs = go [] [] trs where
  go rs1 rs2 [] = (rs1, rs2)
  go rs1 rs2 (r:rs) = if p r then go (r:rs1) rs2 rs else go rs1 (r:rs2) rs

fromEither :: Either a a -> a
fromEither (Left x)  = x
fromEither (Right x) = x
