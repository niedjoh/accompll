module RewritingAC(rewriteCommaAC, rewriteSlashAC, solveValidityProblem) where

import Data.Maybe (maybeToList)

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Rules.Rewrite (Reduct (..))

import qualified Data.Rewriting.Rules as Rs
import qualified Data.Rewriting.Rules.Rewrite as Rw
import qualified Data.Rewriting.Substitution as S

import Rewriting
import UtilsAC
import MatchingAC

{-
  fvs is an infinite list of "fresh" variables, fresh meaning
  that we assume that "変数i" for any natural number i is not
  used as a variable name

  note that 変数 (read: hensuu) is a Japanese word for variable.
-}
fvs = map (\i -> "変数" ++ show i) [1..]

rewriteCommaAC :: [String] -> RewriteFunction
rewriteCommaAC acss trs = markedRewriteAtRoot (matchAC acss fvs) trs

-- use R^e,AC (same normal forms modulo AC)
rewriteSlashAC :: [String] -> RewriteFunction
rewriteSlashAC acss trs = markedRewriteAtRoot (matchAC acss fvs) (extension acss trs)

-- if rs is AC-complete, this solves the validity problem s = t for rs 
solveValidityProblem :: [String] -> TRS -> Term String String -> Term String String ->
                        (Bool, Term String String, Term String String)
solveValidityProblem acss rs s t = (equivalentModuloAC acss s' t', s', t') where
  nfRs = nf normalRewriting rs
  s' = nfRs s
  t' = nfRs t
