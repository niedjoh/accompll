module Rewriting where

import Data.List (nub, intercalate,sortOn)

import qualified Data.Map as M
import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Rules as Rs
import qualified Data.Rewriting.CriticalPair as C
import qualified Data.Rewriting.Substitution.Type as S
import qualified Data.Rewriting.Substitution.Match as Sm

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Rules.Rewrite(Reduct (..))
import Data.Rewriting.CriticalPair (CP)

import Utils (fromEither)

type Equation = Rule String String
type TRS = [Rule String String]

termSize :: Term f v -> Int
termSize = T.fold (\v -> 1) (\f xs -> 1 + sum xs)

ruleSize :: Rule f v -> Int
ruleSize r = termSize (lhs r) + termSize (rhs r)

sortBySize :: [Rule f v] -> [Rule f v]
sortBySize = sortOn ruleSize

-- if function and variable symbols have same type
rootSymbol :: Term a a -> a
rootSymbol = fromEither . T.root

flipRules :: TRS -> TRS
flipRules = map (\e -> Rule {lhs = rhs e, rhs = lhs e})

renameVars :: Eq v => Rule f v -> Rule f String
renameVars e = R.rename ren e where
  enum = zip (nub $ R.vars e) [1..]
  ren v = case lookup v enum of {Just i -> 'x': show i; Nothing -> undefined}

extractCP :: CP String String String -> Equation
extractCP cp = renameVars $ Rule {lhs = cpLhs, rhs = cpRhs} where
  cpLhs = C.left cp
  cpRhs = C.right cp

critPairs :: TRS -> TRS -> [Equation]
critPairs rs1 rs2 = [extractCP $ cp | cp <- C.cps rs1 rs2]

critPairs' :: TRS -> [Equation]
critPairs' rs = [extractCP  $ cp | cp <- C.cps' rs]

extractPCP :: TRS -> CP String String String -> [Equation]
extractPCP rs cp = case T.subtermAt (C.top cp) (C.leftPos cp) of
  Just (Fun _ ts) -> if   all (\t -> null $ Rs.fullRewrite rs t) ts
                     then [renameVars $ Rule {lhs = C.left cp, rhs = C.right cp}]
                     else []

-- the first argument is the set of rules wrt. which primality is tested
primeCritPairs :: TRS -> TRS -> TRS -> [Equation]
primeCritPairs rs rs1 rs2 = [pcp | cp <- C.cps rs1 rs2, pcp <- extractPCP rs cp]

-- the first argument is the set of rules wrt. which primality is tested
primeCritPairs' :: TRS -> TRS -> [Equation]
primeCritPairs' rs rs1 = [pcp | cp <- C.cps' rs1, pcp <- extractPCP rs cp]

criticalPairs :: Bool -> TRS -> TRS -> TRS -> [Equation]
criticalPairs True rs = primeCritPairs rs
criticalPairs False _ = critPairs

criticalPairs' :: Bool -> TRS -> TRS -> [Equation]
criticalPairs' True rs = primeCritPairs' rs
criticalPairs' False _ = critPairs'

-- marked innermost rewriting & bottom-up construction of normal forms

type Substitution = S.GSubst String String String
type MatchingFunction = Term String String -> Term String String -> Maybe (Substitution) 
type RewriteFunction = TRS -> Term String String -> MarkedTerm


data MarkedTerm = MFun String [MarkedTerm] | NF (Term String String)

mark :: Term String String -> MarkedTerm
mark = T.fold (\v -> NF (Var v)) (\f ts -> MFun f ts)

markedSubstitute :: Term String String -> Substitution -> MarkedTerm
markedSubstitute v@(Var x) sigma = case M.lookup x (S.toMap sigma) of
  Just t -> NF t
  Nothing -> NF v
markedSubstitute (Fun f ts) sigma = MFun f [markedSubstitute t sigma | t <- ts]

markedRewriteAtRoot :: MatchingFunction -> TRS -> Term String String -> MarkedTerm
markedRewriteAtRoot mf trs s =
  case [markedSubstitute (rhs r) sigma | r <- trs, Just sigma <- [mf (lhs r) s]] of
    mt : _ -> mt
    _     -> NF s

normalRewriting :: RewriteFunction
normalRewriting trs = markedRewriteAtRoot Sm.match trs

nf' :: RewriteFunction -> TRS -> MarkedTerm -> Term String String
nf' rw trs (NF s) = s
nf' rw trs (MFun f mts) = case rw trs (Fun f [nf' rw trs mt | mt <- mts]) of
  NF t -> t
  ms'  -> nf' rw trs ms'

nf :: RewriteFunction -> TRS -> Term String String -> Term String String
nf rw trs t = nf' rw trs (mark t)
