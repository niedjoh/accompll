module UtilsAC where

import Data.List (nub, sort, intercalate)

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rule (Rule (..))

import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Rule as R

import Rewriting (TRS, renameVars, rootSymbol)
import Utils (fromEither)

-- first argument should be list of three different variables
rulesAC :: [String] -> [String] -> TRS
rulesAC acss vs =
  let
    c1 f = Fun f [Var $ vs !! 0, Var $ vs !! 1]
    c2 f = Fun f [Var $ vs !! 1, Var $ vs !! 0]
    a1 f = Fun f [Var $ vs !! 0, Fun f [Var $ vs !! 1, Var $ vs !! 2]]
    a2 f = Fun f [Fun f [Var $ vs !! 0, Var $ vs !! 1], Var $ vs !! 2]
  in
    [Rule {lhs = c1 f, rhs = c2 f} | f <- acss] ++
    [Rule {lhs = a1 f, rhs = a2 f} | f <- acss] ++
    [Rule {lhs = a2 f, rhs = a1 f} | f <- acss]

-- compute R^e given R
extension :: [String] -> TRS -> TRS
extension acss trs = trs ++ map extend rs where
  lhsRootSymbol r = rootSymbol $ lhs r
  rs = filter (\r -> lhsRootSymbol r `elem` acss) trs
  extend r =
    let
      f  = lhsRootSymbol r
      r' = renameVars r
      i  = length . nub $ R.vars r'
      v  = 'x' : show (i+1)
    in
      Rule { lhs = Fun f [lhs r, Var v], rhs = Fun f [rhs r, Var v] }

-- flatten AC symbols and impose total order on arguments of AC symbols
canonicalRepr :: (Ord f, Ord v) => [f] -> Term f v -> Term f v
canonicalRepr acss t = sortArgs . go $ t where
  sortArgs (Fun f ts) =
    if   f `elem` acss
    then Fun f $ sort (map sortArgs ts)
    else Fun f $ map sortArgs ts
  sortArgs t@(Var x) = t
  go (Fun f [t@(Fun g  ts1), s@(Fun h ts2)]) =
    if   not $ f `elem` acss 
    then Fun f [go t, go s]
    else if   f == g && g == h
         then Fun f $ goList f (ts1 ++ ts2)
         else if   f == g
              then Fun f $ goList f ts1 ++ [go s]
              else if   f == h
                   then Fun f $ go t : goList f ts2
                   else Fun f [go t, go s]
  go (Fun f [t@(Fun g ts1), s@(Var x)]) =
    if   f == g && f `elem` acss
    then Fun f $ goList f ts1 ++ [s]
    else Fun f [go t, s]
  go (Fun f [t@(Var x), s@(Fun g ts2)]) =
    if   f == g && f `elem` acss
    then Fun f $ t : goList f ts2
    else Fun f [t, go s]
  go (Fun f ts) = Fun f (map go ts)
  go t@(Var x)  = t
  goList f ts = concat [aux f t | t <- ts]
  aux f t@(Fun g ss) = if f == g then goList f ss else [go t] 
  aux f t@(Var x) = [t]

equivalentModuloAC :: (Ord f, Ord v) => [f] -> Term f v -> Term f v -> Bool
equivalentModuloAC acss s t = s' == t' where
  s' = canonicalRepr acss s
  t' = canonicalRepr acss t
