module MatchingAC (matchAC) where

import Control.Exception (assert)
import Data.Map (Map)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List ((\\), sort)

import qualified Data.Map as M
import qualified Data.List as L

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Substitution (Subst (..))

import qualified Data.Rewriting.Substitution as S
import qualified Data.Rewriting.Substitution.Type as St

import UtilsAC (canonicalRepr, equivalentModuloAC)

-- implementation of Contejean's certified AC matching algorithm
-- paper: https://doi.org/10.1007/978-3-540-25979-4_5

type MatchingProblem f v = ( [(Term f v, Term f v)] -- unsolved part
                           , Map v (f, v, Term f v) -- partly solved part
                           , [v]                    -- stack
                           , Map v (Term f v)       -- solved part
                           )
type Solution f v = (Map v (Term f v), MatchingProblem f v)

tryAll :: [Maybe a] -> Maybe a
tryAll = listToMaybe . catMaybes

-- needed when the + ... + notation from the paper collapses to a single term (ts will not be empty)
term :: f -> [Term f v] -> Term f v
term f ts = if   length ts == 1
            then head ts
            else Fun f ts

-- the following functions correspond to the inference rules in Fig. 1 of the paper

initialize :: (Ord f, Ord v) => [f] -> Term f v -> Term f v -> MatchingProblem f v
initialize acss s t = ([(canonicalRepr acss s, canonicalRepr acss t)], M.empty, [], M.empty)

extractS :: Ord v => Solution f v -> Maybe (Solution f v)
extractS (sigma, ([], p, vs, s)) = Just (M.union s sigma, ([], p, vs, M.empty))
extractS _ = Nothing

extractP :: Ord v => Solution f v -> Maybe (Subst f v)
extractP sol@(_, (_, _, _, s)) = if M.null s then go sol else Nothing where
  go (sigma, ([], p, y:vs, s)) = do
    ySigma <- M.lookup y sigma
    let (x,(f,_,t)) = head . filter (\(_,(_,z,_)) -> z == y) $ M.assocs p
    let xSigma = Fun f [ySigma, t]
    go (M.insert x xSigma (M.delete y sigma) , ([], M.delete x p, vs, s))
  go (sigma, ([], p, [], s)) = if M.null p then Just $ St.fromMap sigma else Nothing
  go _ = Nothing

extract :: Ord v => MatchingProblem f v -> Maybe (Subst f v)
extract mp = return (M.empty, mp) >>= extractS >>= extractP

{-
  The two main equations for "go" correspond to the inference rules in Fig. 2 and Fig. 3 of
  the original paper, respectively. Note that the rule Dec_C is missing as we do not have
  symbols which are only C and not AC.

  acss and vs supply the AC function symbols and an infinite list of fresh variables,
  respectively.
-}
matchAC :: (Ord f, Ord v) => [f] -> [v] -> Term f v -> Term f v -> Maybe (Subst f v)
matchAC acss fvs s t = go $ initialize acss s t where
  go ((Var x, t):u, p, vs, s) = case M.lookup x s of
    Just t' -> if t == t' then go (u, p, vs, s) else Nothing
    Nothing -> case M.lookup x p of
      Just (f, y, t') -> case t of
        Fun f' ts -> if   f == f' && t' `elem` ts
                     then go ((Var y, term f (L.delete t' ts)):u, p, vs, s)
                     else Nothing
        Var _ -> Nothing
      Nothing -> go (u, p, vs, M.insert x t s)
  go ((Fun f ts, Fun g ss):u, p, vs, s) =
    if f /= g
    then Nothing
    else if   not $ f `elem` acss
         then if length ts == length ss then go ((zip ts ss) ++ u, p, vs, s) else Nothing
         else case ts of
           Var x : ts' -> case M.lookup x s of
             Just w -> if   length ts' < length ss && w `elem` ss
                       then go ((term f ts', term f (L.delete w ss)):u, p, vs, s)
                       else case w of
                         Fun h ws -> if   h == f && length ts' <= length ss - length ws
                                     then go ((term f ts', term f (ss \\ ws)):u, p, vs, s)
                                     else Nothing
                         _ -> Nothing
             Nothing -> case M.lookup x p of
               Just (h, y, w) ->
                 if   h == f
                 then if   w `elem` ss && length ts' + 1 < length ss
                      then go ((Fun f (Var y:ts'), term f (L.delete w ss)):u, p, vs, s)
                      else Nothing
                 else if   length ts' < length ss
                      then tryAll [ go ((Var y, term h (L.delete w ws)):u, p, vs, s)
                                  | Fun i ws <- ss, i == h, w `elem` ws]
                      else Nothing
               Nothing -> tryAll $ ac_neq ++ ac_eq where
                 v = fvs !! length vs
                 ac_neq = 
                   if   length ts' < length ss
                   then [ go ((term f ts', term f (L.delete w ss)):u, p, vs, M.insert x w s)
                        | w <- ss]
                   else []
                 ac_eq =
                   if   length ts' + 1 < length ss
                   then [ go ((Fun f (sort $ (Var v):ts'), term f (L.delete w ss)):u
                             , M.insert x (f, v, w) p
                             , v:vs
                             , s
                             )
                        | w <- ss]
                   else []
           t@(Fun h _) : ts' ->
             if 1 <= length ts' && length ts' < length ss
             then tryAll [go ((t,w):(term f ts', term f (L.delete w ss)):u, p, vs, s)
                         | w@(Fun i ws) <- ss, i == h]
             else Nothing
           _ -> Nothing
  go mp@([], _, _, _) = case extract mp of
    Just sigma -> assert (equivalentModuloAC acss (S.apply sigma s) t) (Just sigma)
    Nothing -> Nothing
  go _ = Nothing
