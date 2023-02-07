module Completion(CompletionQuadruple (..), OrientationPreference (..), complete) where

import System.IO

import Data.List (nubBy)

import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Rules as Rs

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Rules.Rewrite (fullRewrite)

import Control.Monad (when)

import Utils
import Rewriting
import UtilsAC (rulesAC, equivalentModuloAC)
import RewritingAC
import TTInterface

type CompletionQuadruple = ([Equation], TRS, TRS, TRS)
data OrientationPreference = LeftToRight | RightToLeft deriving Eq

instance Show OrientationPreference where
  show LeftToRight = "Left"
  show RightToLeft = "Right"

-- invoke TT only if rule is valid & left-linear
invokeTTSafely :: TTInfo -> [String] -> TRS -> IO TTAnswer
invokeTTSafely ttinfo acss cs@(r:rs) =
  if R.isValid r && R.isLeftLinear r then invokeTT ttinfo acss cs else return No

-- corresponding inference rule applied to all equations
simplify :: RewriteFunction -> CompletionQuadruple -> CompletionQuadruple
simplify rw (es, ps, rs, cs) =
  (map (\e -> Rule {lhs = nf rw trs $ lhs e, rhs = nf rw trs $ rhs e}) es, ps, rs, cs) where
    trs = rs ++ ps

-- corresponding inference rule applied to all equations
delete :: [String] -> CompletionQuadruple -> CompletionQuadruple
delete acss (es, ps, rs, cs) =
  (filter (\e -> not $ equivalentModuloAC acss (lhs e) (rhs e)) es, ps, rs, cs)

-- orient with orientation preference (depending on thread) which is able to postpone decisions
orient :: TTInfo -> [String] -> OrientationPreference ->
          CompletionQuadruple -> [Equation] -> IO (Maybe CompletionQuadruple)
orient _ _ _ ([], _, _, _) _ = return Nothing
orient ttinfo acss o (e:es', ps, rs, cs) postponedEs =
  let
    sel1 = case o of {LeftToRight -> lhs; RightToLeft -> rhs}
    sel2 = case o of {LeftToRight -> rhs; RightToLeft -> lhs}
    r1 = Rule {lhs = sel1 e, rhs = sel2 e}
    r2 = Rule {lhs = sel2 e, rhs = sel1 e}
    checkTermination = invokeTTSafely ttinfo acss
  in do
    res <- checkTermination (r1:cs)
    case res of
      Yes -> return $ Just (postponedEs ++ es', ps, r1:rs, r1:cs)
      No  -> do
        res <- checkTermination (r2:cs)
        case res of
          Yes -> return $ Just (postponedEs ++ es', ps, r2:rs, r2:cs)
          No  -> orient ttinfo acss o (es', ps, rs, cs) (e:postponedEs)
          Error s -> error s
      Error s -> error s

done :: CompletionQuadruple -> Bool
done ([],[],_,_) = True
done _        = False

-- this controls the order in which orient is tried/pending rules are considered
sortEsAndPs :: CompletionQuadruple -> CompletionQuadruple
sortEsAndPs (es, ps, rs, cs) = (sortBySize es, sortBySize ps, rs, cs)

-- given a rule, deduce new CPs and then compose/collapse existing
-- rules as well as pending rules and new rules added by CPs
deduceAndComposeAndCollapse :: [String] -> [String] -> Bool -> RewriteFunction ->
                               Rule String String -> CompletionQuadruple -> CompletionQuadruple
deduceAndComposeAndCollapse acss vs p rw r (es,ps,rs,cs) =
  let
    newPs = ps ++ criticalPairs p (r:rs) (rulesAC acss vs) [r] ++
            flipRules (criticalPairs p (r:rs) [r] (rulesAC acss vs))
    newEs = es ++ criticalPairs p (r:rs) [r] rs ++ criticalPairs p (r:rs) rs [r] ++
                  criticalPairs' p (r:rs) [r]   
    collapseSplit rs = splitTRS (\r' -> null $ Rs.fullRewrite rs (lhs r'))
    (com, col) = collapseSplit [r] rs
    (newCom, newCol) = collapseSplit (r:rs) newPs
    compose rs' = [Rule {lhs = lhs r', rhs = nf rw (r:(rs++ps)) (rhs r')} | r' <- rs']
    collapse rs rs' = [Rule {lhs = Rs.result . head $ Rs.fullRewrite rs (lhs r'), rhs = rhs r'} | r' <- rs']
  in (newEs ++ collapse [r] col ++ collapse (r:rs) newCol, compose newCom, r : compose com, cs)

-- pending rules must not be empty
consumePendingRule :: CompletionQuadruple -> CompletionQuadruple
consumePendingRule (es,ps,rs,cs) = (es, tail ps, (head ps) : rs, cs)

-- governs strategy on consuming pending rules and orienting equations
orientOrConsumePendingRule :: TTInfo -> [String] -> OrientationPreference ->
                              CompletionQuadruple -> IO (Maybe CompletionQuadruple)
orientOrConsumePendingRule ttinfo acss o (es,ps,rs,cs) = do
  mcQuadruple <- if   null ps || (not $ null es) && ruleSize (head es) < ruleSize (head ps)
                 then orient ttinfo acss o (es,ps,rs,cs) []
                 else return . Just $ consumePendingRule (es,ps,rs,cs)
  case mcQuadruple of
    Nothing -> if null ps then return Nothing else return . Just $ consumePendingRule (es,ps,rs,cs)
    Just cq -> return (Just cq)

complete :: TTInfo -> [String] -> [String] -> Bool -> RewriteFunction ->
            OrientationPreference -> CompletionQuadruple ->
            IO (Maybe CompletionQuadruple)
complete ttinfo acss vs p rw o cQuadruple = do
  let cQuadruple' = (delete acss) . (simplify rw) $ cQuadruple
  if done cQuadruple' then return . Just $ cQuadruple' else do
    mcQuadruple'' <- orientOrConsumePendingRule ttinfo acss o (sortEsAndPs cQuadruple')
    case  mcQuadruple'' of
      Nothing                 -> return Nothing                                 
      Just (es, ps, r:rs, cs) -> do
        complete ttinfo acss vs p rw o cQuadruple''' where
        cQuadruple''' = deduceAndComposeAndCollapse acss vs p rw r (es,ps,rs,cs)
