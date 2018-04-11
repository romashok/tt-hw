module Reduction where

import           Control.Monad.State.Lazy
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (catMaybes)
import           Data.Set                 (Set)
import qualified Data.Set                 as S

import           Expr


normalize :: Expr -> Expr
normalize expr = case redexStep expr of
    Just redexed -> normalize redexed
    Nothing      -> expr

redexStep :: Expr -> Maybe Expr
redexStep (Var _) = Nothing
redexStep (f@(Lambda _ _) :$: arg)  = Just $ applyRedex f arg
redexStep (f :$: arg)
    | Just reduced <- redexStep f   = Just $ reduced :$: arg
    | Just reduced <- redexStep arg = Just $ f :$: reduced
    | otherwise                     = Nothing
redexStep (Lambda var expr) = case redexStep expr of
                                  Just step -> Just $ Lambda var step
                                  _         -> Nothing

applyRedex :: Expr -> Expr -> Expr
applyRedex (Lambda var expr1) expr2 = substitution (renameVars renamings expr1) var expr2
  where
    freeVars = getFreeVars expr2
    boundedVars = getBoundedVars expr1 var
    maybesRenamings = evalState (mapM varNewName $ S.toList boundedVars) freeVars
    renamings = M.fromList $ catMaybes maybesRenamings

substitution :: Expr -> Var -> Expr -> Expr
substitution expr@(Var var) free otherExpr = if var == free then otherExpr else expr
substitution self@(Lambda var expr) free otherExpr
    | var == free = self
    | otherwise    = Lambda var $ substitution expr free otherExpr
substitution (f :$: arg) free otherExpr = substitution f free otherExpr :$: substitution arg free otherExpr

varNextName :: Var -> Set Var -> Var
varNextName var otherVars
    | S.member nextName otherVars = varNextName nextName otherVars
    | otherwise                   = nextName
  where
    nextName = var ++ "'"

varNewName :: Var -> State (Set Var) (Maybe (Var, Var))
varNewName var = do
    nameContext <- get
    if S.member var nameContext
        then do
            let newVar = varNextName var nameContext
            modify $ S.insert newVar
            return $ Just (var, newVar)
        else return Nothing

renameVars :: Map Var Var -> Expr -> Expr
renameVars = go M.empty
  where
    go :: Map Var Var -> Map Var Var -> Expr -> Expr
    go subsB subsV (Lambda var expr)
        | M.member var subsV = Lambda newVar $ go (M.insert var newVar subsB) (M.delete var subsV) expr
        | M.member var subsB = Lambda var    $ go (M.delete var subsB)        subsV                expr
        | otherwise          = Lambda var    $ go subsB                       subsV                expr
      where
        newVar = subsV M.! var
    go subsB subsV (f :$: arg) = go subsB subsV f :$: go subsB subsV arg
    go subsB _ (Var var) = Var $ M.findWithDefault var var subsB


getFreeVars :: Expr -> Set Var
getFreeVars (Lambda var expr) = S.delete var $ getFreeVars expr
getFreeVars (f :$: arg)       = S.union (getFreeVars f) (getFreeVars arg)
getFreeVars (Var var)         = S.singleton var

getBoundedVars :: Expr -> Var -> Set Var
getBoundedVars = go S.empty
  where
    go :: Set Var -> Expr -> Var -> Set Var
    go bounded (Lambda var expr) free = if var == free then S.empty else go (S.insert var bounded) expr free
    go bounded (f :$: arg)       free = S.union (go bounded f free) (go bounded arg free)
    go bounded (Var var)         free = if var == free then bounded else S.empty
