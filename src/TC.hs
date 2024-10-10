{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TC where

import Control.Arrow (Arrow (first, second))
import Control.Monad (mapAndUnzipM, unless, zipWithM)
import Data.List (elemIndex, findIndex)
import qualified Raw as R (Lit (..), Name, Pttrn (..), Tm (..), Ty (..))
import Tm (emptyConstr, (<$), (<^))
import qualified Tm as T (Constr (..), Prim (..), Pttrn (..), Ty (..))
import Utils (elemAndUpdate, mapE, mapME, zipWithMapME)
import Val (Closure (..))
import qualified Val as V (Ty (..))
import Prelude hiding ((<$))

data Env = Env {vars :: [(R.Name, T.Ty)], tnames :: [(TName, T.Constr)], lvl :: Lvl}

type TName = R.Name

type Lvl = Int

type TCResult = Either TCErrors

data TCErrors
  = UnboundVariable
  | UnboundTypeVariable
  | MultipleAnnotation
  | UndefinedPattern
  | IncorrectParameterCount

impure :: e -> Either e a
impure = Left

extendT :: Env -> TName -> T.Constr -> Env
extendT env x c = env {tnames = (x, c) : tnames env, lvl = lvl env + 1}

extendVar :: Env -> R.Name -> T.Ty -> Env
extendVar env x ty = env {vars = (x, ty) : vars env, lvl = lvl env + 1}

extend :: Env -> TName -> T.Constr -> R.Name -> Env
extend env n c x =
  env
    { vars = (x, T.TyVar (lvl env)) : vars env,
      tnames = (n, c) : tnames env,
      lvl = lvl env + 1
    }

concatEnv :: [Env] -> Env
concatEnv envs =
  Env {vars = concatMap vars envs, tnames = concatMap tnames envs, lvl = maximum (map lvl envs)}

-- Type Inferring

infer :: Env -> R.Tm -> TCResult (T.Ty, Env)
infer env = \case
  R.Var x -> maybe (impure UnboundVariable) (pure . (,env)) (lookup x (vars env))
  R.Lit l -> pure . (,env) $
    T.TyPrim $ case l of
      R.LitNum _ -> T.PrimNum
      R.LitBool _ -> T.PrimBool
      R.LitStr _ -> T.PrimStr
      R.LitUnit -> T.PrimUnit
  R.Lam ps ty tm -> do
    ps' <- mapM (indexPattern env) ps
    (tys, env') <- mapME inferPattern env ps'
    let tCounts = lvl env' - lvl env
    (ret, env'') <- infer env' tm
    ty' <- maybe (pure ret) (fmap (T.TyCast ret) . toTmTy env'') ty
    pure . (,env'') $ T.TyLam tCounts (T.TyArrow tys ty')
  R.App f args -> do
    (fTy, env') <- infer env f
    (argTys, env'') <- mapME infer env' args
    case fTy of
      T.TyLam tCounts (T.TyArrow argTys' ret) -> do
        let env3 = mapE constrain env'' (zip argTys' argTys)
        unless (length argTys == tCounts) $
          impure IncorrectParameterCount
        let seq = zipWith T.TyCast argTys' argTys ++ pure ret
        pure . (,env3) $ T.TyApp (T.TyLam tCounts (T.TySeq seq)) argTys
      _ -> error "Never happens"
  R.Ann tm ty -> do
    ty <- toTmTy env ty
    (ty', env') <- infer env tm
    let env'' = constrain env' (ty', ty)
    pure (T.TyCast ty' ty, env'')
  R.Rcd fs -> do
    let f e (l, tm) = first (l,) <$> infer e tm
    (tys, env') <- mapME f env fs
    pure (T.TyRcd tys, env')
  R.Cond pTm tTm fTm ->
    mapME infer env [pTm, tTm, fTm] >>= \case
      ([pTy, tTy, fTy], env') -> do
        let env'' = constrain env' (pTy, T.TyPrim T.PrimBool)
        let env3 = constrain env'' (fTy, tTy)
        pure (tTy, env3)
      _ -> error "Never happens"
  R.Tuple tms -> do
    (tys, env') <- mapME infer env tms
    pure (T.TyTuple tys, env')
  _ -> error "Not implemented"

constrain :: Env -> (T.Ty, T.Ty) -> Env
constrain env = \case
  (T.TyVar i, T.TyVar j) ->
    let tops = T.tops $ snd (tnames env !! j)
        bots = T.bots $ snd (tnames env !! j)
     in let f c = c {T.tops = tops ++ T.tops c, T.bots = bots ++ T.bots c}
         in update env i f
  (T.TyVar i, ty) -> update env i (<$ ty)
  (ty, T.TyVar i) -> update env i (<^ ty)
  (T.TyArrow tys1 ty1, T.TyArrow tys2 ty2) ->
    let env' = mapE constrain env (zip tys1 tys2)
     in constrain env' (ty1, ty2)
  (T.TyTuple tys1, T.TyTuple tys2) ->
    mapE constrain env (zip tys1 tys2)
  (T.TyRcd tys1, T.TyRcd tys2) ->
    mapE constrain env [(ty1, ty2) | (l, ty1) <- tys1, (l', ty2) <- tys2, l == l']
  (T.TyApp ty1 tys1, T.TyApp ty2 tys2) ->
    let env' = constrain env (ty1, ty2)
     in mapE constrain env' (zip tys1 tys2)
  (T.TyCast ty1 ty2, T.TyCast ty3 ty4) ->
    constrain (constrain env (ty1, ty3)) (ty2, ty4)
  (T.TySeq tys1, T.TySeq tys2) ->
    mapE constrain env (zip tys1 tys2)
  _ -> env
  where
    update env i f = env {tnames = elemAndUpdate i (second f) (tnames env)}

inferPattern :: Env -> T.Pttrn -> TCResult (T.Ty, Env)
inferPattern env = \case
  T.PttrnAtom x ->
    pure (T.TyVar (lvl env), extend env n emptyConstr x)
    where
      n = "T%" ++ x
  T.PttrnTuple ps -> do
    (ts, env') <- mapME inferPattern env ps
    pure (T.TyTuple ts, env')
  T.PttrnAnn p ty -> do
    env' <- checkPattern env p ty
    pure (ty, env')

checkPattern :: Env -> T.Pttrn -> T.Ty -> TCResult Env
checkPattern env = curry $ \case
  (T.PttrnAtom x, ty) -> pure $ extendVar env x ty
  (T.PttrnTuple ps, T.TyTuple ts) -> zipWithMapME checkPattern env ps ts
  (T.PttrnAnn _ _, _) -> impure MultipleAnnotation
  _ -> impure UndefinedPattern

indexPattern :: Env -> R.Pttrn -> TCResult T.Pttrn
indexPattern env = \case
  R.PttrnAtom x -> pure $ T.PttrnAtom x
  R.PttrnTuple ps -> T.PttrnTuple <$> mapM (indexPattern env) ps
  R.PttrnAnn p ty -> T.PttrnAnn <$> indexPattern env p <*> toTmTy env ty

toTmTy :: Env -> R.Ty -> TCResult T.Ty
toTmTy env = \case
  R.TyVar x -> go 0 (tnames env) x
    where
      go _ [] _ = impure UnboundTypeVariable
      go i ((x', _) : ns) x
        | x' == x = pure $ T.TyVar i
        | otherwise = go (i + 1) ns x
  R.TyPrim p -> pure $ T.TyPrim p
  R.TyArrow ty1 ty2 -> do
    ty1' <- mapM (toTmTy env) ty1
    ty2' <- toTmTy env ty2
    pure $ T.TyArrow ty1' ty2'
  R.TyTuple tys -> T.TyTuple <$> mapM (toTmTy env) tys
  R.TyRcd tys -> do
    tys' <- mapM (toTmTy env . snd) tys
    pure $ T.TyRcd (zip (fst <$> tys) tys')
  R.TyApp ty tys -> do
    ty' <- toTmTy env ty
    tys' <- mapM (toTmTy env) tys
    pure $ T.TyApp ty' tys'
  R.TyCast ty1 ty2 -> do
    ty1' <- toTmTy env ty1
    ty2' <- toTmTy env ty2
    pure $ T.TyCast ty1' ty2'
  R.TySeq tys -> T.TySeq <$> mapM (toTmTy env) tys