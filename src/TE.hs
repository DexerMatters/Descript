{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TE where

import Control.Arrow (returnA, (>>^))
import TEUtils
import qualified Tm as T
import Utils
import qualified Val as V

eval :: FI T.Ty ->> FI V.Ty
eval = proc fity -> do
  let FI p ty = fity
  let fi = FI p
  case ty of
    T.TyVar i -> getType -< i
    T.TyPrim prim -> returnA -< fi $ V.TyPrim prim
    T.TyArrow l r -> do
      l' <- fmapA eval -< l
      r' <- eval -< r
      returnA -< fi $ V.TyArrow l' r'
    T.TyTuple ts -> do
      ts' <- fmapA eval -< ts
      returnA -< fi $ V.TyTuple ts'
    T.TyRcd rcd -> do
      rcd' <- go -< rcd
      returnA -< fi $ V.TyRcd rcd'
    T.TyLam n tm -> do
      env <- getEnv -< ()
      returnA -< fi $ V.TyLam n (V.Closure env tm)
    T.TyApp tm args -> do
      FI p' tm' <- eval -< tm
      args' <- fmapA eval -< args
      case tm' of
        V.TyLam _ (V.Closure env tm'') ->
          apply -< (args', V.Closure env tm'')
        _ -> p' >- throwWith NotAFunction
    T.TyCast from to -> do
      from' <- eval -< from
      to' <- eval -< to
      b <- conv -< (from', to')
      if b
        then returnA -< to'
        else
          (pos from', (prettyShow from', prettyShow to'))
            >-
              throwWith (uncurry BadCast)
    T.TyBiCast from to -> do
      from' <- eval -< from
      to' <- eval -< to
      b' <- conv -< (to', from')
      b <- conv -< (from', to')
      if b && b'
        then returnA -< to'
        else
          (p, (prettyShow from', prettyShow to'))
            >-
              throwWith (uncurry BadCast)
      returnA -< to'
    T.TySeq tys -> do
      tys' <- fmapA eval -< tys
      returnA -< last tys'
  where
    go = proc rcd -> case rcd of
      [] -> returnA -< []
      (l, t) : rcd' -> do
        t' <- eval -< t
        rcd'' <- go -< rcd'
        returnA -< (l, t') : rcd''

conv :: (FI V.Ty, FI V.Ty) ->> Bool
conv = proc (lhs, rhs) -> do
  let FI p1 val = lhs
  let FI p2 val' = rhs
  case (val, val') of
    (_, V.TyTop) -> returnA -< True
    (V.TyBot, _) -> returnA -< True
    (V.TyVar i1, V.TyVar i2) -> do
      (bot, top) <- getBorder -< i1
      (bot', top') <- getBorder -< i2
      conv -< (bot, bot')
      conv -< (top', top)
    (V.TyVar i, ty) -> do
      (bot, top) <- getBorder -< i
      conv -< (bot, FI p2 ty)
      conv -< (FI p1 ty, top)
    (ty, V.TyVar i) -> do
      (bot, top) <- getBorder -< i
      conv -< (bot, FI p2 ty)
      conv -< (FI p1 ty, top)
    (V.TyPrim prim1, V.TyPrim prim2)
      | prim1 == prim2 -> returnA -< True
    (V.TyArrow l1 r1, V.TyArrow l2 r2) -> do
      fmapA' conv -< zip l1 l2
      conv -< (r2, r1)
    (V.TyTuple ts1, V.TyTuple ts2) -> do
      fmapA conv >>^ and -< zip ts1 ts2
    (V.TyLam n1 cls1, V.TyLam n2 cls2) | n1 == n2 -> do
      l <- () >- getEnv >>^ V.types >>^ length
      let args = V.TyVar <$> [l .. l + n1 - 1]
      t1 <- apply -< (FI p1 <$> args, cls1)
      t2 <- apply -< (FI p2 <$> args, cls2)
      conv -< (t1, t2)
    (V.TyRcd rcd1, V.TyRcd rcd2) ->
      go -< (rcd1, rcd2)
    _ -> returnA -< False
  where
    go = proc (r1, r2) -> case (r1, r2) of
      (_, []) -> returnA -< True
      ([], _) -> returnA -< False
      ((l1, t1) : r1', (l2, t2) : r2')
        | l1 == l2 -> do
            b <- conv -< (t1, t2)
            if b
              then
                go -< (r1', r2')
              else returnA -< False
        | otherwise -> returnA -< False

apply :: ([FI V.Ty], V.Closure) ->> FI V.Ty
apply = proc (args, V.Closure env tm) -> do
  env0 <- getEnv -< ()
  setEnv -< env
  fmapA' addType -< tr (reverse $ concatTypes args)
  ty <- eval -< tm
  setEnv -< env0
  returnA -< ty

concatTypes :: [FI V.Ty] -> [FI V.Ty]
concatTypes = foldr go []
  where
    go (FI _ (V.TyTuple ts1)) acc = ts1 ++ acc
    go (FI _ (V.TyRcd rcd1)) acc = fmap snd rcd1 ++ acc
    go ty acc = ty : acc

getBorder :: Int ->> V.Border
getBorder = proc i -> do
  state <- getConstrs -< i
  case state of
    Interpreted border -> returnA -< border
    Uninterpreted (T.Constr tops bots) -> do
      tops' <- fmapA eval -< tops
      bots' <- fmapA eval -< bots
      bot <- bottommost -< bots'
      top <- topmost -< tops'
      returnA -< (bot, top)
  where
    bottommost = proc tys -> case tys of
      [] -> returnA -< FI (0, 0) V.TyTop
      [ty] -> returnA -< ty
      ty : tys' -> do
        bot <- bottommost -< tys'
        r <- conv -< (ty, bot)
        if r
          then returnA -< bot
          else returnA -< ty

    topmost = proc tys -> case tys of
      [] -> returnA -< FI (0, 0) V.TyBot
      [ty] -> returnA -< ty
      ty : tys' -> do
        top <- topmost -< tys'
        r <- conv -< (top, ty)
        if r
          then returnA -< top
          else returnA -< ty