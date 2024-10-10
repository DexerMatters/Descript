{-# LANGUAGE LambdaCase #-}

module Utils (mapME, mapE, zipWithMapME, elemAndUpdate) where

mapME ::
  (Monad m) => (a -> b -> m (c, a)) -> a -> [b] -> m ([c], a)
mapME f env = \case
  [] -> pure ([], env)
  p : ps -> do
    (t, env') <- f env p
    (ts, env'') <- mapME f env' ps
    pure (t : ts, env'')

mapE :: (a -> b -> a) -> a -> [b] -> a
mapE f env = \case
  [] -> env
  p : ps -> mapE f (f env p) ps

zipWithMapME ::
  (Monad m) => (a -> b -> c -> m a) -> a -> [b] -> [c] -> m a
zipWithMapME f env ps ts = case (ps, ts) of
  ([], []) -> pure env
  (p : ps', t : ts') -> do
    env' <- f env p t
    zipWithMapME f env' ps' ts'
  _ -> error "chain': lists have different lengths"

elemAndUpdate :: Int -> (a -> a) -> [a] -> [a]
elemAndUpdate i f xs = case splitAt i xs of
  (ys, z : zs) -> ys ++ f z : zs
  _ -> error "elemAndUpdate: index out of bounds"
