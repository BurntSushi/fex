{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Development.FexH.Experiment
where

class Monad m => Base m repr where
  str :: String -> repr m String
  lam :: (repr m a -> repr m b) -> repr m (a -> m b)
  app :: repr m (a -> m b) -> repr m a -> repr m b

newtype Eval m a = E { unE :: m a }

instance Monad m => Base m Eval where
  str = E . return
  lam f = E $ return $ unE . f . E . return
  app f a = E $ do
    f' <- unE f
    a' <- unE a
    f' a'

-- Shortcuts for our evaluators.
eval :: Eval IO a -> IO a
eval = unE

