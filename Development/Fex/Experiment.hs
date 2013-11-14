module Development.Fex.Experiment
where

import Control.Monad (liftM)
import Data.Monoid (Monoid, mappend, mempty)
import System.IO.Unsafe (unsafeInterleaveIO)

newtype Experiment a = Experiment { runExper :: Exper -> IO (IO a, Exper) }

instance Monad Experiment where
  return a = Experiment $ \e -> return (return a, e)
  (Experiment e) >>= f = Experiment $ \exper -> do
                           (io, exper') <- e exper
                           a <- unsafeInterleaveIO io
                           runExper (f a) exper'

  -- return a >>= f = Exp $ (\e -> return (a, e)) >>= f  [def. return]
  --                = Exp $ \e' -> runExper (f a) e'     [def. >>=]
  --                = Exp $ runExper (f a)               [eta]
  --                = f a

instance Functor Experiment where
  fmap = liftM

data Exper = Exper { depends :: [Dependency]
                   , effects :: [Effect]
                   }
             deriving Show

instance Monoid Exper where
  mempty = Exper { depends = [], effects = [] }
  (Exper d1 e1) `mappend` (Exper d2 e2) = Exper (d1 ++ d2) (e1 ++ e2)

type Dependency = (Dep, String)

data Dep
  = DFlag { fshort :: Char, flong :: String, fdefault :: String }
  | DEnv { ename :: String, edefault :: String }
  | DExec String
  | DFile String
  | DDir String
  deriving Show

type Effect = (Eff, String)

data Eff
  = EFile String
  deriving Show

evalExper :: Experiment a -> IO a
evalExper e = do
  (io, _) <- runExper e mempty
  io

dependsExper :: Experiment a -> IO [Dependency]
dependsExper e = liftM (depends . snd) $ runExper e mempty

effectsExper :: Experiment a -> IO [Effect]
effectsExper e = liftM (effects . snd) $ runExper e mempty

depend :: Dependency -> Experiment ()
depend d = Experiment $ \e -> return (return (), e { depends = d:depends e })

effect :: Effect -> Experiment ()
effect eff = Experiment $ \e -> return (return (), e { effects = eff:effects e })

