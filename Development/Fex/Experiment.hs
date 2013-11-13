module Development.Fex.Experiment
where

import Data.Monoid (Monoid, mappend, mempty)

newtype Experiment a = Experiment { runExper :: Exper -> IO (a, Exper) }

instance Monad Experiment where
  return a = Experiment $ \e -> return (a, e)
  (Experiment e) >>= f = Experiment $ \exper -> do
                           (a, exper') <- e exper
                           runExper (f a) exper'

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

dependsExper :: Experiment a -> IO [Dependency]
dependsExper e = do
  (_, e') <- runExper e mempty
  return $ depends e'

evalExper :: Experiment a -> IO a
evalExper e = runExper e mempty >>= \(a, _) -> return a

depend :: Dependency -> Experiment ()
depend d = Experiment $ \e -> return ((), e { depends = d:depends e })

effect :: Effect -> Experiment ()
effect eff = Experiment $ \e -> return ((), e { effects = eff:effects e })

