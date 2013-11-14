module Development.Fex.Experiment
where

import Control.Monad (liftM)
import Data.Monoid (Monoid, mappend, mempty)
import System.IO.Unsafe (unsafeInterleaveIO)

-- | The Experiment monad maintains state (dependencies and effects) while
-- also composing IO computations.
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

-- | Exper represents the state of an experiment. Namely, a list of
-- dependencies and a list of effects.
--
-- In particular, an experiment can run successfully if and only if all of the
-- given dependencies are satisfied.
--
-- Also, after an experiment is executed, all of the effects *must* be
-- observable.
data Exper = Exper { depends :: [Dependency]
                   , effects :: [Effect]
                   }
             deriving Show

instance Monoid Exper where
  mempty = Exper { depends = [], effects = [] }
  (Exper d1 e1) `mappend` (Exper d2 e2) = Exper (d1 ++ d2) (e1 ++ e2)

-- | A Dependency describes something that is necessary in order for an
-- experiment to complete. It is represented as a description of something
-- in the environment along with a human readable string describing the
-- dependency.
type Dependency = (Dep, String)

data Dep
  = DFlag { fshort :: Char, flong :: String, fdefault :: String }
  | DEnv { ename :: String, edefault :: String }
  | DExec String
  | DFile String
  | DDir String
  deriving Show

-- | An Effect describes something that must be observable after an experiment
-- completes. It is represented as a description of something in the
-- environment along with a human readable string describing the effect.
type Effect = (Eff, String)

data Eff
  = EFile String
  deriving Show

-- | Evaluates the experiment by executing it in the IO monad. If the
-- experiment executes successfully, then all dependencies will have been
-- satisfied and all effects will have been observed.
evalExper :: Experiment a -> IO a
evalExper e = do
  (io, _) <- runExper e mempty
  io

-- | Returns a list of all dependencies in the given experiment, including
-- all sub-experiments.
dependsExper :: Experiment a -> IO [Dependency]
dependsExper e = liftM (depends . snd) $ runExper e mempty

-- | Returns a list of all effects in the given experiment, including
-- all sub-experiments.
effectsExper :: Experiment a -> IO [Effect]
effectsExper e = liftM (effects . snd) $ runExper e mempty

-- | Add a dependency to the current experiment.
depend :: Dependency -> Experiment ()
depend d = Experiment $ \e -> return (return (), e { depends = d:depends e })

-- | Add an effect to the current experiment.
effect :: Effect -> Experiment ()
effect eff = Experiment $ \e -> return (return (), e { effects = eff:effects e })

