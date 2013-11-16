{-# LANGUAGE MultiParamTypeClasses #-}
module Development.Fex.Experiment
where

import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

-- | The Experiment monad maintains state (dependencies and effects) while
-- also composing IO computations.
--
-- Algebraic laws:
--
-- > evalExper e >> evalExper e' = evalExper (e >> e')
-- > depend d (e >> e') = depend d e >> depend d e' 
-- > effect eff (e >> e') = effect eff e >> effect eff e' 
newtype Experiment a = Experiment { runExper :: Exper -> IO (Maybe a, Exper) }

instance Monad Experiment where
  return a = Experiment $ \e -> return (return a, e)
  (Experiment e) >>= f = Experiment $ \exper -> do
                           (a, exper') <- e exper
                           case a of
                             Just a' -> runExper (f a') exper'
                             Nothing -> return (Nothing, exper')

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
--
-- Dependencies should be specified up front. That is, the set of dependencies
-- for an experiment (and all of its sub-experiments) must be derivable
-- independent of an experiment's computation.
--
-- I would like a similar restraint for effects, but I'm not sure if it's
-- possible.
data Exper = Exper { depends :: [Dependency]
                   , effects :: [Effect]
                   , eval :: Bool
                   }
             deriving Show

instance Monoid Exper where
  mempty = Exper { depends = [], effects = [], eval = True }
  ex1 `mappend` ex2 =
    Exper { depends = depends ex1 ++ depends ex2
          , effects = effects ex1 ++ effects ex2
          , eval    = eval ex1
          }

-- | A Dependency describes something that is necessary in order for an
-- experiment to complete. It is represented as a description of something
-- in the environment along with a human readable string describing the
-- dependency.
class Show a => Depend a b where
  depend :: b -> IO (Either String a)

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
evalExper e = liftM (fromJust . fst) $ runExper e mempty

-- | Returns a list of all dependencies in the given experiment, including
-- all sub-experiments.
dependsExper :: Experiment a -> IO [Dependency]
dependsExper e = liftM (depends . snd) $ runExper e (mempty { eval = False })

-- | Returns a list of all effects in the given experiment, including
-- all sub-experiments.
effectsExper :: Experiment a -> IO [Effect]
effectsExper e = liftM (effects . snd) $ runExper e (mempty { eval = False })

-- | Add a dependency to the current experiment.
-- depend :: Dependency -> Experiment () 
-- depend d = Experiment $ \e -> return (return (), e { depends = d:depends e }) 
dep :: Depend a b => b -> Experiment a
dep b = liftIO (depend b) >>= \r ->
          case r of
            Left s -> error s
            Right a -> return a

-- | Add an effect to the current experiment.
effect :: Effect -> Experiment ()
effect eff = Experiment $ \e -> return (return (), e { effects = eff:effects e })

liftIO :: IO a -> Experiment a
liftIO io = Experiment $ \e -> io >>= \a -> return (return a, e)

-- | experIO executes an arbitrary IO computation in the Experiment monad.
-- (I think this should probably be `liftIO`.)
experIO :: IO a -> Experiment a
experIO io = Experiment $ \e ->
  if not $ eval e then
    return (Nothing, e)
  else
    io >>= \a -> return (return a, e)

