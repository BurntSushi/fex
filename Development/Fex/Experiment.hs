{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
module Development.Fex.Experiment
where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust)
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

instance MonadIO Experiment where
  liftIO io = Experiment $ \e ->
    if not $ eval e then
      return (Nothing, e)
    else
      io >>= \a -> return (return a, e)

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
class Show a => Depend a where
  type Args
  depend :: Args -> Experiment a
  missing :: a -> IO (Maybe String)

data Dependency = forall a. Depend a => D a

instance Show Dependency where
  show (D d) = show d

-- depMap :: Depend a => (a -> b) -> Dependency -> b 
-- depMap f (D a) = f a 

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
evalExper :: Experiment a -> IO (Either String a)
evalExper e = do
  deps <- dependsExper e
  errors <- fmap catMaybes $ mapM (\(D d) -> missing d) deps
  if null errors then
    liftM (Right . fromJust . fst) $ runExper e mempty
  else
    return $ Left $ intercalate "\n" errors

-- | Returns a list of all dependencies in the given experiment, including
-- all sub-experiments.
dependsExper :: Experiment a -> IO [Dependency]
dependsExper e = liftM (depends . snd) $ runExper e (mempty { eval = False })

-- | Returns a list of all effects in the given experiment, including
-- all sub-experiments.
effectsExper :: Experiment a -> IO [Effect]
effectsExper e = liftM (effects . snd) $ runExper e (mempty { eval = False })

-- | Add a dependency to the current experiment.
dep :: Depend a => Args -> Experiment a
dep args = do
  d <- depend args
  Experiment $ \e -> return (return d, e { depends = D d:depends e })

-- | Add an effect to the current experiment.
effect :: Effect -> Experiment ()
effect eff = Experiment $ \e -> return (return (), e { effects = eff:effects e })

-- forceIO is an unsafe variant of liftIO. Namely, it runs an arbitrary
-- IO computation regardless of whether the experiment is being evaluated.
forceIO :: IO a -> Experiment a
forceIO io = Experiment $ \e -> io >>= \a -> return (return a, e)

