{-# LANGUAGE ExistentialQuantification #-}
module Development.Fex.Experiment
  ( Experiment, Dependency, Effect, Depend(..)
  , dependsExper, dep, dependStatus, resolve
  , effectsExper, effect
  , flagsExper, addFlag, flagValue
  , evalExper
  , liftIO
  , experFail
  , setName, getName
  )
where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (liftM, ap)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, nub)
import Data.Maybe (isJust)
import Data.Monoid (Monoid, mappend, mempty)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Printf (printf)

import qualified Development.Fex.Flag as F

-- | The Experiment monad maintains state (dependencies and effects) while
-- also composing IO computations.
--
-- Algebraic laws:
--
-- > evalExper args e >> evalExper args e' = evalExper args (e >> e')
-- > depend d (e >> e') = depend d e >> depend d e' 
-- > effect eff (e >> e') = effect eff e >> effect eff e' 
newtype Experiment a = Experiment { runExper :: Exper -> IO (Either String (a, Exper)) }

instance Monad Experiment where
  return a = Experiment $ \e -> return $ Right (a, e)
  (Experiment e) >>= f = Experiment $ \exper -> do
                           r <- e exper
                           case r of
                             Left err -> return $ Left err
                             Right (a, exper') -> runExper (f a) exper'

instance Functor Experiment where
  fmap = liftM

instance Applicative Experiment where
  pure = return
  e1 <*> e2 = ap e1 e2

instance MonadIO Experiment where
  -- When the context of an experiment does not include evaluation,
  -- lazy IO is used to defer the evaluation of IO side effects in experiments.
  --
  -- It'd be ideal to have two distinct IO combinators: one for use when
  -- specifying dependencies and one for use when running experiments.
  -- But I don't know how to tie it together.
  liftIO io = Experiment $ \e -> do { a <- io' e; return $ Right (a, e) }
    where io' e = if not (eval e) then unsafeInterleaveIO io else io

-- | Fails the current experiment.
-- As of now, this is pretty much useless, since the type is fixed at unit.
experFail :: String -> Experiment ()
experFail msg = Experiment $ \e -> do
  if not (eval e)
    then return $ Right ((), e)
    else return $ Left msg


-- forceIO is an unsafe variant of liftIO. Namely, it runs an arbitrary
-- IO computation regardless of whether the experiment is being evaluated.
--
-- This isn't being used (and it seems useless), marked for death?
-- forceIO :: IO a -> Experiment a 
-- forceIO io = Experiment $ \e -> do { a <- io; return (a, e) } 

-- | Exper represents the state of an experiment. Namely, a list of
-- dependencies, a list of effects, an evaluation context and a representation
-- of command line parameters.
--
-- In particular, an experiment can run successfully if and only if all of the
-- given dependencies are satisfied. (DONE.)
--
-- Also, after an experiment is executed, all of the effects /must/ be
-- observable. (TODO.)
--
-- Dependencies should be specified up front. That is, the set of dependencies
-- for an experiment (and all of its sub-experiments) must be derivable
-- independent of an experiment's computation, otherwise the aforementioned
-- guarantee does not hold.
--
-- Similarly for effects.
data Exper = Exper { name :: String
                   , depends :: [Dependency]
                   , effects :: [Effect]
                   , eval :: Bool -- false when querying dependencies/effects
                   , flagSpec :: F.Flags
                   , flagVals :: F.Config
                   }

instance Show Exper where
  show (Exper { depends = ds, effects = efs }) =
    printf "Dependencies: %s\nEffects: %s" (show ds) (show efs)

instance Monoid Exper where
  mempty = Exper { name = "UNNAMED"
                 , depends = []
                 , effects = []
                 , eval = True
                 , flagSpec = F.emptyFlags
                 , flagVals = F.emptyConfig
                 }
  ex1 `mappend` ex2 =
    Exper { name = name ex1
          , depends = depends ex2 ++ depends ex1
          , effects = effects ex2 ++ effects ex1
          , eval    = eval ex1
          , flagSpec = F.appendFlags (flagSpec ex2) (flagSpec ex1)
          , flagVals = F.appendConfig (flagVals ex2) (flagVals ex1)
          }

-- | A Dependency describes something that is necessary in order for an
-- experiment to complete. The fundamental property of a dependency is that
-- it can detect whether it's missing or not.
--
-- Note that each dependency /must/ have a unique `Show` instance. (Why?)
class Show a => Depend a where
  missing :: a -> Experiment (Maybe String)

-- | A container for holding any value that satisfies the `Depend` constraint.
data Dependency = forall a. Depend a => D a



instance Eq Dependency where
  (D d1) == (D d2) = show d1 == show d2

instance Show Dependency where
  show (D d) = show d

-- A dependency contain is a dependency!
instance Depend Dependency where
  missing (D a) = missing a

-- | An Effect describes something that must be observable after an experiment
-- completes. It is represented as a description of something in the
-- environment along with a human readable string describing the effect.
--
-- Incomplete.
type Effect = (Eff, String)

-- Incomplete.
data Eff
  = EFile String
  deriving (Show, Eq)

-- | Evaluates the experiment by executing it in the IO monad. If the
-- experiment executes successfully, then all dependencies will have been
-- satisfied and all effects will have been observed.
evalExper :: [String] -- ^ command line arguments
          -> Experiment a -- ^ the experiment to evaluate
          -> IO (Either String a) -- ^ the result of an experiment or an error
evalExper args e = do
  flags <- flagsExper e
  conf <- F.configIO flags args
  deps <- dependsExper e
  status <- resolve e args deps
  let errors = filter (isJust . snd) $ zip deps status
  if null errors
    then do r <- runExper e (mempty { flagVals = conf })
            case r of
              Left err -> error err
              Right (r', _) -> return $ Right r'
    else return $ Left $ intercalate "\n" $ map dependStatus errors

-- | An empty experiment with no evaluation context.
noEval :: Exper
noEval = mempty { eval = False }

-- | Resolves a list of dependencies with the given command line
-- arguments in the context of a particular experiment. The list of
-- statuses returned is in correspondence with the list of dependencies
-- given. If a dependency is not satisfied, the status will contain a
-- human readable string explaining what is wrong.
resolve :: Experiment a -> [String] -> [Dependency] -> IO [Maybe String]
resolve e args d = do
  flags <- flagsExper e
  conf <- F.configIO flags args
  r <- runExper (mapM missing d) (mempty { flagSpec = flags, flagVals = conf })
  case r of
    Left err -> error err
    Right (r', _) -> return r'

-- kill me.
right :: Either String b -> b
right (Right b) = b
right (Left err) = error $ printf "BUG with Either: %s" err

-- | Returns a list of all dependencies in the given experiment, including
-- all sub-experiments. No experiments will be evaluated.
dependsExper :: Experiment a -> IO [Dependency]
dependsExper e = liftM (nub . reverse . depends . snd . right) $ runExper e noEval

-- | Add a dependency to the current experiment.
dep :: Depend a => a -> Experiment a
dep a = Experiment $ \e -> return (Right (a, e { depends = D a:depends e }))

-- | Convert a dependency and its status to a human readable string.
dependStatus :: (Dependency, Maybe String) -> String
dependStatus (d, Nothing)  = show d ++ " ... OK."
dependStatus (d, Just err) = show d ++ " ... Not found!\n" ++ indent err
  where indent = dropWhileEnd isSpace . unlines . map ("    " ++) . lines

-- | Returns a list of all effects in the given experiment, including
-- all sub-experiments. No experiments will be evaluated.
effectsExper :: Experiment a -> IO [Effect]
effectsExper e = liftM (nub . reverse . effects . snd . right) $ runExper e noEval

-- | Add an effect to the current experiment.
effect :: Effect -> Experiment ()
effect eff = Experiment $ \e -> return (Right ((), e { effects = eff:effects e }))

-- | Returns the experiment's command line flag specification.
-- No experiments will be evaluated.
flagsExper :: Experiment a -> IO F.Flags
flagsExper e = liftM (flagSpec . snd . right) $ runExper e noEval

-- | Adds a flag specification to the experiment.
addFlag :: F.FlagSpec -> Experiment ()
addFlag spec = Experiment $ \e ->
                 return (Right ((), e { flagSpec = F.addFlag spec (flagSpec e) }))

-- | Sets the name of this experiment and any unnamed sub-experiments.
setName :: String -> Experiment ()
setName s = Experiment $ \e -> return (Right ((), e { name = s }))

-- | Gets the name of the current experiment.
getName :: Experiment String
getName = Experiment $ \e -> return (Right (name e, e))

-- | Retrieves a flag value given its long form name from the current
-- experiment. If the given name cannot be found, then the flag specification
-- will be checked for a default value. If it cannot be found there, then
-- there is a bug in fex. (The public API guarantees that all calls to
-- `flagValue` will succeed.)
flagValue :: String -> Experiment F.FlagV
flagValue k =
  Experiment $ \e ->
    case F.confLookup k (flagVals e) of
      Just flagv  -> return $ Right (flagv, e)
      Nothing -> -- degrade to default value
        case F.flagLookup k (flagSpec e) of
          Just spec -> return $ Right (F.defv spec, e)
          Nothing -> error $ printf "BUG: Could not find flag '%s'" k

