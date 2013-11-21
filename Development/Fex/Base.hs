module Development.Fex.Base
  (baseExper)
where

import Control.Monad (liftM, when)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Development.Fex.Experiment
import Development.Fex.Depend

data ShowDeps a = ShowDeps (Experiment a) FlagSpec

instance Show (ShowDeps a) where
  show (ShowDeps _ flag) = "Show Dependencies [" ++ show flag ++ "]"

instance Depend (ShowDeps a) where
  missing (ShowDeps e flag) = do
    showDeps <- runFlagBool flag
    when showDeps $ do
      args <- liftM noDepends $ liftIO getArgs
      deps <- liftIO $ dependsExper e
      status <- liftIO $ resolve e args deps
      liftIO $ putStrLn $ intercalate "\n" $ zipWith (curry dependStatus) deps status
      liftIO exitSuccess
    return Nothing
    where noDepends [] = []
          noDepends ("--depends":xs) = noDepends xs
          noDepends (x:xs) = x : noDepends xs

-- | Wraps an experiment to provide the "--depends" switch. When activated,
-- the program will show a list of dependencies (and each dependency's status)
-- and then terminate without executing any experiments.
baseExper :: Experiment a -> Experiment a
baseExper e = do
  flag <- mkFlagBool "depends" ("Show a list of dependencies and exit\n"
                                ++ "without evaluating any experiments.")
  dep (ShowDeps e flag)
  e 

