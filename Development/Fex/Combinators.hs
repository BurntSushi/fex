module Development.Fex.Combinators
  (cached, existsAfter)
where

import Control.Monad (filterM, liftM)
import System.Directory (doesFileExist)
import Text.Printf (printf)

import Development.Fex.Experiment

-- | Conditionally executes an experiment if any file given does not exist.
-- In particular, the experiment given should generate the files listed.
-- If it doesn't, the experiment fails.
-- Otherwise, if all of the files already exist, the experiment is not executed.
cached :: [String] -- ^ A list of files generated by an experiment.
       -> Experiment () -- ^ The experiment that generates a list of files.
       -> Experiment ()
cached files e = do
  missing <- whichNotExists files
  if length missing > 0
    then existsAfter files e
    else return ()

-- | Executes the given experiment and checks that the files provided exist
-- after completion. If any file does not exist, the experiment fails.
existsAfter :: [String] -- ^ A list of files generated by an experiment.
            -> Experiment () -- ^ The experiment that generates the files.
            -> Experiment ()
existsAfter files e = do
  e
  after <- whichNotExists files
  if length after > 0
    then getName >>= \name ->
           experFail $ printf ("The following files still don't exist "
                               ++ "after running %s: %s")
                              name (show after)
    else return ()

whichNotExists :: [String] -> Experiment [String]
whichNotExists = liftIO . filterM (liftM not . doesFileExist)

