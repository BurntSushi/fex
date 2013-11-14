module Development.Fex.IO where

import qualified System.IO as I

import Development.Fex.Experiment

-- | experIO executes an arbitrary IO computation in the Experiment monad.
-- (I think this should probably be `liftIO`.)
experIO :: IO a -> Experiment a
experIO a = Experiment $ \e -> return (a, e)
-- experIO io = Experiment $ \e -> io >>= \a -> return (a, e) 

-- | dummy tries to add a dependency while also printing something to stdout.
dummy :: Experiment String
dummy = do
  depend (DExec "dummy", "dummy executable")
  experIO $ do
    putStrLn "dummy executable called"
    return "dummy"

-- | openFile wraps the same function in the "System.IO" module, but also
-- adds the given file as a dependency or effect to the current experiment.
-- If the file is opened in read or readwrite mode, then it's added as a
-- dependency.
-- If the file is opened in write, append or readwrite mode, then it's added
-- as an effect.
openFile :: I.FilePath -> I.IOMode -> Experiment I.Handle
openFile fp mode = do
  case mode of
    I.ReadMode -> depend (DFile fp, "file opened in read mode")
    I.WriteMode -> effect (EFile fp, "file opened in write mode")
    I.AppendMode -> effect (EFile fp, "file opened in append mode")
    I.ReadWriteMode ->
      depend (DFile fp, "file opened in read/write mode")
      >> effect (EFile fp, "file opened in read/write mode")
  experIO $ I.openFile fp mode

