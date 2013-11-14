module Development.Fex.IO where

import qualified System.IO as I

import Development.Fex.Experiment

experIO :: IO a -> Experiment a
experIO a = Experiment $ \e -> return (a, e)
-- experIO io = Experiment $ \e -> io >>= \a -> return (a, e) 

dummy :: Experiment String
dummy = do
  depend (DExec "dummy", "dummy executable")
  experIO $ do
    putStrLn "dummy executable called"
    return "dummy"

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

