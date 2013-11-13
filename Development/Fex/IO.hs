module Development.Fex.IO where

import qualified System.IO as I

import Development.Fex.Experiment

dummy :: Experiment String
dummy = depend (DExec "dummy", "dummy executable")
        >> (Experiment $ \e -> do
             putStrLn "dummy executable called"
             return ("dummy", e))

openFile :: I.FilePath -> I.IOMode -> Experiment I.Handle
openFile fp mode = depEffs >> expHandle
  where expHandle = Experiment $ \e -> do
                      handle <- I.openFile fp mode
                      return (handle, e)
        depEffs =
          case mode of
            I.ReadMode -> depend (DFile fp, "file opened in read mode")
            I.WriteMode -> effect (EFile fp, "file opened in write mode")
            I.AppendMode -> effect (EFile fp, "file opened in append mode")
            I.ReadWriteMode ->
              depend (DFile fp, "file opened in read/write mode")
              >> effect (EFile fp, "file opened in read/write mode")

