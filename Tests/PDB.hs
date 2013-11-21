module Tests.PDB
  (PDB, mkPDB, pdbPath)
where

import System.Directory (doesFileExist)
import System.FilePath (joinPath)
import Text.Printf (printf)

import Development.Fex

newtype PDB = PDB FlagSpec deriving Show

instance Depend PDB where
  missing pdb = do
    fp <- pdbPath "1ctf" pdb
    exists <- liftIO $ doesFileExist fp
    return $
      if exists then Nothing else
        Just $ printf "Could not find sample PDB file at '%s'." fp

mkPDB :: Experiment PDB
mkPDB = do
  f <- mkFlagDir "pdb" "/data/bio/pdb" "The path to your local PDB database."
  dep $ PDB f

-- Expands a PDB identifier to its full path using the official PDB directory
-- hierarchy. Note that the file path is not checked for existence.
-- e.g., "1ctf" --> "/pdb/path/ct/pdb1ctf.ent.gz"
pdbPath :: String -- ^ The PDB identifier.
        -> PDB
        -> Experiment String
pdbPath idcode (PDB flag) = do
  path <- runFlagDir flag
  return $ pdbPath' idcode path

pdbPath' idcode root =
  if length idcode == 4 then
    joinPath [root, take 2 $ drop 1 idcode, printf "pdb%s.ent.gz" idcode]
  else
    error "PDB identifier must be 4 characters long"
