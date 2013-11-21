module Tests.SCOP
where

import System.FilePath (joinPath)
import Text.Printf (printf)

import Development.Fex

import Tests.FragLib

data SCOP = SCOP LibraryType FlagSpec deriving Show

instance Depend SCOP where
  missing (SCOP Structure flag) = do
    dir' <- runFlagDir flag
    let dir = joinPath [dir', "pdb"]
    mkDir dir >>= missing
  missing (SCOP Sequence flag) = do
    dir' <- runFlagDir flag
    let dir = joinPath [dir', "fasta"]
    mkDir dir >>= missing

mkSCOP :: LibraryType -> Experiment SCOP
mkSCOP t = do
  f <- mkFlagDir "scop" "/data/bio/scop" "The path to your local SCOP database."
  dep $ SCOP t f

scopPath :: String -- ^ A SCOP identifier.
         -> SCOP
         -> Experiment String
scopPath idcode (SCOP t flag) = do
  path <- runFlagDir flag
  return $ case t of
    Structure -> let dir = take 2 $ drop 2 idcode
                  in joinPath [path, "pdb", dir, printf "%s.ent" idcode]
    Sequence -> joinPath [path, "fasta", printf "%s.fasta" idcode]
