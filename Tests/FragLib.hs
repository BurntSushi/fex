module Tests.FragLib
where

import Development.Fex

data LibraryType = Structure | Sequence deriving Show

getLibraryType :: Experiment LibraryType
getLibraryType = do
  structure <- mkFlagBool "structure" "Use structure fragment libraries."
                 >>= runFlagBool
  return $ if structure then Structure else Sequence
