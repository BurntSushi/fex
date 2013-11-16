{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Development.Fex.Depends where

import System.Directory (findExecutable)
import Text.Printf (printf)

import Development.Fex.Experiment

newtype Exe = Exe String deriving Show

instance Depend Exe String where
  depend = return . Right . Exe

runExe :: Exe -> Experiment String
runExe (Exe cmd) = experIO $ do
  printf "Running command '%s'...\n" cmd
  return $ "result of '" ++ cmd ++ "'"

