module Main where

import Debug.Trace (trace)

import Data.Monoid
import qualified System.IO as I

import Development.Fex.Experiment
import Development.Fex.IO

test1 :: Experiment I.Handle
test1 = do
  depend (DExec "ls", "list files")
  depend (DExec "cat", "cat files")
  openFile "./cauchy" I.WriteMode

main = do
  -- deps <- dependsExper dummy 
  -- print deps 
--  
  -- d <- evalExper dummy 
  -- print d 

  deps <- dependsExper test1
  print deps
  effs <- effectsExper test1
  print effs

  -- handle <- evalExper test1 
  -- I.hPutStrLn handle "test test" 
  -- I.hClose handle 

