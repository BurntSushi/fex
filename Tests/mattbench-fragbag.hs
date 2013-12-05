module Main where

import Data.List (intercalate)
import System.FilePath (combine)

import Development.FexA.Experiment

lift2Str :: (String -> String -> String) -> Fex
lift2Str f = lit $ Pure (\s1 -> Pure (String . f (show s1) . show))

cat :: Fex
cat = lift2Str (++)

pjoin :: Fex
pjoin = lift2Str combine

cutTree :: Fex
cutTree =
  let threshold = flag $ FDouble "threshold" 0.097702
                                 "Homology distance at which to cut tree."
      dists     = file $ flag $ FString "dists" "/data/bio/mattbench/astral.gob"
                                        "Mattbench alignment distances."
      tree      = file $ flag $ FString "tree" "/data/bio/mattbench/dendrogram.tree"
                                        "The dendrogram in Newick tree format."
      resDir    = dir $ flag $ FString "results" "/tmp/fex/mattbench"
                                       "Directory to store results."
      outf      = cat `app` (pjoin `app` resDir `app` threshold) `app` str ".csv"
      args      = list [str "-threshold", threshold, dists, tree, outf]
   in runExe (str "mattbench-cluster") `app` args

main :: IO ()
-- main = do 
  -- let deps = staticDeps cutTree 
  -- stats <- fmap (zip deps) $ mapM depMissing deps 
  -- putStrLn $ intercalate "\n" $ map depStatus stats 
-- main = print $ depTree cutTree 
main = evalIO' cutTree >>= print

