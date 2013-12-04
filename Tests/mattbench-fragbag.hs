module Main where

import Data.List (intercalate)
import System.FilePath (joinPath)

import Development.FexA.Experiment

cat :: Fex
cat = pure (\s1 -> Pure (\s2 -> String $ show s1 ++ show s2))

pjoin :: Fex
pjoin = pure (\s1 -> Pure (\s2 -> String $ joinPath [show s1, show s2]))

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
      args      = [str "-threshold", threshold, dists, tree, outf]
   in runExe (str "mattbench-cluster") args

main :: IO ()
main = do
  let deps = staticDeps cutTree
  stats <- fmap (zip deps) $ mapM depMissing deps
  putStrLn $ intercalate "\n" $ map depStatus stats
-- main = print $ depTree cutTree 
-- main = evalIO' cutTree >>= print 

