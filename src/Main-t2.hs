
module Main where

import Data.Map as Map

dat = [("a", "apache"), ("e", "emacs"), ("a", "ant"), ("c", "ceph")]

main :: IO ()
main = do
  putStrLn (show ks)
  putStrLn (show es)
  where
    m = tomap dat
    ks = Map.keys m
    es = Map.elems m

tomap :: [(String, String)] -> Map String [String]
tomap [] = Map.empty
tomap (x:xs) = insertItem x (tomap xs)

insertItem :: (String, String) -> Map String [String] -> Map String [String]
insertItem x m = Map.insert k l m
  where
    k  = fst x
    l  = tolist x (Map.lookup k m)

tolist :: (String, String) -> Maybe [String] -> [String]
tolist x Nothing = [snd x]
tolist x (Just l) = (snd x:l)

