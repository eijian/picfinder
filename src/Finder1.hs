--
-- Finder.hs
--

module Finder where

findSame :: [String] -> [[String]]
findSame fs = map toPair fs

toPair :: String -> [String]
toPair f = [f, f]
