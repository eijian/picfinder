--
-- Finder.hs
--

module Finder where

import Data.ByteString.Char8 as BS (ByteString, hGetLine)
import Data.Map as Map (Map, elems, empty, insert, lookup)
import System.IO
import System.Process

type FingerPrint = ByteString

findSame :: [FilePath] -> IO [[FilePath]]
findSame fs = do
  fps <- mapM getFingerPrint4 fs
  let es = Map.elems $ foldl insertItem Map.empty (zip fps fs)
  return $ filter (\x -> length x > 1) es

insertItem :: Map FingerPrint [FilePath] -> (FingerPrint, FilePath)
              -> Map FingerPrint [FilePath]
insertItem m x = Map.insert k l m
  where
    k = fst x
    l = toList x (Map.lookup k m)
    toList :: (FingerPrint, FilePath) -> Maybe [FilePath] -> [FilePath]
    toList x Nothing = [snd x]
    toList x (Just l) = (snd x:l)

getFingerPrint :: Int -> FilePath -> IO FingerPrint
getFingerPrint r f = do
  (sin, sout, serr, ph) <- runInteractiveCommand command
  waitForProcess ph
  fp <- BS.hGetLine sout
  return fp
  where
    geo = (show r) ++ "x" ++ (show r)
    size = r * r * 3
    command = "convert -define jpeg:size=" ++ geo
           ++ " -filter Cubic -resize " ++ geo ++ "! "
           ++ f ++ " PPM:- | tail -c " ++ (show size)

getFingerPrint4 = getFingerPrint 4
