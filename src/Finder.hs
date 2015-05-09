--
-- Finder.hs
--

module Finder where

import Data.ByteString as B (pack, unpack)
import Data.ByteString.Char8 as BS (ByteString, hGet)
import Data.List
import Data.Word
import System.IO
import System.Process

type FingerPrint = [Word8]
type Image = (FingerPrint, FilePath)
type Edge = (FilePath, FilePath)

-- (r, t) = resolution of fingerprint and threshold of difference
findSame :: Int -> Int -> Int -> [FilePath] -> IO [[FilePath]]
findSame r t s fs = do
  fps <- mapM (getFingerPrint r) fs
  let ps = matchImage (fromIntegral t) (fromIntegral s) $ zip fps fs
  return $ deduplicate ps ps

deduplicate :: [[FilePath]] -> [[FilePath]] -> [[FilePath]]
deduplicate _ [] = []
deduplicate xs (y:ys)
  | any isProperSubset xs = deduplicate xs ys
  | otherwise             = y:deduplicate xs ys
  where
    isProperSubset :: [FilePath] -> Bool
    isProperSubset x = x /= y && y `isInfixOf` x

-- t = threshold
matchImage :: Word8 -> Double -> [Image] -> [[FilePath]]
matchImage t s []     = []
matchImage t s (x:[]) = []
matchImage t s (x:xs)
 | ps == []  = matchImage t s xs
 | otherwise = (snd x:ps):matchImage t s xs
 where
   ps = roundRobin t s x xs

roundRobin :: Word8 -> Double -> Image -> [Image] -> [FilePath]
roundRobin t s x [] = []
roundRobin t s x (y:ys) = isSame t s x y ++ roundRobin t s x ys

isSame :: Word8 -> Double -> Image -> Image -> [FilePath]
isSame t s x y
  | fst x == [] = []
  | fst y == [] = []
  | otherwise = if overcount > limit then [] else [snd y]
  where
    xy = zip (fst x) (fst y)
    limit = ceiling (fromIntegral(length xy) * (100 - s) / 100.0)
    overcount = length $ filter differ xy
    differ :: (Word8, Word8) -> Bool
    differ (a, b) = (if a > b then a - b else b - a) > t

-- | otherwise = if any differ (zip (fst x) (fst y)) then [] else [snd y]

getFingerPrint :: Int -> FilePath -> IO FingerPrint
getFingerPrint r f = do
  (sin, sout, serr, ph) <- runInteractiveCommand command
  waitForProcess ph
  fp <- BS.hGet sout size
  return $ B.unpack fp
  where
    geo = (show r) ++ "x" ++ (show r)
    size = r * r * 3
    command = "convert -filter Cubic -resize " ++ geo ++ "! "
           ++ f ++ " PPM:- | tail -c " ++ (show size)
