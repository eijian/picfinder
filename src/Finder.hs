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

findSame :: [FilePath] -> IO [[FilePath]]
findSame fs = do
  fps <- mapM (getFingerPrint 4) fs
  return $ matchImage $ zip fps fs

matchImage :: [Image] -> [[FilePath]]
matchImage []     = []
matchImage (x:[]) = []
matchImage (x:xs)
 | ps == []  = matchImage xs
 | otherwise = (snd x:ps):(matchImage xs)
 where
   ps = roundRobin x xs

roundRobin :: Image -> [Image] -> [FilePath]
roundRobin x [] = []
roundRobin x (y:ys)
  | isSame x y == False = roundRobin x ys
  | otherwise           = (snd y):(roundRobin x ys)

threshold = 4 :: Word8 -- Threshold for comparison between Images

isSame :: Image -> Image -> Bool
isSame x y = d == Nothing
  where
    d = find differ (zip (fst x) (fst y))
    differ :: (Word8, Word8) -> Bool
    differ (a, b) = d' > threshold
      where
        d' = if a > b then a - b else b - a

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

getFingerPrint4 = getFingerPrint 4

