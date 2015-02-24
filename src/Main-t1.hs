

module Main where

import Data.ByteString.Char8 as BS
import System.IO
import System.Process

reso = 4

getFingerPrint :: String -> IO ByteString
getFingerPrint f = do
  (sin, sout, serr, ph) <- runInteractiveCommand command
  waitForProcess ph
  BS.hGetLine sout
  where
    geo = (show reso) ++ "x" ++ (show reso)
    size = reso * reso * 3
    command = "convert -define jpeg:size=" ++ geo
           ++ " -filter Cubic -resize " ++ geo ++ "! "
           ++ f ++ " PPM:- | tail -c " ++ (show size)

main :: IO ()
main = do
  s <- getFingerPrint "~/work/1227-26.jpg"
  BS.putStr s


