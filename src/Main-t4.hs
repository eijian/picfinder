module Main where

import Data.ByteString as B
import Finder

main :: IO ()
main = do
  bs <- getFingerPrint4 "/Users/eiji/work/1227-26.jpg"
  Prelude.putStrLn $ show $ B.unpack bs
  bs' <- getFingerPrint4 "/Users/eiji/work/1227-1.jpg"
  Prelude.putStrLn $ show $ B.unpack bs'
