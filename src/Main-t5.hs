module Main where

import Data.ByteString as B
import Data.Char
import Data.Word
import Data.ByteString.Internal
import Finder

main :: IO ()
main = do
  bs <- getFingerPrint 8 "/Users/eiji/work/sample1.jpg"
  Prelude.putStrLn $ show bs
  Prelude.putStrLn $ show $ Prelude.length bs
  bs' <- getFingerPrint 8 "/Users/eiji/work/sample7.jpg"
  Prelude.putStrLn $ show bs'
  Prelude.putStrLn $ show $ Prelude.length bs'
  let bs'' = Prelude.map df (Prelude.zip bs bs')
  Prelude.putStrLn $ show bs''

df :: (Word8, Word8) -> Word8
df (a, b) = if a > b then a - b else b - a

