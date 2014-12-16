

module Main where

import System.Directory
import Data.List.Split
import Data.Char


main = do
  fs <- getDirectoryContents "c:\\users\\s950142\\work"
  putStrLn $ unlines $ filter isJpeg fs

isJpeg :: String -> Bool
isJpeg f = if ext == "csv" then True else False
  where
  ext = last . splitOn "." f

