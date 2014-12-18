--
-- Main.hs
--

module Main where

import System.Environment
import System.Directory
import Data.List.Split
import Data.Char

picext = "JPG"
delimiter = "/"

main :: IO ()
main = do
  ds <- getArgs
  fs <- mapM getFileLists ds
  putPics $ concat fs

getFileLists :: FilePath -> IO [(String, String)]
getFileLists f = do
  fs <- getDirectoryContents f
  return $ map (toFileInfo f) (filter isJpeg fs)

isJpeg :: String -> Bool
isJpeg f = if ext == picext then True else False
  where
  ext = map toUpper (last $ splitOn "." f)

toFileInfo :: String -> String -> (String, String)
toFileInfo d f = (d ++ delimiter ++ f, f)

putPics :: [(String, String)] -> IO ()
putPics [] = putStr ""
putPics (p:ps) = do
  putStrLn $ fst p
  putPics ps
