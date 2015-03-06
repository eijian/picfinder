--
-- Main.hs
--

module Main where

import System.Directory
import System.Environment
import Data.List.Split
import Data.Char
import Finder

picext = "JPG"
delimiter = "/"

main :: IO ()
main = do
  ds <- getArgs
  fs <- mapM getFileLists ds
  ss <- findSame $ concat fs
  putGroups ss

getFileLists :: FilePath -> IO [FilePath]
getFileLists d = do
  fs <- getDirectoryContents d
  return $ map (\ x -> d ++ delimiter ++ x) (filter isJpeg fs)

isJpeg :: FilePath -> Bool
isJpeg f = if ext == picext then True else False
  where
  ext = map toUpper (last $ splitOn "." f)

putGroups :: [[FilePath]] -> IO ()
putGroups [] = putStr ""
putGroups (p:ps) = do
  putStrLn ("probably same: " ++ showGroup p)
  putGroups ps

showGroup :: [FilePath] -> String
showGroup [] = ""
showGroup (f:[]) = f
showGroup (f:fs) = f ++ ", " ++ showGroup fs
