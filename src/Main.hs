--
-- Main.hs
--

module Main where

import System.Directory
import System.Environment
import Data.List
import Data.List.Split
import Data.Char
import Finder

picext = "JPG"
delimiter = "/"

main :: IO ()
main = do
  ds <- getArgs
  let (r, t, ds') = parseOpt ds
  fs <- mapM getFileLists ds'
  ss <- findSame r t $ concat fs
  putGroups ss

parseOpt :: [String] -> (Int, Int, [FilePath])
parseOpt (d:ds)
  | "-p" `isPrefixOf` d = (r, t, ds)
  | otherwise           = (8, 8, d:ds)
  where
    [r, t] = map (read :: String -> Int) (splitOn "," (drop 2 d))

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
