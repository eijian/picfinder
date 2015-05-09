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
  let (r, t, s, ds') = parseOpt ds
  fs <- mapM getFileLists ds'
  ss <- findSame r t s $ concat fs
  putGroups ss

parseOpt :: [String] -> (Int, Int, Int, [FilePath])
parseOpt (d:ds)
  | "-p" `isPrefixOf` d = (r, t, s, ds)
  | otherwise           = (8, 8, 100, d:ds)
  where
    [r, t, s] = map (read :: String -> Int) (splitOn "," (drop 2 d))

getFileLists :: FilePath -> IO [FilePath]
getFileLists d = do
  fs <- getDirectoryContents d
  return $ map (\ x -> d ++ delimiter ++ x) (filter isJpeg fs)

isJpeg :: FilePath -> Bool
isJpeg f
  | head f == '.' = False
  | ext /= picext = False
  | otherwise     = True
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
