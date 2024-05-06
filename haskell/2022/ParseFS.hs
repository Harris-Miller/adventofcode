module ParseFS where

import Control.Lens
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import System.IO.Unsafe

-- types

data File = File
  { name :: String,
    size :: Int
  }
  deriving (Show)

data Directory = Directory
  { _path :: String,
    _files :: [File],
    _subDirectories :: [String] -- need to be full paths
  }
  deriving (Show)

makeLenses ''File
makeLenses ''Directory

type FS = M.Map String Directory

type KeyedFS = (String, FS)

-- dir utils

splitDirString :: String -> [String]
splitDirString "/" = []
splitDirString xs = splitOn "/" $ tail xs -- remove leading "/"

joinDirList :: [String] -> String
joinDirList = ("/" ++) . intercalate "/" -- join and add back leading "/"

-- directory insertions

pushFile :: (Int, String) -> KeyedFS -> KeyedFS
pushFile (s, n) (pwd, fs) = (pwd, nextFs)
  where
    dir = fromJust $ M.lookup pwd fs
    fileL = view files dir
    nextFs = M.insert pwd (set files (fileL <> [File {name = n, size = s}]) dir) fs

pushSubDirectory :: String -> KeyedFS -> KeyedFS
pushSubDirectory dirName (pwd, fs) = (pwd, nextFs)
  where
    dir = fromJust $ M.lookup pwd fs
    subDir = view subDirectories dir
    dirPath = pwd ++ (if pwd == "/" then "" else "/") ++ dirName
    nextFs = M.insert pwd (set subDirectories (subDir <> [dirPath]) dir) fs

addContentToDirectory :: String -> KeyedFS -> KeyedFS
addContentToDirectory line kfs
  | "dir" `isPrefixOf` line = pushSubDirectory (drop 4 line) kfs
  | otherwise =
      let [s, n] = splitOn " " line
       in pushFile (read s, n) kfs

-- line parsers

changeDirectory :: String -> KeyedFS -> KeyedFS
changeDirectory "/" (_, fs) = ("/", M.insert "/" (Directory {_path = "/", _files = [], _subDirectories = []}) fs)
changeDirectory ".." (pwd, fs) = ((joinDirList . init . splitDirString) pwd, fs)
changeDirectory dirName (pwd, fs) = (nextPwd, nextFs)
  where
    nextPwd = (joinDirList . (<> [dirName]) . splitDirString) pwd
    nextFs =
      if M.notMember nextPwd fs
        then M.insert nextPwd (Directory {_path = nextPwd, _files = [], _subDirectories = []}) fs
        else fs

processInstruction :: String -> KeyedFS -> KeyedFS
processInstruction line kfs
  | "ls" `isPrefixOf` line = kfs -- no-op
  | otherwise = changeDirectory (drop 3 line) kfs

parseLine :: String -> KeyedFS -> KeyedFS
parseLine line kfs
  | "$" `isPrefixOf` line = processInstruction (drop 2 line) kfs
  | otherwise = addContentToDirectory line kfs

parseKeyedFS :: [String] -> KeyedFS
parseKeyedFS = foldl (flip parseLine) ("", M.empty)
