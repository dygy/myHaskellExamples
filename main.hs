import Data.List
import System.Directory
import System.FilePath

main = do
  homeDir <- getHomeDirectory
  setCurrentDirectory homeDir
  _files <- listDirectory homeDir
  _search _files

-- basically i wrote such function because i was tiered to debug whole home directory
-- or doing hardcode like _files!!45, so this one just search Downloads dir in home.
_search _files = do
  if null _files then pure()
  else do
    let _head = head _files
    let _name = takeBaseName _head

    if _name == "Downloads"
      then do
        print _name
        _absolute <- makeAbsolute _head
        _dir _absolute
    else do
      let _newFiles = tail _files
      _search _newFiles

-- this one setting working directory and getting files/subdirectories from directory
_dir _path = do
    setCurrentDirectory _path
    _files <- listDirectory _path
    _move _files

-- this one is moving trough directory, with recursive call to itself with new list (cut tail)
_move _files = do
  if null _files
    then do
      _cd <- getCurrentDirectory
      print("DONE with" <> " " <> _cd)
  else do
    _scan (head _files)
    _move (tail _files)

-- this one is scanning directory, it's checking either element is file or sub directory,
-- and call _dir for directories.
-- there can be more code in else. Like reading files, checking extension
_scan _file = do
  isDir <- doesDirectoryExist _file
  if isDir
    then do
     _cd <- getCurrentDirectory
     _absolute <- makeAbsolute _file
     _dir _absolute
     setCurrentDirectory _cd
     print("CHECKING DIR" <> " " <> _file)
  else do
    print("GOT FILE" <> " " <> _file)
    pure ()
