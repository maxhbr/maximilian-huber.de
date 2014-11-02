module Static (
  static
  ) where

{-import           System.Path   -}
import           Control.Monad (liftM, filterM, forM_, mapM_,when)
import           Control.Applicative
import           System.Directory
import           System.FilePath
import           Data.List ((\\),isPrefixOf)

import           Common
import           Debug.Trace (trace)

static :: SiteCfg -> IO ()
static sc = static' (outPath sc) (statics sc)
  where static' op []     = print "static files done"
        static' op (d:ds) = do
          fex <- doesFileExist d
          when fex $ copyFile d (op </> d)
          dex <- doesDirectoryExist d
          when dex $ copyDir d (op </> d)
          static' op ds

-------------------------------------------------------------------------------
-- stolen from:
-- ???

-- | Remove useless paths from a list of paths.
filterUseless :: [FilePath] -> [FilePath]
filterUseless = (\\ [".", ".."])

-- | Returns a list of nodes in a tree via a depth-first walk.
mtreeList :: Monad m => (a -> m [a]) -> a -> m [a]
mtreeList children root = do
  xs <- children root
  subChildren <- mapM (mtreeList children) xs
  return $ root : concat subChildren

-- | Get a list of files in path, but not recursively. Removes '.' and '..'.
topFileList :: FilePath -> IO [FilePath]
topFileList path =
  (map (path </>) . filterUseless) <$> getDirectoryContents path

-- | We can use this data type to represent the pieces of a directory.
data Directory = Directory
                 { -- | The path of the directory itself.
                   dirPath :: FilePath
                   -- | All subdirectories of this directory.
                 , subDirs :: [FilePath]
                   -- | All files contained in this directory.
                 , files   :: [FilePath]
                 }
               deriving (Show)

-- | Creates a Directory instance from a FilePath.
createDir :: FilePath -> IO Directory
createDir path = do
    contentsPre <- topFileList path
    let contents = filter (\d -> not $ "." `isPrefixOf` takeFileName d) contentsPre
    subdirs  <- filterM doesDirectoryExist contents
    files    <- filterM doesFileExist contents
    return (Directory path subdirs files)

-- | Walk a directory depth-first. Similar to Python's os.walk and fs.core/walk
-- from the fs Clojure library.
walkDir :: FilePath -> IO [Directory]
walkDir root = createDir root >>= mtreeList children
  where children path = do
          let dirs = subDirs path
          mapM createDir dirs

-- | Given a root (prefix), remove it from a path. This is useful
-- for getting the filename and subdirs of a path inside of a root.
removeRoot :: FilePath -> FilePath -> FilePath
removeRoot prefix = drop . length $ addTrailingPathSeparator prefix

-- | Given a root path, a new root path, and a path to be changed,
-- removes the old root from the path and replaces it with to.
replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
replaceRoot root to path = to </> removeRoot root path

-- | Copy a directory recursively. Moves every file, creates every directory.
copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  createDirectoryIfMissing True to
  walked <- walkDir from
  forM_ walked $ \(Directory _ dirs files) -> do
    mapM_ (createDirectoryIfMissing True . replaceRoot from to) dirs
    forM_ files $ \path -> copyFile path (replaceRoot from to path)
