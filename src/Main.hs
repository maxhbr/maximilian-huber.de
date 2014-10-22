--------------------------------------------------------------------------------
import           System.FilePath.Posix ((</>))
import           System.Directory
import           Control.Monad
import           Data.Monoid
import           Data.Maybe

import           Core

main = do
  curr <- getCurrentDirectory
  makePage (\x -> (scPre x) { outPath = curr </> outPath (scPre x) })
