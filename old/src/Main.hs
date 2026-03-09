--------------------------------------------------------------------------------
import           System.FilePath.Posix ((</>))
import           System.Directory
import           System.Environment
import           System.Exit
import           Control.Monad
import           Data.Monoid
import           Data.Maybe

import           Core

main :: IO()
main = do
  args <- getArgs
  curr <- getCurrentDirectory
  if (args == ["local"])
    then makePage (\x -> (scPre x) { url     = curr </> outPath (scPre x) ,
                                     outPath = curr </> outPath (scPre x) })
    else makePage (\x -> (scPre x) { outPath = curr </> outPath (scPre x) })
