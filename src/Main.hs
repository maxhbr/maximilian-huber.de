--------------------------------------------------------------------------------
import           System.FilePath.Posix ((</>))
import           System.Directory
import           Control.Monad

import           Css
import           Gallery
import           Common
import           Static
import           TemplateSystem

--------------------------------------------------------------------------------
--  Global config
sc = SC { staticFolders = ["css","galerie","images","gpg-pubkey.asc"]
        , url           = "maximilian-huber.de"
        , outPath       = "_site"
        , cssFile       = "css" </> "default.css"
        , defaultPage   = defaultPage
        }

--------------------------------------------------------------------------------
--  Run
makePage :: SiteCfg -> IO ()
makePage sc = do
    makePagePre
    -- copy static files:
    static sc
    compileRaws sc ["gpg-pubkey.asc", "impress.html"]
    -- generate more css with clay:
    css sc
    -- read the gallery
    rG <- genGal sc
  where makePagePre = do
          ex <- doesDirectoryExist (outPath sc)
          when ex (do 
            ex <- doesDirectoryExist (outPath sc ++ "-old")
            removeDirectoryRecursive (outPath sc ++ "-old")
            renameDirectory (outPath sc) (outPath sc ++ "-old")) 
          unless ex (createDirectoryIfMissing True (outPath sc))

main = do
  curr <- getCurrentDirectory
  makePage (sc { url = curr </> outPath sc
               , outPath = curr </> outPath sc })
