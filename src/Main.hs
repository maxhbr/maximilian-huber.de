--------------------------------------------------------------------------------
import           System.FilePath.Posix ((</>))

import           Css
import           Gallery
import           Common
import           Static
import           TemplateSystem

--------------------------------------------------------------------------------
--  Global config
sc = SC { staticFolders = ["css","galerie","images"]
        , url           = "maximilian-huber.de"
        , outPath       = "_site"
        , cssFile       = "css" </> "default.css"
        }

--------------------------------------------------------------------------------
--  Run
makePage :: SiteCfg -> IO ()
makePage sc = do
  static sc
  css sc
  genGalerieRoutine sc

main = makePage sc
