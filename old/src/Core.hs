module Core
  (makePage
  , module X
  ) where
import           System.FilePath.Posix ((</>))
import           System.Directory
import           Control.Monad
import           Data.Monoid
import           Data.Maybe

import           Css
import           Common as X
import           Config as X
import           Static
import           TemplateSystem
import           Pages

makePage :: (Nav -> SiteCfg) -> IO ()
makePage scPre = do
    -- read gallery to data structure:
    fai <- readGal

    let sc = scPre (faiToNav fai)
    -- copy static files:
    static sc
    -- compile blaze-html pages
    compilePages' sc [ webdesign
                     , kontakt
                     , impress
                     , gpgPubkey ]
    -- generate more css with clay:
    genCss sc
    -- make the gallery:
    genGal sc fai
    -- -- make the blog
    -- genBlog sc
    -- copy to main index file:
    when (isJust (indexP sc)) ( do
      ex <- doesFileExist (outPath sc </> fromJust (indexP sc))
      when ex $
        copyFile (outPath sc </> fromJust (indexP sc)) 
                 (outPath sc </> "index.html"))

