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
import           Gallery
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
    makePagePre sc
    -- copy static files:
    static sc
    -- compile blaze-html pages
    compilePages sc [webdesign sc,impress sc,gpgPubkey sc]
    -- generate more css with clay:
    genCss sc
    -- read the gallery:
    genGal sc fai
    -- copy to main index file:
    when (isJust (indexP sc)) ( do
      ex <- doesFileExist (outPath sc </> fromJust (indexP sc))
      when ex $
        copyFile (outPath sc </> fromJust (indexP sc)) 
                 (outPath sc </> "index.html"))
  where makePagePre sc = do
          ex <- doesDirectoryExist (outPath sc)
          when ex (do
            ex <- doesDirectoryExist (outPath sc ++ "-old")
            when ex (removeDirectoryRecursive (outPath sc ++ "-old"))
            renameDirectory (outPath sc) (outPath sc ++ "-old"))
          unless ex (createDirectoryIfMissing True (outPath sc))

