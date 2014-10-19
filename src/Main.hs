--------------------------------------------------------------------------------
import           System.FilePath.Posix ((</>))
import           System.Directory
import           Control.Monad
import           Data.Monoid

import           Css
import           Gallery
import           Common
import           Static
import           TemplateSystem
import           Pages

--------------------------------------------------------------------------------
--  Global config
genDefaultNav galNav = N { navTitle = "Home"
                         , navPath  = Just ""
                         , subs     =
                           [ galNav
                           , N { navTitle = "Webdesign"
                               , navPath  = Just "webdesign.html"
                               , subs     = []}
                           , N { navTitle = "Kontakt"
                               , navPath  = Nothing
                               , subs     =
                                 [ N { navTitle = "gpg-pubkey"
                                     , navPath  = Just "gpg-pubkey.html"
                                     , subs     = []}
                                 , N { navTitle = "Impress"
                                     , navPath  = Just "impress.html"
                                     , subs     = []}]}]}
scPre galNav = SC { statics = ["css","galerie","images"
                              ,"gpg-pubkey.asc","favicon.ico"
                              ,"qr.jpg","qr_large.jpg"]
                  , url           = "http://maximilian-huber.de/develop/3"
                  , outPath       = "_site"
                  , cssFile       = "css" </> "default.css"
                  , defaultP      = P { pPath  = []
                                      , pTitle = Nothing
                                      , pStyle = "text"
                                      , pCtn   = mempty
                                      , pNav   = genDefaultNav galNav
                                      , pLine  = Nothing}}

--------------------------------------------------------------------------------
--  Run
makePage :: (Nav -> SiteCfg) -> IO ()
makePage scPre = do
    -- read gallery to data structure:
    fai <- readGal

    let sc = scPre (faiToNav fai)
    makePagePre sc
    -- copy static files:
    static sc
    -- compile Raw files:
    compileRaws sc ["gpg-pubkey.asc"]
    -- compile blaze-html pages
    compilePages sc [webdesign sc,impress sc]
    -- generate more css with clay:
    css sc
    -- read the gallery:
    genGal sc fai
  where makePagePre sc = do
          ex <- doesDirectoryExist (outPath sc)
          when ex (do
            ex <- doesDirectoryExist (outPath sc ++ "-old")
            when ex (removeDirectoryRecursive (outPath sc ++ "-old"))
            renameDirectory (outPath sc) (outPath sc ++ "-old"))
          unless ex (createDirectoryIfMissing True (outPath sc))

main = do
  curr <- getCurrentDirectory
  -- makePage (\x -> (scPre x) { url     = curr </> outPath (scPre x)
  --                          , outPath = curr </> outPath (scPre x) })
  makePage (\x -> (scPre x) { outPath = curr </> outPath (scPre x) })
