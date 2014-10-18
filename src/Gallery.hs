{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Gallery
  ( Gallery (G)
  , genGal
  , readGalDirs
  ) where
import           Control.Monad
import           Data.Typeable        (Typeable)
import           Data.List
import           Data.Monoid
import           System.Directory
import           System.FilePath
import           System.FilePath.Posix
import           System.Posix.Files

import           Text.Blaze.Internal
import           Text.Blaze.Html5 hiding (html, param, map)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy.IO           as L

import           Common
import           TemplateSystem

import           Debug.Trace (trace)

data FoldrAndImgs = FAI { faiPath :: FilePath
                        , subFais :: [FoldrAndImgs]
                        , subImgs :: [FilePath] }
  deriving (Show)

data Gallery = G { galPath :: FilePath
                 , galImgs :: [(Int, FilePath)] }
  deriving (Show, Typeable)

{- ============================================================================
 - read Gallery to list
 -}

readGalDirs :: IO [Nav]
readGalDirs = return []

readGal :: IO [Gallery]
readGal = liftM flattenFais (getCurrentDirectory >>= (`readGal'` "galerie"))
  where
    readGal' :: FilePath -> FilePath -> IO FoldrAndImgs
    readGal' topdir curdir = let
      filterDots  = filter (`notElem` [".", ".."])
      filterDirs  = filterM (\d -> doesDirectoryExist $ topdir </> curdir </> d)
      filterFiles = filterM (\d -> doesFileExist (topdir </> curdir </> d))
      getAllSubImgs :: [FoldrAndImgs] -> [FilePath]
      getAllSubImgs []                = []
      getAllSubImgs (FAI _ _ is:fais) = is ++ getAllSubImgs fais
      in do
        allfilespre <- getDirectoryContents $ topdir </> curdir
        let allfiles = filterDots allfilespre
        fais <- filterDirs allfiles
          >>= mapM (\d -> readGal' topdir (curdir </> d))
        imgs <- filterFiles allfiles
        let allImgs = getAllSubImgs fais ++ map (curdir </>) [i | i <- imgs , "jpg" `isInfixOf` i]
        return $ FAI curdir fais (sortBy (flip compare) allImgs)

    flattenFais :: FoldrAndImgs -> [Gallery]
    flattenFais (FAI p fais is) = G p (zip [1..] is) : concatMap flattenFais fais

galleryPage = defaultPage { pStyle = "maximize" }

genGal :: SiteCfg -> IO [Gallery]
genGal sc = do
    rG <- readGal
    mapM_ genGalDirs rG
    compilePages sc $ concatMap genGalPs rG
    return rG
  where
    genGalDirs (G subdir _) = do
      ex <- doesDirectoryExist path
      unless ex (createDirectoryIfMissing True path)
        where path = outPath sc </> subdir

    genGalPs (G subdir l) = map (genGalP subdir) l
    genGalP subdir (c,img) = galleryPage { pPath = paths
                                         , pTitle = Just (show c)
                                         , pCtn = genHTML img}
      where paths = if' (c/=1) [ subdir </> (show c ++ ".html") ]
                              [ subdir </> (show c ++ ".html")
                              , subdir </> "index.html" ]

    genHTML img = H.div ! A.id "super" $ do
      H.img ! A.src (stringValue (url sc </> img))
      H.div ! A.id "imageOverlay" $ " "
