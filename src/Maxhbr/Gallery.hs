module Maxhbr.Gallery
  ( readGallery
  , getGalleryPagesInAllGalleries
  ) where

import           System.FilePath.Posix ((</>))
import           System.Directory
import           System.FilePath
import           Data.List
import           Data.Digest.Pure.MD5 (hash', MD5Digest)
import           Data.ByteString.Char8 (pack)

import           Maxhbr.Types

data Gallery
  = GalleryImage FilePath
  | GalleryDir FilePath [Gallery]
  deriving (Show)

data GalleryPage
  = GalleryPage
    { gpImagePath :: FilePath
    , gpIndex :: Int
    , gpHasNext :: Bool
    , gpOutputPathes :: [FilePath]
    , gpPermalink :: FilePath
    } deriving (Show, Eq)

readGallery :: FilePath -> IO Gallery
readGallery file = let
    readGalleryDir = do
      allFiles <- getDirectoryContents file
      gals <- mapM (readGallery . (file </>)) $
              filter (\d -> not $ "." `isPrefixOf` d)
              allFiles
      return $ GalleryDir file gals
  in do
    isdir <- doesDirectoryExist file
    if isdir
      then readGalleryDir
      else return $ GalleryImage file

flattenGallery :: Gallery -> [Gallery]
flattenGallery gallery@(GalleryImage _)  = [gallery]
flattenGallery gallery@(GalleryDir _ gs) = gallery : concatMap flattenGallery gs

getGalleryPagesInGallery :: Gallery -> [GalleryPage]
getGalleryPagesInGallery gallery@(GalleryDir galleryDir _) = let
    getImagesInGallery' :: Gallery -> [FilePath]
    getImagesInGallery' (GalleryImage img)     = [img]
    getImagesInGallery' (GalleryDir _ entries) = concatMap getImagesInGallery' entries

    sortImagesInGallery :: FilePath -> FilePath -> Ordering
    sortImagesInGallery fp1 fp2 = compare (takeFileName fp1) (takeFileName fp2)

    sortedImages :: [FilePath]
    sortedImages = (sortBy sortImagesInGallery . getImagesInGallery') gallery

    mkGalleryPage :: Bool -> (FilePath, Int) -> GalleryPage
    mkGalleryPage hasNext (img,i) = let
        permalink = galleryDir </> ("plink" ++ (show . (\p -> hash' p :: MD5Digest) . pack) img) ++ ".html"
        pathes = [ galleryDir </> (show i ++ ".html")
                 , permalink ]
                 ++ [galleryDir </> "index.html" | i == 1]
      in GalleryPage img i hasNext pathes permalink
    mkGalleryPages :: [(FilePath, Int)] -> [GalleryPage]
    mkGalleryPages []      = []
    mkGalleryPages [e]     = [mkGalleryPage False e]
    mkGalleryPages (e:fps) = mkGalleryPage True e : mkGalleryPages fps
  in mkGalleryPages $ zip sortedImages [1..]
getGalleryPagesInGallery (GalleryImage _) = []

getGalleryPagesInAllGalleries :: Producer Gallery GalleryPage
getGalleryPagesInAllGalleries = Producer $ nub . concatMap getGalleryPagesInGallery . flattenGallery

getPagesOfGalleryPage :: Producer GalleryPage Page
getPagesOfGalleryPage = Producer (\gp ->
                                    [Page undefined undefined undefined (gpOutputPathes gp)]
                                 )

getNavOfGalleries :: Gallery -> Navigation
getNavOfGalleries (GalleryImage _)          = []
getNavOfGalleries (GalleryDir directory gs) = [NavEntry (takeFileName directory) directory (concatMap getNavOfGalleries gs) 0]
