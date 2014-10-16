{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Gallery
  ( Gallery (G)
  , genGalerieRoutine
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

import           Debug.Trace (trace)

data FoldrAndImgs = FAI { faiPath :: FilePath
                        , subFais :: [FoldrAndImgs]
                        , subImgs :: [FilePath] }
  deriving (Show)

data Gallery = G { galPath :: FilePath
                 , galImgs :: [(Int, FilePath)] }
  deriving (Show, Typeable)

--getGP (G gp _ ) = gp

{-
data Menu = M { subMenu :: [Menu]
              , menuTitle :: String }
 -}

{- ============================================================================
 - Main function
 -}

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

{-genMenuLists :: [[]]-}

{- ============================================================================
 - Helper functions
 -}

{-
customZipper l = zip3 l1 l2 l3
  where e  = (0,"")
        l1 = e : (e : l)
        l2 = e : l ++ [e]
        l3 = l ++ [e,e]
 -}

siteGalery = S { sPath = []
               , sTitle = ""
               , sStyle = "galery"
               , sCtn = mempty
               , sNav = N { navTitle = "home"
                         , navPath = "/"
                         , subs = []
                         }
               , sLine = mempty
               }

genGalerieRoutine sc = do
    ex <- doesDirectoryExist (outPath sc)
    unless ex (createDirectoryIfMissing True (outPath sc))

    rG <- readGal
    mapM_ customMkDir rG
    mapM_ customMkFile rG
  where
    customMkDir (G subdir _) = do
      ex <- doesDirectoryExist path
      unless ex (createDirectoryIfMissing True path)
        where path = outPath sc </> subdir

    customMkFile (G subdir l) = customMkFile' subdir l
    customMkFile' subdir (cimg:[]) =
      customMkFile'' subdir cimg
    customMkFile' subdir (cimg:is) = do
      customMkFile'' subdir cimg
      customMkFile' subdir is
    customMkFile'' subdir (c,img) = do
      when (c == 1)
           (L.writeFile (outPath sc </> subdir </> "index.html") (genHTML img))
      L.writeFile (outPath sc </> subdir </> (show c ++ ".html")) (genHTML img)

    customMkSite (G subdir l) = mapM (customMkSite' subdir) l
    customMkSite' subdir (c,img) = undefined

    genHTML img = renderHtml $
      H.div ! A.id "super" $ do
        H.img ! A.src (stringValue ("/" ++ img))
        H.div ! A.id "imageOverlay" $ " "
