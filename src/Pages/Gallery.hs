{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Pages.Gallery
  ( Gallery (G), FoldrAndImgs
  , readGal
  , genGal, faiToNav
  ) where
import           Control.Monad
import           Data.Typeable        (Typeable)
import           Data.List
import           Data.Char
import           System.Directory
import           System.FilePath

import           Text.Blaze.Internal
import           Text.Blaze.Html5 hiding (html, param, map)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Common
import           TemplateSystem

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

imageSort :: FilePath -> FilePath -> Ordering
imageSort i1 i2 = compare (takeFileName i2) (takeFileName i1)

readGal :: IO FoldrAndImgs
readGal = getCurrentDirectory >>= (`readGal'` "galerie")
  where
    readGal' :: FilePath -> FilePath -> IO FoldrAndImgs
    readGal' topdir curdir = let
      filterDots  = filter (\d -> not $ "." `isPrefixOf` d)
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
        let allImgs = getAllSubImgs fais ++ map (curdir </>) [i | i <- imgs , "jpg" `isSuffixOf` i || "png" `isSuffixOf` i || "JPG" `isSuffixOf` i]
        return $ FAI curdir fais (sortBy imageSort allImgs)

faiToNav :: FoldrAndImgs -> Nav
faiToNav fai = N { navTitle = normalize (takeFileName (faiPath fai))
                  , navPath  = Just $ faiPath fai </> "index.html"
                  , subs     = map faiToNav (subFais fai)}
  where normalize ""    = ""
        normalize (h:t) = toUpper h : t

genGal :: SiteCfg -> FoldrAndImgs -> IO ()
genGal sc fai = do
    let rG = flattenFais fai
    mapM_ genGalDirs rG
    compilePages sc $ concatMap genGalPs rG
    putStrLn "gallery done"
  where
    flattenFais :: FoldrAndImgs -> [Gallery]
    flattenFais (FAI p fais is) = G p (zip [1..] is) : concatMap flattenFais fais

    galleryPage = (defaultP sc) { pStyle = "maximize" }

    genGalDirs (G subdir _) = do
      ex <- doesDirectoryExist path
      unless ex (createDirectoryIfMissing True path)
        where path = outPath sc </> subdir

    genGalPs :: Gallery -> [Page]
    genGalPs (G subdir l) = map (genGalP subdir (length l)) l
    genGalP :: FilePath -> Int -> (Int, FilePath) -> Page
    genGalP subdir num (c,img) =
        galleryPage { pPath  = paths
                    , pTitle = Just $ subdir ++ "/" ++ show c
                    , pCtn   = genHTML img
                    , pLine  = Just line }
      where paths = if' (c/=1) [ subdir </> (show c ++ ".html") ]
                              [ subdir </> (show c ++ ".html")
                              , subdir </> "index.html"
                              , subdir ++ ".html" ]

            prevPage = stringValue $
              myTrimUrl sc $
                url sc </> subdir </> (show (c-1) ++ ".html")
            nextPage = stringValue $
              myTrimUrl sc $
                url sc </> subdir </> (show (c+1) ++ ".html")

            line :: Html
            line = ul $ do
              li "Bild "
              when (c > 1) (li $
                a ! A.href prevPage $ "<")
              li $ toHtml $ show c
              li " von "
              li $ toHtml $ show num
              when (c < num) (li $
                a ! A.href nextPage $ ">")
              li " in "
              li $ toHtml subdir

            genHTML img = do
              H.div ! A.id "imgBackgroundWrapper" $
                H.div ! A.id "imgBackground"
                      ! A.style (stringValue $ "background-image: url(" ++ (url sc </> img) ++ ")") $
                " "
              H.img ! A.src (stringValue (url sc </> img))
              H.div ! A.id "imageOverlay" $ do
                when (c > 1) ( a ! A.href prevPage
                                 ! A.id "toleft" $
                  H.div $
                    H.div ! A.class_ "inner" $
                       " ")
                when (c < num) ( a ! A.href nextPage
                                   ! A.id "toright" $
                  H.div $
                    H.div ! A.class_ "inner" $
                      " ")

