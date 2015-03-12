{-# LANGUAGE OverloadedStrings #-}
module Pages.Blog
  ( genBlog
  ) where

import           Control.Monad
import           Data.List
import           Data.List.Split (splitOn)
import           System.Directory
import           System.FilePath
import           System.FilePath.Posix
import           System.Posix.Files
import           Text.Blaze.Internal
import           Text.Blaze.Html5 hiding (html, param, map, head)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5            as H hiding (head)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Markdown               as M
import qualified Data.Text.Lazy.IO           as L
import qualified Data.Text                   as T

import           Common
import           TemplateSystem

{-
 -  blog entrys are mardown files, saved in the blogDir and named as:
 -      yyyy-mm-dd_Title_of_the_Blog-Entry.md
 -}

blogDir :: FilePath
blogDir = "blog"

data BlogEntry = B { blgTitle :: String
                   , blgPath :: String
                   , blgDate :: String
                   , blgContent :: Html
                   }

genBlog sc = do
    topdir <- getCurrentDirectory
    ex <- doesDirectoryExist path
    unless ex (createDirectoryIfMissing True path)
    blogs <- readBlogs (topdir </> blogDir)
    compilePages sc $ mapM makeBlogPage blogs sc
    compilePage sc $ blogOverview blogs sc
  where path = outPath sc </> blogDir

readBlogs :: FilePath -> IO [BlogEntry]
readBlogs curdir = let
    filterDots  = filter (\d -> not $ "." `isPrefixOf` d)
    filterFiles = filterM (\d -> doesFileExist (curdir </> d))
    readBlog :: FilePath -> IO BlogEntry
    readBlog p = let
      bn  = takeBaseName p
      itm = splitOn "_" bn
      in do
        f <- L.readFile $ blogDir </> p
        return B { blgTitle   = unwords $ tail itm
                 , blgPath    = bn
                 , blgDate    = intercalate "." $ 
                     reverse $
                       splitOn "-" $
                         head itm
                 , blgContent = M.markdown M.def f
                 }
    in do
      allfilespre <- getDirectoryContents curdir
      files <- filterFiles (filterDots allfilespre)
      mapM readBlog [ p | p <- files , "md" `isSuffixOf` p]

makeBlogPage :: BlogEntry -> SiteCfg -> Page
makeBlogPage blg sc = (defaultP sc) { pPath  = [ "blog/"
                                               ++ blgPath blg
                                               ++ ".html" ]
                                    , pTitle = Just $ blgTitle blg
                                    , pCtn   = ctn
                                    }
  where
    ctn = do
      H.div ! A.id "blgContent" $ do
        h1 $ toHtml $ blgTitle blg
        blgContent blg
      H.div ! A.class_ "spacer" $ " "
      a ! A.href (stringValue $ myTrimUrl sc $ url sc </> "blog/") $
        "⇦ Zurück zur Übersicht"

blogOverview bP sc = (defaultP sc) { pPath  = [ "blog.html"
                                              , "blog/index.html" ]
                                   , pTitle = Just "Blog"
                                   , pCtn   = ctn
                                   }
  where
    ctn         = do
      h1 "Blog"
      H.span $ toHtml $ unwords
        [ "Das hier ist noch kein wirklicher Blog, eher ein Proof of Concept."
        , "Vielleicht werde ich aber irgendwann Inhalt nachliefern."]
      H.div ! A.class_ "spacer" $ " "
      H.span $
        toHtml $
          blgDate $
            head bP
      H.div ! A.id "blgContent" $ do
        h1 $
          toHtml $
            blgTitle $
              head bP
        blgContent $
          head bP
      a ! A.id "permalink"
        ! A.href (stringValue $
          myTrimUrl sc $
            url sc </> "blog/" ++ blgPath (head bP) ++ ".html") $
        "permalink"
      list $ tail bP
    list bP     = do
      H.div ! A.class_ "spacer" $ " "
      h2 "Mehr:"
      ul $ mapM_ listItm bP
    listItm blg = li $ do
      a ! A.href (stringValue $
          myTrimUrl sc $
            url sc </> "blog/" ++ blgPath blg ++ ".html") $
        toHtml $
          blgTitle blg
      toHtml $ " (" ++ blgDate blg ++ ")"
