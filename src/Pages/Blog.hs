{-# LANGUAGE OverloadedStrings #-}
module Pages.Blog
  ( blogPages
  ) where

import           Text.Blaze.Internal
import           Text.Blaze.Html5 hiding (html, param, map)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy.IO           as L
import           System.FilePath.Posix
import           System.Posix.Files
import           System.Directory
import           Control.Monad
import           Data.Maybe
import           System.IO.Unsafe
import qualified Data.Text as T

import           Common

blogs = getCurrentDirectory >>= (`readBlogs` "blog")
  where
    readBlogs :: FilePath -> FilePath -> IO FoldrAndImgs
    readBlogs topdir curdir = let
      filterDots  = filter (\d -> not $ "." `isPrefixOf` d)
      filterFiles = filterM (\d -> doesFileExist (topdir </> curdir </> d))
      in do
        allfilespre <- getDirectoryContents $ topdir </> curdir
        blogs <- filterFiles (filterDots allfilespre)
        return $ map (curdir </>) [i | i <- blogs , "html" `isSuffixOf` i]

blogPages :: [SiteCfg]
blogPages = []
-- kontakt sc = (defaultP sc) { pPath  = ["kontakt.html"]
--                            , pTitle = Just "Kontakt"
--                            , pCtn   = ctn }
