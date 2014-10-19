{-# LANGUAGE OverloadedStrings #-}
module Pages
  (webdesign, impress) where

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

import           Common

webdesign sc = (defaultP sc) { pPath  = ["webdesign.html"]
                             , pTitle = Just "Webdesign"
                             , pCtn   = ctn }
  where ctn = h1 $ "Webdesign"
impress sc = (defaultP sc) { pPath  = ["impress.html"]
                           , pTitle = Just "Impress"
                           , pCtn   = ctn }
  where ctn = h1 $ "Impress"
