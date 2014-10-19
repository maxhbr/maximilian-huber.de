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
  where ctn :: Html
        ctn = do
          h1 "Webdesign"
          a ! A.href "https://github.com/maximilianhuber/maximilian-huber.de" $
            "Github"

impress sc = (defaultP sc) { pPath  = ["impress.html"]
                           , pTitle = Just "Impress"
                           , pCtn   = ctn }
  where ctn :: Html
        ctn = do 
          h1 "Impress"
          H.span "Maximilian Huber"
          H.br
          H.span "Reichenbacher Str. 20"
          H.br
          H.span "87677 Stöttwang"
          H.br
          H.br
          H.span "Tel: 083459813"
          H.br
          H.span "Email: hubi135 (at) live (punkt) de"
          H.br
          H.br
          H.br
          H.br
          H.div ! A.class_ "center" $ do
            a ! A.rel "license"
              ! A.href "http://creativecommons.org/licenses/by-nc-nd/4.0/" $
              img ! A.alt "Creative Commons Lizenzvertrag"
                  ! A.style "border-width:0"
                  ! A.src "https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png"
            H.br
            "Alle Bilder sind lizenziert unter einer "
            a ! A.rel "license"
              ! A.href "http://creativecommons.org/licenses/by-nc-nd/4.0/" $
                "Creative Commons Namensnennung - Nicht kommerziell - Keine Bearbeitungen 4.0 International Lizenz"




          -- <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">
          --    <img alt="Creative Commons Lizenzvertrag" 
          --         style="border-width:0"
          --         src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" />
          -- </a>
          -- <br />
          -- Dieses Werk ist lizenziert unter einer 
          -- </a>.


