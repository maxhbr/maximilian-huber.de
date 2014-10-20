{-# LANGUAGE OverloadedStrings #-}
module Pages
  where

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

import           Common

myBr :: Int -> Html
myBr i = forM_ [1..i] (const H.br)

-------------------------------------------------------------------------------
webdesign sc = (defaultP sc) { pPath  = ["webdesign.html"]
                             , pTitle = Just "Webdesign"
                             , pCtn   = ctn }
  where ctn :: Html
        ctn = do
          h1 "Webdesign"
          H.br
          "Diese Seite ist in purem " 
          a ! A.href "http://www.haskell.org/haskellwiki/Haskell" $
            "Haskell"
          " geschrieben. Hierbei habe ich die folgenden Tools genutzt"
          ul $ do
            li $ do
              a ! A.href "http://jaspervdj.be/blaze/" $ "blazeHtml"
              " zum erzeugen des HTML codes und "
            li $ do
              a ! A.href "http://fvisser.nl/clay/" $ "clay"
              " um die CSS-Dateien zu erzeugen."
          "um eine statische HTML Seite zu erzeugen. Der vollständige Code ist auf "
          a ! A.href "https://github.com/maximilianhuber/maximilian-huber.de" $
            "Github"
          " zu finden."

-------------------------------------------------------------------------------
gpgPubkey sc = unsafePerformIO $ do
    fex <- doesFileExist "gpg-pubkey.asc"
    return (if fex 
      then (defaultP sc) { pPath  = ["gpg-pubkey.html"]
                         , pTitle = Just "GPG public key"
                         , pCtn   = ctn }
      else defaultP sc)
  where ctn :: Html
        ctn = do
          h1 "GPG public key"
          myBr 3
          a ! A.href "gpg-pubkey.asc" $ 
            pre $
              toHtml $
                unsafePerformIO subCtn
        subCtn = readFile "gpg-pubkey.asc"

-------------------------------------------------------------------------------
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
          myBr 4
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
