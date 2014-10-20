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
import qualified Data.Text as T

import           Common

myBr :: Int -> Html
myBr i = forM_ [1..i] (const H.br)

-------------------------------------------------------------------------------
kontakt sc = (defaultP sc) { pPath  = ["kontakt.html"]
                           , pTitle = Just "Kontakt"
                           , pCtn   = ctn }
  where ctn :: Html
        ctn = do
          h1 "Kontakt"
          ul $ do
            li $ do -- Mail
              "Email: "
              a ! A.href "javascript:e()"
                ! A.id "emlToRepl" $ 
                "kontakt ⟨at⟩ maximilian ⟨minus⟩ huber ⟨punkt⟩ de"
              script ! A.type_ "text/javascript" $
                H.preEscapedToHtml $
                  T.concat
                    [ "function e (){"
                    ,   "var h = window.location.host;"
                    ,   "var a = String.fromCharCode(64);"
                    ,   "var m = \"kontakt\" + a + h;"
                    ,   "document.getElementById(\"emlToRepl\").innerHTML = m;"
                    ,   "document.getElementById(\"emlToRepl\").href = \"mailto:\" + m;"
                    , "}" ]
            li $ do -- GitHub
              "GitHub: "
              a ! A.href "https://github.com/maximilianhuber/" $
                "github.com/maximilianhuber"
            li $ do -- Facebook
              "Facebook: "
              a ! A.href "https://www.facebook.com/pages/Fotografie-Maximilian-Huber/122707361149256" $
                "Fotografie Maximilian Huber"

-------------------------------------------------------------------------------
webdesign sc = (defaultP sc) { pPath  = ["webdesign.html"]
                             , pTitle = Just "Webdesign"
                             , pCtn   = ctn }
  where ctn :: Html
        ctn = do
          h1 "Webdesign"
          "Diese Seite ist in purem " 
          a ! A.href "http://www.haskell.org/haskellwiki/Haskell" $
            "Haskell"
          " geschrieben. Um diese statische HTML Seite zu erzeugen habe ich die folgenden Tools genutzt"
          ul $ do
            li $ do
              a ! A.href "http://jaspervdj.be/blaze/" $ "blazeHtml"
              " zum erzeugen des HTML codes und "
            li $ do
              a ! A.href "http://fvisser.nl/clay/" $ "clay"
              " um die CSS-Dateien zu erzeugen."
          "Der vollständige Code ist auf "
          a ! A.href "https://github.com/maximilianhuber/maximilian-huber.de" $
            "GitHub"
          " zu finden."
          a ! A.href "https://github.com/maximilianhuber/maximilian-huber.de" $
            img ! A.style "position: absolute; top: 0; right: 0; border: 0;"
                ! A.src "https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67"
                ! A.alt "Fork me on GitHub"

          myBr 3
          h2 "Mehr"
          ul $ forM_ [ ("http://kreativkarten-huber.de/","kreativkarten-huber.de")
                     , ("http://masananda-ra.de/","masananda-ra.de")
                     , ("http://topstyle.preller.org/","topstyle.preller.org")
                     , ("http://clc-leder.de/","clc-leder.de")
                     , ("http://photodesign-huber.de/","photodesign-huber.de") ]
                     (\(u,t) -> li $ a ! A.href u ! A.target "_blank" $ t)

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
          H.span $ do
            "Email: "
            a ! A.href "javascript:e()"
              ! A.id "emlToRepl" $ 
              "hubi135 ⟨at⟩ live ⟨punkt⟩ de"
            script ! A.type_ "text/javascript" $
              H.preEscapedToHtml $
                T.concat
                  [ "function e (){"
                  ,   "var a = String.fromCharCode(64);"
                  ,   "var m = \"hubi135\" + a + \"live.de\";"
                  ,   "document.getElementById(\"emlToRepl\").innerHTML = m;"
                  ,   "document.getElementById(\"emlToRepl\").href = \"mailto:\" + m;"
                  , "}" ]
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
