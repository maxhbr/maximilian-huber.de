{-# LANGUAGE OverloadedStrings #-}
module Pages
  ( webdesign
  , gpgPubkey
  , kontakt
  , impress
  , module X
  ) where

import           Text.Blaze.Internal
import           Text.Blaze.Html5 hiding (html, param, map)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           System.Directory
import           Control.Monad
import           System.IO.Unsafe
import qualified Data.Text as T

import           Common
import           Gallery as X

-------------------------------------------------------------------------------
kontakt :: SiteCfg -> Page
kontakt sc = (defaultP sc) { pPath  = ["kontakt.html"]
                           , pTitle = Just "Kontakt"
                           , pCtn   = ctn }
  where ctn :: Html
        ctn = do
          h1 "Kontakt"
          ul $ do
            li $ do -- Mail
              _ <- "Email: "
              a ! A.href "javascript:twoClkEml('kontakt','emlToRepl')"
                ! A.id "emlToRepl" $
                "kontakt <at> maximilian <minus> huber <punkt> de"
              script ! A.type_ "text/javascript" $
                H.preEscapedToHtml $
                  T.concat
                  [ "function twoClkEml (n,c){"
                  , "var m = n + String.fromCharCode(64) + window.location.host;"
                  , "document.getElementById(c).innerHTML = m;"
                  , "document.getElementById(c).href = decodeURIComponent(atob(\"bWFpbHRvJTNB\")) + m;"
                  , "}" ]
            li $ do -- GitHub
              _ <- "GitHub: "
              a ! A.href "https://github.com/maxhbr/"
                ! A.target "_blank" $
                "github.com/maxhbr"
            li $ do -- Facebook
              _ <- "Facebook: "
              a ! A.href "https://www.facebook.com/pages/Fotografie-Maximilian-Huber/122707361149256"
                ! A.target "_blank" $
                "Fotografie Maximilian Huber"

-------------------------------------------------------------------------------
webdesign :: SiteCfg -> Page
webdesign sc = (defaultP sc) { pPath  = ["webdesign.html"]
                             , pTitle = Just "Webdesign"
                             , pCtn   = ctn }
  where ctn :: Html
        ctn = do
          h1 "Webdesign"
          _ <- "Diese Seite ist in purem "
          a ! A.href "http://www.haskell.org/haskellwiki/Haskell"
            ! A.target "_blank" $
            "Haskell"
          _ <- " geschrieben. Um diese statische HTML Seite zu erzeugen habe ich die folgenden Tools genutzt"
          ul $ do
            li $ do
              a ! A.href "http://jaspervdj.be/blaze/"! A.target "_blank" $
                "blazeHtml"
              " zum erzeugen des HTML codes und "
            li $ do
              a ! A.href "http://fvisser.nl/clay/"! A.target "_blank" $
                "clay"
              " um die CSS-Dateien zu erzeugen."
          _ <- "Der komplette Code ist auf "
          a ! A.href "https://github.com/maxhbr/maximilian-huber.de" $
            "GitHub"
          _ <- " zu finden."
          a ! A.href "https://github.com/maxhbr/maximilian-huber.de"
            ! A.target "_blank" $
            img ! A.style "position: absolute; top: 0; right: 0; border: 0;"
                ! A.src "https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67"
                ! A.alt "Fork me on GitHub"

-------------------------------------------------------------------------------
gpgPubkey :: SiteCfg -> Page
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
impress :: SiteCfg -> Page
impress sc = (defaultP sc) { pPath  = ["impress.html"]
                           , pTitle = Just "Impress"
                           , pCtn   = ctn }
  where ctn :: Html
        ctn = do
          h1 "Impress"
          H.span "Maximilian Huber"
          H.br
          H.span "Goegginger Str. 32a"
          H.br
          H.span "8619 Augsburg"
          H.br
          H.br
          H.span "Tel: 00491743410223"
          H.br
          H.span $ do
            _ <- "Email: "
            a ! A.href "javascript:twoClkEml('kontakt','emlToRepl')"
              ! A.id "emlToRepl" $
              "kontakt <at> maximilian <minus> huber <punkt> de"
            script ! A.type_ "text/javascript" $
              H.preEscapedToHtml $
                T.concat
                [ "function twoClkEml (n,c){"
                , "var m = n + String.fromCharCode(64) + window.location.host;"
                , "document.getElementById(c).innerHTML = m;"
                , "document.getElementById(c).href = decodeURIComponent(atob(\"bWFpbHRvJTNB\")) + m;"
                , "}" ]
          H.div ! A.class_ "spacer" $ " "
          H.div ! A.class_ "center" $ do
            a ! A.rel "license"
              ! A.href "http://creativecommons.org/licenses/by-nc-nd/4.0/" $
              img ! A.alt "Creative Commons Lizenzvertrag"
                  ! A.style "border-width:0"
                  ! A.src "https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png"
            H.br
            _ <- "Alle Bilder sind lizenziert unter einer "
            a ! A.rel "license"
              ! A.href "http://creativecommons.org/licenses/by-nc-nd/4.0/" $
                "Creative Commons Namensnennung - Nicht kommerziell - Keine Bearbeitungen 4.0 International Lizenz"
