{-# LANGUAGE OverloadedStrings #-}
module TemplateSystem
  ( compilePage
  , compilePages
  ) where

import           Text.Blaze.Internal
import           Text.Blaze.Html5 hiding (html, param, map)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy.IO           as L
import           System.FilePath.Posix
import           System.Posix.Files
import           Control.Monad

import           Common

compilePage :: SiteCfg -> Page -> IO()
compilePage sc s = putToFile (sPath s)
  where fullContent = renderHtml $ do
          theHead (sTitle s)
          body! A.class_ (stringValue (sStyle s)) $ do
            -- Content:
            sCtn s
            theHeader (sNav s)
        putToFile []     = print "done"
        putToFile (f:fs) = L.writeFile (outPath sc </> f) fullContent

compilePages :: SiteCfg -> [Page] -> IO()
compilePages sc = mapM_ (compilePage sc)

{- ============================================================================
 - Helper functions
 -}

theHead title = H.head $ do
  meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
  H.title "Maximilian-Huber.de"
  link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/css/reset.css"
  link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/css/default.css"

theHeader nav = H.div ! A.id "header" $ do
  H.div ! A.id "logoWrapper" $
    a ! A.href "/" $
      img ! A.src (stringValue "/images/logo-dark2.png")
          ! A.alt "maximilian-huber.de"
          ! A.id "logo"
  H.div ! A.id "spalte" $ do
    H.div ! A.id "spalteFill1" $ "" -- mempty
    -- Navigation:
    genNavigation nav
    H.div ! A.id "spalteFill2" $ "" -- mempty

genNavigation nav = ul ! A.id "navigation" $ do
  li $
    H.div ! A.class_ "infinitem" $ ul ! A.class_ "submenu" ! A.id "galmenu" $ do
      li ! A.class_ "MenuLi1" ! A.id "MenuLiNatur" $ do
        a ! A.class_ "MenuA1" ! A.href "/galerie/natur" ! A.id "MenuANatur" $ "Natur"
        ul ! A.class_ "MenuUl2" $ do
          li ! A.class_ "MenuLi2" ! A.id "MenuLiDetails" $ a ! A.class_ "MenuA2" ! A.href "/galerie/natur/details" ! A.id "MenuADetails" $ "Details"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiLandschaften" $ a ! A.class_ "MenuA2" ! A.href "/galerie/natur/landschaften" ! A.id "MenuALandschaften" $ "Landschaften"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiTiere" $ a ! A.class_ "MenuA2" ! A.href "/galerie/natur/tiere" ! A.id "MenuATiere" $ "Tiere"
      li ! A.class_ "MenuLi1" ! A.id "MenuLiPeople" $ do
        a ! A.class_ "MenuA1" ! A.href "/galerie/people" ! A.id "MenuAPeople" $ "People"
        ul ! A.class_ "MenuUl2" $ do
          li ! A.class_ "MenuLi2" ! A.id "MenuLiBeauty" $ a ! A.class_ "MenuA2" ! A.href "/galerie/people/beauty" ! A.id "MenuABeauty" $ "Beauty"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiFashion" $ a ! A.class_ "MenuA2" ! A.href "/galerie/people/fashion" ! A.id "MenuAFashion" $ "Fashion"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiLifestyle" $ a ! A.class_ "MenuA2" ! A.href "/galerie/people/lifestyle" ! A.id "MenuALifestyle" $ "Lifestyle"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiMake-up" $ a ! A.class_ "MenuA2" ! A.href "/galerie/people/make-up" ! A.id "MenuAMake-up" $ "Make-up"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiPortrait" $ a ! A.class_ "MenuA2" ! A.href "/galerie/people/portrait" ! A.id "MenuAPortrait" $ "Portrait"
      li ! A.class_ "MenuLi1" ! A.id "MenuLiWeiteres" $ do
        a ! A.class_ "MenuA1" ! A.href "/galerie/weiteres" ! A.id "MenuAWeiteres" $ "Weiteres"
        ul ! A.class_ "MenuUl2" $ do
          li ! A.class_ "MenuLi2" ! A.id "MenuLiArchitektur" $ a ! A.class_ "MenuA2" ! A.href "/galerie/weiteres/architektur" ! A.id "MenuAArchitektur" $ "Architektur"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiProjekte" $ a ! A.class_ "MenuA2" ! A.href "/galerie/weiteres/projekte" ! A.id "MenuAProjekte" $ "Projekte"
          li ! A.class_ "MenuLi2" ! A.id "MenuLiTechnik" $ a ! A.class_ "MenuA2" ! A.href "/galerie/weiteres/technik" ! A.id "MenuATechnik" $ "Technik"
      a ! A.href "/" $ "Galerie"
  li ! A.class_ "active" $ a ! A.href "/webdesign.html" $ "Webdesign"
  li $ do
    H.div ! A.class_ "infinitem" $ ul ! A.class_ "submenu" $ do
      li $ a ! A.href "/gpg-pubkey.html" $ "GPG Key"
      li $ a ! A.href "/impress.html" $ "Impress"
    a ! A.href "/kontakt.html" $ "Kontakt"
