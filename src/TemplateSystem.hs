{-# LANGUAGE OverloadedStrings #-}
module TemplateSystem
  ( compilePage
  , compilePages
  , compilePages'
  -- , compileRaw , compileRaws
  ) where

import           Text.Blaze.Internal
import           Text.Blaze.Html5 hiding (html, param, map)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy.IO           as L
import           System.FilePath.Posix
import           Control.Monad
import           Data.Maybe
import           Data.List
import qualified Data.Text as T
import qualified Data.Foldable as F

import           Common
import           Debug.Trace (trace)

compilePage :: SiteCfg -> Page -> IO()
compilePage sc s =
  trace ("compiling " ++ Data.List.head (pPath s))
    mapM_ putToFile (pPath s)
  where putToFile f = L.writeFile (outPath sc </> f) $
          renderHtml $ do
            docType
            H.html $ do
              theHead sc s
              body! A.class_ (stringValue (show (pStyle s))) $ do
                -- Content:
                H.div ! A.id "super" $
                  if pStyle s == TextStyle
                    then H.div ! A.id "content" $ pCtn s
                    else                          pCtn s
                theHeader sc s
                script ! A.type_ "text/javascript"
                       ! A.src "https://code.jquery.com/jquery-latest.js" $ " "

                when (pStyle s == Maximize) keyMove

compilePage' :: SiteCfg -> (SiteCfg -> Page) -> IO()
compilePage' sc s = compilePage sc $ s sc

headLogo :: SiteCfg -> AttributeValue
headLogo sc = stringValue (url sc </> "images/logo-dark2.png")

theHead :: SiteCfg -> Page -> Html
theHead sc s = let
    sharingTags = do
      meta ! A.httpEquiv "og:title" ! A.content "Maximilian-Huber.de"
      meta ! A.httpEquiv "og:type" ! A.content "website"
      case pTitle s of
        Just t -> meta ! A.httpEquiv "og:description" ! A.content (stringValue t)
        _      -> pure ()
      -- meta ! A.httpEquiv "og:url" ! A.content "..."
      case (pSocial s) of
        Just (Social img imgType) -> do
          meta ! A.httpEquiv "og:image" ! A.content (stringValue img)
          meta ! A.httpEquiv "og:image:type" ! A.content (stringValue imgType)
          -- meta ! A.httpEquiv "og:image:width" ! A.content "..."
          -- meta ! A.httpEquiv "og:image:height" ! A.content "..."
        _ -> do
          meta ! A.httpEquiv "og:image" ! A.content (headLogo sc)
          meta ! A.httpEquiv "og:image:type" ! A.content "image/png"
    tryToMakeFullscreen = do
      meta ! A.httpEquiv "apple-mobile-web-app-capable" ! A.content "yes"
      meta ! A.httpEquiv "viewport" ! A.content "width=device-width, initial-scale=1.0"
      meta ! A.httpEquiv "mobile-web-app-capable" ! A.content "yes"
  in H.head $ do
    meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
    case pTitle s of
      Nothing    -> H.title "Maximilian-Huber.de"
      Just t -> H.title (toHtml $ "Maximilian-Huber.de | " ++ t)
    link ! A.rel "shortcut icon"
         ! A.type_ "image/x-icon"
         ! A.href (stringValue $ myTrimUrl sc $ url sc </> "favicon.ico")
    forM_ ["css/reset.css"
          ,"css/font.css"
          ,"css/default.css"]
      (\css -> link ! A.rel "stylesheet"
                    ! A.type_ "text/css"
                    ! A.href (stringValue $ url sc </> css))
    sharingTags
    tryToMakeFullscreen

theHeader :: SiteCfg -> Page -> Html
theHeader sc s = H.div ! A.id "header" $ do
  H.input ! A.type_ "checkbox"
          ! A.id "menuToggle"
  H.menu ! A.id "menu" $
    genNavigation sc s
  H.div ! A.id "lowerHeader" $ do
    H.label ! A.for "menuToggle"$ do
      H.div ! A.id "logoWrapper" $
        img ! A.src (headLogo sc)
            ! A.alt "maximilian-huber.de"
            ! A.id "logo"
      H.div  ! A.id "menuToggleDiv" $
        H.span $
        "Menu"
    F.forM_ (pLine s) (H.div ! A.id "reihe")

genNavigation :: SiteCfg -> Page -> Html
genNavigation sc s = ul ! A.class_ "MenuUl0"
                        ! A.id "navigation" $
    forM_ (subs $ pNav s) (`genNavigation'` 0)
  where genNavigation' nav lvl =
          li ! A.class_
                 (stringValue $
                   "MenuLi" ++ show lvl ++
                     if' ( isJust (navPath nav)
                           && isActive (fromJust (navPath nav)) (pPath s))
                         " active"
                         "") $ do
            case navPath nav of
              Nothing ->
                H.span ! A.id (stringValue $ "MenuSpan" ++ navTitle nav)$
                  toHtml $
                    navTitle nav
              Just path ->
                a ! A.class_ (stringValue $ "MenuLi" ++ show lvl)
                  ! A.id (stringValue $ "MenuA" ++ navTitle nav)
                  ! A.href (stringValue $
                            if' (not ("http" `isPrefixOf` path))
                            (myTrimUrl sc $ url sc </> path)
                            path) $
                      toHtml $
                        navTitle nav
            unless (Prelude.null (subs nav))
                   (ul ! A.class_ (stringValue $ "submenu" ++ show lvl)
                       ! A.id (stringValue $ "MenuUl" ++ navTitle nav) $
                         forM_ (subs nav) (`genNavigation'` (lvl + 1)))
        isActive p ps = p `elem` ps
                        || ( isActive' p > 0
                             && dropFileName p `elem` map dropFileName ps)
          where isActive' :: String -> Int
                isActive' []    = 0
                isActive' (h:t) | h == '/'  = isActive' t + 1
                                | otherwise = isActive' t

keyMove :: Html
keyMove = script ! A.type_ "text/javascript" $
  H.preEscapedToHtml $
    T.concat
      [ "$(window).keydown(function(event){"
      ,   "switch (event.keyCode) {"
      ,     "case 32:"
      ,     "case 39:"
      ,       "if($('#toright').length != 0)"
      ,         "window.location.href = $('#toright').attr('href');"
      ,       "break;"
      ,     "case 37:"
      ,       "if($('#toleft').length != 0)"
      ,         "window.location.href = $('#toleft').attr('href');"
      ,       "break;"
      ,     "case 80:"
      ,       "if($('#toright').length != 0){"
      ,         "autoPlay(3);"
      ,         "window.location.hash = '#play';"
      ,       "}"
      ,       "break;"
      ,     "case 83:"
      ,       "window.location.hash = '';"
      ,       "break;"
      ,   "}"
      , "});"
      ]

compilePages :: SiteCfg -> [Page] -> IO()
compilePages sc = mapM_ (compilePage sc)

compilePages' :: SiteCfg -> [SiteCfg -> Page] -> IO()
compilePages' sc = mapM_ (compilePage' sc)
