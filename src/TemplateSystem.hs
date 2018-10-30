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
            theHead sc s
            body! A.class_ (stringValue (pStyle s)) $ do
              -- Content:
              H.div ! A.id "super" $
                if pStyle s == "text"
                  then H.div ! A.id "content" $ pCtn s
                  else                          pCtn s
              theHeader sc s
              script ! A.type_ "text/javascript"
                     ! A.src "http://code.jquery.com/jquery-latest.js" $ " "

              stopRightClicks
              when (pStyle s == "maximize") keyMove

compilePage' :: SiteCfg -> (SiteCfg -> Page) -> IO()
compilePage' sc s = compilePage sc $ s sc

theHead :: SiteCfg -> Page -> Html
theHead sc s = H.head $ do
  meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
  case pTitle s of
    Nothing    -> H.title "Maximilian-Huber.de"
    Just t -> H.title (toHtml $ "Maximilian-Huber.de | " ++ t)
  link ! A.rel "shortcut icon"
       ! A.type_ "image/x-icon"
       ! A.href (stringValue $ myTrimUrl sc $ url sc </> "favicon.ico")
  forM_ ["css/reset.css","css/font.css","css/default.css"]
    (\css -> link ! A.rel "stylesheet"
                 ! A.type_ "text/css"
                 ! A.href (stringValue $ url sc </> css))

theHeader :: SiteCfg -> Page -> Html
theHeader sc s = H.div ! A.id "header" $ do
  H.div ! A.id "logoWrapper" $
    -- a ! A.href (stringValue $ url sc) $
      img ! A.src (stringValue ( url sc </> "images/logo-dark2.png"))
          ! A.alt "maximilian-huber.de"
          ! A.id "logo"
  H.div ! A.id "spalte" $ do
    H.div ! A.id "spalteFill1" $ ""
    -- Navigation:
    genNavigation sc s
    H.div ! A.id "spalteFill2" $ ""
  F.forM_ (pLine s) (H.div ! A.id "reihe")

genNavigation :: SiteCfg -> Page -> Html
genNavigation sc s = ul ! A.class_ "MenuUl0"
                        ! A.id "navigation" $
    forM_ (subs $ pNav s) (`genNavigation''` 0)
  where genNavigation'' nav lvl =
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
                   (H.div ! A.class_ (stringValue $ "infinitem" ++ show lvl) $
                      ul ! A.class_ (stringValue $ "submenu" ++ show lvl)
                         ! A.id (stringValue $ "MenuUl" ++ navTitle nav) $
                           forM_ (subs nav) (`genNavigation''` (lvl + 1)))
        isActive p ps = p `elem` ps
                      || ( isActive' p > 0
                        && dropFileName p `elem` map dropFileName ps)
          where isActive' :: String -> Int
                isActive' []    = 0
                isActive' (h:t) | h == '/'   = isActive' t + 1
                                | otherwise = isActive' t

stopRightClicks :: Html
stopRightClicks = script ! A.type_ "text/javascript" $
  H.preEscapedToHtml $
    T.concat [ "$(document).ready(function(){"
             ,     "$(document).bind(\"contextmenu\",function(e){"
             ,         "return false;"
             ,     "});"
             , "});" ]

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
      , "function placement(){"
      ,   "var img = $('#super > img');"
      ,   "$(img).css({ maxWidth: $(window).width() });"
      ,   "$(img).css({ maxHeight: ($(window).height() - 30) });"
      , "}"
      , "$(document).ready(function(){ placement(); });"
      , "$(window).resize(function() { placement(); });"
      ]

compilePages :: SiteCfg -> [Page] -> IO()
compilePages sc = mapM_ (compilePage sc)

compilePages' :: SiteCfg -> [SiteCfg -> Page] -> IO()
compilePages' sc = mapM_ (compilePage' sc)
