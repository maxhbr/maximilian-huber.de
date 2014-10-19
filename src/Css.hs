{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Css (css) where

import           Prelude hiding (div,(**))
import qualified Prelude as P
import           Data.Monoid
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L
import           Data.Text
import           Clay hiding (url)
import qualified Clay as C
import qualified Clay.Media as M
import qualified Clay.Time as T
import           Control.Monad
import           System.FilePath
import           System.FilePath.Posix
import           System.Directory

import           Common

cWidth = 960

bkColor  = "#333"    :: Color
bkColor0 = bkColor
bkColor1 = "#444"    :: Color
bkColor2 = "#555"    :: Color
fgColor  = "#bbb"    :: Color
hiColor  = "#f8d687" :: Color
hoColor  = "#fff"    :: Color

toInvisible = do
  visibility hidden
  opacity 0
  transitions [("visibility",T.sec 0,  linear,T.sec 0.2)
              ,("opacity",   T.sec 0.2,linear,T.sec 0.0)]
toVisible = do
  visibility visible
  opacity 1
  transitionDelay (T.sec 0)

general :: Css
general = do
  body ? do
    background bkColor2
    color fgColor
  h1 ? fontSize (px 24)
  h2 ? fontSize (px 20)
  a ? do
    color fgColor
    fontWeight bold
    textDecoration none
  a # hover ? color hoColor
  div # "#footer" ? do
    width (other "100%")
    "text-align" -: "right"
    position relative
    top (px 15)
  div # ".infinitem0" ? do
    display block
    width (px 0)
    height (px 0)

layout :: Css
layout = do
  -- ########################################################################
  div # "#header" ? do
    position fixed
    left 0
    top 0
    right 0
    height (px 30)
    background bkColor
  -- ########################################################################
  div # "#reihe" ? do
    {-display none-}
    left (px 200)
    top 0
    height (px 30)
    paddingRight (px 30)
  -- ########################################################################
  div # "#logoWrapper" ? do
    background bkColor
    position absolute
    zIndex 10000
    left 0
    top 0
    width (px 180)
    padding (px 10) (px 10) (px 10) (px 10)
    height auto
    -- "border-bottom-right-radius" -: "15px"
    -- "border-bottom-left-radius" -: "15px"
    query M.screen [M.minWidth (px (cWidth +9))] (do
      left (other "50%")
      marginLeft (px (- cWidth `P.div` 2)))
    img # "#logo" ? do
      width (other "100%")
      height auto
  -- ########################################################################
  ((div # "#reihe") <> (div # "#spalte")) ? do
    {-background bkColor-}
    {-borderRight solid (px 2) hiColor-}
    {-borderBottom solid (px 2) hiColor-}
    position absolute
    zIndex 1000
  -- ########################################################################
  div # "#spalte" ? do
    display block
    left 0
    top 0
    bottom 0
    width (px 200)
    padding 0 0 0 0
    query M.screen [M.minWidth (px (cWidth +9))] (do
      left (other "50%")
      marginLeft (px (- cWidth `P.div` 2)))
    query M.screen [M.minHeight (px 450)] (position fixed)
    (ul # "#navigation" |> li) <> (div # "#spalteFill2") ? marginTop (px 10)
    (ul # "#navigation" |> li)
      <> (div # "#spalteFill1")
      <> (div # "#spalteFill2") ?  width (px 200)
    (ul # "#navigation" |> li)
      <> (div # "#spalteFill1")
      <> (div # "#spalteFill2")
      <> (ul # ".MenuUlGalerie" |> li)
      <> (ul # ".submenu0" |> li) ? do
        display block
        background bkColor
    ul # "#navigation" ? li ? do
      position relative
      "vertical-align" -: "middle"
      lineHeight (px 30)
    ul # "#navigation" |> li ? do
      height (px 30)
      a <> Clay.span ? do
        display block
        width (px 170)
        height (other "100%")
        padding 0 0 0 (px 30)
      ul # ".submenu0" ? do
        toInvisible
        position absolute
        left (px 205)
        top (px (-10))
        paddingTop (px 10)
        paddingBottom (px 10)
        background bkColor
      ul # ".submenu0" ? width (px 150)
      ul # "#MenuUlGalerie" ? do
        width (px 450)
        query M.screen [M.maxWidth (px 659)] (do
          width (px 300)
          query M.screen [M.maxWidth (px 509)] (width (px 150)))
        li ? do
          width (px 150)
          li ? a ?  fontWeight normal
        a ? width (px 120)
      ul # "#MenuUlGalerie" |> li ? float floatLeft
    ul # "#navigation" |> li # hover ? do
      {-borderLeft solid (px 2) hiColor-}
      {-marginLeft (px (-2))-}
      ul ? toVisible
      width (px 206)
    div # "#spalteFill1" ? do
      height (px 200)
      query M.screen [M.minHeight (px 600)] (height (px 300))
    div # "#spalteFill2" ? height (other "100%")
    ul # "#navigation" ? li # ".active" ? a ? color hiColor
    ul # "#navigation" ? li # ".active" ? a # hover? color white
  -- ########################################################################
  div # "#reihe" ? do
    top 0
    left (px 200)
    paddingLeft (px 30)
    lineHeight (px 30)
    display block
    background bkColor
    query M.screen [M.minWidth (px (cWidth +9))] (do
      left (other "50%")
      marginLeft (px (- cWidth `P.div` 2 + 200)))
    query M.screen [M.minHeight (px 450)] (position fixed)
    display none
    li ? do 
      float floatLeft
      marginRight (px 5)
    -- "border-bottom-right-radius" -: "15px"

textCss :: Css
textCss = div # "#super" ? do
  position absolute
  top 0
  left 0
  width (other "100%")
  query M.screen [M.minWidth (px (cWidth +9))] (do
    width (px cWidth)
    left (other "50%")
    marginLeft (px (- cWidth `P.div` 2)))
  div # "#content" ? do
    background bkColor1
    position absolute
    top (px 10)
    left (px 210)
    padding (px 30) (px 30) (px 30) (px 30)
    minHeight (px 200)
    query M.screen [M.maxWidth (px 969)] (right (px 0))
    query M.screen [M.minWidth (px 969)] (width (px 596))
    li ? listStyleType disc
    div # ".center" ? ("text-align" -: "center")

maximize :: SiteCfg -> Css
maximize sc = do
  div # "#reihe" ? display block
  div # "#spalte" ? toInvisible
  div # "#header" # hover ? div # "#spalte" ? toVisible
  (div # "#super") <> (div # "#imageOverlay") ? do
    position absolute
    top 0
    right 0
    bottom 0
    left 0
  div # "#imageOverlay" ? position fixed
  div # "#super" |> img ? do
    maxHeight (other "100%")
    maxWidth (other "100%")
    marginLeft auto
    marginRight auto
    display (other "table")
  a # "#toleft" <> a # "#toright" ? do
    position absolute
    display block
    top 0
    bottom 0
    div # ".inner" ? do
      position absolute
      right 0
      top 0
      bottom 0
      width (px 100)
      transitions [("opacity",T.sec 0.25,easeOut,T.sec 0.0)]
      opacity 0.3
    div # ".inner" # hover ?
      opacity 1
  a # "#toleft" ? do
    left 0
    width (px 100)
    div # ".inner" ? do
      backgroundImage (C.url (pack $ url sc </> "images/2arrow-l.png"))
      backgroundRepeat noRepeat
      backgroundPosition (placed sideLeft sideCenter)
    div # ".inner" # hover ?
      backgroundImage (C.url (pack $ url sc </> "images/2arrow-l-active.png"))
  a # "#toright" ? do
    left (px 100)
    right 0
    div # ".inner" ? do
      backgroundImage (C.url (pack $ url sc </> "images/2arrow-r.png"))
      backgroundRepeat noRepeat
      backgroundPosition (placed sideRight sideCenter)
    div # ".inner" # hover ?
      backgroundImage (C.url (pack $ url sc </> "images/2arrow-r-active.png"))

css :: SiteCfg -> IO ()
css sc = do
  ex <- doesDirectoryExist (takeDirectory (cssFile sc))
  unless ex (createDirectoryIfMissing True (takeDirectory (cssFile sc)))
#if 0
  L.writeFile (outPath sc </> cssFile sc $ renderWith compact [] $ do
#else
  L.writeFile (outPath sc </> cssFile sc) $ render $ do
#endif
    general
    layout
    body # ".text" ? textCss
    body # ".maximize" ? maximize sc
  print "css done"
