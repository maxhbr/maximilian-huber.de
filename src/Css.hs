{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Css (genCss) where

import           Prelude hiding (div,(**))
import qualified Prelude as P
import           Data.Monoid
import qualified Data.Text.Lazy.IO as L
import           Data.Text
import           Clay hiding (url)
import           Clay.Box
import qualified Clay as C
import qualified Clay.Media as M
import qualified Clay.Time as T
import           Control.Monad
import           System.FilePath
import           System.FilePath.Posix
import           System.Directory

import           Common

cWidth = 960
cHeight = 800

wide = query M.screen [M.minWidth (px (cWidth +9))]
tall = query M.screen [M.minHeight (px cHeight)]
notTall = query M.screen [M.maxHeight (px cHeight)]

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
  transitions [ ("visibility",T.sec 0,  linear,T.sec 0.2)
              , ("opacity",   T.sec 0.2,linear,T.sec 0.0) ]
toVisible = do
  visibility visible
  opacity 1
  transitionDelay (T.sec 0)

general :: Css
general = do
  body ? do
    background bkColor2
    color fgColor
    fontSize (px 16)
  h1 ? fontSize (px 24)
  h2 ? fontSize (px 20)
  a ? do
    color fgColor
    fontWeight bold
    textDecoration none
  a # hover ? color hoColor
  div # "#footer" ? do
    width (pct 100)
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
    left (px 0)
    top (px 0)
    right (px 0)
    height (px 30)
    background bkColor
    zIndex 10000
  -- ########################################################################
  div # "#reihe" ? do
    {-display none-}
    left (px 200)
    top (px 0)
    height (px 30)
    paddingRight (px 30)
  -- ########################################################################
  div # "#logoWrapper" ? do
    background bkColor
    position absolute
    zIndex 10000
    left (px 0)
    top (px 0)
    width (px 180)
    padding (px 10) (px 10) (px 10) (px 10)
    height auto
    wide $ do
      left (pct 50)
      marginLeft (px (- cWidth) @/ 2)
    img # "#logo" ? do
      width (pct 100)
      height auto
  -- ########################################################################
  ((div # "#reihe") <> (div # "#spalte")) ? do
    position absolute
    zIndex 1000
  -- ########################################################################
  div # "#spalte" ? do
    display block
    left (px 0)
    top (px 0)
    bottom (px 0)
    width (px 200)
    padding (px 0) (px 0) (px 0) (px 0)
    wide $ do
      left (pct 50)
      marginLeft (px (- cWidth) @/ 2)
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
        height (pct 100)
        padding (px 0) (px 0) (px 0) (px 30)
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
      ul ? toVisible
      width (px 206)
    div # "#spalteFill1" ? do
      height (px 200)
      query M.screen [M.minHeight (px 600)] (height (px 300))
    div # "#spalteFill2" ? height (pct 100)
    ul # "#navigation" ? li # ".active" ? a ? color hiColor
    ul # "#navigation" ? li # ".active" ? a # hover? color white
  -- ########################################################################
  div # "#reihe" ? do
    top (px 0)
    left (px 200)
    paddingLeft (px 30)
    lineHeight (px 30)
    display block
    background bkColor
    wide $ do
      left (pct 50)
      marginLeft (px (- cWidth) @/ 2 @+@ px 200)
    query M.screen [M.minHeight (px 450)] (position fixed)
    display none
    li ? do
      float floatLeft
      marginRight (px 5)

textCss :: Css
textCss = div # "#super" ? do
  wide $ do
    width (px cWidth)
    left (pct 50)
    marginLeft (px (- cWidth) @/ 2)
  div # "#content" ? do
    background bkColor1
    position absolute
    top (px 10)
    left (px 210)
    padding (px 30) (px 30) (px 30) (px 30)
    marginBottom (px 10)
    minHeight (px 200)
    query M.screen [M.maxWidth (px 969)] (right (px 0))
    query M.screen [M.minWidth (px 969)] (width (px 596))
    li ? listStyleType disc
    div # ".center" ?
      ("text-align" -: "center")
    pre ? fontSize (em 0.9)
    h1 ? paddingBottom (px 50)
    div # ".spacer" ? do
      display block
      width (pct 100)
      height (px 10)
      padding (px 0) (px 30) (px 0) (px 30)
      margin (px 20) 0 (px 20) (px (-30))
      background bkColor2

maximize :: SiteCfg -> Css
maximize sc = let
    fitMaximizedWith v = do
      position absolute
      top v
      right v
      left v
      bottom v
    fitMaximized = fitMaximizedWith (px 0)
    blurRadius = 50
  in do
    div # "#super" ? overflow hidden
    div # "#reihe" ? display block
    div # "#spalte" ? toInvisible
    div # "#header" # hover ? div # "#spalte" ? toVisible
    (div # "#super") <> (div # "#imageOverlay") ?  bottom (px 0)
    (div # "#imageOverlay") ? do
      fitMaximized
      zIndex 1010
    div # "#imageOverlay" ? position fixed
    div # "#super" |> (div # "#imgBackgroundWrapper") ? do
      fitMaximized
      zIndex 100
      overflow hidden
    div # "#super" |> (div # "#imgBackgroundWrapper") |> (div # "#imgBackground") ? do
      fitMaximized
      margin (px (-blurRadius * 2)) (px (-blurRadius * 2)) (px (-blurRadius * 2)) (px (-blurRadius * 2))
      zIndex 100
      backgroundPosition (placed sideCenter sideCenter)
      backgroundSize cover
      "filter" -: pack ("blur(" ++ show blurRadius ++ "px)")
    div # "#super" |> img ? do
      tall $  let
          imgSpacing = 20
        in do
          maxHeight (pct 100 @-@ px (imgSpacing * 2))
          maxWidth (pct 100 @-@ px (imgSpacing * 2))
          fitMaximizedWith (px imgSpacing)
      notTall $ do
        maxHeight (pct 100)
        maxWidth (pct 100)
        fitMaximized
      margin auto auto auto auto
      zIndex 1000
      boxShadow . pure $
        bkColor `bsColor` shadowWithSpread (px 0) (px 0) (px (5 * blurRadius)) (px (- blurRadius))
    a # "#toleft" <> a # "#toright" ? do
      position absolute
      zIndex 5000
      display block
      top (px 0)
      bottom (px 0)
      div # ".inner" ? do
        position absolute
        right (px 0)
        top (px 0)
        bottom (px 0)
        width (px 100)
        transitions [("opacity",T.sec 0.25,easeOut,T.sec 0.0)]
        opacity 0.3
      div # ".inner" # hover ?
        opacity 1
    a # "#toleft" ? do
      left (px 0)
      width (px 100)
      div # ".inner" ? do
        backgroundImage (C.url (pack $ url sc </> "images/2arrow-l.png"))
        backgroundRepeat noRepeat
        backgroundPosition (placed sideLeft sideCenter)
      div # ".inner" # hover ?
        backgroundImage (C.url (pack $ url sc </> "images/2arrow-l-active.png"))
    a # "#toright" ? do
      left (px 100)
      right (px 0)
      div # ".inner" ? do
        backgroundImage (C.url (pack $ url sc </> "images/2arrow-r.png"))
        backgroundRepeat noRepeat
        backgroundPosition (placed sideRight sideCenter)
      div # ".inner" # hover ?
        backgroundImage (C.url (pack $ url sc </> "images/2arrow-r-active.png"))
    a # ".permalink" ? fontSize (px 10)

defaultCss sc = do
  general
  layout
  div # "#super" ? do
    position absolute
    top (px 30)
    left (px 0)
    right (px 0)
  body # ("." ++ show TextStyle) ? do
    textCss
  body # ("." ++ show Maximize) ? maximize sc

-- mobileCss sc = do
--   ((div # "#logoWrapper") <> (div # "#spalte") <> (div # "#reihe")) ? do
--     left 0
--     marginLeft 0
--   div # "#reihe" ? left (px 200)
--   -- div # "#spalte" ? do
--   --   visibility visible
--   --   opacity 1

genCss :: SiteCfg -> IO ()
genCss sc = do
    ex <- doesDirectoryExist (outPath sc </> "css")
    unless ex (createDirectoryIfMissing True (outPath sc </> "css"))
    L.writeFile (outPath sc </> "css/default.css") $
      if myTst sc
        then renderWith compact [] $ defaultCss sc
        else render                $ defaultCss sc
    -- L.writeFile (outPath sc </> "css/mobile.css") $
    --   if "http" `Li.isPrefixOf` url sc
    --     then render                $ mobileCss sc
    --     else renderWith compact [] $ mobileCss sc
    putStrLn "css done"
