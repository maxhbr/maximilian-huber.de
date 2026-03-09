{-# LANGUAGE ExistentialQuantification #-}
module Maxhbr.Types
  where

import           Text.Blaze.Html5 (Html)
import           Data.Text (Text)

data NavEntry
  = NavEntry
  { navEntryTitle :: String
  , navEntryTarget :: String
  , navEntryChildren :: [NavEntry]
  , navOrderingIndex :: Int
  } deriving (Eq)

instance Ord NavEntry where
  NavEntry{navOrderingIndex = i1, navEntryTitle = t1} `compare` NavEntry{navOrderingIndex = i2, navEntryTitle = t2}
    = if i1 == i2
      then t1 `compare` t2
      else i1 `compare` i2

type Navigation = [NavEntry]

data Page
  = Page
  { pageTitle :: String
  , pageNavigation :: Navigation
  , pageContent :: Html
  , pageBreadcrump :: Html
  , pageOutputPathes :: [FilePath]
  }

data Output
  = HtmlOutput Html [FilePath]
  -- | TextOutput Text [FilePath]
  -- | BSOutput ByteString [FilePath]
  | CopyOutput FilePath [FilePath]

class Pageable a where
  mkToPage :: a -> Page

type Style = String

data RenderableA
  = forall a.
    Renderable a => RenderableA a

class Renderable a where
  getTitle :: a -> Text
  getBreadCrump :: a -> [Text]
  getNavigation :: Navigation -> a -> Navigation
  getStyle :: a -> Style
  getContent :: a -> Html

newtype Producer a b
  = Producer
  { produce :: a -> [b]
  }

data State
  = State
  { sNavigation :: Navigation
  , sOutputs :: [Output]
  , sRenderables :: [RenderableA]
  }
