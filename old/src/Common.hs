{-# LANGUAGE ExistentialQuantification #-}
module Common
  where
import           Text.Blaze.Html5 (Html)
import           Data.List

--------------------------------------------------------------------------------
--  Data definitions

data PStyle = Maximize | TextStyle
  deriving (Show, Eq)

data Nav = N { navTitle :: String         -- Title
             , navPath :: Maybe FilePath  -- Adress
             , subs :: [Nav]              -- SubNav
             }

data Social = Social { socialImg :: String
                     , socialImgType :: String
                     -- , socialImgWidth :: Int
                     -- , socialImgHeight :: Int
                     }

data Page = P { pPath :: [FilePath]       -- urls/filenames
              , pTitle :: Maybe String    -- title
              , pStyle :: PStyle          -- class
              , pCtn :: Html              -- content
              , pNav :: Nav               -- Navigation
              , pLine :: Maybe Html       -- Second nav
              , pSocial :: Maybe Social    -- social img for sharing
              }

data SiteCfg = SC { statics :: [FilePath] -- static files
                  , url :: String
                  , outPath :: FilePath
                  , defaultP :: Page
                  , indexP :: Maybe FilePath
                  }

--------------------------------------------------------------------------------
--  Helper

if' :: forall t. Bool -> t -> t -> t
if' True a _ = a
if' False _ b = b

myTst :: SiteCfg -> Bool
myTst sc = "http" `isPrefixOf` url sc

myTrimUrl :: SiteCfg -> String -> String
myTrimUrl sc url = if' (myTst sc && (".html" `isSuffixOf` url)) 
                       (if' ("index.html" `isSuffixOf` url)
                            (take (length url - 10) url)
                            (take (length url - 5) url))
                       url
