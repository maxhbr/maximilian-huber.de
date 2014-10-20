module Common
  where
import           Text.Blaze.Html5 (Html)
import           Data.Monoid

--------------------------------------------------------------------------------
--  Data definitions

data Nav = N { navTitle :: String         -- Title
             , navPath :: Maybe FilePath  -- Adress
             , subs :: [Nav]              -- SubNav
             }

data Page = P { pPath :: [FilePath]       -- urls/filenames
              , pTitle :: Maybe String    -- title
              , pStyle :: String          -- class
              , pCtn :: Html              -- content
              , pNav :: Nav               -- Navigation
              , pLine :: Maybe Html       -- Second nav
              }

data SiteCfg = SC { statics :: [FilePath] -- static fieles
                  , url :: String
                  , outPath :: FilePath
                  , defaultP :: Page
                  , indexP :: Maybe FilePath
                  }

--------------------------------------------------------------------------------
--  Helper

if' True a _ = a
if' False _ b = b
