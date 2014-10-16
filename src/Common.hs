module Common
  where
import           Text.Blaze.Html5 (Html)
import           Data.Monoid


--------------------------------------------------------------------------------
--  Data definitions

data SiteCfg = SC { staticFolders :: [FilePath] -- static fieles
                  , url :: String
                  , outPath :: FilePath
                  , cssFile :: FilePath
                  }

data Nav = N { navTitle :: String  -- Title
             , navPath :: FilePath -- Adress
             , subs :: [Nav]       -- SubNav
             }

data Page = P { pPath :: [FilePath] -- urls/filenames
              , pTitle :: String    -- title
              , pStyle :: String    -- class
              , pCtn :: Html        -- content
              , pNav :: Nav         -- Navigation
              , pLine :: Html       -- Second nav
              }
defaultPage = P { pPath = []                    -- where to save
                , pTitle = ""                   -- title of the page
                , pStyle = "text"               -- style of the page
                , pCtn = mempty                 -- content of the page
                , pNav = N { navTitle = "home"  -- navigation information
                          , navPath = "/"
                          , subs = [] 
                          }
                , pLine = mempty                -- more overlays
                }

--------------------------------------------------------------------------------
--  Helper

if' True a _ = a
if' False _ b = b
