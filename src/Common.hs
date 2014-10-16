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

data Site = S { sPath :: [FilePath] -- urls/filenames
              , sTitle :: String    -- title
              , sStyle :: String    -- class
              , sCtn :: Html        -- content
              , sNav :: Nav         -- Navigation
              , sLine :: Html       -- Second nav
              }
siteDefault = S { sPath = []                    -- where to save
                , sTitle = ""                   -- title of the page
                , sStyle = "text"               -- style of the page
                , sCtn = mempty                 -- content of the page
                , sNav = N { navTitle = "home"  -- navigation information
                          , navPath = "/"
                          , subs = [] 
                          }
                , sLine = mempty                -- more overlays
                }
