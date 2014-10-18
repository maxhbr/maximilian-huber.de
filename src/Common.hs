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
             , navPath :: Maybe FilePath -- Adress
             , subs :: [Nav]       -- SubNav
             }
genDefaultNav galNav = 
  N { navTitle = "Home" 
    , navPath = Just "" 
    , subs = [ N { navTitle = "Galerie"
                 , navPath = Just "galerie"
                 , subs = galNav}
             , N { navTitle = "Webdesign"
                 , navPath = Just "webdesign.html"
                 , subs = []}
             , N { navTitle = "Kontakt"
                 , navPath = Nothing
                 , subs = [ N { navTitle = "gpg-pubkey"
                              , navPath = Just "gpg-pubkey.html"
                              , subs = []}
                          , N { navTitle = "Impress"
                              , navPath = Just "impress.html"
                              , subs = []}]}]}
defaultNav = genDefaultNav []

data Page = P { pPath :: [FilePath] -- urls/filenames
              , pTitle :: Maybe String    -- title
              , pStyle :: String    -- class
              , pCtn :: Html        -- content
              , pNav :: Nav         -- Navigation
              , pLine :: Html       -- Second nav
              }
defaultPage = P { pPath = []                    -- where to save
                , pTitle = Nothing                   -- title of the page
                , pStyle = "text"               -- style of the page
                , pCtn = mempty                 -- content of the page
                , pNav = defaultNav             -- navigation information
                , pLine = mempty                -- more overlays
                }

--------------------------------------------------------------------------------
--  Helper

if' True a _ = a
if' False _ b = b
