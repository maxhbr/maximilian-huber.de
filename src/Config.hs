module Config
  ( scPre
  ) where
import           Data.Monoid

import           Common

genDefaultNav galNav = N { navTitle = "Home"
                         , navPath  = Just ""
                         , subs     =
                           [ galNav
                           , N { navTitle = "Webdesign"
                               , navPath  = Just "webdesign.html"
                               , subs     = []}
                           , N { navTitle = "Kontakt"
                               , navPath  = Just "kontakt.html"
                               , subs     =
                                 [ N { navTitle = "gpg-pubkey"
                                     , navPath  = Just "gpg-pubkey.html"
                                     , subs     = []}
                                 , N { navTitle = "GitHub"
                                     , navPath  = Just
                                         "https://github.com/maxhbr"
                                     , subs     = []}
                                 , N { navTitle = "Impress"
                                     , navPath  = Just "impress.html"
                                     , subs     = []}]}]}

scPre galNav = SC { statics  = ["css","galerie","images","scripts"
                               ,"gpg-pubkey.asc","favicon.ico"
                               ,"qr.jpg","qr_large.jpg"]
                  , url      = "https://maximilian-huber.de"
                  , outPath  = "_site"
                  , defaultP = P { pPath   = []
                                 , pTitle  = Nothing
                                 , pStyle  = TextStyle
                                 , pCtn    = mempty
                                 , pNav    = genDefaultNav galNav
                                 , pLine   = Nothing
                                 , pSocial = Nothing}
                  , indexP   = Just "galerie/index.html"}

