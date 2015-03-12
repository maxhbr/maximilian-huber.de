module Config (scPre)where
import           Data.Monoid

import           Common

genDefaultNav galNav = N { navTitle = "Home"
                         , navPath  = Just ""
                         , subs     =
                           [ galNav
                           , N { navTitle = "Webdesign"
                               , navPath  = Just "webdesign.html"
                               , subs     = []}
                           -- , N { navTitle = "Blog"
                           --     , navPath  = Just "blog.html"
                           --     , subs     = []}
                           , N { navTitle = "Kontakt"
                               , navPath  = Just "kontakt.html"
                               , subs     =
                                 [ N { navTitle = "gpg-pubkey"
                                     , navPath  = Just "gpg-pubkey.html"
                                     , subs     = []}
                                 -- , N{ navTitle = "Facebook"
                                 --     , navPath  = Just
                                 --         "https://www.facebook.com/pages/Fotografie-Maximilian-Huber/122707361149256"
                                 --     , subs     = []}
                                 , N{ navTitle = "Impress"
                                     , navPath  = Just "impress.html"
                                     , subs     = []}]}]}
scPre galNav = SC { statics  = ["css","galerie","images"
                               ,"gpg-pubkey.asc","favicon.ico"
                               ,"qr.jpg","qr_large.jpg"]
                  , url      = "http://maximilian-huber.de"
                  , outPath  = "_site"
                  , defaultP = P { pPath  = []
                                 , pTitle = Nothing
                                 , pStyle = "text"
                                 , pCtn   = mempty
                                 , pNav   = genDefaultNav galNav
                                 , pLine  = Nothing}
                  , indexP   = Just "galerie/index.html"}

