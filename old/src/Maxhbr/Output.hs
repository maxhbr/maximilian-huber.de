module Maxhbr.Output
    ( doOutput
    , doOutputs
    ) where

import           Control.Monad (mapM_)
import qualified Data.Text.Lazy.IO as L
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           System.Directory (createDirectoryIfMissing, copyFile)
import           System.FilePath (takeDirectory)

import           Maxhbr.Types

mkParentDirectories :: FilePath -> IO ()
mkParentDirectories = createDirectoryIfMissing True . takeDirectory

doOutput :: Output -> IO()
doOutput (HtmlOutput html targets) = let
    renderedHtml = renderHtml html
  in mapM_ (\ target -> do
               mkParentDirectories target
               L.writeFile target renderedHtml
           ) targets
doOutput (CopyOutput src targets)  =
  mapM_ (\target -> do
            mkParentDirectories target
            copyFile src target
        ) targets

doOutputs :: [Output] -> IO()
doOutputs = mapM_ doOutput

