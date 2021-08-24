{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Web.Scotty
import Data.Monoid (mconcat)

run :: IO ()
run = scotty 9000 $
    get "/" $
        html $ renderHtml $
            H.html $
                H.body $ do
                    H.h1 "Jaguar, the vicious web server!"
