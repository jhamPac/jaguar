{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run,
  )
where

import           Data.Monoid                        (mconcat)
import           Text.Blaze.Html.Renderer.Text      (renderHtml)
import qualified Text.Blaze.Html4.Strict.Attributes as A
import qualified Text.Blaze.Html5                   as H
import           Web.Scotty

run :: IO ()
run = scotty 9000 $
    get "/" $
        html $
            renderHtml $
                H.html $
                    H.body $ do
                        H.h1 "Shortener"
                        H.form H.! A.method "post" H.! A.action "/" $ do
                            H.input H.! A.type_ "text" H.! A.name "url"
                            H.input H.! A.type_ "submit"

