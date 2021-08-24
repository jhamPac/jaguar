{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Web.Scotty
import Data.Monoid (mconcat)

run :: IO ()
run = scotty 9000 $
    get "/:word" $ do
        w <- param "word"
        html $ mconcat ["<h1>Jaguar, ", w, " web server with Haskell!</h2>"]
