{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Web.Scotty
import Data.Monoid (mconcat)

run :: IO ()
run = scotty 9000 $
    get "/" $ html "<h1>Jaguar</h1>"
