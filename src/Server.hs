{-# LANGUAGE OverloadedStrings #-}

module Server
  ( run,
  )
where

import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Data.IORef                         (modifyIORef, newIORef)
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Monoid                        (mconcat)
import           Data.Text                          (Text)
import           Text.Blaze.Html.Renderer.Text      (renderHtml)
import qualified Text.Blaze.Html4.Strict.Attributes as A
import qualified Text.Blaze.Html5                   as H
import           Web.Scotty                         (get, html, param, post,
                                                     redirect, scotty)

run :: IO ()
run = do
    urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
    scotty 9000 $ do
        get "/" $
            html $
                renderHtml $
                    H.html $
                        H.body $ do
                            H.h1 "Shortener"
                            H.form H.! A.method "post" H.! A.action "/" $ do
                                H.input H.! A.type_ "text" H.! A.name "url"
                                H.input H.! A.type_ "submit"
        post "/" $ do
            url <- param "url"
            liftIO $ modifyIORef urlsR $
                \(i, urls) ->
                    (i + 1, M.insert i url urls)
            redirect "/"

