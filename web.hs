{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Data.FileEmbed
import Data.Time
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import Web.Scotty
import Control.Monad.IO.Class (liftIO)

import Lib

main :: IO ()
main = do
  putStrLn "Please open this link: http://localhost:3333/index.html"
  scotty 3333 $ do
    get "/" $ redirect "/zeit/index.html"
    get "/index.html"  . html . TL.fromStrict $ E.decodeUtf8 $(embedFile "index.html")
    post "/navigate" $ do
      rawInput <- param "input"
      let input = filter (`elem` ['A'..'Z'] ++ ['0'..'9']) rawInput
      let result = gpxFor input
      html $ TL.pack result
      t <- liftIO getCurrentTime
      liftIO $ appendFile "zeit.log" (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) t ++ ";" ++ input ++ "\n")
