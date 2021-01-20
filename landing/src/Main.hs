{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}


module Main where

import           Reflex
import           Reflex.Dom
import           Data.Text (pack, unpack, Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Text.Read (readMaybe)
import           Data.Monoid ((<>))
import           Data.FileEmbed

main :: IO ()
main = mainWidget $ do
  elClass "h1" "mainTitle" $ text "This is the main title"
  el "h1" $ text "A link to Google in a new tab"
  elAttr "a" attrs $ text "Google!"

attrs :: Map.Map T.Text T.Text
attrs = ("target" =: "_blank") <> ("href" =: "http://google.com")

main2 :: IO ()
main2 = mainWidget $ do
  rec
    dynBool <- toggle False evClick
    let dynAttrs = attrs2 <$> dynBool
    elDynAttr "h2" dynAttrs $ text "Changing color"
    evClick <- button "Change Color"
  return ()

attrs2 :: Bool -> Map.Map T.Text T.Text
attrs2 b = "style" =: ("color: " <> color b)
  where
    color True = "red"
    color _ = "green"

main3 :: IO ()
main3 = mainWidgetWithCss css $ do
  rec
    dynBool <- toggle False evClick
    let dynAttrs = attrs2 <$> dynBool
    elDynAttr "h2" dynAttrs $ text "Changing color"
    evClick <- button "Change Color"
  return ()
  where css = $(embedFile "css/main.css")


main4 :: IO ()
main4 = mainWidgetWithCss css $ do
  el "h1" $ text "This title should be Blue"
  rec
    dynBool <- toggle False evClick
    let dynAttrs = attrs2 <$> dynBool
    elDynAttr "h2" dynAttrs $ text "Changing color"
    evClick <- button "Change Color"
  return ()
  where css = $(embedFile "css/main.css")

mainmain :: IO ()
mainmain = mainWidgetWithHead headElement bodyElement

headElement :: DomBuilder t m => m ()
headElement = do
  el "title" $ text "Main Title"
  styleSheet "css/simple.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

bodyElement :: DomBuilder t m => m ()
bodyElement = el "div" $ do
  el "h2" $ text "This text should be blue"
  return ()
