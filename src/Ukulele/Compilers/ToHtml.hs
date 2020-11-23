module Ukulele.Compilers.ToHtml where

import qualified Data.Text as T

import Lucid

import Ukulele.Ukulele as Uke

--------------------------------------------------------------------------------

scoreToFile :: FilePath -> Score -> IO ()
scoreToFile fpath score =
  renderToFile fpath (scoreToHtml score)

scoreToHtml :: Score -> Html ()
scoreToHtml s =
  doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml (title s)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "uke_tabs.css"]
  html_ $ do
    body_ [class_ "body"] $
      showHeader (title s) (attributions s) <>
      br_ [] <>
      showSections (sections s)

showHeader :: T.Text -> Maybe [T.Text] -> Html ()
showHeader t mAttrs =
  div_ [class_ "title"] $ toHtml t <>
  case mAttrs of
    Nothing -> br_ []
    Just attrs ->
      (div_ [class_ "attribution"] $ toHtml $ T.unlines attrs) <>
      br_ []

showSections :: [Section] -> Html ()
showSections _ = p_ ""
