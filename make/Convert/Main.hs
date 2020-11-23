module Convert.Main where

import           Data.Aeson
import           Data.List as L
import           Data.Text as T
import           System.Environment (getArgs)
import           System.FilePath

import Ukulele.Compilers.ToHtml as ToHtml

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if L.length args < 1 then
    putStrLn "No path to file provided" >> pure ()
    else convert $ L.head args

packs :: (Show a) => a -> T.Text
packs = T.pack . show

--------------------------------------------------------------------------------

convert :: FilePath -> IO ()
convert fpath = do
  case takeExtension fpath of
    ".tab" -> pure ()
    ".json" -> jsonToHtml fpath >> pure ()
    _ -> putStrLn "Input file has invalid extension" >> pure ()

jsonToHtml :: FilePath -> IO ()
jsonToHtml fpath = do
  result <- eitherDecodeFileStrict fpath
  case result of
    Left err ->
      putStrLn err >> pure ()
    Right score ->
      ToHtml.scoreToFile (htmlPath fpath) score >> pure ()

htmlPath :: FilePath -> FilePath
htmlPath fpath = dropExtension fpath <.> "html"

