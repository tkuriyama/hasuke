module Ukulele.Compilers.ToHtml where

import qualified Data.Text as T

import Lucid

import Ukulele.Ukulele

--------------------------------------------------------------------------------

data PrintConfig
  = PrintConfig { maxNotesPerRow :: Int -- horizontal max
                , maxRowsPerPage :: Int  -- vertical max
                , charsPerNote :: Int
                }

configs :: PrintConfig
configs = PrintConfig 16 6 4

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
      div_ [class_ "attributions"] $ do
          mapM_ (\line -> (span_ $ toHtml line) <> br_ []) attrs

showSections :: [Section] -> Html ()
showSections xs =
  div_ [class_ "sections"] $ do
  mapM_ f xs
  where f x = case x of
                TextSection t -> showText t
                ChordSection ps -> showChords ps
                TabSection tmp n d bs -> showTabs tmp n d bs

--------------------------------------------------------------------------------

showText :: T.Text -> Html ()
showText t = div_ [] $ toHtml t

--------------------------------------------------------------------------------

showChords :: [ChordPairs] -> Html ()
showChords _ = p_ ""

--------------------------------------------------------------------------------

type Page = [Bars]
type Bars = [Bar]

showTabs :: Maybe Tempo -> Int -> Maybe Int -> Bars -> Html ()
showTabs mTempo num mDenom bs =
  div_ [] $ do
  headers
  mapM_ (div_ [class_ "tabPage"] . showPage num) pages
  where
    headers =
      case mTempo of
        Nothing -> time
        (Just t) -> span_ (toHtml $ "Tempo: " ++ show t ++ "   ")
                    <> br_ [] <> time
    time =
      case mDenom of
        Nothing -> span_ (toHtml $ show num ++ "beats per bar")
        Just denom -> span_ (toHtml $ show num ++ "/" ++ show denom)
    pages = genPages $ genRows num bs

showPage :: Int -> Page -> Html ()
showPage beats page =
  div_ [] $ do { mapM_ (showRow beats) page }

showRow :: Int -> Bars -> Html ()
showRow beats bs =
  div_ [] $
  br_ [] <> 
  toHtml chords <> br_ [] <>
  toHtml as <> br_ [] <>
  toHtml es <> br_ [] <>
  toHtml cs <> br_ [] <>
  toHtml gs <> br_ []
  where
    (chords, as, es, cs, gs) = joinBars beats bs

joinBars :: Int -> Bars -> (T.Text, T.Text, T.Text, T.Text, T.Text)
joinBars beats bs = (chords, as, es, cs, gs)
  where chords = joinChords beats $ map barChord bs
        f accessor = joinNotes $ map accessor bs
        (as, es, cs, gs) = (f a, f e, f c, f g)

joinChords :: Int -> [Maybe Chord] -> T.Text
joinChords beats xs = foldl f "" xs
  where
    n = (beats * (charsPerNote configs) + 1)
    f acc mChord = case mChord of
                     Nothing -> acc <> fill "" "-" n
                     Just chord -> acc <> fill chord "-" n

joinNotes :: [[Note]] -> T.Text
joinNotes nss =
  T.intercalate "|" $ map (\ns -> T.concat $ map (showNote noteLen) ns) nss
  where
    noteLen = charsPerNote configs

showNote :: Int -> Note -> T.Text
showNote noteLen (Rest dur) = T.replicate (noteLen * dur) "-"
showNote noteLen (Note fret dur mMod) = case mMod of
  Nothing -> note 0
  (Just Hammer) -> "h" <> note 1
  (Just Slide) -> "/" <> note 1
  where
    fillChar = if dur == 1 then "-" else "="
    note offset = fill (packs fret) fillChar (noteLen * dur - offset)

fill :: T.Text -> T.Text -> Int -> T.Text
fill t fillChar n = t <> filler
  where filler = T.replicate (n - T.length t) fillChar

genPages :: [Bars] -> [Page]
genPages bs = group [] bs ns
  where n = maxRowsPerPage configs
        ns = (n-1) : repeat n

genRows :: Int -> Bars -> [Bars]
genRows beats bs = group [] bs (repeat barsPerRow)
  where m = maxNotesPerRow configs
        barsPerRow = m `div` beats

--------------------------------------------------------------------------------

packs :: (Show a) => a -> T.Text
packs = T.pack . show

group :: [[a]] -> [a] -> [Int] -> [[a]]
group acc [] _ = reverse acc
group acc xs [] = reverse (xs:acc) -- this branch shouldn'y really be called
group acc xs (n:ns) = let (taken, rest) = (take n xs, drop n xs)
                      in group (taken:acc) rest ns
