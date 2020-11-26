module Ukulele.Ukulele where

import Data.Aeson
import Data.Text as T
import GHC.Generics

--------------------------------------------------------------------------------

customOptions :: Options
customOptions =
  defaultOptions
  { sumEncoding = UntaggedValue
  }
--------------------------------------------------------------------------------

data Score
  = Score { title :: T.Text
          , attributions :: Maybe [T.Text]
          , sections :: [Section]
          }
    deriving (Generic, Show)

instance ToJSON Score where
  toEncoding = genericToEncoding customOptions

instance FromJSON Score where
  parseJSON = genericParseJSON customOptions

--------------------------------------------------------------------------------

data Section
  = TextSection { text :: T.Text }
  | ChordSection { pairs :: [ChordPairs]}
  | TabSection { tempo :: Maybe Tempo
               , timeNumerator :: Int
               , timeDenominator :: Maybe Int
               , bars :: [Bar]
               }
  deriving (Generic, Show)

type ChordPairs = [ChordPair]
data ChordPair = ChordPair Chord T.Text
  deriving (Generic, Show)

instance ToJSON ChordPair where
  toEncoding = genericToEncoding customOptions

instance FromJSON ChordPair where
  parseJSON = genericParseJSON customOptions

type Tempo = Int
type Time = Rational
type Chord = T.Text

instance ToJSON Section where
  toEncoding = genericToEncoding customOptions

instance FromJSON Section where
  parseJSON = genericParseJSON customOptions

--------------------------------------------------------------------------------

data Bar
  = Bar { barChord :: Maybe Chord
        , a :: [Note]
        , e :: [Note]
        , c :: [Note]
        , g :: [Note]
        }
    deriving (Generic, Show)

instance ToJSON Bar where
  toEncoding = genericToEncoding customOptions

instance FromJSON Bar where
  parseJSON = genericParseJSON customOptions

--------------------------------------------------------------------------------

data Note
  = Rest Duration
  | Note Fret Duration (Maybe Modifier)
  deriving (Generic, Show)

type Fret = Int
type Duration = Int

instance ToJSON Note where
  toEncoding = genericToEncoding customOptions

instance FromJSON Note where
  parseJSON = genericParseJSON customOptions

data Modifier
  = SlideUp
  | SlideDown
  | Hammer
  | Mordent
  deriving (Generic, Show)

instance ToJSON Modifier where
  toEncoding = genericToEncoding customOptions

instance FromJSON Modifier where
  parseJSON = genericParseJSON customOptions
