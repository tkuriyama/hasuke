module Ukulele.Compilers.ToMidi where

import           Data.Ratio

import           Euterpea
import           Euterpea.IO.MIDI (writeMidi)

import            Ukulele.Ukulele as Uke

--------------------------------------------------------------------------------

scoreToMidi :: FilePath -> Score -> IO ()
scoreToMidi fpath (Score _ _ xs) =
  writeMidi fpath $ line $ map interpret xs

interpret :: Section -> Music (Pitch, Volume)
interpret s = case s of
  TextSection _ -> addVolume 0 $ rest 0
  ChordSection _ -> addVolume 0 $ rest 0
  TabSection t num denom xs -> interpretTab t num denom xs

--------------------------------------------------------------------------------

interpretTab :: Maybe Uke.Tempo -> Int -> Maybe Int -> [Uke.Bar] ->
                Music (Pitch, Volume)
interpretTab mt _ md xs =
  instrument AcousticGuitarNylon $
  Euterpea.tempo t' $
  line $ map (interpretBar time) xs
  where
    t' = case mt of
           Just t -> (time / qn) * ((toInteger t) % 120)
           Nothing -> 1
    time = case md of
              Just denom -> 1 % (toInteger denom)
              Nothing -> 1 % 4

interpretBar :: Rational -> Bar -> Music (Pitch, Volume)
interpretBar time (Bar _ aStr eStr cStr gStr) =
  h aStr :=: h eStr :=: h cStr :=: h gStr
  where
    h notes = addVolume 100 $ interpretString time notes

interpretString :: Rational -> [Uke.Note] -> Music Pitch
interpretString time notes = _
