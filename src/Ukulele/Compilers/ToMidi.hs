module Ukulele.Compilers.ToMidi where

import           Data.Ratio

import           Euterpea
import           Euterpea.IO.MIDI (writeMidi)

import            Ukulele.Ukulele as Uke

--------------------------------------------------------------------------------

type Tuning = (Int, Int, Int, Int)

lowG :: Tuning
lowG = (69, 64, 60, 55)

scoreToMidi :: FilePath -> Score -> IO ()
scoreToMidi fpath (Score _ _ xs) =
  writeMidi fpath $ line $ map (interpret lowG) xs

interpret :: Tuning -> Section -> Music (Pitch, Volume)
interpret s = case s of
  TextSection _ -> addVolume 0 $ rest 0  -- skip text sections
  ChordSection _ -> addVolume 0 $ rest 0 -- not (yet) implemented
  TabSection t num denom xs -> interpretTab tuning t num denom xs

--------------------------------------------------------------------------------

interpretTab :: Tuning -> Maybe Uke.Tempo -> Int -> Maybe Int -> [Uke.Bar] ->
                Music (Pitch, Volume)
interpretTab tuning mt _ md xs =
  instrument AcousticGuitarNylon $
  Euterpea.tempo t' $
  line $ map (interpretBar tuning time) xs
  where
    t' = case mt of
           Just t -> (time / qn) * ((toInteger t) % 120)
           Nothing -> 1
    time = case md of
              Just denom -> 1 % (toInteger denom)
              Nothing -> 1 % 4

interpretBar :: Tuning -> Rational -> Bar -> Music (Pitch, Volume)
interpretBar tuning time (Bar _ aStr eStr cStr gStr) =
  h aStr aAp :=: h eStr eAp:=: h cStr cAp:=: h gStr gAp
  where
    h notes ap = addVolume 100 $ interpretString ap time notes
    (aAp, eAp, cAp, gAp) = tuning

interpretString :: AbsPitch -> Rational -> [Uke.Note] -> Music Pitch
interpretString ap time notes = _
