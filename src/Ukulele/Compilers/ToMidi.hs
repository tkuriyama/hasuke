module Ukulele.Compilers.ToMidi where

import           Data.Ratio

import           Euterpea

import            Ukulele.Ukulele as Uke

--------------------------------------------------------------------------------

type Tuning = (Int, Int, Int, Int)

lowG :: Tuning
lowG = (69, 64, 60, 55)

scoreToMidi :: FilePath -> Score -> IO ()
scoreToMidi fpath (Score _ _ xs) =
  writeMidi fpath $ line $ map (interpret lowG) xs

interpret :: Tuning -> Section -> Music (Pitch, Volume)
interpret tuning s = case s of
  TextSection _ -> addVolume 0 $ rest 0  -- skip text sections
  ChordSection _ -> addVolume 0 $ rest 0 -- not (yet) implemented
  TabSection t num denom xs -> interpretTab tuning t num denom xs

--------------------------------------------------------------------------------

interpretTab :: Tuning -> Maybe Uke.Tempo -> Int -> Maybe Int -> [Uke.Bar] ->
                Music (Pitch, Volume)
interpretTab tuning mt _ md xs =
  instrument AcousticGuitarNylon $
  Euterpea.tempo t' $
  line $ map (interpretBar tuning baseDur) xs
  where
    t' = case mt of
           Just t -> (baseDur / qn) * ((toInteger t) % 120)
           Nothing -> 1
    baseDur = case md of
                Just denom -> 1 % (toInteger denom)
                Nothing -> 1 % 4

interpretBar :: Tuning -> Rational -> Bar -> Music (Pitch, Volume)
interpretBar tuning baseDur (Bar _ aStr eStr cStr gStr) =
  h aStr aAp :=: h eStr eAp:=: h cStr cAp:=: h gStr gAp
  where
    h notes ap = addVolume 100 $ interpretString ap baseDur notes
    (aAp, eAp, cAp, gAp) = tuning

interpretString :: AbsPitch -> Rational -> [Uke.Note] -> Music Pitch
interpretString ap baseDur notes = line $ map h notes
  where
    h (Uke.Rest restDur) =
      rest (baseDur * (toInteger restDur % 1))
    h (Uke.Note fret noteDur _) =
      note (baseDur * (toInteger noteDur % 1)) (pitch $ ap + fret)
