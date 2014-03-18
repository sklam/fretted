import Fretted.Basic
import Fretted.LilyPond
import Data.List

main = putStrLn $ out
    where d3 = Pitch D 3
          f4 = sharp $ Pitch F 4
          fret = standardGuitarFretboard
          g1 = quarter $ makeNoteGroup [(d3, 4), (f4, 2)]
          g2 = compress 2 $ fromFretPos fret [7, 9, 9, 8, 7, 7]
          g3 = compress 4 $ fromFretPos fret [0, 2, 2, 1, 0, 0]
          finger = autoFingering (limitFret fret (0, 12)) False
          g4 = finger [d3, f4, (Pitch E 4)]
          scale1 = ionian $ Pitch C 4
          scale2 = reverse $ lydian $ sharp $ Pitch D 3
          g5 = map (compress 4) $ map finger $ zipWith (\a b -> [a, b]) scale1 scale2
          --out = show g5
          fmt = unlines $ map formatNote $ [g1, g2, g3, g4] ++ g5
          out = singleVoiceFile fmt 120
