import Fretted.Basic
import Fretted.LilyPond

main = putStrLn $ out
    where d3 = Pitch D 3
          f4 = sharp $ Pitch F 4
          fret = standardGuitarFretboard
          g1 = makeNoteGroup [(d3, 4), (f4, 2)]
          g2 = fromFretPos fret [7, 9, 9, 8, 7, 7]
          g3 = fromFretPos fret [0, 2, 2, 1, 0, 0]
          fmt = unlines $ map formatNote [g1, g2, g3]
          out = singleVoiceFile fmt 120
