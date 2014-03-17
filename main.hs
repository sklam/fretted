import Fretted.Basic
import Fretted.LilyPond

main = putStrLn $ out
    where d4 = Pitch D 4
          f4 = sharp $ Pitch F 5
          fret = standardGuitarFretboard
          g1 = makeNoteGroup [(d4, 4), (f4, 3)]
          g2 = makeNoteGroup [(sharp d4, 4), (sharp f4, 2)]
          fmt = unlines $ map formatNoteGroup [g1, g2]
          out = singleVoiceFile fmt 120
          --t = fromFretPos [(7,2,6), (10,8)]



