module Fretted.Basic where

data PitchSymbol = C | Cis | D | Dis | E | F | Fis | G | Gis | A | Ais | B
                   deriving (Enum, Eq, Show, Bounded, Ord)

data Pitch = Pitch              -- Absolute Pitch
    { symbol     :: PitchSymbol
    , octave     :: Int
    } deriving (Eq, Show)


instance Ord Pitch where
    compare (Pitch sym1 oct1) (Pitch sym2 oct2)
                   | oct1 == oct2 = compare sym1 sym2
                   | oct1 /= oct2 = compare oct1 oct2


data FretPitch = FretPitch
    { pitch   :: Pitch  -- pitch
    , string  :: Int    -- which string
    } deriving Show

data NoteGroup = NoteGroup
    { pitches      :: [FretPitch]
    , duration     :: Int             -- as a demoninator of time
    } deriving Show

data Fretboard = Fretboard
    { tuning      :: String
    , stringNotes :: [[Pitch]]
    } deriving Show


flat :: Pitch -> Pitch
flat (Pitch sym oct) = Pitch symn octn
    where
        (octn, symn)
            | sym == lobound   = (oct - 1, upbound)
            | otherwise        = (oct, (pred sym))
        upbound = (maxBound :: PitchSymbol)
        lobound = (minBound :: PitchSymbol)


sharp :: Pitch -> Pitch
sharp (Pitch sym oct) = Pitch symn octn
    where
        (octn, symn)
            | sym == upbound   = (oct + 1, lobound)
            | otherwise        = (oct, (succ sym))
        upbound = (maxBound :: PitchSymbol)
        lobound = (minBound :: PitchSymbol)


standardGuitarFretboard :: Fretboard
standardGuitarFretboard = Fretboard "EADGBE" strings
    where
        strings = [s1, s2, s3, s4, s5, s6]
        n = 20
        s1 = generateStringNotes n (Pitch E 4)
        s2 = generateStringNotes n (Pitch B 3)
        s3 = generateStringNotes n (Pitch G 3)
        s4 = generateStringNotes n (Pitch D 3)
        s5 = generateStringNotes n (Pitch A 2)
        s6 = generateStringNotes n (Pitch E 2)


generateStringNotes :: Int -> Pitch -> [Pitch]
generateStringNotes numFret initPitch
    | numFret > 0   = [initPitch] ++ next
    | numFret == 0  = []
        where
            next = (generateStringNotes (numFret - 1) (sharp initPitch))


makeNoteGroup :: [(Pitch, Int)] -> NoteGroup
makeNoteGroup pitchString = NoteGroup (map asFretPitch pitchString) 1


asFretPitch :: (Pitch, Int) -> FretPitch
asFretPitch (pitch, string) = FretPitch pitch string


formatNoteGroup :: NoteGroup -> String
formatNoteGroup ng = unwords ["<", notes, ">", dur]
    where
        notes = unwords $ map formatFretPitch (pitches ng)
        dur = show $ duration ng


formatFretPitch :: FretPitch -> String
formatFretPitch fp = concat [(formatPitch $ pitch fp), "\\", (show $ string fp)]


formatPitch :: Pitch -> String
formatPitch p = sym ++ oct
    where sym = case (symbol p) of
                 C   -> "c"
                 Cis -> "cis"
                 D   -> "d"
                 Dis -> "dis"
                 E   -> "e"
                 F   -> "f"
                 Fis -> "fis"
                 G   -> "g"
                 Gis -> "gis"
                 A   -> "a"
                 Ais -> "ais"
                 B   -> "b"
          oct = addOctaveMarkMidC (octave p)

middleC :: Int
middleC = 4

addOctaveMarkMidC :: Int -> String
addOctaveMarkMidC oct = addOctaveMark (oct - middleC)

addOctaveMark :: Int -> String
addOctaveMark n
    | n > 0     = "'" ++ addOctaveMark (n - 1)
    | n == 0    = ""
    | n < 0     = "," ++ addOctaveMark (n + 1)

