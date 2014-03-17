module Fretted.Basic where
import Data.Maybe

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

data Note =
      NoteGroup
        { ngPitches      :: [FretPitch]
        , ngDuration     :: Int             -- as a demoninator of time
        }
    | Note
        { nPitch        :: FretPitch
        , nDuration     :: Int
        }
    deriving Show

data Fretboard = Fretboard
    { tuning      :: String
    , stringNotes :: [[Pitch]]
    } deriving Show


x :: Int
x = -1

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


makeNoteGroup :: [(Pitch, Int)] -> Note
makeNoteGroup pitchString = NoteGroup (map asFretPitch pitchString) 1


asFretPitch :: (Pitch, Int) -> FretPitch
asFretPitch (pitch, string) = FretPitch pitch string


formatNote :: Note -> String
formatNote (NoteGroup pitches duration) = unwords ["<", notes, ">", dur]
    where
        notes = unwords $ map formatFretPitch pitches
        dur = show duration


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


{-|
    The guitar is a tranposing instructment.  We will use treble-8 clef.
    The `guitarMiddleC` is for LilyPond absolute mode where 'c' would mean the
    guitar middle C, which is an octave belofe the actual middle C.
-}
guitarMiddleC :: Int
guitarMiddleC = 3

addOctaveMarkMidC :: Int -> String
addOctaveMarkMidC oct = addOctaveMark (oct - guitarMiddleC)

addOctaveMark :: Int -> String
addOctaveMark n
    | n > 0     = "'" ++ addOctaveMark (n - 1)
    | n == 0    = ""
    | n < 0     = "," ++ addOctaveMark (n + 1)


{-|
    The `fromFretPos` function returns a NoteGroup
-}
fromFretPos :: Fretboard -> [Int] -> Note
fromFretPos fretboard positions = notegroup
    where
        pos = reverse positions
        bitmap = map (\x -> x >= 0) pos
        selector = \a b -> if a then Just b else Nothing
        validpos = catMaybes $ zipWith selector bitmap pos
        snotes = catMaybes $ zipWith selector bitmap $ stringNotes fretboard
        notes = zipWith (\a i -> a !! i) snotes validpos
        validstr = catMaybes $ zipWith selector bitmap [1..(length positions)]
        fretpitches = map asFretPitch $ zip notes validstr
        notegroup = NoteGroup fretpitches 1

