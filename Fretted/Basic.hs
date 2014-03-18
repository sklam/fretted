module Fretted.Basic where
import Data.Maybe
import Data.List

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
    | Rest
        { restDuration   :: Int
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


semiUp = sharp
semiDown = flat

wholeUp = sharp.sharp
wholeDown = flat.flat


setDuration :: Note -> Int -> Note
setDuration note dur =
    case note of
        NoteGroup pitch duration -> NoteGroup pitch dur
        Rest duration -> Rest dur

getDuration :: Note -> Int
getDuration note = case note of
                        NoteGroup _ dur ->  dur
                        Rest dur -> dur

whole :: Note -> Note
whole note = setDuration note 1

half :: Note -> Note
half note = setDuration note 2

quarter :: Note -> Note
quarter note = setDuration note 4

compress :: Int -> Note -> Note
compress factor note = setDuration note $ (getDuration note) * factor


standardGuitarFretboard :: Fretboard
standardGuitarFretboard = Fretboard "EADGBE" strings
    where
        strings = [s1, s2, s3, s4, s5, s6]
        n = 16
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
    guitar middle C, which is an octave below the actual middle C.
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
    The `fromFretPos` function returns a NoteGroup given the `positions` on the
    `fretboard`.
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


limitFret :: Fretboard -> (Int, Int) -> Fretboard
limitFret frets (lo, hi) = Fretboard (tuning frets) limfrets
    where
        limitString ps = take (hi + 1) $ drop lo $ ps
        limfrets = map limitString (stringNotes frets)


autoFingering :: Fretboard -> Bool -> [Pitch] -> Note
autoFingering fretboard useOpen pitches  = makeNoteGroup $ zip pitches strings
    where
        bestFn = sortFingeringByDist.sortFingeringByMinFret
        candids = allFingering fretboard pitches
        legalized = filter isLegalFingering candids
        filterOpen = filter (not.useOpenString)
        presort = if useOpen then legalized else filterOpen legalized
        sorted = bestFn $ presort
        optimal = head $ sorted          -- XXX error?
        (strings, _) = unzip optimal


useOpenString :: [(Int, Int)] -> Bool
useOpenString fingering = uses
    where
        (_, frets) = unzip fingering
        uses = isJust $ elemIndex 0 frets


{-|
    Check:
     - overlapping
-}
isLegalFingering :: [(Int, Int)] -> Bool
isLegalFingering fingering = not overlapped
    where
        (strings, _) = unzip fingering
        dedup = nub strings
        overlapped = (length dedup) /= (length strings)

{-|
    Returns a list of fingerings represented as list of (string, fret).
-}
allFingering :: Fretboard -> [Pitch] -> [[(Int, Int)]]
allFingering fretboard pitches = sequence candids
    where
        {-| List of tuples of (string, fret) -}
        listCandid = \x -> map (\y -> (string y, (posOnString fretboard y)))
                                $ findAllFret fretboard x
        candids = map listCandid pitches

{-|
    Sort by minimum fret position (non-open)
-}
sortFingeringByMinFret :: [[(Int, Int)]] -> [[(Int, Int)]]
sortFingeringByMinFret fl = sortByRank minNonZero fl
    where
        nonzeros fretpos = (filter (0 /=) $ (\(_,b)-> b) $ unzip fretpos)
        minNonZero fretpos = minimum $ nonzeros fretpos

{-|
    Sort by fingering distance
-}
sortFingeringByDist :: [[(Int, Int)]] -> [[(Int, Int)]]
sortFingeringByDist fl = sortByRank fingerDist fl


sortByRank :: ([(Int, Int)] -> Int) -> [[(Int, Int)]] -> [[(Int, Int)]]
sortByRank rankFn fl = (\(a, b) -> b) $ unzip sorted
    where
        ranks = map rankFn fl
        ranked = zipWith (\a b -> (a,b)) ranks fl
        sorter = \(a, _) (b, _) -> compare a b
        sorted = sortBy sorter ranked


fingerDist :: [(Int, Int)] -> Int
fingerDist fretpos = dist
    where
        frets = (\(_,b)-> b) $ unzip fretpos
        nonzeros = filter (0 /=) frets
        dist = (maximum nonzeros) - (minimum nonzeros)


{-|
    Find all occurances of a pitch in the fretboard.
-}
findAllFret :: Fretboard -> Pitch -> [FretPitch]
findAllFret fretboard pitch = fretteds
    where
        fretteds = map (\(i,strnotes) -> FretPitch pitch i) valids
        valids = filter sel indexed
        strings = stringNotes fretboard
        strnum = [1..(length strings)]
        indexed = zip strnum strings
        sel = \(i,strnotes) -> isOnString strnotes pitch


isOnString :: [Pitch] -> Pitch -> Bool
isOnString stringNotes target = result
    where
        matches = filter (target ==) stringNotes
        result = (length matches) /= 0


posOnString :: Fretboard -> FretPitch -> Int
posOnString fret target = pos
    where
        maybepos = elemIndex (pitch target) str
        str = (stringNotes fret) !! ((string target) - 1)
        pos = case maybepos of
                Just a -> a
                Nothing -> error "posOnString not found"


applySteps :: [Pitch->Pitch] -> Pitch -> [Pitch]
applySteps steps root =
    case steps of
        (x:xs) -> [root] ++ (applySteps xs (x root))
        []     -> [root]


ionianModeStep = [wholeUp, wholeUp, semiUp, wholeUp, wholeUp, wholeUp, semiUp]


generateScale :: Pitch -> Int -> Int -> [Pitch]
generateScale root n mode = scale
    where
        steps = take n $ drop (mode - 1) $ cycle ionianModeStep
        scale = applySteps steps root


ionian :: Pitch -> [Pitch]
ionian root = generateScale root 7 1


dorian :: Pitch -> [Pitch]
dorian root = generateScale root 7 2


phrygian :: Pitch -> [Pitch]
phrygian root = generateScale root 7 3


lydian :: Pitch -> [Pitch]
lydian root = generateScale root 7 4


mixolydian :: Pitch -> [Pitch]
mixolydian root = generateScale root 7 5


aeolian :: Pitch -> [Pitch]
aeolian root = generateScale root 7 6


locrian :: Pitch -> [Pitch]
locrian root = generateScale root 7 7
