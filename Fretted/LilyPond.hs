module Fretted.LilyPond where

version :: String
version = "2.18.0"

enclose :: String -> String -> String -> String
enclose open close body = unlines [open, body, close]

quote :: String -> String
quote body = "\"" ++ body ++ "\""

curly :: String -> String
curly body = enclose " { " " } " body

arrow :: String -> String
arrow body = enclose " << " " >> " body

command :: String -> String
command cmd = "\\" ++ cmd

assign :: String -> String -> String
assign lhs rhs = lhs ++ " = " ++ rhs

singleVoiceFile :: String -> Int -> String
singleVoiceFile voice tempo = unlines [voiceSection, scoreSection, versionSection]
    where
        voiceSection = assign "voice" $ curly voice
        scoreSection = unwords [(command "score"), (curly scoreBody)]
        scoreBody = unlines [staffs, layout, midi]
        staffs = arrow $ unlines [tradStaff, tabStaff]
        tradStaff = unwords [(command "new"), "Staff", (curly staffBody)]
        staffBody = unlines [instrument, clef, (command "voice")]
        instrument = unwords [(command "set"),
                              (assign "Staff.midiInstrument"
                                      "#\"acoustic guitar (nylon)\"")]
        clef = unwords [(command "clef"), (quote "treble_8")]
        tabStaff = unwords [(command "new"), "TabStaff", (curly tabBody)]
        tabBody = unlines [instrument, (command "voice")]
        layout = concat [(command "layout"), (curly "")]
        midi = concat [(command "midi"), (curly tempoSection)]
        tempoSection = unwords [(command "tempo"), (assign "4" (show tempo))]
        versionSection = unwords [(command "version"), (quote version)]

