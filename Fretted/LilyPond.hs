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
        tradStaff = unwords [(command "new"), "Staff", (curly clef)]
        clef = unwords [(command "clef"), (quote "treble_8"), (command "voice")]
        tabStaff = unwords [(command "new"), "TabStaff", (curly (command "voice"))]
        layout = concat [(command "layout"), (curly "")]
        midi = concat [(command "midi"), (curly tempoSection)]
        tempoSection = unwords [(command "tempo"), (assign "4" (show tempo))]
        versionSection = unwords [(command "version"), (quote version)]

