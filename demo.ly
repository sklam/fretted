voice =  { 
< d\4 fis'\2 > 4
< b'\1 fis'\2 dis'\3 b\4 fis\5 b,\6 > 2
< e'\1 b\2 gis\3 e\4 b,\5 e,\6 > 4
< d\5 fis'\1 e'\2 > 1
< c'\3 dis'\2 > 4
< d'\2 d'\3 > 4
< e'\2 c'\3 > 4
< f'\1 ais\3 > 4
< g'\3 a\5 > 4
< a'\1 g\4 > 4
< b'\1 f\5 > 4
< c''\1 dis\5 > 4

 } 

\score  { 
 << 
\new Staff  { 
\set Staff.midiInstrument = #"acoustic guitar (nylon)"
\clef "treble_8"
\voice

 } 

\new TabStaff  { 
\set Staff.midiInstrument = #"acoustic guitar (nylon)"
\voice

 } 


 >> 

\layout { 

 } 

\midi { 
\tempo 4 = 120
 } 


 } 

\version "2.18.0"

