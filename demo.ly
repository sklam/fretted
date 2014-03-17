voice =  { 
< d\4 fis'\2 > 1
< b'\1 fis'\2 dis'\3 b\4 fis\5 b,\6 > 1
< e'\1 b\2 gis\3 e\4 b,\5 e,\6 > 1

 } 

\score  { 
 << 
\new Staff  { 
\clef "treble_8" \voice
 } 

\new TabStaff  { 
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

