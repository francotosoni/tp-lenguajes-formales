10 INPUT "WHAT IS YOUR NAME"; U$
20 PRINT "HELLO "; U$
30 INPUT "HOW MANY STARS DO YOU WANT"; N
40 IF N < 1 THEN GOTO 90
50 S$ = "" : FOR I = 1 TO N
60 S$ = S$ + "*"
70 NEXT I
80 PRINT S$
90 INPUT "DO YOU WANT MORE STARS [Y/N]"; A$
100 IF A$ = "N" THEN 130
110 IF A$ = "Y" GOTO 30
120 GOTO 90
130 PRINT "GOODBYE "; U$
140 END
