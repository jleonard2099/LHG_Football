'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Manage Records"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

Dim D$(2), BRC(50), BRC$(50, 2), TRC(50), TRC$(50), TP$(46), TP1$(43)
Dim A$(1 To 15)

Call Comline(N, A$(), 10) ' Get what was typed on the command line
If A$(1) = "1" Then F$ = "RECORDS.PFB"
If A$(1) = "2" Or A$(1) = "3" Then F$ = "RECORDS.CFB"

Data RUSHING ATTEMPTS,RUSHING YARDS,RUSHING TOUCHDOWNS,RUSHING AVERAGE,LONGEST RUN
Data RECEPTIONS,RECEIVING YARDS,RECEIVING TOUCHDOWNS,RECEIVING AVERAGE,LONGEST CATCH
Data PASS ATTEMPTS,PASS COMPLETIONS,PASSING TOUCHDOWNS,HAD INTERCEPTED,PASSING YARDS
Data COMPLETION PCT.,PASSER RATING,LONGEST PASS,TOTAL YARDS,SCORING (NON-KICKER)
Data SCORING (KICKERS),TOTAL SCORING,KICKOFF RETURNS,KICKOFF YARDS,KICKOFF TOUCHDOWNS
Data KICKOFF RETURN AVERAGE,LONGEST KO RETURN,PUNT RETURNS,PUNT RETURN YARDS
Data PUNT RETURN TOUCHDOWNS,PUNT RETURN AVERAGE,LONGEST PUNT RETURN,PUNTS
Data PUNTING YARDS,PUNTING AVERAGE,FIELD GOALS MADE,FIELD GOALS ATTEMPTED
Data FIELD GOAL %,LONGEST FIELD GOAL,POINT AFTER MADE,POINT AFTER ATTEMPTS,PAT%
Data INTERCEPTIONS,INTERCEPTION YARDS,INTERCEPTION TD'S,LONGEST INTERCEPTION
For I = 0 To 45: Read TP$(I): Next

Data YARDS/GAME,TOTAL YARDS,GAIN/PLAY,RUSHING YARDS/GAME,TOTAL RUSHING
Data GAIN/RUSH,PASSING YARDS/GAME,PASSING YARDS,YARDS/ATTEMPT
Data YARDS/COMPLETION,COMPLETION PCT.,INTERCEPTION PCT.,POINTS SCORED
Data POINTS/GAME,MARGIN OF VICTORY,PUNTING AVERAGE,PUNT RETURN AVG
Data KICKOFF RETURN AVG,FIRST DOWNS,FIRST DOWNS/GAME,MOST FUMBLES
Data MOST FUMBLES LOST,FEWEST FUMBLES LOST,PENALTIES/GAME,PENALTY YARDS/GAME
Data YARDS/GAME,YARDS ALLOWED,YARDS/PLAY,MOST YARDS/GAME
Data RUSHING YARDS/GAME,RUSHING YARDS,GAIN/RUSH,PASSING YARDS/GAME
Data PASSING YARDS,YARDS/ATTEMPT,YARDS/COMPLETION,COMPLETION PCT.
Data INTERCEPTION PCT.,POINTS ALLOWED,POINTS/GAME,MOST POINTS ALLOWED
Data MOST PTS ALLOWED/GAME,PUNT RET AVERAGE,KICKOFF RET AVERAGE

For I = 0 To 43: Read TP1$(I): Next

Open "DEV.BAS" For Input As #2
For I = 0 To 2
    Input #2, D$(I)
Next
Close 2

10 If Not _FileExists(D$(1) + F$) Then 5025
Open D$(1) + F$ For Input As #1
For I = 0 To 50
    Input #1, BRC(I)
    Input #1, BRC$(I, 0)
    Input #1, BRC$(I, 1)
    Input #1, BRC$(I, 2)
Next
For I = 0 To 50
    Input #1, TRC(I)
    Input #1, TRC$(I)
Next
Close 1

2300 Color 0, 0: Cls: GoSub 2375
Locate 9, 30: Color 3, 1: Print "SEASON RECORDS MENU": Print
Locate , 15: Color 15: Print "V";: Color 7: Print "IEW RECORDS"
Locate , 15: Color 15: Print "P";: Color 7: Print "RINT RECORDS"
Locate , 15: Color 15: Print "R";: Color 7: Print "ETURN TO MAIN MENU"
2004 I$ = GetKeyPress$
If I$ = Chr$(27) Then 2099
If UCase$(I$) = "V" Then 3115
If UCase$(I$) = "P" Then 3200
If UCase$(I$) = "R" Then 2099
GoTo 2004

2375 J = 6

2382 Locate 8, 8: Color 15, 1: Print " Ö"; String$(61, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(61, 32); "º ";: Next

2385 Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ "

2400 Color , 2: Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
Print "3 IN 1 FOOTBALL LEADERS"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) ROD DAHLBERG/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 2, 0
Return

2099 Cls
Run "HELLO.EXE 3"

3115 GoSub 5000
Color 15: Locate 2, 23: Print "******* INDIVIDUAL RECORDS *******"
Locate , 4: Print "CATEGORY                PLAYER           TEAM            RECORD"
Color 7
For I = 0 To 2
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(3); Tab(28); BRC$(3, 0); Tab(42); BRC$(3, 1); Tab(62); Using "##.#"; BRC(3)
Locate , 4: Print TP$(4); Tab(28); BRC$(4, 0); Tab(42); BRC$(4, 1); Tab(62); Using " ###"; BRC(4);: Print BRC$(4, 2)
For I = 5 To 7
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(8); Tab(28); BRC$(8, 0); Tab(42); BRC$(8, 1); Tab(62); Using "##.#"; BRC(8)
Locate , 4: Print TP$(9); Tab(28); BRC$(9, 0); Tab(42); BRC$(9, 1); Tab(62); Using " ###"; BRC(9);: Print BRC$(9, 2)
For I = 10 To 14
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(15); Tab(28); BRC$(15, 0); Tab(42); BRC$(15, 1); Tab(62); Using "##.#"; BRC(15)
Locate , 4: Print TP$(16); Tab(28); BRC$(16, 0); Tab(42); BRC$(16, 1); Tab(61); Using "###.#"; BRC(16)
Locate , 4: Print TP$(17); Tab(28); BRC$(17, 0); Tab(42); BRC$(17, 1); Tab(62); Using " ###"; BRC(17);: Print BRC$(17, 2)
GoSub 3500
Color 7
Locate 4
For I = 18 To 24
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(25); Tab(28); BRC$(25, 0); Tab(42); BRC$(25, 1); Tab(62); Using "##.#"; BRC(25)
Locate , 4: Print TP$(26); Tab(28); BRC$(26, 0); Tab(42); BRC$(26, 1); Tab(62); Using " ###"; BRC(26);: Print BRC$(26, 2)
For I = 27 To 29
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(30); Tab(28); BRC$(30, 0); Tab(42); BRC$(30, 1); Tab(62); Using "##.#"; BRC(30)
Locate , 4: Print TP$(31); Tab(28); BRC$(31, 0); Tab(42); BRC$(31, 1); Tab(62); Using " ###"; BRC(31);: Print BRC$(31, 2)
For I = 32 To 33
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(34); Tab(28); BRC$(34, 0); Tab(42); BRC$(34, 1); Tab(62); Using "##.#"; BRC(34)
GoSub 3500
Color 7
Locate 4
For I = 35 To 36
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(37); Tab(28); BRC$(37, 0); Tab(42); BRC$(37, 1); Tab(61); Using "###.#"; BRC(37)
Locate , 4: Print TP$(38); Tab(28); BRC$(38, 0); Tab(42); BRC$(38, 1); Tab(62); Using " ###"; BRC(38);: Print BRC$(38, 2)
For I = 39 To 40
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(41); Tab(28); BRC$(41, 0); Tab(42); BRC$(41, 1); Tab(61); Using "###.#"; BRC(41)
For I = 42 To 44
    Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
Locate , 4: Print TP$(45); Tab(28); BRC$(45, 0); Tab(42); BRC$(45, 1); Tab(62); Using " ###"; BRC(45);: Print BRC$(45, 2)
GoSub 3500
GoSub 5100
Color 15: Locate 2, 23: Print "******* OFFENSIVE TEAM RECORDS *******"
Locate , 4: Print "CATEGORY                   TEAM                  RECORD            "
Color 7
Locate , 4: Print TP1$(0); Tab(28); TRC$(0); Tab(53); Using "###.#"; TRC(0)
Locate , 4: Print TP1$(1); Tab(28); TRC$(1); Tab(53); Using "#####"; TRC(1)
For I = 2 To 3
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
Locate , 4: Print TP1$(4); Tab(28); TRC$(4); Tab(53); Using "#####"; TRC(4)
For I = 5 To 6
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
Locate , 4: Print TP1$(7); Tab(28); TRC$(7); Tab(53); Using "#####"; TRC(7)
For I = 8 To 9
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
For I = 10 To 11
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I) * 100
Next I
Locate , 4: Print TP1$(12); Tab(28); TRC$(12); Tab(53); Using "#####"; TRC(12)
For I = 13 To 14
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
GoSub 3500
Color 7
Locate 4
For I = 15 To 17
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
Locate , 4: Print TP1$(18); Tab(28); TRC$(18); Tab(53); Using "#####"; TRC(18)
Locate , 4: Print TP1$(19); Tab(28); TRC$(19); Tab(53); Using "###.#"; TRC(19)
For I = 20 To 22
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "#####"; TRC(I)
Next I
Locate , 4: Print TP1$(23); Tab(28); TRC$(23); Tab(53); Using "###.#"; TRC(23)
Locate , 4: Print TP1$(24); Tab(28); TRC$(24); Tab(53); Using "###.#"; TRC(24)
GoSub 3500
Color 15: Locate 2, 23: Print "******* DEFENSIVE TEAM RECORDS *******"
Locate , 4: Print "CATEGORY                   TEAM                  RECORD            "
Color 7
Locate , 4: Print TP1$(25); Tab(28); TRC$(25); Tab(53); Using "###.#"; TRC(25)
Locate , 4: Print TP1$(26); Tab(28); TRC$(26); Tab(53); Using "#####"; TRC(26)
For I = 27 To 29
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
Locate , 4: Print TP1$(30); Tab(28); TRC$(30); Tab(53); Using "#####"; TRC(30)
For I = 31 To 32
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
Locate , 4: Print TP1$(33); Tab(28); TRC$(33); Tab(53); Using "#####"; TRC(33)
For I = 34 To 35
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
For I = 36 To 37
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I) * 100
Next
GoSub 3500
Color 7
Locate 4
Locate , 4: Print TP1$(38); Tab(28); TRC$(38); Tab(53); Using "#####"; TRC(38)
Locate , 4: Print TP1$(39); Tab(28); TRC$(39); Tab(53); Using "###.#"; TRC(39)
Locate , 4: Print TP1$(40); Tab(28); TRC$(40); Tab(53); Using "#####"; TRC(40)
Locate , 4: Print TP1$(41); Tab(28); TRC$(41); Tab(53); Using "###.#"; TRC(41)
For I = 42 To 43
    Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
GoSub 3500
GoTo 2300

3200 ' PRINT RECORDS
LPrint Tab(23); "******* INDIVIDUAL RECORDS *******"
LPrint
LPrint "CATEGORY                   PLAYER           TEAM           RECORD"
For I = 0 To 2
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(3); Tab(28); BRC$(3, 0); Tab(42); BRC$(3, 1); Tab(62); Using "##.#"; BRC(3)
LPrint TP$(4); Tab(28); BRC$(4, 0); Tab(42); BRC$(4, 1); Tab(62); Using " ###"; BRC(4);: LPrint BRC$(4, 2)
For I = 5 To 7
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(8); Tab(28); BRC$(8, 0); Tab(42); BRC$(8, 1); Tab(62); Using "##.#"; BRC(8)
LPrint TP$(9); Tab(28); BRC$(9, 0); Tab(42); BRC$(9, 1); Tab(62); Using " ###"; BRC(9);: LPrint BRC$(9, 2)
For I = 10 To 14
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(15); Tab(28); BRC$(15, 0); Tab(42); BRC$(15, 1); Tab(62); Using "##.#"; BRC(15)
LPrint TP$(16); Tab(28); BRC$(16, 0); Tab(42); BRC$(16, 1); Tab(61); Using "###.#"; BRC(16)
LPrint TP$(17); Tab(28); BRC$(17, 0); Tab(42); BRC$(17, 1); Tab(62); Using " ###"; BRC(17);: LPrint BRC$(17, 2)
For I = 18 To 24
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(25); Tab(28); BRC$(25, 0); Tab(42); BRC$(25, 1); Tab(62); Using "##.#"; BRC(25)
LPrint TP$(26); Tab(28); BRC$(26, 0); Tab(42); BRC$(26, 1); Tab(62); Using " ###"; BRC(26);: LPrint BRC$(26, 2)
For I = 27 To 29
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(30); Tab(28); BRC$(30, 0); Tab(42); BRC$(30, 1); Tab(62); Using "##.#"; BRC(30)
LPrint TP$(31); Tab(28); BRC$(31, 0); Tab(42); BRC$(31, 1); Tab(62); Using " ###"; BRC(31);: LPrint BRC$(31, 2)
For I = 32 To 33
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(34); Tab(28); BRC$(34, 0); Tab(42); BRC$(34, 1); Tab(62); Using "##.#"; BRC(34)
For I = 35 To 36
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(37); Tab(28); BRC$(37, 0); Tab(42); BRC$(37, 1); Tab(61); Using "###.#"; BRC(37)
LPrint TP$(38); Tab(28); BRC$(38, 0); Tab(42); BRC$(38, 1); Tab(62); Using " ###"; BRC(38);: LPrint BRC$(38, 2)
For I = 39 To 40
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(41); Tab(28); BRC$(41, 0); Tab(42); BRC$(41, 1); Tab(61); Using "###.#"; BRC(41)
For I = 42 To 44
    LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC(I)
Next
LPrint TP$(45); Tab(28); BRC$(45, 0); Tab(42); BRC$(45, 1); Tab(62); Using " ###"; BRC(45);: LPrint BRC$(45, 2)
LPrint Chr$(12)
LPrint Tab(23); "******* OFFENSIVE TEAM RECORDS *******"
LPrint
LPrint "CATEGORY                      TEAM                 RECORD            "
LPrint TP1$(0); Tab(28); TRC$(0); Tab(53); Using "###.#"; TRC(0)
LPrint TP1$(1); Tab(28); TRC$(1); Tab(53); Using "#####"; TRC(1)
For I = 2 To 3
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
LPrint TP1$(4); Tab(28); TRC$(4); Tab(53); Using "#####"; TRC(4)
For I = 5 To 6
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
LPrint TP1$(7); Tab(28); TRC$(7); Tab(53); Using "#####"; TRC(7)
For I = 8 To 9
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
For I = 10 To 11
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I) * 100
Next I
LPrint TP1$(12); Tab(28); TRC$(12); Tab(53); Using "#####"; TRC(12)
For I = 13 To 14
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
For I = 15 To 17
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next I
LPrint TP1$(18); Tab(28); TRC$(18); Tab(53); Using "#####"; TRC(18)
LPrint TP1$(19); Tab(28); TRC$(19); Tab(53); Using "###.#"; TRC(19)
For I = 20 To 22
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "#####"; TRC(I)
Next I
LPrint TP1$(23); Tab(28); TRC$(23); Tab(53); Using "###.#"; TRC(23)
LPrint TP1$(24); Tab(28); TRC$(24); Tab(53); Using "###.#"; TRC(24)
LPrint
LPrint
LPrint Tab(23); "******* DEFENSIVE TEAM RECORDS *******"
LPrint
LPrint "CATEGORY                      TEAM                 RECORD            "
LPrint TP1$(25); Tab(28); TRC$(25); Tab(53); Using "###.#"; TRC(25)
LPrint TP1$(26); Tab(28); TRC$(26); Tab(53); Using "#####"; TRC(26)
For I = 27 To 29
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
LPrint TP1$(30); Tab(28); TRC$(30); Tab(53); Using "#####"; TRC(30)
For I = 31 To 32
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
LPrint TP1$(33); Tab(28); TRC$(33); Tab(53); Using "#####"; TRC(33)
For I = 34 To 35
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
For I = 36 To 37
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I) * 100
Next
LPrint TP1$(38); Tab(28); TRC$(38); Tab(53); Using "#####"; TRC(38)
LPrint TP1$(39); Tab(28); TRC$(39); Tab(53); Using "###.#"; TRC(39)
LPrint TP1$(40); Tab(28); TRC$(40); Tab(53); Using "#####"; TRC(40)
LPrint TP1$(41); Tab(28); TRC$(41); Tab(53); Using "###.#"; TRC(41)
For I = 42 To 43
    LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC(I)
Next
LPrint Chr$(12)
GoTo 2300

3500 Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
3501 I$ = GetKeyPress$
For I = 4 To 21: Locate I, 4
    Print "                                                                        "
Next

3502 Return

5000 Color 0, 0: Cls: J = 20

5001 Locate 1, 1: Color 15, 4: Print " Ö"; String$(73, 196); "· ";
For II = 1 To J: Color 15, 4: Locate 1 + II, 1: Print " º"; String$(73, 32); "º ";: Color 7, 4: Print String$(2, 177): Next

5002 Color 15, 4: Locate 2 + J, 1: Print " Ó"; String$(73, 196); "½ ";: Color 7, 4: Print String$(2, 177)
Locate , 3: Print String$(77, 177)

5003 Return

5100 Color 0, 0: Cls: J = 20

5101 Locate 1, 1: Color 15, 1: Print " Ö"; String$(73, 196); "· ";
For II = 1 To J: Color 15, 1: Locate 1 + II, 1: Print " º"; String$(73, 32); "º ";: Color 7, 1: Print String$(2, 177): Next

5102 Color 15, 1: Locate 2 + J, 1: Print " Ó"; String$(73, 196); "½ ";: Color 7, 1: Print String$(2, 177)
Locate , 3: Print String$(77, 177)

5103 Return

5025 GoSub 2375
Locate 9, 30: Print "ERROR::: UNABLE TO LOCATE RECORDS"
I$ = GetKeyPress
GoTo 2099

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
