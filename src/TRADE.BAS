DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
DECLARE FUNCTION EXIST% (F$)
DECLARE FUNCTION GetDrive% ()
DECLARE SUB TITLE ()
DIM A(2, 7), A1(2, 7), A2%(2, 15), A3(2, 7), A4(2, 6), A5(2, 6), A6(2, 3), A7(2, 3), A8(2, 3), A9(2, 3), B(2, 3), B1(2, 3), B2(2, 3), B3(2, 3), B6(2, 7), Q6%(2, 1)
DIM K(2, 12), K1(20, 5), K2(2, 10, 17), V4%(2, 2), C2(15), GS%(1, 14, 1), LC(2, 15)
DIM T1(1), K3(2, 6), R9%(1), Q$(300), RB(2, 11, 11), WR(2, 11, 11), QB(2, 11, 11), KR(2, 11, 11), PR(2, 11, 11)
DIM A$(1), A1$(2, 9), A2$(2, 6), A3$(2, 4), A4$(2, 3), A5$(2, 3), A6$(2, 3), VV$(500), V%(500)
DIM TT(2, 15), OT(2, 15), DS$(1, 14), DS%(1, 14), Z2(1, 13, 17), Z(1, 38), Z1(1, 38)
DIM D$(1), YN$(1), NY$(1), item$(4), SA$(1), TN$(1), RB$(1, 9), WR$(1, 5)
DIM QB$(1, 3), KR$(1, 2), PR$(1, 2), PU$(1), FG$(1, 1), DI$(1, 9), LR(1, 9)
DIM LR$(1, 9, 1), LC$(1, 15, 1), LP$(1, 3, 1), F$(1), GI%(1, 9, 2)
DIM LK(1, 2), LK$(1, 2, 1), PK(1, 2), PK$(1, 2, 1), AF(1, 1, 4), AM(1, 1, 4)
DIM LI(1, 9), LI$(1, 9, 1), PA%(1, 4), YC%(1), LP(1, 3)
DIM LF(1, 1), LF$(1, 1), A0%(1, 9), A1%(1, 9), A3%(1, 9), B6%(1, 7), AA$(1), T(1), YC(1)
DIM A4%(1, 5), A5%(1, 5), A6%(1, 3), A7%(1, 3), A8%(1, 3), QX%(1, 3)
DIM A9%(1, 2), B%(1, 2), B1%(1, 2), B2%(1, 2), B3%(1, 3), DI%(1, 9)
DIM B4%(1, 1), B5%(1, 1), B7%(1, 1), B8%(1, 1), A7$(1, 1), S(1)
OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): INPUT #1, D$(1): CLOSE #1

99 D% = GetDrive%
   CALL TITLE
   item$(1) = "TRADE PLAYERS            "
   item$(2) = "RETURN TO 3 & 1 MAIN MENU"
     SELECTION% = 1
     Count% = 2
     Start% = 1
     SCANCODE% = 2
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 10, 50, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 OR SELECTION% = 2 THEN 1
     IF SELECTION% = 1 THEN 2110

1   IF D% >= 67 THEN RUN "HELLO.EXE 2"
    CLS : PRINT "MAKE SURE PROGRAM DISK IS IN DRIVE"
15 I$ = INKEY$: IF I$ = "" THEN 15
   RUN "HELLO.EXE 2"


100 FOR XX = 4 TO 11: IF MID$(SA$(J), XX, 1) = " " OR MID$(SA$(J), XX, 1) = "/" OR MID$(SA$(J), XX, 1) = "." THEN MID$(SA$(J), XX, 1) = "_"
    NEXT XX
    FOR XX = 12 TO 14: IF MID$(SA$(J), XX, 2) = "ST" THEN MID$(SA$(J), 9, 1) = "_": MID$(SA$(J), 10, 2) = "ST"
    NEXT XX
    QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(SA$(J), XX, 1) = "_" THEN QQ = XX - 4: GOTO 106
    GOTO 107
106 NEXT XX
107 YR$ = LEFT$(SA$(J), 2): F$(J) = MID$(SA$(J), 4, QQ) + "." + YR$
    IF NOT EXIST%(D$(1) + F$(J)) THEN S(J) = 1: GOTO 108
    OPEN D$(1) + F$(J) FOR INPUT AS #1
    INPUT #1, TN$(J)
    FOR I = 0 TO 9: INPUT #1, RB$(J, I): NEXT
    FOR I = 0 TO 5: INPUT #1, WR$(J, I): NEXT
    FOR I = 0 TO 3: INPUT #1, QB$(J, I): NEXT
    FOR I = 0 TO 2: INPUT #1, KR$(J, I): NEXT
    FOR I = 0 TO 2: INPUT #1, PR$(J, I): NEXT
    INPUT #1, PU$(J)
    FOR I = 0 TO 1: INPUT #1, FG$(J, I): NEXT
    FOR X = 0 TO 13
    FOR Y = 0 TO 17: INPUT #1, Z2(J, X, Y)
    NEXT: NEXT
    FOR X = 0 TO 38: INPUT #1, Z(J, X): NEXT
    FOR X = 0 TO 38: INPUT #1, Z1(J, X): NEXT
    FOR X = 0 TO 9: INPUT #1, DI$(J, X): FOR J1 = 0 TO 2: INPUT #1, GI%(J, X, J1): NEXT: NEXT
    FOR X = 0 TO 14: INPUT #1, DS$(J, X): FOR J1 = 0 TO 1: INPUT #1, GS%(J, X, J1): NEXT: NEXT
    FOR I = 0 TO 9: INPUT #1, LR(J, I): FOR J1 = 0 TO 1: INPUT #1, LR$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 15: INPUT #1, LC(J, I): FOR J1 = 0 TO 1: INPUT #1, LC$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 3: INPUT #1, LP(J, I): FOR J1 = 0 TO 1: INPUT #1, LP$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 2: INPUT #1, LK(J, I): FOR J1 = 0 TO 1: INPUT #1, LK$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 2: INPUT #1, PK(J, I): FOR J1 = 0 TO 1: INPUT #1, PK$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 1: INPUT #1, LF(J, I): INPUT #1, LF$(J, I): FOR I1 = 0 TO 4: INPUT #1, AM(J, I, I1): INPUT #1, AF(J, I, I1): NEXT: NEXT
    FOR I = 0 TO 9: INPUT #1, LI(J, I): INPUT #1, LI$(J, I, 0): INPUT #1, LI$(J, I, 1): NEXT
    CLOSE #1
108 RETURN

330 LOCATE 12, 1
    PRINT "SAVING "; : COLOR PA%(J, 2), PA%(J, 1)
    PRINT A$(J);
    COLOR 7, 0: PRINT " STATS": Z = 0

    OPEN D$(1) + F$(J) FOR OUTPUT AS #1
    PRINT #1, TN$(J)
    FOR I = 0 TO 9: PRINT #1, RB$(J, I): NEXT
    FOR I = 0 TO 5: PRINT #1, WR$(J, I): NEXT
    FOR I = 0 TO 3: PRINT #1, QB$(J, I): NEXT
    FOR I = 0 TO 2: PRINT #1, KR$(J, I): NEXT
    FOR I = 0 TO 2: PRINT #1, PR$(J, I): NEXT
    PRINT #1, PU$(J)
    FOR I = 0 TO 1: PRINT #1, FG$(J, I): NEXT
    FOR X = 0 TO 13
    FOR Y = 0 TO 17: PRINT #1, Z2(J, X, Y)
    NEXT: NEXT
    FOR X = 0 TO 38: PRINT #1, Z(J, X): NEXT
    FOR X = 0 TO 38: PRINT #1, Z1(J, X): NEXT
    FOR X = 0 TO 9: PRINT #1, DI$(J, X): FOR J1 = 0 TO 2: PRINT #1, GI%(J, X, J1): NEXT: NEXT
    FOR X = 0 TO 14: PRINT #1, DS$(J, X): FOR J1 = 0 TO 1: PRINT #1, GS%(J, X, J1): NEXT: NEXT
    FOR I = 0 TO 9: PRINT #1, LR(J, I): FOR J1 = 0 TO 1: PRINT #1, LR$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 15: PRINT #1, LC(J, I): FOR J1 = 0 TO 1: PRINT #1, LC$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 3: PRINT #1, LP(J, I): FOR J1 = 0 TO 1: PRINT #1, LP$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 2: PRINT #1, LK(J, I): FOR J1 = 0 TO 1: PRINT #1, LK$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 2: PRINT #1, PK(J, I): FOR J1 = 0 TO 1: PRINT #1, PK$(J, I, J1): NEXT: NEXT
    FOR I = 0 TO 1: PRINT #1, LF(J, I): PRINT #1, LF$(J, I): FOR I1 = 0 TO 4: PRINT #1, AM(J, I, I1): PRINT #1, AF(J, I, I1): NEXT: NEXT
    FOR I = 0 TO 9: PRINT #1, LI(J, I): PRINT #1, LI$(J, I, 0): PRINT #1, LI$(J, I, 1): NEXT
    CLOSE #1
    RETURN

1000 FOR I = 0 TO 9
        A0%(J, I) = 0: A1%(J, I) = 0: A2%(J, I) = 0: A3%(J, I) = 0: A1$(J, I) = ""
    NEXT
    FOR I = 0 TO 5
        A4%(J, I) = 0: A5%(J, I) = 0: A2$(J, I) = ""
    NEXT
    FOR I = 0 TO 3
        A6%(J, I) = 0: A7%(J, I) = 0: A8%(J, I) = 0: QX%(J, I) = 0: A3$(J, I) = ""
        NEXT
        FOR I = 0 TO 2
        A9%(J, I) = 0: B%(J, I) = 0: B1%(J, I) = 0: B2%(J, I) = 0: A4$(J, I) = "": A5$(J, I) = ""
        NEXT
        A6$(J, 0) = "": B3%(J, I) = 0
        FOR I = 0 TO 1
        B4%(J, I) = 0: B5%(J, I) = 0: B7%(J, I) = 0: B8%(J, I) = 0: A7$(J, I) = ""
        NEXT
        RETURN

1100 NT = 0
     OPEN D$(0) + "FOOT." + YN$(J) FOR RANDOM AS #1 LEN = 934
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0), 640 AS X$
     FOR I1 = 1 TO Y / 934
     GET #1, I1
     FOR I2 = 1 TO 15
     IF ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1105
     NEXT I2
     NT = NT + 1
     VV$(NT) = Q$(0): V%(NT) = I1
1105 NEXT I1
    CLOSE 1
TS% = INT((NT - 1) / 80)
1102 SELECTION% = 1
     Count% = NT
     Start% = 1
     FileColr% = 79
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
     COLOR , 4: CLS : LOCATE , 33: COLOR 14
     PRINT "TEAM SELECTION": COLOR 15
     LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
     PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
     PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
     CALL ShowCursor
20   MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     CALL HideCursor
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
     IF EXITCODE% = -79 THEN Start% = NT: EXITCODE% = 3: SELECTION% = NT: GOTO 20
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NT THEN Start% = NT
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 20
     T(J) = V%(SELECTION%)
     GOTO 9500

2110 GOSUB 9900
2111 J = 0
    COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID FOR TEAM A: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE , 12: PRINT
    LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
    LOCATE 7, 38: INPUT NY$(J)
    IF NY$(J) = "" THEN GOSUB 1102: GOTO 2112
    YN$(J) = NY$(J)
    FOR I = 1 TO 500: VV$(I) = "": NEXT
    GOSUB 1100
2112 IF BO% = 1 THEN BO% = 0: GOTO 2111
2113 LOCATE 2, 33: COLOR PA%(J, 2), PA%(J, 1): PRINT A$(J): COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 2113
    IF UCASE$(I$) = "Y" THEN 2111
    COLOR 15, 0
    GOSUB 9900
    LOCATE 3: PRINT "DOES THIS TEAM HAVE COMPILED STATS (YN)": S(0) = 0
2136 I$ = INKEY$: IF I$ = "" THEN 2136
     IF UCASE$(I$) = "N" THEN S(0) = 1: GOTO 2150
     IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 2145
     GOSUB 9900: LOCATE 3: PRINT "INSERT STATS DISK IN DRIVE "; D$(1); " FOR ": COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0
     GOSUB 4091
2145 GOSUB 100
2150 J = 1
    COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID FOR TEAM B: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE , 12: PRINT
    LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
    LOCATE 7, 38: INPUT NY$(J)
    IF NY$(J) = "" THEN GOSUB 1102: GOTO 2152
    YN$(J) = NY$(J)
    FOR I = 1 TO 500: VV$(I) = "": NEXT
    GOSUB 1100
2152 IF BO% = 1 THEN BO% = 0: GOTO 2150
2153 LOCATE 2, 33: COLOR PA%(J, 2), PA%(J, 1): PRINT A$(J): COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 2153
    IF UCASE$(I$) = "Y" THEN 2150
    COLOR 15, 0
     GOSUB 9900: LOCATE 3: PRINT "DOES THIS TEAM HAVE COMPILED STATS (YN)": S(1) = 0
2186 I$ = INKEY$: IF I$ = "" THEN 2186
     IF UCASE$(I$) = "N" THEN S(1) = 1: GOTO 2200
     GOSUB 9900: LOCATE 3: PRINT "INSERT STATS DISK IN DRIVE "; D$(1); " FOR ": COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 2195
     GOSUB 4091
2195 GOSUB 100: J = 1
2200 GOSUB 9900: LOCATE 5
     LOCATE , 5: COLOR 3: PRINT "TRADE "; : COLOR 15: PRINT "R"; : COLOR 3: PRINT "UNNING BACKS"
     LOCATE , 5: COLOR 3: PRINT "TRADE "; : COLOR 15: PRINT "W"; : COLOR 3: PRINT "IDE RECEIVERS"
     LOCATE , 5: COLOR 3: PRINT "TRADE "; : COLOR 15: PRINT "Q"; : COLOR 3: PRINT "UARTERBACKS"
     LOCATE , 5: COLOR 3: PRINT "TRADE "; : COLOR 15: PRINT "K"; : COLOR 3: PRINT "ICKOFF RETURN"
     LOCATE , 5: COLOR 3: PRINT "TRADE "; : COLOR 15: PRINT "P"; : COLOR 3: PRINT "UNT RETURN"
     LOCATE , 5: COLOR 3: PRINT "TRADE P"; : COLOR 15: PRINT "U"; : COLOR 3: PRINT "NTERS"
     LOCATE , 5: COLOR 3: PRINT "TRADE PLACE K"; : COLOR 15: PRINT "I"; : COLOR 3: PRINT "CKERS"
     LOCATE , 5: COLOR 15: PRINT "E"; : COLOR 3: PRINT "ND AND SAVE TRADES"
2202 I$ = INKEY$: IF I$ = "" THEN 2202
     IF UCASE$(I$) = "E" THEN 2500
     IF UCASE$(I$) = "R" THEN GOSUB 2216: GOTO 2222
     IF UCASE$(I$) = "W" THEN GOSUB 2216: GOTO 2256
     IF UCASE$(I$) = "Q" THEN GOSUB 2216: GOTO 2290
     IF UCASE$(I$) = "K" THEN GOSUB 2216: GOTO 2324
     IF UCASE$(I$) = "P" THEN GOSUB 2216: GOTO 2358
     IF UCASE$(I$) = "U" THEN GOSUB 2216: GOTO 2392
     IF UCASE$(I$) = "I" THEN GOSUB 2216: GOTO 2426
     GOTO 2202
     IF S(0) = 1 OR S(1) = 1 THEN 2221
2216 GOSUB 9900
      LOCATE 5, 5
      COLOR 15: PRINT "T"; : COLOR 3: PRINT "RADE PLAYERS' REPLAY STATS"
      LOCATE , 5: COLOR 15: PRINT "Z"; : COLOR 3: PRINT "ERO PLAYERS' STATS"
2217 I$ = INKEY$: IF I$ = "" THEN 2217
     IF UCASE$(I$) <> "T" AND UCASE$(I$) <> "Z" THEN 2216
     IF UCASE$(I$) = "T" THEN W1 = 1
     IF UCASE$(I$) = "Z" THEN W1 = 0
2221 RETURN
2222 GOSUB 9900: : LOCATE 4, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0: LOCATE 4, 20: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     FOR I = 0 TO 9: LOCATE I + 5, 1: PRINT I; " "; A1$(0, I): LOCATE I + 5, 20: PRINT I; " "; A1$(1, I)
     NEXT I
2225 LOCATE 17, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I = VAL(I$): IF I < 0 OR I > 9 THEN 2225
     PRINT A1$(0, I)
2228 LOCATE 19, 1: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I1 = VAL(I$): IF I1 < 0 OR I1 > 9 THEN 2228
     PRINT A1$(1, I1)
     LOCATE 23, 1: PRINT "ANY CHANGE (YN) ";
2232 I$ = INKEY$: IF I$ = "" THEN 2232
     IF UCASE$(I$) = "Y" THEN 2225
     GOSUB 2234: GOTO 2237
2234 GOSUB 9900: LOCATE 3
     LOCATE , 7: COLOR PA%(0, 2), PA%(0, 1): PRINT A1$(0, I); : COLOR 7, 0: LOCATE , 49: COLOR PA%(1, 2), PA%(1, 1): PRINT A1$(1, I1): COLOR 7, 0
     PRINT
     LOCATE , 5: PRINT "ACTUAL     REPLAY                         ACTUAL     REPLAY"
     PRINT
     LOCATE , 5: PRINT USING "       TC    ###                                 TC    ###"; Z2(0, I, 0); Z2(1, I1, 0)
     LOCATE , 5: PRINT USING "      YDS   ####                                YDS   ####"; Z2(0, I, 1); Z2(1, I1, 1)
     LOCATE , 5: PRINT USING " ##.# AVG   "; A1%(0, I) * .3;
     IF Z2(0, I, 0) > 0 THEN PRINT USING "##.#"; Z2(0, I, 1) / Z2(0, I, 0);  ELSE PRINT " -.-";
     PRINT USING "                           ##.# AVG   "; A1%(1, I1) * .3;
     IF Z2(1, I1, 0) > 0 THEN PRINT USING "##.#"; Z2(1, I1, 1) / Z2(1, I1, 0) ELSE PRINT " -.-"
     LOCATE , 5: PRINT USING "      TDS     ##                                TDS     ##"; Z2(0, I, 2); Z2(1, I1, 2)
     PRINT
     LOCATE , 5: PRINT USING "       RC     ##                                 RC     ##"; Z2(0, I, 3); Z2(1, I1, 3)
     LOCATE , 5: PRINT USING "      YDS   ####                                YDS   ####"; Z2(0, I, 4); Z2(1, I1, 4)
     LOCATE , 5: PRINT USING " ##.# AVG   "; A3%(0, I) * 3;
     IF Z2(0, I, 3) > 0 THEN PRINT USING "##.#"; Z2(0, I, 4) / Z2(0, I, 3);  ELSE PRINT " -.-";
     PRINT USING "                           ##.# AVG   "; A3%(1, I1) * 3;
     IF Z2(1, I1, 3) > 0 THEN PRINT USING "##.#"; Z2(1, I1, 4) / Z2(1, I1, 3) ELSE PRINT " -.-"
     LOCATE , 5: PRINT USING "      TDS     ##                                TDS     ##"; Z2(0, I, 5); Z2(1, I1, 5)
2212 I$ = INKEY$: IF I$ = "" THEN 2212
     RETURN
2237 SWAP A1$(0, I), A1$(1, I1)
     SWAP A0%(0, I), A0%(1, I1)
     SWAP LR(0, I), LR(1, I1)
     SWAP LR$(0, I, 0), LR$(1, I1, 0)
     SWAP LR$(0, I, 1), LR$(1, I1, 1)
     SWAP LC(0, I), LC(1, I1)
     SWAP LC$(0, I, 0), LC$(1, I1, 0)
     SWAP LC$(0, I, 1), LC$(1, I1, 1)
     SWAP A1%(0, I), A1%(1, I1)
     SWAP A2%(0, I), A2%(1, I1)
     SWAP A3%(0, I), A3%(1, I1)
     SWAP RB$(0, I), RB$(1, I1)
     IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 2244
     FOR I2 = 0 TO 5: SWAP Z2(0, I, I2), Z2(1, I1, I2): NEXT
     GOSUB 2234
     GOTO 2200
2244 FOR I2 = 0 TO 5: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: NEXT I2: GOSUB 2234
     GOTO 2200
2256 GOSUB 9900: LOCATE 4, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0: LOCATE 4, 20: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     FOR I = 0 TO 5: LOCATE I + 5, 1: PRINT I; " "; A2$(0, I): LOCATE I + 5, 20: PRINT I; " "; A2$(1, I)
     NEXT I
2259 LOCATE 17, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I = VAL(I$): IF I < 0 OR I > 5 THEN 2259
     PRINT A2$(0, I)
2262 LOCATE 19, 1: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I1 = VAL(I$): IF I1 < 0 OR I1 > 5 THEN 2262
     PRINT A2$(1, I1)
     LOCATE 23, 1: PRINT "ANY CHANGE (YN) ";
2266 I$ = INKEY$: IF I$ = "" THEN 2266
     IF UCASE$(I$) = "Y" THEN 2259
     GOSUB 2268: GOTO 2270
2268 GOSUB 9900: LOCATE 3
     LOCATE , 7: COLOR PA%(0, 2), PA%(0, 1): PRINT A2$(0, I); : COLOR 7, 0: LOCATE , 49: COLOR PA%(1, 2), PA%(1, 1): PRINT A2$(1, I1): COLOR 7, 0
     PRINT
     LOCATE , 5: PRINT "ACTUAL     REPLAY                         ACTUAL     REPLAY"
     PRINT
     LOCATE , 5: PRINT USING "       RC     ##                                 RC     ##"; Z2(0, I, 6); Z2(1, I1, 6)
     LOCATE , 5: PRINT USING "      YDS   ####                                YDS   ####"; Z2(0, I, 7); Z2(1, I1, 7)
     LOCATE , 5: PRINT USING " ##.# AVG   "; A5%(0, I) * 3;
     IF Z2(0, I, 6) > 0 THEN PRINT USING "##.#"; Z2(0, I, 7) / Z2(0, I, 6);  ELSE PRINT " -.-";
     PRINT USING "                           ##.# AVG   "; A5%(1, I1) * 3;
     IF Z2(1, I1, 6) > 0 THEN PRINT USING "##.#"; Z2(1, I1, 7) / Z2(1, I1, 6) ELSE PRINT " -.-"
     LOCATE , 5: PRINT USING "      TDS     ##                                TDS     ##"; Z2(0, I, 8); Z2(1, I1, 8)
2269 I$ = INKEY$: IF I$ = "" THEN 2269
     RETURN
2270 SWAP A2$(0, I), A2$(1, I1)
     SWAP LC(0, I + 10), LC(1, I1 + 10)
     SWAP LC$(0, I + 10, 0), LC$(1, I1 + 10, 0)
     SWAP LC$(0, I + 10, 1), LC$(1, I1 + 10, 1)
     SWAP A4%(0, I), A4%(1, I1)
     SWAP A5%(0, I), A5%(1, I1)
     SWAP WR$(0, I), WR$(1, I1)
     IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 2278
     FOR I2 = 6 TO 8: SWAP Z2(0, I, I2), Z2(1, I1, I2): NEXT
     GOSUB 2268: GOTO 2200
2278 FOR I2 = 6 TO 8: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: NEXT I2: GOSUB 2268
     GOTO 2200
2290 GOSUB 9900: LOCATE 4, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0: LOCATE 4, 20: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     FOR I = 0 TO 3: LOCATE I + 5, 1: PRINT I; " "; A3$(0, I): LOCATE I + 5, 20: PRINT I; " "; A3$(1, I)
     NEXT I
2293 LOCATE 17, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I = VAL(I$): IF I < 0 OR I > 3 THEN 2293
     PRINT A3$(0, I)
2296 LOCATE 19, 1: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I1 = VAL(I$): IF I1 < 0 OR I1 > 3 THEN 2296
     PRINT A3$(1, I1)
     LOCATE 23, 1: PRINT "ANY CHANGE (YN) ";
2300 I$ = INKEY$: IF I$ = "" THEN 2300
     IF UCASE$(I$) = "Y" THEN 2293
     GOSUB 2302: GOTO 2304
2302 GOSUB 9900: LOCATE 3
     LOCATE , 7: COLOR PA%(0, 2), PA%(0, 1): PRINT A3$(0, I); : COLOR 7, 0: LOCATE , 49: COLOR PA%(1, 2), PA%(1, 1): PRINT A3$(1, I1): COLOR 7, 0
     PRINT
     LOCATE , 5: PRINT "ACTUAL     REPLAY                         ACTUAL     REPLAY"
     PRINT
     LOCATE , 5: PRINT USING "       PC    ###                                 PC    ###"; Z2(0, I, 10); Z2(1, I1, 10)
     LOCATE , 5: PRINT USING "       PA   ####                                 PA   ####"; Z2(0, I, 9); Z2(1, I1, 9)
     LOCATE , 5: PRINT USING "       PI     ##                                 PI     ##"; Z2(0, I, 11); Z2(1, I1, 11)
     LOCATE , 5: PRINT USING "      TDS     ##                                TDS     ##"; Z2(0, I, 13); Z2(1, I1, 13)
     LOCATE , 5: PRINT USING "      YDS   ####                                YDS   ####"; Z2(0, I, 12); Z2(1, I1, 12)
     LOCATE , 5: PRINT USING " ##.# CMP%  "; A7%(0, I);
     IF Z2(0, I, 9) > 0 THEN PRINT USING "##.#"; Z2(0, I, 10) / Z2(0, I, 9) * 100;  ELSE PRINT " -.-";
     PRINT USING "                           ##.# CMP%  "; A7%(1, I1);
     IF Z2(1, I1, 9) > 0 THEN PRINT USING "##.#"; Z2(1, I1, 10) / Z2(1, I1, 9) * 100 ELSE PRINT " -.-"
     LOCATE , 5: PRINT USING " ##.# INT%  "; A8%(0, I);
     IF Z2(0, I, 9) > 0 THEN PRINT USING "##.#"; Z2(0, I, 11) / Z2(0, I, 9) * 100;  ELSE PRINT " -.-";
     PRINT USING "                           ##.# INT%  "; A8%(1, I1);
     IF Z2(1, I1, 9) > 0 THEN PRINT USING "##.#"; Z2(1, I1, 11) / Z2(1, I1, 9) * 100 ELSE PRINT " -.-"
2303 I$ = INKEY$: IF I$ = "" THEN 2303
     RETURN
2304 SWAP A3$(0, I), A3$(1, I1)
     SWAP LP(0, I), LP(1, I1)
     SWAP LP$(0, I, 0), LP$(1, I1, 0)
     SWAP LP$(0, I, 1), LP$(1, I1, 1)
     SWAP A6%(0, I), A6%(1, I1)
     SWAP A7%(0, I), A7%(1, I1)
     SWAP A8%(0, I), A8%(1, I1): SWAP QX%(0, I), QX%(1, I1)
     SWAP QB$(0, I), QB$(1, I1)
     IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 2312
     FOR I2 = 9 TO 13: SWAP Z2(0, I, I2), Z2(1, I1, I2): NEXT I2
     FOR I2 = 0 TO 2: SWAP Z2(0, I + 10, I2), Z2(1, I1 + 10, I2): NEXT I2
     GOSUB 2302: GOTO 2200
2312 FOR I2 = 9 TO 13: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: NEXT I2
     FOR I2 = 0 TO 2: Z2(0, I + 10, I2) = 0: Z2(1, I1 + 10, I2) = 0: NEXT
     GOSUB 2302: GOTO 2200
2324 GOSUB 9900: LOCATE 4, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0: LOCATE 4, 20: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     FOR I = 0 TO 2: LOCATE I + 5, 1: PRINT I; " "; A4$(0, I): LOCATE I + 5, 20: PRINT I; " "; A4$(1, I)
     NEXT I
2327 LOCATE 17, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I = VAL(I$): IF I < 0 OR I > 2 THEN 2327
     PRINT A4$(0, I)
2328 LOCATE 19, 1: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I1 = VAL(I$): IF I1 < 0 OR I1 > 2 THEN 2328
     PRINT A4$(1, I1)
     LOCATE 23, 1: PRINT "ANY CHANGE (YN) ";
2334 I$ = INKEY$: IF I$ = "" THEN 2334
     IF UCASE$(I$) = "Y" THEN 2327
     GOSUB 2336: GOTO 2338
2336 GOSUB 9900: LOCATE 3
     LOCATE , 7: COLOR PA%(0, 2), PA%(0, 1): PRINT A4$(0, I); : COLOR 7, 0: LOCATE , 49: COLOR PA%(1, 2), PA%(1, 1): PRINT A4$(1, I1): COLOR 7, 0
     PRINT
     LOCATE , 5: PRINT "ACTUAL     REPLAY                         ACTUAL     REPLAY"
     PRINT
     LOCATE , 5: PRINT USING "      RET     ##                                RET     ##"; Z2(0, I, 14); Z2(1, I1, 14)
     LOCATE , 5: PRINT USING "      YDS   ####                                YDS   ####"; Z2(0, I, 15); Z2(1, I1, 15)
     LOCATE , 5: PRINT USING " ##.# AVG   "; B%(0, I) * 3;
     IF Z2(0, I, 14) > 0 THEN PRINT USING "##.#"; Z2(0, I, 15) / Z2(0, I, 14);  ELSE PRINT " -.-";
     PRINT USING "                           ##.# AVG   "; B%(1, I1) * 3;
     IF Z2(1, I1, 14) > 0 THEN PRINT USING "##.#"; Z2(1, I1, 15) / Z2(1, I1, 14) ELSE PRINT " -.-"
     LOCATE , 5: PRINT USING "      TDS     ##                                TDS     ##"; Z2(0, I, 16); Z2(1, I1, 16)
2337 I$ = INKEY$: IF I$ = "" THEN 2337
     RETURN
2338 SWAP A4$(0, I), A4$(1, I1)
     SWAP A9%(0, I), A9%(1, I1)
     SWAP LK(0, I), LK(1, I1)
     SWAP LK$(0, I, 0), LK$(1, I1, 0)
     SWAP LK$(0, I, 1), LK$(1, I1, 1)
     SWAP B%(0, I), B%(1, I1)
     SWAP KR$(0, I), KR$(1, I1)
     IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 2346
     FOR I2 = 14 TO 16: SWAP Z2(0, I, I2), Z2(1, I1, I2): NEXT I2
     GOSUB 2336: GOTO 2200
2346 FOR I2 = 14 TO 16: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: NEXT I2
     GOSUB 2336: GOTO 2200
2358 GOSUB 9900: LOCATE 4, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0: LOCATE 4, 20: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     FOR I = 0 TO 2: LOCATE I + 5, 1: PRINT I; " "; A5$(0, I): LOCATE I + 5, 20: PRINT I; " "; A5$(1, I)
     NEXT I
2361 LOCATE 17, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I = VAL(I$): IF I < 0 OR I > 2 THEN 2361
     PRINT A5$(0, I)
2362 LOCATE 19, 1: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I1 = VAL(I$): IF I1 < 0 OR I1 > 2 THEN 2362
     PRINT A5$(1, I1)
     LOCATE 23, 1: PRINT "ANY CHANGE (YN) ";
2368 I$ = INKEY$: IF I$ = "" THEN 2368
     IF UCASE$(I$) = "Y" THEN 2361
     GOSUB 2370: GOTO 2372
2370 GOSUB 9900: LOCATE 3
     LOCATE , 7: COLOR PA%(0, 2), PA%(0, 1): PRINT A5$(0, I); : COLOR 7, 0: LOCATE , 49: COLOR PA%(1, 2), PA%(1, 1): PRINT A5$(1, I1): COLOR 7, 0
     PRINT
     LOCATE , 5: PRINT "ACTUAL     REPLAY                         ACTUAL     REPLAY"
     PRINT
     LOCATE , 5: PRINT USING "      RET     ##                                RET     ##"; Z2(0, I + 3, 14); Z2(1, I1 + 3, 14)
     LOCATE , 5: PRINT USING "      YDS   ####                                YDS   ####"; Z2(0, I + 3, 15); Z2(1, I1 + 3, 15)
     LOCATE , 5: PRINT USING " ##.# AVG   "; B2%(0, I) * 3;
     IF Z2(0, I, 14) > 0 THEN PRINT USING "##.#"; Z2(0, I + 3, 15) / Z2(0, I + 3, 14);  ELSE PRINT " -.-";
     PRINT USING "                           ##.# AVG   "; B2%(1, I1) * 3;
     IF Z2(1, I1, 14) > 0 THEN PRINT USING "##.#"; Z2(1, I1 + 3, 15) / Z2(1, I1 + 3, 14) ELSE PRINT " -.-"
     LOCATE , 5: PRINT USING "      TDS     ##                                TDS     ##"; Z2(0, I + 3, 16); Z2(1, I1 + 3, 16)
2371 I$ = INKEY$: IF I$ = "" THEN 2371
     RETURN
2372 SWAP A5$(0, I), A5$(1, I1)
     SWAP PK(0, I), PK(1, I1)
     SWAP PK$(0, I, 0), PK$(1, I1, 0)
     SWAP PK$(0, I, 1), PK$(1, I1, 1)
     SWAP B1%(0, I), B1%(1, I1)
     SWAP B2%(0, I), B2%(1, I1)
     SWAP PR$(0, I), PR$(1, I1)
     IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 2380
     FOR I2 = 14 TO 16: SWAP Z2(0, I + 3, I2), Z2(1, I1 + 3, I2): NEXT I2
     GOSUB 2370: GOTO 2200
2380 FOR I2 = 14 TO 16: Z2(0, I + 3, I2) = 0: Z2(1, I1 + 3, I2) = 0: NEXT I2
     GOSUB 2370: GOTO 2200
2392 GOSUB 9900: LOCATE 4, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0: LOCATE 4, 20: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     PRINT : PRINT A6$(0, 0); : LOCATE , 20: PRINT " "; A6$(1, 0)
     LOCATE 22, 1: PRINT "ANY CHANGE (YN) ";
2395 I$ = INKEY$: IF I$ = "" THEN 2395
     IF UCASE$(I$) = "Y" THEN 2200
     GOSUB 2397: GOTO 2399
2397 GOSUB 9900: LOCATE 3
     LOCATE , 7: COLOR PA%(0, 2), PA%(0, 1): PRINT A6$(0, 0); : COLOR 7, 0: LOCATE , 49: COLOR PA%(1, 2), PA%(1, 1): PRINT A6$(1, 0): COLOR 7, 0
     PRINT
     LOCATE , 5: PRINT "ACTUAL     REPLAY                         ACTUAL     REPLAY"
     PRINT
     LOCATE , 5: PRINT USING "      PNT     ##                                RET     ##"; Z(0, 7); Z(1, 7)
     LOCATE , 5: PRINT USING " ##.# AVG   "; B3%(0, 0) * 2 + 28;
     IF Z(0, 7) > 0 THEN PRINT USING "##.#"; Z(0, 8) / Z(0, 7);  ELSE PRINT " -.-";
     PRINT USING "                           ##.# AVG   "; B3%(1, 0) * 2 + 28;
     IF Z(1, 7) > 0 THEN PRINT USING "##.#"; Z(1, 8) / Z(1, 7) ELSE PRINT " -.-"
2398 I$ = INKEY$: IF I$ = "" THEN 2398
     RETURN
2399 SWAP A6$(0, 0), A6$(1, 0)
     SWAP B3%(0, 0), B3%(1, 0)
     SWAP PU$(0), PU$(1)
     IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 2403
     SWAP Z(0, 7), Z(1, 7): SWAP Z(0, 8), Z(1, 8)
2403 GOSUB 2397: GOTO 2200
2426 GOSUB 9900: LOCATE 4, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0): COLOR 7, 0: LOCATE 4, 20: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1): COLOR 7, 0
     FOR I = 0 TO 1: LOCATE I + 5, 1: PRINT I; " "; A7$(0, I): LOCATE I + 5, 20: PRINT I; " "; A7$(1, I)
     NEXT I
2427 LOCATE 17, 1: COLOR PA%(0, 2), PA%(0, 1): PRINT A$(0); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I = VAL(I$): IF I < 0 OR I > 1 THEN 2427
     PRINT A7$(0, I)
2428 LOCATE 19, 1: COLOR PA%(1, 2), PA%(1, 1): PRINT A$(1); : COLOR 7, 0: PRINT " PLAYER: "; : INPUT I$
     I1 = VAL(I$): IF I1 < 0 OR I1 > 1 THEN 2428
     PRINT A7$(1, I1)
     LOCATE 22, 1: PRINT "ANY CHANGE (YN) ";
2429 I$ = INKEY$: IF I$ = "" THEN 2429
     IF UCASE$(I$) = "Y" THEN 2427
     GOSUB 2431: GOTO 2433
2431 GOSUB 9900: LOCATE 3
     LOCATE , 7: COLOR PA%(0, 2), PA%(0, 1): PRINT A7$(0, I); : COLOR 7, 0: LOCATE , 49: COLOR PA%(1, 2), PA%(1, 1): PRINT A7$(1, I1): COLOR 7, 0
     PRINT
     LOCATE , 5: PRINT "ACTUAL     REPLAY                         ACTUAL     REPLAY"
     PRINT
     LOCATE , 5: PRINT USING "      FGA  ##                                   FGA  ##"; Z2(0, 4 * I + 4, 17); Z2(1, 4 * I1 + 4, 17)
     LOCATE , 5: PRINT USING "      FGM  ##                                   FGM  ##"; Z2(0, 4 * I + 3, 17); Z2(1, 4 * I1 + 3, 17)
     LOCATE , 5: PRINT USING " ###  FG% "; B4%(0, I) * 5 + 25;
     IF Z2(0, 4 * I + 4, 17) > 0 THEN PRINT USING "### "; Z2(0, I * 4 + 3, 17) / Z2(0, I * 4 + 4, 17) * 100;  ELSE PRINT " -.-";
     PRINT USING "                             ###  FG% "; B4%(1, I1) * 5 + 25;
     IF Z2(1, 4 * I1 + 4, 17) > 0 THEN PRINT USING "### "; Z2(1, I1 * 4 + 3, 17) / Z2(1, I1 * 4 + 4, 17) * 100 ELSE PRINT " -.-"
     LOCATE , 5: PRINT USING "      EPA  ##                                   EPA  ##"; Z2(0, 4 * I + 6, 17); Z2(1, 4 * I1 + 6, 17)
     LOCATE , 5: PRINT USING "      EPM  ##                                   EPM  ##"; Z2(0, 4 * I + 5, 17); Z2(1, 4 * I1 + 5, 17)
     LOCATE , 5: PRINT USING " ###  EP% "; B7%(0, I);
     IF Z2(0, 4 * I + 6, 17) > 0 THEN PRINT USING "### "; Z2(0, 4 * I + 5, 17) / Z2(0, 4 * I + 6, 17) * 100;  ELSE PRINT " -.-";
     PRINT USING "                             ###  EP% "; B7%(1, I1);
     IF Z2(1, 4 * I1 + 6, 17) > 0 THEN PRINT USING "### "; Z2(1, 4 * I1 + 5, 17) / Z2(1, 4 * I1 + 6, 17) * 100 ELSE PRINT " -.-"
2432 I$ = INKEY$: IF I$ = "" THEN 2432
     RETURN
2433 SWAP A7$(0, I), A7$(1, I1)
     SWAP B4%(0, I), B4%(1, I1)
     SWAP LF(0, I), LF(1, I1)
     FOR I2 = 0 TO 4
     SWAP AM(0, I, I2), AM(1, I1, I2)
     SWAP AF(0, I, I2), AF(1, I1, I2)
     NEXT I2
     SWAP B5%(0, I), B5%(1, I1)
     SWAP B7%(0, I), B7%(1, I1)
     SWAP B8%(0, I), B8%(1, I1)
     SWAP FG$(J, I), FG$(J, I1)
     IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 2437
     SWAP Z2(0, 4 * I + 3, 17), Z2(1, 4 * I1 + 3, 17)
     SWAP Z2(0, 4 * I + 4, 17), Z2(1, 4 * I1 + 4, 17)
     SWAP Z2(0, 4 * I + 5, 17), Z2(1, 4 * I1 + 5, 17)
     SWAP Z2(0, 4 * I + 6, 17), Z2(1, 4 * I1 + 6, 17)
     GOSUB 2431: GOTO 2200
2437 Z2(0, 4 * I + 3, 17) = 0: Z2(1, 4 * I1 + 3, 17) = 0
     Z2(0, 4 * I + 4, 17) = 0: Z2(1, 4 * I1 + 4, 17) = 0
     Z2(0, 4 * I + 5, 17) = 0: Z2(1, 4 * I1 + 5, 17) = 0
     Z2(0, 4 * I + 6, 17) = 0: Z2(1, 4 * I1 + 6, 17) = 0
     GOSUB 2431: GOTO 2200
2500 FOR J = 0 TO 1
     GOSUB 9900
     LOCATE 3
     PRINT "INSERT TEAM DISK FOR "; : COLOR PA%(J, 2), PA%(J, 1): PRINT A$(J); : COLOR 7, 0: PRINT " IN DRIVE "; D$(0)
     IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 2505
     GOSUB 4091
2505 GOSUB 9600
     NEXT J
     FOR J = 0 TO 1
     IF S(J) = 1 THEN 2525
     GOSUB 9900
     LOCATE 3: PRINT "INSERT STATS DISK FOR "; : COLOR PA%(J, 2), PA%(J, 1): PRINT A$(J); : COLOR 7, 0: PRINT " IN DRIVE "; D$(1)
     IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 2520
     GOSUB 4091
2520 GOSUB 330
2525 NEXT J
     GOTO 99
4091 LOCATE 23, 1: PRINT "HIT ANY KEY TO CONTINUE"
4092 I$ = INKEY$: IF I$ = "" THEN 4092
     RETURN
9400 FOR I = 0 TO 15: C2(I) = 0: NEXT
     FOR I = 9 TO 1 STEP -1
     IF A0%(J, I) = 0 THEN 9403
     A0%(J, I) = A0%(J, I) - A0%(J, I - 1)
9403 NEXT
     FOR I = 0 TO 5: A2%(J, I + 10) = A4%(J, I): NEXT
     U = 0: FOR I = 1 TO 15
     IF A2%(J, I) = 0 THEN U = U + 1: GOTO 9408
     C2(I) = A2%(J, I) - A2%(J, I - 1 - U): U = 0
9408 NEXT
     FOR I = 1 TO 15: A2%(J, I) = C2(I): NEXT
     FOR I = 0 TO 5: A4%(J, I) = A2%(J, I + 10): NEXT
     FOR I = 3 TO 1 STEP -1
     IF A6%(J, I) = 0 THEN 9414
     A6%(J, I) = A6%(J, I) - A6%(J, I - 1)
9414 NEXT
     FOR I = 2 TO 1 STEP -1
     IF A9%(J, I) = 0 THEN 9418
     A9%(J, I) = A9%(J, I) - A9%(J, I - 1)
9418 NEXT
     FOR I = 2 TO 1 STEP -1
     IF B1%(J, I) = 0 THEN 9422
     B1%(J, I) = B1%(J, I) - B1%(J, I - 1)
9422 NEXT
     FOR I = 1 TO 1 STEP -1
     IF B5%(J, I) = 0 THEN 9426
     B5%(J, I) = B5%(J, I) - B5%(J, I - 1)
9426 NEXT
     FOR I = 1 TO 1 STEP -1
     IF B8%(J, I) = 0 THEN 9430
     B8%(J, I) = B8%(J, I) - B8%(J, I - 1)
9430 NEXT
     RETURN
9500 OPEN D$(0) + "FOOT." + YN$(J) FOR RANDOM AS #1 LEN = 934
     FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
     FIELD #1, 56 AS Q$(0), 12 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 AS Q$(19), 2 AS Q$(20), 12 AS Q$(21), 2 AS Q$(22), 2 AS Q$(23), 2 AS Q$(24), 2 AS Q$(25), 200 AS Q$, 200 AS Q$(200), 238 AS Q$(201), 200 AS Q$
     FIELD #1, 96 AS Q$(0), 12 AS Q$(26), 2 AS Q$(27), 2 AS Q$(28), 2 AS Q$(29), 2 AS Q$(30), 12 AS Q$(31), 2 AS Q$(32), 2 AS Q$(33), 2 AS Q$(34), 2 AS Q$(35), 200 AS Q$(199), 200 AS Q$(200), 198 AS Q$(201), 200 AS Q$
     FIELD #1, 136 AS Q$(0), 12 AS Q$(36), 2 AS Q$(37), 2 AS Q$(38), 2 AS Q$(39), 2 AS Q$(40), 12 AS Q$(41), 2 AS Q$(42), 2 AS Q$(43), 2 AS Q$(44), 2 AS Q$(45), 200 AS Q$(199), 200 AS Q$(200), 158 AS Q$(201), 200 AS Q$
     FIELD #1, 176 AS Q$(0), 12 AS Q$(46), 2 AS Q$(47), 2 AS Q$(48), 2 AS Q$(49), 2 AS Q$(50), 12 AS Q$(51), 2 AS Q$(52), 2 AS Q$(53), 2 AS Q$(54), 2 AS Q$(55), 200 AS Q$(199), 200 AS Q$(200), 118 AS Q$(201), 200 AS Q$
     FIELD #1, 216 AS Q$(0), 12 AS Q$(56), 2 AS Q$(57), 2 AS Q$(58), 2 AS Q$(59), 2 AS Q$(60), 12 AS Q$(61), 2 AS Q$(62), 2 AS Q$(63), 2 AS Q$(64), 2 AS Q$(65), 200 AS Q$(199), 200 AS Q$(200), 78 AS Q$(201), 200 AS Q$
     FIELD #1, 200 AS Q$(0), 56 AS Q$, 12 AS Q$(66), 2 AS Q$(67), 2 AS Q$(68), 12 AS Q$(69), 2 AS Q$(70), 2 AS Q$(71), 12 AS Q$(72), 2 AS Q$(73), 2 AS Q$(74), 200 AS Q$(99), 200 AS Q$(200), 200 AS Q$, 30 AS Q$
     FIELD #1, 200 AS Q$(0), 100 AS Q$, 4 AS Q$(201), 12 AS Q$(75), 2 AS Q$(76), 2 AS Q$(77), 12 AS Q$(78), 2 AS Q$(79), 2 AS Q$(80), 12 AS Q$(81), 2 AS Q$(82), 2 AS Q$(83), 200 AS Q$(199), 182 AS Q$(200), 200 AS Q$
     FIELD #1, 200 AS Q$(0), 152 AS Q$(201), 12 AS Q$(84), 2 AS Q$(85), 2 AS Q$(86), 2 AS Q$(87), 2 AS Q$(88), 12 AS Q$(89), 2 AS Q$(90), 2 AS Q$(91), 2 AS Q$(92), 2 AS Q$(93), 200 AS Q$(199), 200 AS Q$(200), 142 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 192 AS Q$(201), 12 AS Q$(94), 2 AS Q$(95), 2 AS Q$(96), 2 AS Q$(97), 2 AS Q$(98), 12 AS Q$(99), 2 AS Q$(100), 2 AS Q$(101), 2 AS Q$(102), 2 AS Q$(103), 200 AS Q$(199), 200 AS Q$(200), 102 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 200 AS Q$(199), 32 AS Q$(200), 12 AS Q$(104), 2 AS Q$(105), 2 AS Q$(106), 12 AS Q$(107), 2 AS Q$(108), 2 AS Q$(109), 12 AS Q$(110), 2 AS Q$(111), 2 AS Q$(112), 200 AS Q$(201), 200 AS Q$(202), 54 AS Q$(203)
     FIELD #1, 200 AS Q$(0), 200 AS Q$(199), 80 AS Q$(200), 12 AS Q$(113), 2 AS Q$(114), 2 AS Q$(115), 12 AS Q$(116), 2 AS Q$(117), 2 AS Q$(118), 12 AS Q$(119), 2 AS Q$(120), 2 AS Q$(121), 200 AS Q$(201), 200 AS Q$(202), 6 AS Q$(203)
     FIELD #1, 200 AS Q$, 200 AS Q$, 128 AS Q$, 12 AS Q$(122), 2 AS Q$(123), 12 AS Q$(124), 2 AS Q$(125), 2 AS Q$(126), 2 AS Q$(127), 2 AS Q$(128), 12 AS Q$(129), 2 AS Q$(130), 2 AS Q$(131), 2 AS Q$(132), 2 AS Q$(133), 2 AS Q$(134), 200 AS Q$, 150  _
AS Q$
     FIELD #1, 200 AS Q$(0), 200 AS Q$(199), 184 AS Q$(200), 12 AS Q$(135), 2 AS Q$(136), 12 AS Q$(137), 2 AS Q$(138), 12 AS Q$(139), 2 AS Q$(140), 12 AS Q$(141), 2 AS Q$(142), 12 AS Q$(143), 2 AS Q$(144), 200 AS Q$(201), 80 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 100 AS Q$, 200 AS Q$(199), 154 AS Q$(200), 12 AS Q$(145), 2 AS Q$(146), 12 AS Q$(147), 2 AS Q$(148), 12 AS Q$(149), 2 AS Q$(150), 12 AS Q$(151), 2 AS Q$(152), 12 AS Q$(153), 2 AS Q$(154), 200 AS Q$(201), 10 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 100 AS Q$, 200 AS Q$(199), 224 AS Q$(200), 12 AS Q$(155), 2 AS Q$(156), 12 AS Q$(157), 2 AS Q$(158), 12 AS Q$(159), 2 AS Q$(160), 12 AS Q$(161), 2 AS Q$(162), 12 AS Q$(163), 2 AS Q$(164), 140 AS Q$(201)
     FIELD #1, 200 AS Q$(0), 200 AS Q$, 200 AS Q$(199), 194 AS Q$(200), 12 AS Q$(165), 2 AS Q$(166), 12 AS Q$(167), 2 AS Q$(168), 12 AS Q$(169), 2 AS Q$(170), 12 AS Q$(171), 2 AS Q$(172), 12 AS Q$(173), 2 AS Q$(174), 70 AS Q$(201)
     FIELD #1, 232 AS Q$(0), 200 AS Q$, 200 AS Q$(199), 232 AS Q$(200), 12 AS Q$(175), 2 AS Q$(176), 12 AS Q$(177), 2 AS Q$(178), 12 AS Q$(179), 2 AS Q$(180), 12 AS Q$(181), 2 AS Q$(182), 12 AS Q$(183), 2 AS Q$(184)
     GET #1, T(J)
     A$(J) = Q$(1): AA$(J) = Q$(2)
     FOR I = 0 TO 7: B6%(J, I) = CVI(Q$(I + 3)): NEXT I
     FOR I = 0 TO 4: PA%(J, I) = CVI(Q$(I + 11)): NEXT I
     FOR I = 0 TO 9
     A1$(J, I) = Q$(I * 5 + 16)
     A0%(J, I) = CVI(Q$(I * 5 + 17))
     A1%(J, I) = CVI(Q$(I * 5 + 18))
     A2%(J, I) = CVI(Q$(I * 5 + 19))
     A3%(J, I) = CVI(Q$(I * 5 + 20))
     NEXT I
     FOR I = 0 TO 5
     A2$(J, I) = Q$(I * 3 + 66)
     A4%(J, I) = CVI(Q$(I * 3 + 67))
     A5%(J, I) = CVI(Q$(I * 3 + 68))
     NEXT I
     FOR I = 0 TO 3
     A3$(J, I) = Q$(I * 5 + 84)
     A6%(J, I) = CVI(Q$(I * 5 + 85))
     A7%(J, I) = CVI(Q$(I * 5 + 86))
     A8%(J, I) = CVI(Q$(I * 5 + 87))
     QX%(J, I) = CVI(Q$(I * 5 + 88))
     NEXT I
     FOR I = 0 TO 2
     A4$(J, I) = Q$(I * 3 + 104)
     A9%(J, I) = CVI(Q$(I * 3 + 105))
     B%(J, I) = CVI(Q$(I * 3 + 106))
     NEXT I
     FOR I = 0 TO 2
     A5$(J, I) = Q$(I * 3 + 113)
     B1%(J, I) = CVI(Q$(I * 3 + 114))
     B2%(J, I) = CVI(Q$(I * 3 + 115))
     NEXT I
     A6$(J, 0) = Q$(122)
     B3%(J, 0) = CVI(Q$(123))
     A7$(J, 0) = Q$(124)
     B4%(J, 0) = CVI(Q$(125))
     B5%(J, 0) = CVI(Q$(126))
     B7%(J, 0) = CVI(Q$(127))
     B8%(J, 0) = CVI(Q$(128))
     A7$(J, 1) = Q$(129)
     B4%(J, 1) = CVI(Q$(130))
     B5%(J, 1) = CVI(Q$(131))
     B7%(J, 1) = CVI(Q$(132))
     B8%(J, 1) = CVI(Q$(133))
     YC(J) = CVI(Q$(134))
     FOR I = 0 TO 14
     DS$(J, I) = Q$(I * 2 + 135)
     DS%(J, I) = CVI(Q$(I * 2 + 136))
     NEXT I
     FOR I = 0 TO 9
     DI$(J, I) = Q$(I * 2 + 165)
     DI%(J, I) = CVI(Q$(I * 2 + 166))
     NEXT I
9598 IF RIGHT$(A$(J), 1) = " " THEN A$(J) = LEFT$(A$(J), LEN(A$(J)) - 1): GOTO 9598
     SA$(J) = A$(J)
     CLOSE #1: GOTO 9400
9600 REM
     YC%(J) = YC(J)
     S0 = 0: S1 = 0: S2 = 0: S3 = 0
     FOR I = 0 TO 9: S0 = S0 + A0%(J, I): S2 = S2 + A2%(J, I): NEXT
     S4 = 100 / S0
     FOR I = 0 TO 9: IF A0%(J, I) <> 0 THEN A0%(J, I) = INT(S4 * A0%(J, I) + .5) + S1: S1 = A0%(J, I)
     IF A0%(J, I) > 100 THEN A0%(J, I) = 100
     NEXT I
     FOR I = 0 TO 5: S2 = S2 + A4%(J, I): NEXT
     S5 = 100 / S2
     FOR I = 0 TO 9: IF A2%(J, I) <> 0 THEN A2%(J, I) = INT(S5 * A2%(J, I) + .5) + S3: S3 = A2%(J, I)
     IF A2%(J, I) > 100 THEN A2%(J, I) = 100
     NEXT I
     FOR I = 0 TO 5: IF A4%(J, I) <> 0 THEN A4%(J, I) = INT(S5 * A4%(J, I) + .5) + S3: S3 = A4%(J, I)
     IF A4%(J, I) > 100 THEN A4%(J, I) = 100
     NEXT I
     S6 = 0: S7 = 0
     FOR I = 0 TO 3: S6 = S6 + A6%(J, I): NEXT
     S8 = 100 / S6
     FOR I = 0 TO 3: IF A6%(J, I) <> 0 THEN A6%(J, I) = INT(S8 * A6%(J, I) + .5) + S7: S7 = A6%(J, I)
     IF A6%(J, I) > 100 THEN A6%(J, I) = 100
     NEXT
     G0 = 0: G1 = 0: G3 = 0: G4 = 0
     FOR I = 0 TO 2: G0 = G0 + A9%(J, I): G3 = G3 + B1%(J, I): NEXT
     G2 = 100 / G0: G5 = 100 / G3
     FOR I = 0 TO 2: IF A9%(J, I) <> 0 THEN A9%(J, I) = INT(G2 * A9%(J, I) + .5) + G1: G1 = A9%(J, I)
     IF A9%(J, I) > 100 THEN A9%(J, I) = 100
     NEXT
     FOR I = 0 TO 2: IF B1%(J, I) <> 0 THEN B1%(J, I) = INT(G5 * B1%(J, I) + .5) + G4: G4 = B1%(J, I)
     IF B1%(J, I) > 100 THEN B1%(J, I) = 100
     NEXT
     G0 = 0: G1 = 0: G3 = 0: G4 = 0
     FOR I = 0 TO 1: G0 = G0 + B5%(J, I): G3 = G3 + B8%(J, I): NEXT
     G2 = 100 / G0: G5 = 100 / G3
     FOR I = 0 TO 1: IF B5%(J, I) <> 0 THEN B5%(J, I) = INT(G2 * B5%(J, I) + .5) + G1: G1 = B5%(J, I)
     IF B5%(J, I) > 100 THEN B5%(J, I) = 100
     NEXT
     FOR I = 0 TO 1: IF B8%(J, I) <> 0 THEN B8%(J, I) = INT(G5 * B8%(J, I) + .5) + G4: G4 = B8%(J, I)
     IF B8%(J, I) > 100 THEN B8%(J, I) = 100
     NEXT
     FOR I = 9 TO 0 STEP -1: IF A0%(J, I) <> 0 THEN A0%(J, I) = 100: GOTO 9776
     NEXT
9776 FOR I = 5 TO 0 STEP -1: IF A4%(J, I) <> 0 THEN A4%(J, I) = 100: GOTO 9778
     NEXT
9778 FOR I = 3 TO 0 STEP -1: IF A6%(J, I) <> 0 THEN A6%(J, I) = 100: GOTO 9780
     NEXT
9780 FOR I = 2 TO 0 STEP -1: IF A9%(J, I) <> 0 THEN A9%(J, I) = 100: GOTO 9782
     NEXT
9782 FOR I = 2 TO 0 STEP -1: IF B1%(J, I) <> 0 THEN B1%(J, I) = 100: GOTO 9784
     NEXT
9784 FOR I = 1 TO 0 STEP -1: IF B5%(J, I) <> 0 THEN B5%(J, I) = 100: GOTO 9786
     NEXT
9786 FOR I = 1 TO 0 STEP -1: IF B8%(J, I) <> 0 THEN B8%(J, I) = 100: GOTO 9800
     NEXT
9800 OPEN D$(0) + "FOOT." + YN$(J) FOR RANDOM AS #1 LEN = 934
     FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
     FIELD #1, 56 AS Q$(0), 12 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 AS Q$(19), 2 AS Q$(20), 12 AS Q$(21), 2 AS Q$(22), 2 AS Q$(23), 2 AS Q$(24), 2 AS Q$(25), 200 AS Q$, 200 AS Q$(200), 238 AS Q$(201), 200 AS Q$
     FIELD #1, 96 AS Q$(0), 12 AS Q$(26), 2 AS Q$(27), 2 AS Q$(28), 2 AS Q$(29), 2 AS Q$(30), 12 AS Q$(31), 2 AS Q$(32), 2 AS Q$(33), 2 AS Q$(34), 2 AS Q$(35), 200 AS Q$(199), 200 AS Q$(200), 198 AS Q$(201), 200 AS Q$
     FIELD #1, 136 AS Q$(0), 12 AS Q$(36), 2 AS Q$(37), 2 AS Q$(38), 2 AS Q$(39), 2 AS Q$(40), 12 AS Q$(41), 2 AS Q$(42), 2 AS Q$(43), 2 AS Q$(44), 2 AS Q$(45), 200 AS Q$(199), 200 AS Q$(200), 158 AS Q$(201), 200 AS Q$
     FIELD #1, 176 AS Q$(0), 12 AS Q$(46), 2 AS Q$(47), 2 AS Q$(48), 2 AS Q$(49), 2 AS Q$(50), 12 AS Q$(51), 2 AS Q$(52), 2 AS Q$(53), 2 AS Q$(54), 2 AS Q$(55), 200 AS Q$(199), 200 AS Q$(200), 118 AS Q$(201), 200 AS Q$
     FIELD #1, 216 AS Q$(0), 12 AS Q$(56), 2 AS Q$(57), 2 AS Q$(58), 2 AS Q$(59), 2 AS Q$(60), 12 AS Q$(61), 2 AS Q$(62), 2 AS Q$(63), 2 AS Q$(64), 2 AS Q$(65), 200 AS Q$(199), 200 AS Q$(200), 78 AS Q$(201), 200 AS Q$
     FIELD #1, 200 AS Q$(0), 56 AS Q$, 12 AS Q$(66), 2 AS Q$(67), 2 AS Q$(68), 12 AS Q$(69), 2 AS Q$(70), 2 AS Q$(71), 12 AS Q$(72), 2 AS Q$(73), 2 AS Q$(74), 200 AS Q$(99), 200 AS Q$(200), 200 AS Q$, 30 AS Q$
     FIELD #1, 200 AS Q$(0), 100 AS Q$, 4 AS Q$(201), 12 AS Q$(75), 2 AS Q$(76), 2 AS Q$(77), 12 AS Q$(78), 2 AS Q$(79), 2 AS Q$(80), 12 AS Q$(81), 2 AS Q$(82), 2 AS Q$(83), 200 AS Q$(199), 182 AS Q$(200), 200 AS Q$
     FIELD #1, 200 AS Q$(0), 152 AS Q$(201), 12 AS Q$(84), 2 AS Q$(85), 2 AS Q$(86), 2 AS Q$(87), 2 AS Q$(88), 12 AS Q$(89), 2 AS Q$(90), 2 AS Q$(91), 2 AS Q$(92), 2 AS Q$(93), 200 AS Q$(199), 200 AS Q$(200), 142 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 192 AS Q$(201), 12 AS Q$(94), 2 AS Q$(95), 2 AS Q$(96), 2 AS Q$(97), 2 AS Q$(98), 12 AS Q$(99), 2 AS Q$(100), 2 AS Q$(101), 2 AS Q$(102), 2 AS Q$(103), 200 AS Q$(199), 200 AS Q$(200), 102 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 200 AS Q$(199), 32 AS Q$(200), 12 AS Q$(104), 2 AS Q$(105), 2 AS Q$(106), 12 AS Q$(107), 2 AS Q$(108), 2 AS Q$(109), 12 AS Q$(110), 2 AS Q$(111), 2 AS Q$(112), 200 AS Q$(201), 200 AS Q$(202), 54 AS Q$(203)
     FIELD #1, 200 AS Q$(0), 200 AS Q$(199), 80 AS Q$(200), 12 AS Q$(113), 2 AS Q$(114), 2 AS Q$(115), 12 AS Q$(116), 2 AS Q$(117), 2 AS Q$(118), 12 AS Q$(119), 2 AS Q$(120), 2 AS Q$(121), 200 AS Q$(201), 200 AS Q$(202), 6 AS Q$(203)
     FIELD #1, 200 AS Q$, 200 AS Q$, 128 AS Q$, 12 AS Q$(122), 2 AS Q$(123), 12 AS Q$(124), 2 AS Q$(125), 2 AS Q$(126), 2 AS Q$(127), 2 AS Q$(128), 12 AS Q$(129), 2 AS Q$(130), 2 AS Q$(131), 2 AS Q$(132), 2 AS Q$(133), 2 AS Q$(134), 200 AS Q$, 150  _
AS Q$
     FIELD #1, 200 AS Q$(0), 200 AS Q$(199), 184 AS Q$(200), 12 AS Q$(135), 2 AS Q$(136), 12 AS Q$(137), 2 AS Q$(138), 12 AS Q$(139), 2 AS Q$(140), 12 AS Q$(141), 2 AS Q$(142), 12 AS Q$(143), 2 AS Q$(144), 200 AS Q$(201), 80 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 100 AS Q$, 200 AS Q$(199), 154 AS Q$(200), 12 AS Q$(145), 2 AS Q$(146), 12 AS Q$(147), 2 AS Q$(148), 12 AS Q$(149), 2 AS Q$(150), 12 AS Q$(151), 2 AS Q$(152), 12 AS Q$(153), 2 AS Q$(154), 200 AS Q$(201), 10 AS Q$(202)
     FIELD #1, 200 AS Q$(0), 100 AS Q$, 200 AS Q$(199), 224 AS Q$(200), 12 AS Q$(155), 2 AS Q$(156), 12 AS Q$(157), 2 AS Q$(158), 12 AS Q$(159), 2 AS Q$(160), 12 AS Q$(161), 2 AS Q$(162), 12 AS Q$(163), 2 AS Q$(164), 140 AS Q$(201)
     FIELD #1, 200 AS Q$(0), 200 AS Q$, 200 AS Q$(199), 194 AS Q$(200), 12 AS Q$(165), 2 AS Q$(166), 12 AS Q$(167), 2 AS Q$(168), 12 AS Q$(169), 2 AS Q$(170), 12 AS Q$(171), 2 AS Q$(172), 12 AS Q$(173), 2 AS Q$(174), 70 AS Q$(201)
     FIELD #1, 232 AS Q$(0), 200 AS Q$, 200 AS Q$(199), 232 AS Q$(200), 12 AS Q$(175), 2 AS Q$(176), 12 AS Q$(177), 2 AS Q$(178), 12 AS Q$(179), 2 AS Q$(180), 12 AS Q$(181), 2 AS Q$(182), 12 AS Q$(183), 2 AS Q$(184)
     LSET Q$(1) = A$(J): LSET Q$(2) = AA$(J)
     FOR I = 0 TO 7: LSET Q$(I + 3) = MKI$(B6%(J, I)): NEXT I
     FOR I = 0 TO 4: LSET Q$(I + 11) = MKI$(PA%(J, I)): NEXT I
     FOR I = 0 TO 9
     LSET Q$(I * 5 + 16) = A1$(J, I)
     LSET Q$(I * 5 + 17) = MKI$(A0%(J, I))
     LSET Q$(I * 5 + 18) = MKI$(A1%(J, I))
     LSET Q$(I * 5 + 19) = MKI$(A2%(J, I))
     LSET Q$(I * 5 + 20) = MKI$(A3%(J, I))
     NEXT I
     FOR I = 0 TO 5
     LSET Q$(I * 3 + 66) = A2$(J, I)
     LSET Q$(I * 3 + 67) = MKI$(A4%(J, I))
     LSET Q$(I * 3 + 68) = MKI$(A5%(J, I))
     NEXT I
     FOR I = 0 TO 3
     LSET Q$(I * 5 + 84) = A3$(J, I)
     LSET Q$(I * 5 + 85) = MKI$(A6%(J, I))
     LSET Q$(I * 5 + 86) = MKI$(A7%(J, I))
     LSET Q$(I * 5 + 87) = MKI$(A8%(J, I))
     LSET Q$(I * 5 + 88) = MKI$(QX%(J, I))
     NEXT I
     FOR I = 0 TO 2
     LSET Q$(I * 3 + 104) = A4$(J, I)
     LSET Q$(I * 3 + 105) = MKI$(A9%(J, I))
     LSET Q$(I * 3 + 106) = MKI$(B%(J, I))
     NEXT I
     FOR I = 0 TO 2
     LSET Q$(I * 3 + 113) = A5$(J, I)
     LSET Q$(I * 3 + 114) = MKI$(B1%(J, I))
     LSET Q$(I * 3 + 115) = MKI$(B2%(J, I))
     NEXT I
     LSET Q$(122) = A6$(J, 0)
     LSET Q$(123) = MKI$(B3%(J, 0))
     LSET Q$(124) = A7$(J, 0)
     LSET Q$(125) = MKI$(B4%(J, 0))
     LSET Q$(126) = MKI$(B5%(J, 0))
     LSET Q$(127) = MKI$(B7%(J, 0))
     LSET Q$(128) = MKI$(B8%(J, 0))
     LSET Q$(129) = A7$(J, 1)
     LSET Q$(130) = MKI$(B4%(J, 1))
     LSET Q$(131) = MKI$(B5%(J, 1))
     LSET Q$(132) = MKI$(B7%(J, 1))
     LSET Q$(133) = MKI$(B8%(J, 1))
     LSET Q$(134) = MKI$(YC%(J))
     FOR I = 0 TO 14
     LSET Q$(I * 2 + 135) = DS$(J, I)
     LSET Q$(I * 2 + 136) = MKI$(DS%(J, I))
     NEXT I
     FOR I = 0 TO 9
     LSET Q$(I * 2 + 165) = DI$(J, I)
     LSET Q$(I * 2 + 166) = MKI$(DI%(J, I))
     NEXT I
     PUT #1, T(J)
     CLOSE #1
     GOSUB 1000
     RETURN
9900 CALL TITLE
     FOR XY = 2 TO 23: LOCATE XY, 1: PRINT TAB(79);
     NEXT
     RETURN

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 32: COLOR 12
   PRINT "3 IN 1 FOOTBALL V6.0"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
             PRINT "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
   COLOR 10
END SUB

