DECLARE SUB POSTLEADERS (Z1$(), O1!(), O2!(), O3!(), PTSF!(), PTSA!(), Z2$(), Z3$(), Z1!(), TT!(), TT$(), D$(), NC!, CN$())
DECLARE SUB READFILES (D$(), H1$(), XW!, Y%, O%(), O$(), PTSF!(), T%(), PTSA!(), H$(), AP%())
DECLARE SUB POSTNATIONAL (D$(), WE$, Z1$(), O1!(), O2!(), O3!(), PTSF!(), PTSA!(), N2$(), N3$(), N1!(), NT!(), NT$())
DECLARE SUB STATNAME (O$())
DECLARE SUB BOXDRAW ()
DECLARE SUB GETKEY ()
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, START%, COUNT%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB MQPrint (X$, Colr)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION EXIST% (F$)
  DIM RB(11, 11), QB(11, 11), KR(11, 11), PR(11, 11), WR$(16), WR(16, 13)
  DIM O1(60), Z1$(60), DT(38), OT(38), TB$(18), YD(16), TD(16), TP(18), TG(16)
  DIM PS$(10), RB$(10), QB$(4), KR$(3), PR$(3), PK$(2)
  DIM FM(2), FA(2), EM(2), EA(2)
  DIM D$(2), FG(2), EP(2), BRC(50), BRC$(50, 2), TRC(50), TRC$(50)
  DIM PTSF(60), PTSA(60), O2(60), O%(120), O$(120), T%(120), H1$(30)
  DIM Z2$(240), Z3$(240), TT(10, 20, 9), TT$(10, 20)
  DIM Z1(240, 2), H$(120), O3(60), AP%(1, 120), CN$(40)
  DIM Z2(14, 18), DI$(9), GI%(9, 2), DS$(14), GS%(14, 1)
  DIM N2$(240), N3$(240), N1(240, 2), NT(10, 20, 9), NT$(10, 20)
  
  DIM LR(9), LR$(9, 1), LC(15), LC$(15, 1), LP(3), LP$(3, 1), LK(2), LK$(2, 1)
  DIM PK(2), PKR$(2, 1), LF(1), LF$(1), AM(1, 4), AF(1, 4), LI$(9, 1), LI(9)
  D% = GetDrive%
  OPEN "DEV.BAS" FOR INPUT AS #1: FOR I = 0 TO 2: INPUT #1, D$(I): NEXT: CLOSE 1
  CLS
  IF UCASE$(LEFT$(D$(2), 1)) <> "A" AND UCASE$(LEFT$(D$(2), 1)) <> "B" THEN 4001
  GOSUB 4600
  U$ = "INSERT DISK WITH CONFERENCE FILES IN DRIVE " + D$(2)
  LOCATE , (80 - LEN(U$)) / 2
  PRINT U$
  U$ = "HIT ANY KEY TO CONTINUE"
  LOCATE , (80 - LEN(U$)) / 2
  PRINT U$
  COLOR 15, 0
7 I$ = INKEY$: IF I$ = "" THEN 7

4001 TRC(11) = 999:TRC(22) = 999
   FOR I=25 TO 27:TRC(I)=9999:NEXT
   FOR I=29 TO 36:TRC(I)=9999:NEXT
   TRC(38) = 999:TRC(39) = 999
   TRC(42) = 999:TRC(43) = 999

   IF NOT EXIST%(D$(1) + "RECORDS.CFB") THEN 4003
   OPEN D$(1) + "RECORDS.CFB" FOR INPUT AS #1
   FOR I = 0 TO 50: INPUT #1, BRC(I):INPUT #1, BRC$(I, 0):INPUT #1, BRC$(I, 1)
   INPUT #1, BRC$(I, 2)
   NEXT
   FOR I = 0 TO 50: INPUT #1, TRC(I):INPUT #1, TRC$(I): NEXT
   CLOSE 1

4003 Spec$ = D$(2) + "*.LIB"
    COUNT% = FCount%(Spec$)
   IF COUNT% = 0 THEN 4500
   REDIM f$(0 TO COUNT%)
   FOR X = 1 TO COUNT%
       f$(X) = SPACE$(12)
   NEXT
   f$(0) = Spec$
   CALL ReadFile(VARPTR(f$(0)))
   CALL SortStr(VARPTR(f$(1)), COUNT%, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT LEAGUE TO UPDATE": COLOR 15
    SELECTION% = 1
    START% = 1
    FileColr% = 31
    HiLiteColr% = 112
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    LOCATE 25, 1
    COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS LEAGUE";
CALL SHOWCURSOR
20 MAMenu VARPTR(f$(1)), SELECTION%, START%, COUNT%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF EXITCODE% = 27 THEN 2900
     IF EXITCODE% = -71 THEN START% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
     IF EXITCODE% = -79 THEN START% = COUNT%: EXITCODE% = 3: SELECTION% = COUNT%: GOTO 20
     IF EXITCODE% = -73 THEN START% = START% - 80: IF START% < 1 THEN START% = 1
     IF EXITCODE% = -81 THEN START% = START% + 80: IF START% > COUNT% THEN START% = COUNT%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = START%: GOTO 20
     f$ = f$(SELECTION%)
   LOCATE 2, 33: PRINT f$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
3911 f$ = QPRTrim$(f$)
    WE$ = LEFT$(f$, LEN(f$) - 4)
    CLS : PRINT "IS THIS THE FINAL UPDATE FOR THE SEASON (YN)";
     COLOR 15, 0
3912 I$ = INKEY$: IF I$ = "" THEN 3912
    IF UCASE$(I$) = "Y" THEN UP$ = "Y"
   C$ = ","
   FOR X = 1 TO 60
   PTSF(X) = 0
   PTSA(X) = 0
   NEXT X
   FOR X = 1 TO 240
   Z2$(X) = "*"
   NEXT
   CA = 2
   FOR I = 1 TO 20
   TT(CA, I, 5) = 9999
   NEXT
   CA = 4
   FOR I = 1 TO 20
   TT(CA, I, 5) = 9999
   NEXT
   CA = 6
   FOR I = 1 TO 20
   TT(CA, I, 3) = 9999
   NEXT
   CA = 8
   FOR I = 1 TO 20
   TT(CA, I, 8) = 9999
   NEXT
   CA = 10
   FOR I = 1 TO 20
   TT(CA, I, 7) = -99
   NEXT

   CLS
   OPEN D$(2) + WE$ + ".TMS" FOR INPUT AS #2
   INPUT #2, FF$
   INPUT #2, D1$
   INPUT #2, D2$
   FOR XW = 1 TO 30
   INPUT #2, H1$(XW)
   NEXT
   CLOSE 2
   GOSUB 3018

   IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 70
   CLS : PRINT "INSERT DISK WITH TEAM STATS IN DRIVE "; D$(1)
   LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"

66 I$ = INKEY$: IF I$ = "" THEN 66

70 FOR XW = 1 TO 30
   GOSUB 700: GOSUB 900
   NEXT
   AA = 1: ZZ = 15: GOSUB 950
   AA = 16: ZZ = 30:GOSUB 950
   AA = 31: ZZ = 45:GOSUB 950
   AA = 46: ZZ = 60:GOSUB 950
   GOSUB 4000

99 REM:
2900 IF D% >= 67 THEN 2905
     CLS
     PRINT "MAKE SURE PROGRAM DISK 3 IS IN DRIVE"
     CALL GETKEY
2905 RUN "HELLO.EXE 3"
     END
700 WN = 0: LS = 0: TY = 0
    IF H1$(XW) = CHR$(42) THEN RETURN
    GOSUB 9131
    IF NOT EXIST%(D$(1) + H1$(XW) + "S") THEN 895

    OPEN D$(1) + H1$(XW) + "S" FOR INPUT AS #2
    INPUT #2, Y%
    FOR I = 1 TO 120: INPUT #2, O%(I): NEXT I
    FOR I = 1 TO 120: INPUT #2, O$(I): PTSF(XW) = PTSF(XW) + O%(I): NEXT I
    FOR I = 1 TO 120: INPUT #2, T%(I): PTSA(XW) = PTSA(XW) + T%(I): NEXT I
    FOR I = 1 TO 120: INPUT #2, H$(I): NEXT I
    FOR I = 0 TO 1
    FOR I1 = 1 TO 120
    INPUT #2, AP%(I, I1)
    NEXT:NEXT

    CLOSE 2
    WN = 0: LS = 0: TY = 0: CW = 0: CT = 0: CL = 0
    GOSUB 5500

    PRINT "FINDING "; H1$(XW); " STATS"
    RETURN

811 OPEN D$(1) + H1$(XW) FOR INPUT AS #1: INPUT #1, TM$
    FOR I = 1 TO 10:INPUT #1, RB$(I):NEXT
    FOR I = 1 TO 6: INPUT #1, WR$(I):NEXT
    FOR I = 1 TO 4: INPUT #1, QB$(I):NEXT
    FOR I = 1 TO 3: INPUT #1, KR$(I):NEXT
    FOR I = 1 TO 3: INPUT #1, PR$(I):NEXT I
    INPUT #1, PU$
    FOR I = 1 TO 2: INPUT #1, PK$(I):NEXT

    FOR J = 1 TO 14
    FOR K = 1 TO 18
    INPUT #1, Z2(J, K)
    NEXT K:NEXT J
    FOR L = 0 TO 38: INPUT #1, OT(L): NEXT L
    FOR L = 0 TO 38: INPUT #1, DT(L): NEXT L
    FOR J = 0 TO 9: INPUT #1, DI$(J)
    FOR J1 = 0 TO 2
    INPUT #1, GI%(J, J1)
    NEXT:NEXT
    FOR J = 0 TO 14: INPUT #1, DS$(J):FOR J1=0 TO 1: INPUT #1, GS%(J,J1): NEXT:NEXT
     FOR I=0 TO 9:INPUT #1,LR(I):INPUT #1,LR$(I,0):INPUT #1,LR$(I,1):NEXT
     FOR I=0 TO 15:INPUT #1,LC(I):INPUT #1,LC$(I,0):INPUT #1, LC$(I,1):NEXT
     FOR I=0 TO 3:INPUT #1,LP(I):INPUT #1,LP$(I,0):INPUT #1, LP$(I,1):NEXT
     FOR I=0 TO 2:INPUT #1,LK(I):INPUT #1,LK$(I,0):INPUT #1, LK$(I,1):NEXT
     FOR I=0 TO 2:INPUT #1,PK(I):INPUT #1,PKR$(I,0):INPUT #1, PKR$(I,1):NEXT
     FOR I=0 TO 1:INPUT #1,LF(I):INPUT #1, LF$(I):FOR I1=0 TO 4:INPUT #1,AM(I,I1):INPUT #1,AF(I,I1):NEXT:NEXT
     FOR I=0 TO 9:INPUT #1,LI(I):INPUT #1,LI$(I,0):INPUT #1,LI$(I,1):NEXT
    CLOSE 1

    FOR I = 1 TO 10: FOR J = 1 TO 6: RB(I, J) = Z2(I, J): NEXT: NEXT
    FOR I = 1 TO 6: FOR J = 1 TO 3: WR(I, J) = Z2(I, J + 6): NEXT: NEXT
    FOR I = 1 TO 4: FOR J = 1 TO 5: QB(I, J) = Z2(I, J + 9): NEXT: NEXT
    FOR I = 1 TO 3: FOR J = 1 TO 3: KR(I, J) = Z2(I, J + 14): NEXT: NEXT
    FOR I = 1 TO 3: FOR J = 1 TO 3: PR(I, J) = Z2(I + 3, J + 14): NEXT: NEXT
    TG(8) = OT(7): TG(9) = OT(8)
    FM(1) = Z2(4, 18): FA(1) = Z2(5, 18): EM(1) = Z2(6, 18): EA(1) = Z2(7, 18)
    FM(2) = Z2(8, 18): FA(2) = Z2(9, 18): EM(2) = Z2(10, 18): EA(2) = Z2(11, 18)
    FOR I = 1 TO 10: RB(I, 7) = RB(I, 2) + RB(I, 5): RB(I, 8) = RB(I, 3) + RB(I, 6): WR$(I + 6) = RB$(I): WR(I + 6, 2) = RB(I, 5): WR(I + 6, 1) = RB(I, 4): WR(I + 6, 3) = RB(I, 6): NEXT
    FOR I = 1 TO 10: TB$(I) = RB$(I): YD(I) = RB(I, 7): TD(I) = RB(I, 8): NEXT
    FOR I = 1 TO 6: TB$(I + 10) = WR$(I): YD(I + 10) = WR(I, 2): TD(I + 10) = WR(I, 3): NEXT

    FOR I = 1 TO 16
    FOR I1 = 1 TO 3
    IF KR$(I1) = TB$(I) THEN YD(I) = YD(I) + KR(I1,2):TD(I) = TD(I)+KR(I1,3)
    NEXT
    NEXT
    FOR I = 1 TO 16
    FOR I1 = 1 TO 3
    IF PR$(I1) = TB$(I) THEN YD(I) = YD(I) + PR(I1,2):TD(I) = TD(I)+PR(I1,3)
    NEXT
    NEXT

    FOR I = 1 TO 16: TP(I) = TD(I) * 6: NEXT
    FOR I = 1 TO 2: TB$(16 + I) = PK$(I): TP(16 + I) = EM(I) + FM(I) * 3: NEXT
    FOR I = 1 TO 10: RB(I, 9) = 0: IF RB(I, 1) > 0 THEN RB(I, 9) = INT(RB(I, 2) / RB(I, 1) * 10) / 10
    NEXT
    FOR I = 1 TO 10: RB(I, 10) = 0: IF RB(I, 4) > 0 THEN RB(I, 10) = INT(RB(I, 5) / RB(I, 4) * 10) / 10
    NEXT
    FOR I = 1 TO 16: WR(I, 4) = 0: IF WR(I, 1) > 0 THEN WR(I, 4) = INT(WR(I, 2) / WR(I, 1) * 10) / 10
    NEXT
    FOR I = 1 TO 4: QB(I, 6) = 0: IF QB(I, 1) > 0 THEN QB(I, 6) = INT(QB(I, 2) / QB(I, 1) * 1000) / 10
    GOSUB 2000
    NEXT
    FOR I = 1 TO 3: KR(I, 4) = 0: IF KR(I, 1) > 0 AND KR(I, 1) >= Y% THEN KR(I, 4) = INT(KR(I, 2) / KR(I, 1) * 10) / 10: GOTO 859
    KR(I, 4) = 0
859 NEXT
    FOR I = 1 TO 3: PR(I, 4) = 0: IF PR(I, 1) > 0 AND PR(I, 1) >= Y% THEN PR(I, 4) = INT(PR(I, 2) / PR(I, 1) * 10) / 10: GOTO 862
    PR(I, 4) = 0
862 NEXT
    PN = 0
    IF TG(8) > 0 THEN PN = INT(TG(9) / TG(8) * 10) / 10
    FOR I = 1 TO 2
    FG(I) = 0
    IF FA(I) >= Y% THEN FG(I) = INT(FM(I) / FA(I) * 1000) / 10
    EP(I) = 0
    IF EA(I) >= Y% THEN EP(I) = INT(EM(I) / EA(I) * 1000) / 10
    NEXT
895 RETURN


900 L = LEN(H1$(XW)): WT$ = ""
    FOR X = 1 TO L: A = ASC(MID$(H1$(XW), X, 1))
    WT$ = WT$ + CHR$(A): NEXT X
    FOR X = 1 TO 30
    IF Z1$(X) <> WT$ THEN 945
    IF O1(X) = WN AND O2(X) = LS AND O3(X) = TY THEN RETURN
    O1(X) = WN: O2(X) = LS: O3(X) = TY
    O1(X + 30) = CW: O2(X + 30) = CL: O3(X + 30) = CT
    PTSF(X + 30) = PTSFC
    PTSA(X + 30) = PTSAC
    Z1$(X + 30) = Z1$(X)
945 NEXT X
    GOSUB 811
    GOSUB 3100
    RETURN


950 FOR X = AA TO ZZ - 1: FOR Y = X + 1 TO ZZ: IF O1(X) - O2(X) > O1(Y) - O2(Y) THEN 985
    IF O1(X) - O2(X) = O1(Y) - O2(Y) AND O1(X) <= O1(Y) AND O1(X) > O2(X) THEN 985
    IF O1(X) - O2(X) = O1(Y) - O2(Y) AND O1(X) >= O1(Y) AND O1(X) < O2(X) THEN 985
    SWAP Z1$(X), Z1$(Y)
    SWAP O1(X), O1(Y)
    SWAP O2(X), O2(Y)
    SWAP O3(X), O3(Y)
    SWAP PTSF(X), PTSF(Y)
    SWAP PTSA(X), PTSA(Y)
985 NEXT Y
    NEXT X
    RETURN


2000 REM:********** CALCULATE QUARTERBACK PASSING EFFICIENCY ****************
     REM:* QB(1)=PASS ATT, QB(2)=COMP, QB(3)=INT, QB(4)=YDS, QB(5)=TD'S     *            * QB(6)=COMP%, QB(7)=RATING, QB(8)=TD%, QB(9)=INT%, QB(10)=YDS/ATT *
     REM:********************************************************************
     IF QB(I, 1) = 0 THEN QB(I, 6) = 0: QB(I, 7) = 0: QB(I, 8) = 0: QB(I, 10) = 0: QB(I, 9) = 0: GOTO 2005
     QB(I, 7) = INT(QB(I, 4) / QB(I, 1)) * 8.399999 + QB(I, 6) + INT(QB(I, 5) / QB(I, 1) * 1000) / 10 * 3.3 - INT(QB(I, 3) / QB(I, 1) * 1000) / 10 * 2: QB(I, 7) = INT(QB(I, 7) * 10) / 10
2005 RETURN

2100 SWAP TT$(CA, Z1), TT$(CA, Z)
     FOR MN = 1 TO 9
     SWAP TT(CA, Z1, MN), TT(CA, Z, MN)
     NEXT MN
     RETURN

2500 REM:TEAM CATEGORIES
     'CATEGORY 1 - TOTAL OFFENSE
     CA=1
     IF UCASE$(UP$) = "Y" AND (OT(6) + OT(2)) / Y% > TRC(0) THEN TRC(0) = (OT(6) + OT(2)) / Y%: TRC$(0) = TM$
     IF OT(6) + OT(2) > TRC(1) THEN TRC(1) = OT(6) + OT(2): TRC$(1) = TM$
     IF UCASE$(UP$) = "Y" AND (OT(6) + OT(2)) / (OT(1) + OT(3)) > TRC(2) THEN TRC(2) = (OT(6) + OT(2)) / (OT(1) + OT(3)): TRC$(2) = TM$
     IF (OT(6) + OT(2)) / Y% <= TT(CA, 20, 5) THEN 2506
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = OT(1) + OT(3)
     TT(CA, 20, 3) = OT(6) + OT(2)
     TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
     TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 5) >= TT(CA, Z1, 5) THEN 2505
     GOSUB 2100
2505 NEXT Z1: NEXT Z
2506 'CATEGORY 2 - TOTAL DEFENSE
     CA = 2
     IF UCASE$(UP$) = "Y" AND (DT(6) + DT(2)) / Y% < TRC(25) THEN TRC(25) = (DT(6) + DT(2)) / Y%: TRC$(25) = TM$
     IF UCASE$(UP$) = "Y" AND DT(6) + DT(2) < TRC(26) THEN TRC(26) = DT(6) + DT(2): TRC$(26) = TM$
     IF UCASE$(UP$) = "Y" AND (DT(6) + DT(2)) / (DT(1) + DT(3)) < TRC(27) THEN TRC(27) = (DT(6) + DT(2)) / (DT(1) + DT(3)): TRC$(27) = TM$
     IF UCASE$(UP$) = "Y" AND (DT(6) + DT(2)) / Y% > TRC(28) THEN TRC(28) = (DT(6) + DT(2)) / Y%: TRC$(28) = TM$
     IF (DT(6) + DT(2)) / Y% >= TT(CA, 20, 5) THEN 2507
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = DT(1) + DT(3)
     TT(CA, 20, 3) = DT(6) + DT(2)
     TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
     TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 5) <= TT(CA, Z1, 5) THEN 2510
     GOSUB 2100
2510 NEXT Z1: NEXT Z
2507 'CATEGORY 3 - RUSHING OFFENSE
     CA = 3
     IF UCASE$(UP$) = "Y" AND OT(2) / Y% > TRC(3) THEN TRC(3) = OT(2) / Y%: TRC$(3) = TM$
     IF OT(2) > TRC(4) THEN TRC(4) = OT(2): TRC$(4) = TM$
     IF UCASE$(UP$) = "Y" AND OT(2) / OT(1) > TRC(5) THEN TRC(5) = OT(2) / OT(1): TRC$(5) = TM$
     IF OT(2) / Y% <= TT(CA, 20, 5) THEN 2516
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = OT(1)
     TT(CA, 20, 3) = OT(2)
     TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
     TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 5) >= TT(CA, Z1, 5) THEN 2515
     GOSUB 2100
2515 NEXT Z1: NEXT Z
2516 'CATEGORY 4 - RUSHING DEFENSE
     CA = 4
     IF UCASE$(UP$) = "Y" AND DT(2) / Y% < TRC(29) THEN TRC(29) = DT(2) / Y%: TRC$(29) = TM$
     IF UCASE$(UP$) = "Y" AND DT(2) < TRC(30) THEN TRC(30) = DT(2): TRC$(30) = TM$
     IF UCASE$(UP$) = "Y" AND DT(2) / DT(1) < TRC(31) THEN TRC(31) = DT(2) / DT(1): TRC$(31) = TM$
     IF DT(2) / Y% >= TT(CA, 20, 5) THEN 2521
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = DT(1)
     TT(CA, 20, 3) = DT(2)
     TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
     TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 5) <= TT(CA, Z1, 5) THEN 2520
     GOSUB 2100
2520 NEXT Z1: NEXT Z
2521 'CATEGORY 5 - SCORING OFFENSE
     CA = 5
     PTS = 0: OPTS = 0
     FOR I = 1 TO Y%
     PTS = PTS + O%(I)
     OPTS = OPTS + T%(I)
     NEXT
     IF UCASE$(UP$) = "Y" AND PTS / Y% > TRC(13) THEN TRC(13) = PTS / Y%: TRC$(13) = TM$
     IF PTS > TRC(12) THEN TRC(12) = PTS: TRC$(12) = TM$
     IF UCASE$(UP$) = "Y" AND (PTS - OPTS) / Y% > TRC(14) THEN TRC(14) = (PTS - OPTS) / Y%: TRC$(14) = TM$
     IF PTS / Y% <= TT(CA, 20, 3) THEN 2526
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = PTS
     TT(CA, 20, 3) = TT(CA, 20, 2) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 3) >= TT(CA, Z1, 3) THEN 2525
     GOSUB 2100
2525 NEXT Z1: NEXT Z
2526 'CATEGORY 6 - SCORING DEFENSE
     CA = 6
     PTS = 0
     FOR I = 1 TO Y%
     PTS = PTS + T%(I)
     NEXT
     IF UCASE$(UP$) = "Y" AND PTS / Y% < TRC(39) THEN TRC(39) = PTS / Y%: TRC$(39) = TM$
     IF UCASE$(UP$) = "Y" AND PTS < TRC(38) THEN TRC(38) = PTS: TRC$(38) = TM$
     IF UCASE$(UP$) = "Y" AND PTS / Y% > TRC(41) THEN TRC(41) = PTS / Y%: TRC$(41) = TM$
     IF PTS > TRC(40) THEN TRC(40) = PTS: TRC$(40) = TM$
     IF PTS / Y% >= TT(CA, 20, 3) THEN 2531
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = PTS
     TT(CA, 20, 3) = TT(CA, 20, 2) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 3) <= TT(CA, Z1, 3) THEN 2530
     GOSUB 2100
2530 NEXT Z1: NEXT Z

2531 'CATEGORY 7 - PASSING OFFENSE
     CA = 7
     IF UCASE$(UP$) = "Y" AND OT(6) / Y% > TRC(6) THEN TRC(6) = OT(6) / Y%: TRC$(6) = TM$
     IF OT(6) > TRC(7) THEN TRC(7) = OT(6): TRC$(7) = TM$
     IF UCASE$(UP$) = "Y" AND OT(6) / OT(3) > TRC(8) THEN TRC(8) = OT(6) / OT(3): TRC$(8) = TM$
     IF UCASE$(UP$) = "Y" AND OT(6) / OT(4) > TRC(9) THEN TRC(9) = OT(6) / OT(4): TRC$(9) = TM$
     IF UCASE$(UP$) = "Y" AND OT(4) / OT(3) > TRC(10) THEN TRC(10) = OT(4) / OT(3): TRC$(10) = TM$
     IF UCASE$(UP$) = "Y" AND OT(5) / OT(3) < TRC(11) THEN TRC(11) = OT(5) / OT(3): TRC$(11) = TM$
     IF OT(6) / Y% <= TT(CA, 20, 8) THEN 2536
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = OT(3)
     TT(CA, 20, 3) = OT(4)
     TT(CA, 20, 4) = OT(5)
     TT(CA, 20, 5) = TT(CA, 20, 3) / TT(CA, 20, 2)
     TT(CA, 20, 6) = OT(6)
     TT(CA, 20, 7) = OT(6) / OT(3)
     TT(CA, 20, 8) = TT(CA, 20, 6) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 8) >= TT(CA, Z1, 8) THEN 2535
     GOSUB 2100
2535 NEXT Z1: NEXT Z
2536 'CATEGORY 8 - PASSING DEFENSE
     CA = 8
     IF UCASE$(UP$) = "Y" AND DT(6) / Y% < TRC(32) THEN TRC(32) = DT(6) / Y%: TRC$(32) = TM$
     IF UCASE$(UP$) = "Y" AND DT(6) < TRC(33) THEN TRC(33) = DT(6): TRC$(33) = TM$
     IF UCASE$(UP$) = "Y" AND DT(6) / DT(3) < TRC(34) THEN TRC(34) = DT(6) / DT(3): TRC$(34) = TM$
     IF UCASE$(UP$) = "Y" AND DT(6) / DT(4) < TRC(35) THEN TRC(35) = DT(6) / DT(4): TRC$(35) = TM$
     IF UCASE$(UP$) = "Y" AND DT(4) / DT(3) < TRC(36) THEN TRC(36) = DT(4) / DT(3): TRC$(36) = TM$
     IF UCASE$(UP$) = "Y" AND DT(5) / DT(3) > TRC(37) THEN TRC(37) = DT(5) / DT(3): TRC$(37) = TM$
     IF DT(6) / Y% >= TT(CA, 20, 8) THEN 2541
     TT(CA, 20, 1) = Y%
     TT(CA, 20, 2) = DT(3)
     TT(CA, 20, 3) = DT(4)
     TT(CA, 20, 4) = DT(5)
     TT(CA, 20, 5) = TT(CA, 20, 3) / TT(CA, 20, 2)
     TT(CA, 20, 6) = DT(6)
     TT(CA, 20, 7) = DT(6) / DT(3)
     TT(CA, 20, 8) = TT(CA, 20, 6) / Y%
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 8) <= TT(CA, Z1, 8) THEN 2540
     GOSUB 2100
2540 NEXT Z1: NEXT Z
2541 'CATEGORY 9 - PUNTING
     CA = 9
     IF UCASE$(UP$) = "Y" AND OT(8) / OT(7) > TRC(15) THEN TRC(15) = OT(8) / OT(7): TRC$(15) = TM$
     IF OT(8) / OT(7) <= TT(CA, 20, 2) THEN 2546
     TT(CA, 20, 1) = OT(7)
     TT(CA, 20, 2) = OT(8) / OT(7)
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 2) >= TT(CA, Z1, 2) THEN 2545
     GOSUB 2100
2545 NEXT Z1: NEXT Z
2546 'CATEGORY 10 - TURNOVER MARGIN
     CA = 10
     FG = OT(9) - OT(10) + DT(10)
     IG = DT(5)
     FL = OT(10) + DT(9) - DT(10)
     IL = OT(5)
     TM = (FG + IG - FL - IL) / Y%
     IF TM <= TT(CA, 20, 7) THEN 2599
     TT(CA, 20, 1) = FG
     TT(CA, 20, 2) = IG
     TT(CA, 20, 3) = FG + IG
     TT(CA, 20, 4) = FL
     TT(CA, 20, 5) = IL
     TT(CA, 20, 6) = FL + IL
     TT(CA, 20, 7) = TM
     TT$(CA, 20) = WT$
     FOR Z = 1 TO 19
     FOR Z1 = Z + 1 TO 20
     IF TT(CA, Z, 7) >= TT(CA, Z1, 7) THEN 2550
     GOSUB 2100
2550 NEXT Z1: NEXT Z
2599 REM:PUNT RETURNS
     IF UCASE$(UP$) = "Y" AND OT(25) / OT(24) > TRC(16) THEN TRC(16) = OT(25) / OT(24): TRC$(16) = TM$
     IF UCASE$(UP$) = "Y" AND DT(25) / DT(24) < TRC(42) THEN TRC(42) = DT(25) / DT(24): TRC$(42) = TM$
     REM:KO RETURN
     IF UCASE$(UP$) = "Y" AND OT(23) / OT(22) > TRC(17) THEN TRC(17) = OT(23) / OT(22): TRC$(17) = TM$
     IF UCASE$(UP$) = "Y" AND DT(23) / DT(22) < TRC(43) THEN TRC(43) = DT(23) / DT(22): TRC$(43) = TM$
     REM:FIRST DOWNS
     IF OT(0) > TRC(18) THEN TRC(18) = OT(0): TRC$(18) = TM$
     IF UCASE$(UP$) = "Y" AND OT(0) / Y% > TRC(19) THEN TRC(19) = OT(0) / Y%: TRC$(19) = TM$
     REM:FUMBLES
     IF OT(9) > TRC(20) THEN TRC(20) = OT(9): TRC$(20) = TM$
     IF OT(10) > TRC(21) THEN TRC(21) = OT(10): TRC$(21) = TM$
     IF UCASE$(UP$) = "Y" AND OT(10) < TRC(22) THEN TRC(22) = OT(10): TRC$(22) = TM$
     REM:PENALTIES
     IF UCASE$(UP$) = "Y" AND OT(11) / Y% > TRC(23) THEN TRC(23) = OT(11) / Y%: TRC$(23) = TM$
     IF UCASE$(UP$) = "Y" AND OT(12) / Y% > TRC(24) THEN TRC(24) = OT(12) / Y%: TRC$(24) = TM$
     RETURN


3018 OPEN D$(2) + WE$ + ".LIB" FOR INPUT AS #2
     INPUT #2, LG%
     CLOSE 2
     IF LG% <> 0 THEN 3095
     OPEN D$(2) + WE$ + ".TMS" FOR INPUT AS #2
     INPUT #2, FF$
     INPUT #2, D1$:INPUT #2, D2$
     FOR X = 1 TO 30
     INPUT #2, Z1$(X)
     NEXT X
     CLOSE 2
     RETURN


3095 CLS
     PRINT "ERROR:: THIS IS NOT A COLLEGE CONFERENCE"
     PRINT "        RETURNING TO MAIN MENU"
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
3096 I$ = INKEY$: IF I$ = "" THEN 3096
     GOTO 99


3100 CLS
     GOSUB 2500
     FOR X = 1 TO 10
     PRINT "   CHECKING STATS OF "; RB$(X)
     IF RB$(X) = "OTHERS" THEN 3165
     IF RB(X, 1) > BRC(0) THEN BRC(0) = RB(X, 1): BRC$(0, 0) = RB$(X): BRC$(0, 1) = TM$
     IF RB(X, 2) > BRC(1) THEN BRC(1) = RB(X, 2): BRC$(1, 0) = RB$(X): BRC$(1, 1) = TM$
     IF RB(X, 3) > BRC(2) THEN BRC(2) = RB(X, 3): BRC$(2, 0) = RB$(X): BRC$(2, 1) = TM$
     IF RB(X, 9) > BRC(3) AND UCASE$(UP$) = "Y" AND RB(X, 1) >= Y% * 10 THEN BRC(3) = RB(X, 9): BRC$(3, 0) = RB$(X): BRC$(3, 1) = TM$
     IF LR(X - 1) > BRC(4) THEN BRC(4) = LR(X - 1): BRC$(4, 0) = RB$(X): BRC$(4, 1) = TM$: BRC$(4, 2) = LR$(X - 1, 0)
     PS$(X) = WT$
     FOR Y = 1 TO 20: IF Z2$(Y) = RB$(X) AND Z3$(Y) = WT$ THEN GOSUB 4818: GOTO 3118
     NEXT Y
     IF RB(X, 1) < Y% * 10 OR RB(X, 9) < Z1(20, 1) THEN 3118
     Z2$(20) = RB$(X)
     Z3$(20) = WT$: Z1(20, 1) = RB(X, 9): Z1(20, 2) = RB(X, 1): GOSUB 4820
3118 FOR Z = 41 TO 50
     IF Z2$(Z) = RB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = INT(RB(X, 2)): GOTO 3123
     NEXT Z: IF INT(RB(X, 2)) <= Z1(50, 1) THEN 3150
     Z1(50, 1) = INT(RB(X, 2)): Z2$(50) = RB$(X)
     Z3$(50) = WT$
3123 FOR Z = 41 TO 49: FOR Z1 = Z + 1 TO 50: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3130
     GOSUB 3450
3130 NEXT Z1: NEXT Z
3150 FOR Z = 61 TO 70
     IF Z2$(Z) = RB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = RB(X, 3): GOTO 3155
     NEXT Z: IF RB(X, 3) <= Z1(70, 1) THEN 3162
     Z1(70, 1) = RB(X, 3): Z2$(70) = RB$(X)
     Z3$(70) = WT$
3155 FOR Z = 61 TO 69: FOR Z1 = Z + 1 TO 70: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3159
     GOSUB 3450
3159 NEXT Z1: NEXT Z
3162 GOSUB 3520
3165 NEXT X


     FOR X = 1 TO 16: PRINT "   CHECKING STATS OF "; WR$(X)
     IF WR$(X) = "OTHERS" THEN 3237
     IF WR(X, 1) > BRC(5) THEN BRC(5) = WR(X, 1): BRC$(5, 0) = WR$(X): BRC$(5, 1) = TM$
     IF WR(X, 2) > BRC(6) THEN BRC(6) = WR(X, 2): BRC$(6, 0) = WR$(X): BRC$(6, 1) = TM$
     IF WR(X, 3) > BRC(7) THEN BRC(7) = WR(X, 3): BRC$(7, 0) = WR$(X): BRC$(7, 1) = TM$
     IF WR(X, 4) > BRC(8) AND UCASE$(UP$) = "Y" AND WR(X, 1) >= Y% * 3 THEN BRC(8) = WR(X, 4): BRC$(8, 0) = WR$(X): BRC$(8, 1) = TM$
     IF LC(X - 1) > BRC(9) THEN BRC(9) = LC(X - 1): BRC$(9, 0) = WR$(X): BRC$(9, 1) = TM$: BRC$(9, 2) = LC$(X - 1, 0)
     FOR Z = 21 TO 40: IF Z2$(Z) = WR$(X) AND Z3$(Z) = WT$ THEN GOSUB 4825: GOTO 3178
     NEXT Z
     IF WR(X, 1) < Y% * 3 OR WR(X, 4) < Z1(40, 1) THEN 3178
     Z2$(40) = WR$(X)
     Z3$(40) = WT$: Z1(40, 1) = WR(X, 4): Z1(40, 2) = WR(X, 1): GOSUB 4827
3178 FOR Z = 51 TO 60
     IF Z2$(Z) = WR$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = INT(WR(X, 2)): GOTO 3183
     NEXT Z: IF INT(WR(X, 2)) <= Z1(60, 1) THEN 3210
     Z1(60, 1) = INT(WR(X, 2)): Z2$(60) = WR$(X)
     Z3$(60) = WT$
3183 FOR Z = 51 TO 59: FOR Z1 = Z + 1 TO 60: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3190
     GOSUB 3450
3190 NEXT Z1: NEXT Z
3210 FOR Z = 91 TO 100
     IF Z2$(Z) = WR$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = WR(X, 1): GOTO 3215
     NEXT Z: IF WR(X, 1) <= Z1(100, 1) THEN 3225
     Z1(100, 1) = WR(X, 1): Z2$(100) = WR$(X)
     Z3$(100) = WT$
3215 FOR Z = 91 TO 99: FOR Z1 = Z + 1 TO 100: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3219
     GOSUB 3450
3219 NEXT Z1: NEXT Z
3225 FOR Z = 71 TO 80
     IF Z2$(Z) = WR$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = WR(X, 3): GOTO 3230
     NEXT Z: IF WR(X, 3) <= Z1(80, 1) THEN 3237
     Z1(80, 1) = WR(X, 3): Z2$(80) = WR$(X)
     Z3$(80) = WT$
3230 FOR Z = 71 TO 79: FOR Z1 = Z + 1 TO 80: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3234
     GOSUB 3450
3234 NEXT Z1: NEXT Z
3237 NEXT X


     FOR X = 1 TO 16: PRINT "   CHECKING STATS OF "; TB$(X)
     IF TB$(X) = "OTHERS" THEN 3267
     IF YD(X) > BRC(18) THEN BRC(18) = YD(X): BRC$(18, 0) = TB$(X): BRC$(18, 1) = TM$
     IF TP(X) > BRC(19) THEN BRC(19) = TP(X): BRC$(19, 0) = TB$(X): BRC$(19, 1) = TM$
     IF TP(X) > BRC(21) THEN BRC(21) = TP(X): BRC$(21, 0) = TB$(X): BRC$(21, 1) = TM$
     FOR Z = 101 TO 110
     IF Z2$(Z) = TB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = YD(X): GOTO 3245
     NEXT Z: IF YD(X) <= Z1(110, 1) THEN 3255
     Z1(110, 1) = YD(X): Z2$(110) = TB$(X)
     Z3$(110) = WT$
3245 FOR Z = 101 TO 109: FOR Z1 = Z + 1 TO 110: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3249
     GOSUB 3450
3249 NEXT Z1: NEXT Z
3255 FOR Z = 111 TO 120
     IF Z2$(Z) = TB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = TP(X): GOTO 3260
     NEXT Z: IF TP(X) <= Z1(120, 1) THEN 3267
     Z1(120, 1) = TP(X): Z2$(120) = TB$(X)
     Z3$(120) = WT$
3260 FOR Z = 111 TO 119: FOR Z1 = Z + 1 TO 120: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3264
     GOSUB 3450
3264 NEXT Z1: NEXT Z
3267 NEXT X


     GOSUB 3500
     FOR X = 1 TO 4
     PRINT "   CHECKING STATS OF "; QB$(X)
     IF QB$(X) = "OTHERS" THEN 3315
     IF QB(X, 1) > BRC(10) THEN BRC(10) = QB(X, 1): BRC$(10, 0) = QB$(X): BRC$(10, 1) = TM$
     IF QB(X, 2) > BRC(11) THEN BRC(11) = QB(X, 2): BRC$(11, 0) = QB$(X): BRC$(11, 1) = TM$
     IF QB(X, 5) > BRC(12) THEN BRC(12) = QB(X, 5): BRC$(12, 0) = QB$(X): BRC$(12, 1) = TM$
     IF QB(X, 3) > BRC(13) THEN BRC(13) = QB(X, 3): BRC$(13, 0) = QB$(X): BRC$(13, 1) = TM$
     IF QB(X, 4) > BRC(14) THEN BRC(14) = QB(X, 4): BRC$(14, 0) = QB$(X): BRC$(14, 1) = TM$
     IF QB(X, 6) > BRC(15) AND UCASE$(UP$) = "Y" AND QB(X, 1) >= Y% * 15 THEN BRC(15) = QB(X, 6): BRC$(15, 0) = QB$(X): BRC$(15, 1) = TM$
     IF QB(X, 7) > BRC(16) AND UCASE$(UP$) = "Y" AND QB(X, 1) >= Y% * 15 THEN BRC(16) = QB(X, 7): BRC$(16, 0) = QB$(X): BRC$(16, 1) = TM$
     IF LP(X - 1) > BRC(17) THEN BRC(17) = LP(X - 1): BRC$(17, 0) = QB$(X): BRC$(17, 1) = TM$: BRC$(17, 2) = LP$(X - 1, 0)
     FOR Y = 121 TO 130: IF Z2$(Y) = QB$(X) AND Z3$(Y) = WT$ THEN GOSUB 4832: GOTO 3279
     NEXT Y
     IF QB(X, 1) < Y% * 15 OR QB(X, 6) < Z1(130, 1) THEN 3279
     Z2$(130) = QB$(X)
     Z3$(130) = WT$: Z1(130, 1) = QB(X, 6): Z1(130, 2) = QB(X, 1): GOSUB 4834
3279 FOR Y = 131 TO 140: IF Z2$(Y) = QB$(X) AND Z3$(Y) = WT$ THEN GOSUB 4839: GOTO 3287
     NEXT Y
     IF QB(X, 1) < Y% * 15 OR QB(X, 7) < Z1(140, 1) THEN 3287
     Z2$(140) = QB$(X)
     Z3$(140) = WT$: Z1(140, 1) = QB(X, 7): Z1(140, 2) = QB(X, 1): GOSUB 4841
3287 FOR Z = 141 TO 150
     IF Z2$(Z) = QB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = INT(QB(X, 4)): GOTO 3294
     NEXT Z: IF INT(QB(X, 4)) <= Z1(150, 1) THEN 3302
     Z1(150, 1) = INT(QB(X, 4)): Z2$(150) = QB$(X)
3294 Z3$(150) = WT$
     FOR Z = 141 TO 149: FOR Z1 = Z + 1 TO 150: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3299
     GOSUB 3450
3299 NEXT Z1: NEXT Z
3302 FOR Z = 151 TO 160
     IF Z2$(Z) = QB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = QB(X, 5): GOTO 3307
     NEXT Z: IF QB(X, 5) <= Z1(160, 1) THEN 3315
     Z1(160, 1) = QB(X, 5): Z2$(160) = QB$(X)
     Z3$(160) = WT$
3307 FOR Z = 151 TO 159: FOR Z1 = Z + 1 TO 160: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3311
     GOSUB 3450
3311 NEXT Z1: NEXT Z
3315 NEXT X


     FOR X = 1 TO 3
     PRINT "   CHECKING STATS OF "; KR$(X)
     IF KR$(X) = "OTHERS" THEN 3355
     IF KR(X, 1) > BRC(22) THEN BRC(22) = KR(X, 1): BRC$(22, 0) = KR$(X): BRC$(22, 1) = TM$
     IF KR(X, 2) > BRC(23) THEN BRC(23) = KR(X, 2): BRC$(23, 0) = KR$(X): BRC$(23, 1) = TM$
     IF KR(X, 3) > BRC(24) THEN BRC(24) = KR(X, 3): BRC$(24, 0) = KR$(X): BRC$(24, 1) = TM$
     IF KR(X, 4) > BRC(25) AND UCASE$(UP$) = "Y" AND KR(X, 1) >= Y% * 1.2 THEN BRC(25) = KR(X, 4): BRC$(25, 0) = KR$(X): BRC$(25, 1) = TM$
     IF LK(X - 1) > BRC(26) THEN BRC(26) = LK(X - 1): BRC$(26, 0) = KR$(X): BRC$(26, 1) = TM$: BRC$(26, 2) = LK$(X - 1, 0)
     FOR Z = 161 TO 170
     IF Z2$(Z) = KR$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = KR(X, 4): Z1(Z, 2) = KR(X, 1): GOTO 3345
     NEXT Z
     IF KR(X, 1) < Y% * 1.2 OR KR(X, 4) <= Z1(170, 1) THEN 3355
     Z1(170, 1) = KR(X, 4): Z2$(170) = KR$(X)
     Z3$(170) = WT$: Z1(170, 2) = KR(X, 1)
3345 FOR Z = 161 TO 169: FOR Z1 = Z + 1 TO 170: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3349
     GOSUB 3455
3349 NEXT Z1: NEXT Z
3355 NEXT X


     FOR X = 1 TO 3
     PRINT "   CHECKING STATS OF "; PR$(X)
     IF PR$(X) = "OTHERS" THEN 3375
     IF PR(X, 1) > BRC(27) THEN BRC(27) = PR(X, 1): BRC$(27, 0) = PR$(X): BRC$(27, 1) = TM$
     IF PR(X, 2) > BRC(28) THEN BRC(28) = PR(X, 2): BRC$(28, 0) = PR$(X): BRC$(28, 1) = TM$
     IF PR(X, 3) > BRC(29) THEN BRC(29) = PR(X, 3): BRC$(29, 0) = PR$(X): BRC$(29, 1) = TM$
     IF PR(X, 4) > BRC(30) AND UCASE$(UP$) = "Y" AND PR(X, 1) >= Y% * 1.2 THEN BRC(30) = PR(X, 4): BRC$(30, 0) = PR$(X): BRC$(30, 1) = TM$
     IF PK(X - 1) > BRC(31) THEN BRC(31) = PK(X - 1): BRC$(31, 0) = PR$(X): BRC$(31, 1) = TM$: BRC$(31, 2) = PKR$(X - 1, 0)
     FOR Z = 171 TO 180
     IF Z2$(Z) = PR$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = PR(X, 4): Z1(Z, 2) = PR(X, 1): GOTO 3365
     NEXT Z
     IF PR(X, 1) < Y% * 1.2 OR PR(X, 4) <= Z1(180, 1) THEN 3375
     Z1(180, 1) = PR(X, 4): Z2$(180) = PR$(X)
     Z3$(180) = WT$: Z1(180, 2) = PR(X, 1)
3365 FOR Z = 171 TO 179: FOR Z1 = Z + 1 TO 180: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3369
     GOSUB 3455
3369 NEXT Z1: NEXT Z
3375 NEXT X


     PRINT "   CHECKING STATS OF "; PU$
     IF TG(8) > BRC(32) THEN BRC(32) = TG(8): BRC$(32, 0) = PU$: BRC$(32, 1) = TM$
     IF TG(9) > BRC(33) THEN BRC(33) = TG(9): BRC$(33, 0) = PU$: BRC$(33, 1) = TM$
     IF PN > BRC(34) AND UCASE$(UP$) = "Y" THEN BRC(34) = PN: BRC$(34, 0) = PU$: BRC$(34, 1) = TM$
     FOR Z = 181 TO 190
     IF Z2$(Z) = PU$ AND Z3$(Z) = WT$ THEN Z1(Z, 1) = PN: Z1(Z, 2) = TG(8): GOTO 3382
     NEXT Z: IF PN <= Z1(190, 1) THEN 3390
     Z1(190, 1) = PN: Z2$(190) = PU$
     Z3$(190) = WT$: Z1(190, 2) = TG(8)
3382 FOR Z = 181 TO 189: FOR Z1 = Z + 1 TO 190: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3386
     GOSUB 3455
3386 NEXT Z1: NEXT Z


3390 FOR X = 1 TO 2
     PRINT "   CHECKING STATS OF "; PK$(X)
     IF FM(X) > BRC(35) THEN BRC(35) = FM(X): BRC$(35, 0) = PK$(X): BRC$(35, 1) = TM$
     IF FA(X) > BRC(36) THEN BRC(36) = FA(X): BRC$(36, 0) = PK$(X): BRC$(36, 1) = TM$
     IF FG(X) > BRC(37) AND UCASE$(UP$) = "Y" THEN BRC(37) = FG(X): BRC$(37, 0) = PK$(X): BRC$(37, 1) = TM$
     IF LF(X - 1) > BRC(38) THEN BRC(38) = LF(X - 1): BRC$(38, 0) = PK$(X): BRC$(38, 1) = TM$: BRC$(38, 2) = LF$(X - 1)
     IF EM(X) > BRC(39) THEN BRC(39) = EM(X): BRC$(39, 0) = PK$(X): BRC$(39, 1) = TM$
     IF EA(X) > BRC(40) THEN BRC(40) = EA(X): BRC$(40, 0) = PK$(X): BRC$(40, 1) = TM$
     IF EP(X) > BRC(41) AND UCASE$(UP$) = "Y" THEN BRC(41) = EP(X): BRC$(41, 0) = PK$(X): BRC$(41, 1) = TM$
     FOR Z = 201 TO 210
     IF Z2$(Z) = PK$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = FG: Z1(Z, 2) = FA(X): GOTO 3396
     NEXT Z: IF FG(X) <= Z1(210, 1) THEN 3403
     Z1(210, 1) = FG(X): Z2$(210) = PK$(X)
     Z3$(210) = WT$: Z1(210, 2) = FA(X)
3396 FOR Z = 201 TO 209: FOR Z1 = Z + 1 TO 210: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3400
     GOSUB 3455
3400 NEXT Z1: NEXT Z
3403 REM:PRINT"   CHECKING STATS OF ";EP$
     FOR Z = 211 TO 220
     IF Z2$(Z) = PK$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = EP(X): Z1(Z, 2) = EA(X): GOTO 3409
     NEXT Z: IF EP(X) <= Z1(220, 1) THEN 3416
     Z1(220, 1) = EP(X): Z2$(220) = PK$(X)
     Z3$(220) = WT$: Z1(220, 2) = EA(X)
3409 FOR Z = 211 TO 219: FOR Z1 = Z + 1 TO 220: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3413
     GOSUB 3455
3413 NEXT Z1: NEXT Z
3416 NEXT X

     PRINT "   CHECKING TEAM SACKS AND INTERCEPTIONS"
     FOR X = 0 TO 9
     IF LEFT$(DI$(X), 1) = " " OR DI$(X) = "" OR DI$(X) = "END" THEN 3427
     IF GI%(X, 0) > BRC(42) THEN BRC(42) = GI%(X, 0): BRC$(42, 0) = DI$(X): BRC$(42, 1) = TM$
     IF GI%(X, 1) > BRC(43) THEN BRC(43) = GI%(X, 1): BRC$(43, 0) = DI$(X): BRC$(43, 1) = TM$
     IF GI%(X, 2) > BRC(44) THEN BRC(44) = GI%(X, 2): BRC$(44, 0) = DI$(X): BRC$(44, 1) = TM$
     IF LI(X - 1) > BRC(45) THEN BRC(45) = LI(X - 1): BRC$(45, 0) = DI$(X): BRC$(45, 1) = TM$: BRC$(45, 2) = LI$(X - 1, 0)
     FOR Z = 221 TO 230
     IF Z2$(Z) = DI$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = GI%(X, 0): GOTO 3422
     NEXT Z: IF GI%(X, 0) <= Z1(230, 1) THEN 3427
     Z1(230, 1) = GI%(X, 0): Z2$(230) = DI$(X)
     Z3$(230) = WT$
3422 FOR Z = 221 TO 229: FOR Z1 = Z + 1 TO 230: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3426
     GOSUB 3450
3426 NEXT Z1: NEXT Z
3427 NEXT X


     FOR X = 0 TO 14
     IF LEFT$(DS$(X), 1) = " " OR DS$(X) = "" OR DS$(X) = "END" THEN 3439
     IF GS%(X, 0) > BRC(46) THEN BRC(46) = GS%(X, 0): BRC$(46, 0) = DS$(X): BRC$(46, 1) = TM$
     FOR Z = 231 TO 240
     IF Z2$(Z) = DS$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = GS%(X, 0): GOTO 3434
     NEXT Z: IF GS%(X, 0) <= Z1(240, 1) THEN 3439
     Z1(240, 1) = GS%(X, 0): Z2$(240) = DS$(X)
     Z3$(240) = WT$
3434 FOR Z = 231 TO 239: FOR Z1 = Z + 1 TO 240: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3438
     GOSUB 3450
3438 NEXT Z1: NEXT Z
3439 NEXT X


     RETURN

3450 SWAP Z2$(Z1), Z2$(Z)
     SWAP Z3$(Z1), Z3$(Z)
     SWAP Z1(Z1,1), Z1(Z,1)
     RETURN

3455 SWAP Z2$(Z1), Z2$(Z)
     SWAP Z3$(Z1), Z3$(Z)
     FOR MN = 1 TO 2
     SWAP Z1(Z1, MN), Z1(Z, MN)
     NEXT MN
     RETURN

3500 FOR X = 17 TO 18
     IF TP(X) > BRC(20) THEN BRC(20) = TP(X): BRC$(20, 0) = TB$(X): BRC$(20, 1) = TM$
     IF TP(X) > BRC(21) THEN BRC(21) = TP(X): BRC$(21, 0) = TB$(X): BRC$(21, 1) = TM$
     FOR Z = 191 TO 200
     IF Z2$(Z) = TB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = TP(X): GOTO 3505
     NEXT Z: IF TP(X) <= Z1(200, 1) THEN 3512
     Z1(200, 1) = TP(X): Z2$(200) = TB$(X)
     Z3$(200) = WT$
3505 FOR Z = 191 TO 199: FOR Z1 = Z + 1 TO 200: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3509
     GOSUB 3450
3509 NEXT Z1: NEXT Z
3512 NEXT X
     RETURN

3520 FOR Z = 81 TO 90
     IF Z2$(Z) = RB$(X) AND Z3$(Z) = WT$ THEN Z1(Z, 1) = RB(X, 1): GOTO 3525
     NEXT Z: IF RB(X, 1) <= Z1(90, 1) THEN RETURN
     Z1(90, 1) = RB(X, 1): Z2$(90) = RB$(X)
     Z3$(90) = WT$
3525 FOR Z = 81 TO 89: FOR Z1 = Z + 1 TO 90: IF Z1(Z, 1) >= Z1(Z1, 1) THEN 3529
     GOSUB 3450
3529 NEXT Z1: NEXT Z
     RETURN


4000 IF UCASE$(LEFT$(D$(2), 1)) <> "A" AND UCASE$(LEFT$(D$(2), 1)) <> "B" THEN 4005
     CLS : PRINT "INSERT DISK WITH LEAGUE LEADER FILES IN DRIVE "; D$(2)
     LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE";
4002 I$ = INKEY$: IF I$ = "" THEN 4002
4005 CLS
     OPEN D$(2) + WE$ + ".LDR" FOR OUTPUT AS #2
     PRINT "POSTING "; WE$; " CONFERENCE LEADERS"
     FOR X = 1 TO 60: WRITE #2, Z1$(X), O1(X), O2(X), O3(X), PTSF(X), PTSA(X): NEXT
     FOR X = 1 TO 240
     WRITE #2, Z2$(X), Z3$(X), Z1(X, 1), Z1(X, 2): NEXT X
     FOR I = 1 TO 10
     FOR I1 = 1 TO 20
     FOR I2 = 1 TO 9
     WRITE #2, TT(I, I1, I2)
     NEXT:NEXT:NEXT
     FOR I = 1 TO 10
     FOR I1 = 1 TO 20
     WRITE #2, TT$(I, I1)
     NEXT:NEXT
     CLOSE 2

     OPEN D$(1) + "RECORDS.CFB" FOR OUTPUT AS #1
     FOR I = 0 TO 50: PRINT #1, BRC(I):PRINT #1, BRC$(I, 0):PRINT #1, BRC$(I, 1)
   PRINT #1, BRC$(I, 2)
   NEXT
   FOR I = 0 TO 50: PRINT #1, TRC(I):PRINT #1, TRC$(I): NEXT
   CLOSE 1

     RETURN

4818 Z1(Y, 1) = RB(X, 9): Z1(Y, 2) = RB(X, 1): IF Z1(Y, 2) >= Y% * 10 THEN 4820
     Z2$(Y) = "*": Z3$(Y) = "*": Z1(Y, 1) = 0: Z1(Y, 2) = 0
4820 FOR Y = 1 TO 19: FOR Z = Y + 1 TO 20: IF Z1(Y, 1) >= Z1(Z, 1) THEN 4824
     TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
     TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
     FOR Z1 = 1 TO 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: NEXT Z1
4824 NEXT Z: NEXT Y: RETURN
4825 Z1(Z, 1) = WR(X, 4): Z1(Z, 2) = WR(X, 1): IF Z1(Z, 2) >= Y% * 3 THEN 4827
     Z2$(Z) = "*": Z3$(Z) = "*": Z1(Z, 1) = 0: Z1(Z, 2) = 0
4827 FOR Y = 21 TO 39: FOR Z = Y + 1 TO 40: IF Z1(Y, 1) >= Z1(Z, 1) THEN 4831
     TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
     TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
     FOR Z1 = 1 TO 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: NEXT Z1
4831 NEXT Z: NEXT Y: RETURN
4832 Z1(Y, 1) = QB(X, 6): Z1(Y, 2) = QB(X, 1): IF Z1(Y, 2) >= Y% * 15 THEN 4834
     Z2$(Y) = "*": Z3$(Y) = "*": Z1(Y, 1) = 0: Z1(Y, 2) = 0
4834 FOR Y = 121 TO 129: FOR Z = Y + 1 TO 130: IF Z1(Y, 1) >= Z1(Z, 1) THEN 4838
     TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
     TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
     FOR Z1 = 1 TO 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: NEXT Z1
4838 NEXT Z: NEXT Y: RETURN
4839 Z1(Y, 1) = QB(X, 7): Z1(Y, 2) = QB(X, 1): IF Z1(Y, 2) >= Y% * 15 THEN 4841
     Z2$(Y) = "*": Z3$(Y) = "*": Z1(Y, 1) = 0: Z1(Y, 2) = 0
4841 FOR Y = 131 TO 139: FOR Z = Y + 1 TO 140: IF Z1(Y, 1) >= Z1(Z, 1) THEN 4845
     TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
     TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
     FOR Z1 = 1 TO 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: NEXT Z1
4845 NEXT Z: NEXT Y: RETURN
     L = LEN(TN$): WX$ = "": FOR X = 1 TO (L - 3): A = ASC(MID$(TN$, X, 1))
     WT$ = WT$ + CHR$(A): NEXT X
     TN$ = WT$: RETURN

5500 PTSFC = 0: PTSAC = 0: FOR I = 1 TO Y%
     GOSUB 5600
     FOR EC = 1 TO 30: IF O$(I) = H1$(EC) THEN 5511
     NEXT EC
     IF O%(I) > T%(I) THEN WN = WN + 1
     IF O%(I) < T%(I) THEN LS = LS + 1
     IF O%(I) = T%(I) THEN TY = TY + 1
     GOTO 5520
5511 IF O%(I) > T%(I) THEN WN = WN + 1: CW = CW + 1
     IF O%(I) < T%(I) THEN LS = LS + 1: CL = CL + 1
     IF O%(I) = T%(I) THEN TY = TY + 1: CT = CT + 1
     PTSFC = PTSFC + O%(I): PTSAC = PTSAC + T%(I)
5520 EC = 0: NEXT I
     RETURN

5600 FOR P = 4 TO 11: IF MID$(O$(I), P, 1) = " " OR MID$(O$(I), P, 1) = "." THEN MID$(O$(I), P, 1) = "_"
     NEXT P
     FOR P = 12 TO 14: IF MID$(O$(I), P, 2) = "ST" THEN MID$(O$(I), 9, 1) = "_": MID$(O$(I), 10, 2) = "ST"
     NEXT P
     QQ = 8: FOR P = 11 TO 4 STEP -1: IF MID$(O$(I), P, 1) = "_" THEN QQ = P - 4: GOTO 5606
     GOTO 5608
5606 NEXT P
5608 f$ = MID$(O$(I), 4, QQ): R$ = f$ + "." + LEFT$(O$(I), 2)
     O$(I) = R$
     RETURN

9131 H1$(XW) = RIGHT$(H1$(XW), 2) + " " + LEFT$(H1$(XW), LEN(H1$(XW)) - 3): GOSUB 9900: RETURN
9900 FOR XX = 4 TO 11: IF MID$(H1$(XW), XX, 1) = " " OR MID$(H1$(XW), XX, 1) = "." THEN MID$(H1$(XW), XX, 1) = "_"
     NEXT XX
     FOR XX = 12 TO 14: IF MID$(H1$(XW), XX, 2) = "ST" THEN MID$(H1$(XW), 9, 1) = "_": MID$(H1$(XW), 10, 2) = "ST"
     NEXT XX
     QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(H1$(XW), XX, 1) = "_" THEN QQ = XX - 4: GOTO 9906
     GOTO 9907
9906 NEXT XX
9907 G$ = H1$(XW): H1$(XW) = RIGHT$(H1$(XW), LEN(H1$(XW)) - 3): YR$ = LEFT$(G$, 2)
     IF LEN(H1$(XW)) > 8 THEN H1$(XW) = LEFT$(H1$(XW), 8)
     H1$(XW) = H1$(XW) + "." + YR$: RETURN

4600 JJ = 4
     LOCATE 6, 8: COLOR 15, 1
     PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
     FOR II = 1 TO JJ: LOCATE 6 + II, 8
     PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 7 + JJ, 8
     PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
     RETURN

4500 LOCATE 10, 8
     GOSUB 4600
     PRINT "NO LEAGUE FILES WERE FOUND"
     PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     GOTO 2900


SUB GETKEY ()
    LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
1240 I$ = INKEY$: IF I$ = "" THEN 1240
END SUB
