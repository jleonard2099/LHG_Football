DECLARE SUB TITLE ()
   DECLARE FUNCTION QPRTrim$ (Work$)
   DECLARE FUNCTION EXIST% (FILENAME$)
   DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE SUB box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
   DECLARE FUNCTION GetDrive% ()
  DIM VV$(500), F$(500), V%(500), C2(15), C3(15)
  DIM A0%(9), A1%(9), A2%(9), A3%(9), A4%(5), A5%(5), A6%(3), A7%(3), A8%(3), A9%(2)
  DIM C0%(9), C1%(9), C2%(15), C3%(9), C4%(5), C5%(5), C6%(3), C7%(3), C8%(3), C9%(2)
  DIM B%(2), B1%(2), B3%(2), B6%(7), Q6%(1), B4%(1), B5%(1), B7%(1), B8%(1)
  DIM D4%(1), D5%(1), D7%(1), D8%(1), QX%(3), D2%(2), FI$(9), FI%(9), DI$(9), DI%(9)
  DIM D%(2), D1%(2), D3%(2), D6%(7), D$(1), item$(9), PA%(4), QA%(4), SX%(3)
  DIM B2%(2), T1%(1), K3%(1, 6), Q$(300), FS$(14), FS%(14), DS$(14), DS%(14)
  DIM A$(1), A1$(9), A2$(5), A3$(3), A4$(2), A5$(2), A6$(2), A7$(1)
  DIM C$(1), C1$(9), C2$(5), C3$(3), C4$(2), C5$(2), C6$(2), C7$(1)
10  CLEAR
   OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): CLOSE #1
   D% = GetDrive%
   CALL TITLE
   item$(1) = "DRAFT A NEW TEAM         "
   item$(2) = "DRAFT TO AN EXISTING TEAM"
   item$(3) = "EXIT TO MAIN MENU        "
     SELECTION% = 1
     Count% = 3
     Start% = 1
     SCANCODE% = 3
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     CALL box0(7, 24, 11, 50, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 OR SELECTION% = 3 THEN 1
     IF SELECTION% = 1 THEN 205
     IF SELECTION% = 2 THEN GOSUB 530: GOSUB 1200: GOTO 600

1   IF D% >= 67 THEN RUN "HELLO.EXE 2"
    CLS : PRINT "MAKE SURE PROGRAM DISK IS IN DRIVE"
15 I$ = INKEY$: IF I$ = "" THEN 15
   RUN "HELLO.EXE 2"


205  CALL TITLE: GOSUB 9900: LOCATE 1, 1: COLOR 15, 0
400 LOCATE 3: INPUT " 1. ENTER DISK ID FOR THIS TEAM "; YN$: IF C1 = 1 THEN 420
401 LOCATE 4: INPUT " 2. TEAM # (0 FOR PROGRAM TO ASSIGN) "; NU: IF C1 = 1 THEN 420
402 LOCATE 5: INPUT " 3. TEAM NAME "; A$: IF C1 = 1 THEN 420
403 LOCATE 6: INPUT " 4. TEAM NICKNAME "; AA$: IF C1 = 1 THEN 420
404 LOCATE 7: INPUT " 5. DEF RUSH ADJ "; B6%(0): IF C1 = 1 THEN 420
405 LOCATE 8: INPUT " 6. DEF COMP % ADJ "; B6%(1): IF C1 = 1 THEN 420
406 LOCATE 9: INPUT " 7. DEF INT % ADJ "; B6%(2): IF C1 = 1 THEN 420
407 LOCATE 10: INPUT " 8. FUM % "; B6%(3): IF C1 = 1 THEN 420
408 LOCATE 11: INPUT " 9. DEF FUM % "; B6%(4): IF C1 = 1 THEN 420
409 LOCATE 12: INPUT "10. OFF SACKED % "; B6%(5): IF C1 = 1 THEN 420
410 LOCATE 13: INPUT "11. DEF SACK ADJ "; B6%(6): IF C1 = 1 THEN 420
411 LOCATE 14: INPUT "12. DEF YD/COMP ADJ "; Q: IF C1 = 1 THEN 420
412 LOCATE 15: INPUT "13. TM PENALTY ADJ "; PA%(0): IF C1 = 1 THEN 420
413 LOCATE 16: INPUT "14. OFF RUN/PASS % "; B6%(7): IF C1 = 1 THEN 420
414 LOCATE 17: INPUT "15. TM STADIUM (0-OUTDOOR 1-DOME) "; PA%(3): IF C1 = 1 THEN 420
415 LOCATE 18: INPUT "16. POWER RATING "; PA%(4): IF C1 = 1 THEN 420
416 LOCATE 19: INPUT "17. TM BACKGROUND COLOR (0-7) "; PA%(1): IF C1 = 1 THEN 420
417 LOCATE 20: INPUT "18. TM TEXT COLOR (0-15) "; PA%(2): IF C1 = 1 THEN 420
418 LOCATE 21: INPUT "19. STADIUM NAME "; S$: IF C1 = 1 THEN 420
419 LOCATE 22: INPUT "20. AVERAGE ATTENDANCE "; ATT: IF C1 = 1 THEN 420
420 LOCATE 3, 40: COLOR PA%(2), PA%(1): PRINT "SAMPLE TEXT COLOR": COLOR 15, 0
    C1 = 1
    LOCATE 23: INPUT "# TO CHANGE (0 TO CONTINUE ) "; B9
    ON B9 GOTO 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419
500 CALL TITLE: GOSUB 9900: LOCATE 5
    LOCATE , 5: COLOR 3: PRINT "DRAFT "; : COLOR 15: PRINT "R"; : COLOR 3: PRINT "UNNING BACKS"
    LOCATE , 5: COLOR 3: PRINT "DRAFT "; : COLOR 15: PRINT "W"; : COLOR 3: PRINT "IDE RECEIVERS"
    LOCATE , 5: COLOR 3: PRINT "DRAFT "; : COLOR 15: PRINT "Q"; : COLOR 3: PRINT "UARTERBACKS"
    LOCATE , 5: COLOR 3: PRINT "DRAFT "; : COLOR 15: PRINT "K"; : COLOR 3: PRINT "ICKOFF RETURN"
    LOCATE , 5: COLOR 3: PRINT "DRAFT "; : COLOR 15: PRINT "P"; : COLOR 3: PRINT "UNT RETURN"
    LOCATE , 5: COLOR 3: PRINT "DRAFT P"; : COLOR 15: PRINT "U"; : COLOR 3: PRINT "NTERS"
    LOCATE , 5: COLOR 3: PRINT "DRAFT PLACE K"; : COLOR 15: PRINT "I"; : COLOR 3: PRINT "CKERS"
    LOCATE , 5: COLOR 15: PRINT "E"; : COLOR 3: PRINT "ND AND SAVE DRAFT"

502 I$ = INKEY$: IF I$ = "" THEN 502
    IF UCASE$(I$) = "E" THEN 950
    IF UCASE$(I$) = "R" THEN 150
    IF UCASE$(I$) = "W" THEN 250
    IF UCASE$(I$) = "Q" THEN 350
    IF UCASE$(I$) = "K" THEN 450
    IF UCASE$(I$) = "P" THEN 550
    IF UCASE$(I$) = "U" THEN 650
    IF UCASE$(I$) = "I" THEN 750
    GOTO 502

600 CALL TITLE: GOSUB 9900: LOCATE 1, 1: COLOR 15, 0
    LOCATE 3: PRINT " 1. ENTER DISK ID FOR THIS TEAM "; YN$
    LOCATE 4: PRINT " 2. TEAM # "; NU
    LOCATE 5: PRINT " 3. TEAM NAME "; A$
 LOCATE 6: PRINT " 4. TEAM NICKNAME "; AA$
 LOCATE 7: PRINT " 5. DEF RUSH ADJ "; B6%(0)
 LOCATE 8: PRINT " 6. DEF COMP % ADJ "; B6%(1)
 LOCATE 9: PRINT " 7. DEF INT % ADJ "; B6%(2)
 LOCATE 10: PRINT " 8. FUM % "; B6%(3)
 LOCATE 11: PRINT " 9. DEF FUM % "; B6%(4)
 LOCATE 12: PRINT "10. OFF SACKED % "; B6%(5)
 LOCATE 13: PRINT "11. DEF SACK ADJ "; B6%(6)
 LOCATE 14: PRINT "12. DEF YD/COMP ADJ "; Q
 LOCATE 15: PRINT "13. TM PENALTY ADJ "; PA%(0)
 LOCATE 16: PRINT "14. OFF RUN/PASS % "; B6%(7)
 LOCATE 17: PRINT "15. TM STADIUM (0-OUTDOOR 1-DOME) "; PA%(3)
 LOCATE 18: PRINT "16. POWER RATING "; PA%(4)
 LOCATE 19: PRINT "17. TM BACKGROUND COLOR (0-7) "; PA%(1)
 LOCATE 20: PRINT "18. TM TEXT COLOR (0-15) "; PA%(2)
 LOCATE 21: PRINT "19. STADIUM NAME "; S$
 LOCATE 22: PRINT "20. AVERAGE ATTENDANCE "; ATT

     GOTO 420

530 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE , 12: PRINT
    LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
    LOCATE 7, 27: INPUT NY$
    IF NY$ = "" THEN GOSUB 1102: GOTO 532
    YY$ = NY$
    FOR I = 1 TO 500: VV$(I) = "": NEXT
    GOSUB 1100
532 IF BO% = 1 THEN BO% = 0: GOTO 530
533 LOCATE 2, 33: COLOR QA%(2), QA%(1): PRINT N$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 533
    IF UCASE$(I$) = "Y" THEN 530
    COLOR 15, 0
    RETURN

150 GOSUB 530
151 CALL TITLE: GOSUB 9900: LOCATE 2, 33: COLOR 15, 0: PRINT "RUNNING BACKS"
    LOCATE 3, 1
    COLOR PA%(2), PA%(1): PRINT A$; : COLOR 7, 0
    PRINT TAB(17); "RUN AVG CX% RC AVG CX%";
    PRINT TAB(41); : COLOR QA%(2), QA%(1): PRINT N$; : COLOR 7, 0
    PRINT TAB(57); "RUN AVG CX% RC AVG CX%"
    COLOR 15, 0
    FOR I = 0 TO 9: LOCATE I + 4, 1
    PRINT I; " "; A1$(I); TAB(19); : PRINT USING "##.#  ###   ##  ###"; A1%(I) * .3; A0%(I); A3%(I) * 3; A2%(I)
    LOCATE I + 4, 41
    PRINT I; " "; C1$(I); TAB(59); : PRINT USING "##.#  ###   ##  ###"; C1%(I) * .3; C0%(I); C3%(I) * 3; C2%(I)
    NEXT I
155 LOCATE 16, 5: PRINT "SLOT TO DRAFT # (0-9, 10=NONE) "; : INPUT P
    IF P < 0 OR P > 10 THEN 155
    IF P = 10 THEN 500
157 LOCATE 16, 45: PRINT "PLAYER # TO DRAFT "; : INPUT P1
    IF P1 < 0 OR P1 > 9 THEN 157
    A1$(P) = C1$(P1)
    A0%(P) = C0%(P1): A1%(P) = C1%(P1): A2%(P) = C2%(P1)
    A3%(P) = C3%(P1)
    GOTO 151
250 GOSUB 530
251 GOSUB 9900: LOCATE 2, 33: COLOR 15, 0: PRINT "WIDE RECEIVERS"
    LOCATE 3, 1
    COLOR PA%(2), PA%(1): PRINT A$; : COLOR 7, 0
    PRINT TAB(17); "REC AVG  CX% ";
    PRINT TAB(41); : COLOR QA%(2), QA%(1): PRINT N$; : COLOR 7, 0
    PRINT TAB(57); "REC AVG  CX%"
    COLOR 15, 0
    FOR I = 0 TO 5: LOCATE I + 4, 1: PRINT I; " "; A2$(I);
    PRINT TAB(17); USING "  ##    ###"; A5%(I) * 3; A4%(I)
    LOCATE I + 4, 41
    PRINT I; " "; C2$(I);
    PRINT TAB(57); USING "  ##    ###"; C5%(I) * 3; C4%(I)
    NEXT I
255 LOCATE 16, 5: PRINT "SLOT TO DRAFT # (0-5, 6=NONE) "; : INPUT P
    IF P < 0 OR P > 6 THEN 255
    IF P = 6 THEN 500
257 LOCATE 16, 45: PRINT "PLAYER # TO DRAFT "; : INPUT P1
    IF P1 < 0 OR P1 > 5 THEN 257
    A2$(P) = C2$(P1)
    A4%(P) = C4%(P1): A5%(P) = C5%(P1)
    GOTO 251
350 GOSUB 530
351 GOSUB 9900: LOCATE 2, 34: COLOR 15, 0: PRINT "QUARTERBACKS"
    LOCATE 3, 1
    COLOR PA%(2), PA%(1): PRINT A$; : COLOR 7, 0
    PRINT TAB(17); " C%  I%   ARM  CX% ";
    PRINT TAB(41); : COLOR QA%(2), QA%(1): PRINT N$; : COLOR 7, 0
    PRINT TAB(57); " C%  I%   ARM  CX%"
    COLOR 15, 0
    FOR I = 0 TO 3: LOCATE I + 4, 1
    PRINT I; " "; A3$(I); : PRINT TAB(17); USING "###  ##  ###  ###"; A7%(I); A8%(I); QX%(I); A6%(I)
    LOCATE I + 4, 41
    PRINT I; " "; C3$(I); : PRINT TAB(57); USING "###  ##  ###  ###"; C7%(I); C8%(I); SX%(I); C6%(I)
    NEXT I
355 LOCATE 16, 5: PRINT "SLOT TO DRAFT # (0-3, 4=NONE) "; : INPUT P
    IF P < 0 OR P > 4 THEN 355
    IF P = 4 THEN 500
357 LOCATE 16, 45: PRINT "PLAYER # TO DRAFT "; : INPUT P1
    IF P1 < 0 OR P1 > 3 THEN 357
    A3$(P) = C3$(P1)
    A6%(P) = C6%(P1): A7%(P) = C7%(P1): A8%(P) = C8%(P1): QX%(P) = SX%(P1)
    GOTO 351
450 GOSUB 530
451 GOSUB 9900: LOCATE 2, 34: COLOR 15, 0: PRINT "KICKOFF RETURNERS"
    LOCATE 3, 1
    COLOR PA%(2), PA%(1): PRINT A$; : COLOR 7, 0
    PRINT TAB(17); "RET AVG  CX%";
    PRINT TAB(41); : COLOR QA%(2), QA%(1): PRINT N$; : COLOR 7, 0
    PRINT TAB(57); "RET AVG  CX%"
    COLOR 15, 0
    FOR I = 0 TO 2: LOCATE I + 4, 1
    PRINT I; " "; A4$(I);
    PRINT TAB(17); USING "  ##     ###"; B%(I) * 3; A9%(I)
    LOCATE I + 4, 41
    PRINT I; " "; C4$(I);
    PRINT TAB(57); USING "  ##     ###"; D%(I) * 3; C9%(I)
    NEXT I
455 LOCATE 16, 5: PRINT "SLOT TO DRAFT (0-2, 3=NONE) "; : INPUT P
    IF P < 0 OR P > 3 THEN 455
    IF P = 3 THEN 500
457 LOCATE 16, 45: PRINT "PLAYER # TO DRAFT "; : INPUT P1
    IF P1 < 0 OR P1 > 2 THEN 457
    A4$(P) = C4$(P1)
    A9%(P) = C9%(P1): B%(P) = D%(P1)
    GOTO 451
550 GOSUB 530
551 GOSUB 9900: LOCATE 2, 34: COLOR 15, 0: PRINT "PUNT RETURNERS"
    LOCATE 3, 1
    COLOR PA%(2), PA%(1): PRINT A$; : COLOR 7, 0
    PRINT TAB(17); "RET AVG  CX%";
    PRINT TAB(41); : COLOR QA%(2), QA%(1): PRINT N$; : COLOR 7, 0
    PRINT TAB(57); "RET AVG  CX%"
    COLOR 15, 0
    FOR I = 0 TO 2
    LOCATE I + 4, 1
    PRINT I; " "; A5$(I);
    PRINT TAB(17); USING "  ##    ###"; B2%(I) * 3; B1%(I)
    LOCATE I + 4, 41
    PRINT I; " "; C5$(I);
    PRINT TAB(57); USING "  ##    ###"; D2%(I) * 3; D1%(I)
    NEXT I
555 LOCATE 16, 5: PRINT "SLOT TO DRAFT (0-2, 3=NONE) "; : INPUT P
    IF P < 0 OR P > 3 THEN 555
    IF P = 3 THEN 500
557 LOCATE 16, 45: PRINT "PLAYER # TO DRAFT "; : INPUT P1
    IF P1 < 0 OR P1 > 2 THEN 557
    A5$(P) = C5$(P1)
    B1%(P) = D1%(P1): B2%(P) = D2%(P1)
    GOTO 551
650 GOSUB 530
651 GOSUB 9900: LOCATE 2, 37: COLOR 15, 0: PRINT "PUNTER"
    LOCATE 3, 1
    COLOR PA%(2), PA%(1): PRINT A$; : COLOR 7, 0
    PRINT TAB(17); "PUNT AVG";
    PRINT TAB(41); : COLOR QA%(2), QA%(1): PRINT N$; : COLOR 7, 0
    PRINT TAB(57); "PUNT AVG"
    COLOR 15, 0
    LOCATE 4, 1
    PRINT 0; " "; A6$(0);
    PRINT TAB(17); USING "   ##"; B3%(0) * 2 + 28
    LOCATE 4, 41
    PRINT 0; " "; C6$(0);
    PRINT TAB(57); USING "   ##"; D3%(0) * 2 + 28
655 LOCATE 16, 5: PRINT "PLAYER # (0, 1=NONE) "; : INPUT P
    IF P < 0 OR P > 1 THEN 655
    IF P = 1 THEN 500
    GOSUB 900
    A6$(0) = C6$(0)
    B3%(0) = D3%(0)
    GOTO 651
750 GOSUB 530
751 GOSUB 9900: LOCATE 2, 34: COLOR 15, 0: PRINT "PLACE KICKERS"
    LOCATE 3, 1
    COLOR PA%(2), PA%(1): PRINT A$; : COLOR 7, 0
    PRINT TAB(17); "FG%  CX   EP%  CX";
    PRINT TAB(41); : COLOR QA%(2), QA%(1): PRINT N$; : COLOR 7, 0
    PRINT TAB(57); "FG%  CX   EP%  CX"
    COLOR 15, 0
    FOR I = 0 TO 1: LOCATE I + 4, 1
    PRINT I; " "; A7$(I);
    PRINT TAB(17); USING "### ###   ### ###"; B4%(I) * 5 + 25; B5%(I); B7%(I); B8%(I)
    LOCATE I + 4, 41
    PRINT I; " "; C7$(I);
    PRINT TAB(57); USING "### ###   ### ###"; D4%(I) * 5 + 25; D5%(I); D7%(I); D8%(I)
    NEXT I

755 LOCATE 16, 5: PRINT "PLAYER # (0-1, 2=NONE) "; : INPUT P
    IF P < 0 OR P > 2 THEN 755
    IF P = 2 THEN 500
757 LOCATE 16, 45: PRINT "PLAYER # TO DRAFT "; : INPUT P1
    IF P1 < 0 OR P1 > 1 THEN 757
    A7$(P) = C7$(P1)
    B4%(P) = D4%(P1): B5%(P) = D5%(P1): B7%(P) = D7%(P1): B8%(P) = D8%(P1)
    GOTO 751
900 LOCATE 20, 1: PRINT "ANY KEY TO CONTINUE"
901 I$ = INKEY$: IF I$ = "" THEN 901
    RETURN
950 GOSUB 9900
    IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 953
    LOCATE 3, 1: PRINT "ENTER DISK TO SAVE TEAM ON AND HIT ANY KEY"
952 U$ = INKEY$: IF U$ = "" THEN 952
953 GOSUB 9600
    GOTO 10
4091 LOCATE 23, 1: PRINT "HIT ANY KEY TO CONTINUE"
4092 I$ = INKEY$: IF I$ = "" THEN 4092
     RETURN
1100 NT = 0
     OPEN D$(0) + "FOOT." + YY$ FOR RANDOM AS #1 LEN = 934
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
     T2 = V%(SELECTION%)
     OPEN D$(0) + "FOOT." + YY$ FOR RANDOM AS #1 LEN = 934
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
     GET #1, T2
     N$ = Q$(1): CC$ = Q$(2)
     FOR I = 0 TO 7: D6%(I) = CVI(Q$(I + 3)): NEXT I
     FOR I = 0 TO 4: QA%(I) = CVI(Q$(I + 11)): NEXT I
     FOR I = 0 TO 9
     C1$(I) = Q$(I * 5 + 16)
     C0%(I) = CVI(Q$(I * 5 + 17))
     C1%(I) = CVI(Q$(I * 5 + 18))
     C2%(I) = CVI(Q$(I * 5 + 19))
     C3%(I) = CVI(Q$(I * 5 + 20))
     NEXT I
     FOR I = 0 TO 5
     C2$(I) = Q$(I * 3 + 66)
     C4%(I) = CVI(Q$(I * 3 + 67))
     C5%(I) = CVI(Q$(I * 3 + 68))
     NEXT I
     FOR I = 0 TO 3
     C3$(I) = Q$(I * 5 + 84)
     C6%(I) = CVI(Q$(I * 5 + 85))
     C7%(I) = CVI(Q$(I * 5 + 86))
     C8%(I) = CVI(Q$(I * 5 + 87))
     SX%(I) = CVI(Q$(I * 5 + 88))
     NEXT I
     FOR I = 0 TO 2
     C4$(I) = Q$(I * 3 + 104)
     C9%(I) = CVI(Q$(I * 3 + 105))
     D%(I) = CVI(Q$(I * 3 + 106))
     NEXT I
     FOR I = 0 TO 2
     C5$(I) = Q$(I * 3 + 113)
     D1%(I) = CVI(Q$(I * 3 + 114))
     D2%(I) = CVI(Q$(I * 3 + 115))
     NEXT I
     C6$(0) = Q$(122)
     D3%(0) = CVI(Q$(123))
     C7$(0) = Q$(124)
     D4%(0) = CVI(Q$(125))
     D5%(0) = CVI(Q$(126))
     D7%(0) = CVI(Q$(127))
     D8%(0) = CVI(Q$(128))
     C7$(1) = Q$(129)
     D4%(1) = CVI(Q$(130))
     D5%(1) = CVI(Q$(131))
     D7%(1) = CVI(Q$(132))
     D8%(1) = CVI(Q$(133))
     FOR I = 0 TO 14
     FS$(I) = Q$(I * 2 + 135)
     FS%(I) = CVI(Q$(I * 2 + 136))
     NEXT I
     FOR I = 0 TO 9
     FI$(I) = Q$(I * 2 + 165)
     FI%(I) = CVI(Q$(I * 2 + 166))
     NEXT I
     CLOSE #1
9400 FOR I = 0 TO 15: C2(I) = 0: C3(I) = 0: NEXT
     U = 0
     FOR I = 1 TO 9
     IF C0%(I) = 0 THEN U = U + 1: GOTO 9403
     C3(I) = C0%(I) - C0%(I - 1 - U): U = 0
9403 NEXT
     FOR I = 1 TO 9: C0%(I) = C3(I): NEXT
     FOR I = 0 TO 5: C2%(I + 10) = C4%(I): NEXT
     U = 0
     FOR I = 1 TO 15
     IF C2%(I) = 0 THEN U = U + 1: GOTO 9408
     C2(I) = C2%(I) - C2%(I - 1 - U): U = 0
9408 NEXT
     FOR I = 1 TO 15: C2%(I) = C2(I): NEXT
     FOR I = 0 TO 5: C4%(I) = C2%(I + 10): NEXT
     FOR I = 3 TO 1 STEP -1
     IF C6%(I) = 0 THEN 9414
     C6%(I) = C6%(I) - C6%(I - 1)
9414 NEXT
     FOR I = 2 TO 1 STEP -1
     IF C9%(I) = 0 THEN 9418
     C9%(I) = C9%(I) - C9%(I - 1)
9418 NEXT
     FOR I = 2 TO 1 STEP -1
     IF D1%(I) = 0 THEN 9422
     D1%(I) = D1%(I) - D1%(I - 1)
9422 NEXT
     FOR I = 1 TO 1 STEP -1
     IF D5%(I) = 0 THEN 9426
     D5%(I) = D5%(I) - D5%(I - 1)
9426 NEXT
     FOR I = 1 TO 1 STEP -1
     IF D8%(I) = 0 THEN 9430
     D8%(I) = D8%(I) - D8%(I - 1)
9430 NEXT
     RETURN
9600 REM
     S0 = 0: S1 = 0: S2 = 0: S3 = 0
     FOR I = 0 TO 9: S0 = S0 + A0%(I): S2 = S2 + A2%(I): NEXT
     S4 = 100 / S0
     FOR I = 0 TO 9: IF A0%(I) <> 0 THEN A0%(I) = INT(S4 * A0%(I) + .5) + S1: S1 = A0%(I)
     IF A0%(I) > 100 THEN A0%(I) = 100
     NEXT I
     FOR I = 0 TO 5: S2 = S2 + A4%(I): NEXT
     S5 = 100 / S2
     FOR I = 0 TO 9: IF A2%(I) <> 0 THEN A2%(I) = INT(S5 * A2%(I) + .5) + S3: S3 = A2%(I)
     IF A2%(I) > 100 THEN A2%(I) = 100
     NEXT I
     FOR I = 0 TO 5: IF A4%(I) <> 0 THEN A4%(I) = INT(S5 * A4%(I) + .5) + S3: S3 = A4%(I)
     IF A4%(I) > 100 THEN A4%(I) = 100
     NEXT I
     S6 = 0: S7 = 0
     FOR I = 0 TO 3: S6 = S6 + A6%(I): NEXT
     S8 = 100 / S6
     FOR I = 0 TO 3: IF A6%(I) <> 0 THEN A6%(I) = INT(S8 * A6%(I) + .5) + S7: S7 = A6%(I)
     IF A6%(I) > 100 THEN A6%(I) = 100
     NEXT
     G0 = 0: G1 = 0: G3 = 0: G4 = 0
     FOR I = 0 TO 2: G0 = G0 + A9%(I): G3 = G3 + B1%(I): NEXT
     G2 = 100 / G0: G5 = 100 / G3
     FOR I = 0 TO 2: IF A9%(I) <> 0 THEN A9%(I) = INT(G2 * A9%(I) + .5) + G1: G1 = A9%(I)
     IF A9%(I) > 100 THEN A9%(I) = 100
     NEXT
     FOR I = 0 TO 2: IF B1%(I) <> 0 THEN B1%(I) = INT(G5 * B1%(I) + .5) + G4: G4 = B1%(I)
     IF B1%(I) > 100 THEN B1%(I) = 100
     NEXT
     G0 = 0: G1 = 0: G3 = 0: G4 = 0
     FOR I = 0 TO 1: G0 = G0 + B5%(I): G3 = G3 + B8%(I): NEXT
     G2 = 100 / G0: G5 = 100 / G3
     FOR I = 0 TO 1: IF B5%(I) <> 0 THEN B5%(I) = INT(G2 * B5%(I) + .5) + G1: G1 = B5%(I)
     IF B5%(I) > 100 THEN B5%(I) = 100
     NEXT
     FOR I = 0 TO 1: IF B8%(I) <> 0 THEN B8%(I) = INT(G5 * B8%(I) + .5) + G4: G4 = B8%(I)
     IF B8%(I) > 100 THEN B8%(I) = 100
     NEXT
     FOR I = 9 TO 0 STEP -1: IF A0%(I) <> 0 THEN A0%(I) = 100: GOTO 9776
     NEXT
9776 FOR I = 5 TO 0 STEP -1: IF A4%(I) <> 0 THEN A4%(I) = 100: GOTO 9778
     NEXT
9778 FOR I = 3 TO 0 STEP -1: IF A6%(I) <> 0 THEN A6%(I) = 100: GOTO 9780
     NEXT
9780 FOR I = 2 TO 0 STEP -1: IF A9%(I) <> 0 THEN A9%(I) = 100: GOTO 9782
     NEXT
9782 FOR I = 2 TO 0 STEP -1: IF B1%(I) <> 0 THEN B1%(I) = 100: GOTO 9784
     NEXT
9784 FOR I = 1 TO 0 STEP -1: IF B5%(I) <> 0 THEN B5%(I) = 100: GOTO 9786
     NEXT
9786 FOR I = 1 TO 0 STEP -1: IF B8%(I) <> 0 THEN B8%(I) = 100: GOTO 9800
     NEXT
9800 X = INT((Q / .3) + .5): IF X < 0 THEN X = 100 + X
     YC% = X
     IF NU = 0 THEN GOSUB 3000
     OPEN D$(0) + "FOOT." + YN$ FOR RANDOM AS #1 LEN = 934
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
     LSET Q$(1) = A$: LSET Q$(2) = AA$
     FOR I = 0 TO 7: LSET Q$(I + 3) = MKI$(B6%(I)): NEXT I
     FOR I = 0 TO 4: LSET Q$(I + 11) = MKI$(PA%(I)): NEXT I
     FOR I = 0 TO 9
     LSET Q$(I * 5 + 16) = A1$(I)
     LSET Q$(I * 5 + 17) = MKI$(A0%(I))
     LSET Q$(I * 5 + 18) = MKI$(A1%(I))
     LSET Q$(I * 5 + 19) = MKI$(A2%(I))
     LSET Q$(I * 5 + 20) = MKI$(A3%(I))
     NEXT I
     FOR I = 0 TO 5
     LSET Q$(I * 3 + 66) = A2$(I)
     LSET Q$(I * 3 + 67) = MKI$(A4%(I))
     LSET Q$(I * 3 + 68) = MKI$(A5%(I))
     NEXT I
     FOR I = 0 TO 3
     LSET Q$(I * 5 + 84) = A3$(I)
     LSET Q$(I * 5 + 85) = MKI$(A6%(I))
     LSET Q$(I * 5 + 86) = MKI$(A7%(I))
     LSET Q$(I * 5 + 87) = MKI$(A8%(I))
     LSET Q$(I * 5 + 88) = MKI$(QX%(I))
     NEXT I
     FOR I = 0 TO 2
     LSET Q$(I * 3 + 104) = A4$(I)
     LSET Q$(I * 3 + 105) = MKI$(A9%(I))
     LSET Q$(I * 3 + 106) = MKI$(B%(I))
     NEXT I
     FOR I = 0 TO 2
     LSET Q$(I * 3 + 113) = A5$(I)
     LSET Q$(I * 3 + 114) = MKI$(B1%(I))
     LSET Q$(I * 3 + 115) = MKI$(B2%(I))
     NEXT I
     LSET Q$(122) = A6$(0)
     LSET Q$(123) = MKI$(B3%(0))
     LSET Q$(124) = A7$(0)
     LSET Q$(125) = MKI$(B4%(0))
     LSET Q$(126) = MKI$(B5%(0))
     LSET Q$(127) = MKI$(B7%(0))
     LSET Q$(128) = MKI$(B8%(0))
     LSET Q$(129) = A7$(1)
     LSET Q$(130) = MKI$(B4%(1))
     LSET Q$(131) = MKI$(B5%(1))
     LSET Q$(132) = MKI$(B7%(1))
     LSET Q$(133) = MKI$(B8%(1))
     LSET Q$(134) = MKI$(YC%)
     FOR I = 0 TO 14
     LSET Q$(I * 2 + 135) = DS$(I)
     LSET Q$(I * 2 + 136) = MKI$(DS%(I))
     NEXT I
     FOR I = 0 TO 9
     LSET Q$(I * 2 + 165) = DI$(I)
     LSET Q$(I * 2 + 166) = MKI$(DI%(I))
     NEXT I
     PUT #1, NU
     CLOSE #1
     OPEN D$(0) + "STADIUMS." + YN$ FOR RANDOM AS #2 LEN = 50
     FIELD #2, 15 AS Q$(1), 30 AS Q$(2), 4 AS Q$(3), 1 AS Q$(4)
     LSET Q$(1) = A$
     LSET Q$(2) = S$
     LSET Q$(3) = MKL$(ATT)
     LSET Q$(4) = "x"
     PUT #2, NU
     CLOSE 2
     RETURN

9900 CALL TITLE: FOR XY = 2 TO 23: LOCATE XY, 1: PRINT TAB(79); : NEXT: RETURN


1200 A$ = N$: AA$ = CC$
     FOR I = 0 TO 7: B6%(I) = D6%(I): NEXT I
     FOR I = 0 TO 4: PA%(I) = QA%(I): NEXT I
     FOR I = 0 TO 9
     A1$(I) = C1$(I)
     A0%(I) = C0%(I)
     A1%(I) = C1%(I)
     A2%(I) = C2%(I)
     A3%(I) = C3%(I)
     NEXT I
     FOR I = 0 TO 5
     A2$(I) = C2$(I)
     A4%(I) = C4%(I)
     A5%(I) = C5%(I)
     NEXT I
     FOR I = 0 TO 3
     A3$(I) = C3$(I)
     A6%(I) = C6%(I)
     A7%(I) = C7%(I)
     A8%(I) = C8%(I)
     QX%(I) = SX%(I)
     NEXT I
     FOR I = 0 TO 2
     A4$(I) = C4$(I)
     A9%(I) = C9%(I)
     B%(I) = D%(I)
     NEXT I
     FOR I = 0 TO 2
     A5$(I) = C5$(I)
     B1%(I) = D1%(I)
     B2%(I) = D2%(I)
     NEXT I
     A6$(0) = C6$(0)
     B3%(0) = D3%(0)
     A7$(0) = C7$(0)
     B4%(0) = D4%(0)
     B5%(0) = D5%(0)
     B7%(0) = D7%(0)
     B8%(0) = D8%(0)
     A7$(1) = C7$(1)
     B4%(1) = D4%(1)
     B5%(1) = D5%(1)
     B7%(1) = D7%(1)
     B8%(1) = D8%(1)
     FOR I = 0 TO 14
     DS$(I) = FS$(I)
     DS%(I) = FS%(I)
     NEXT I
     FOR I = 0 TO 9
     DI$(I) = FI$(I)
     DI%(I) = FI%(I)
     NEXT I
     NU = T2
     YN$ = YY$
     IF NOT EXIST%(D$(0) + "STADIUMS." + YN$) THEN S$ = "": ATT = 0: GOTO 1205
     OPEN D$(0) + "STADIUMS." + YN$ FOR RANDOM AS #2 LEN = 50
     FIELD #2, 15 AS X$, 30 AS Q$(1), 4 AS Q$(3), 1 AS Q$(4)
     GET #2, NU
     S$ = QPRTrim$(Q$(1))
     ATT = CVL(Q$(3))
     Z$ = Q$(4)
     CLOSE #2
     IF Z$ <> "x" THEN S$ = "": ATT = 0
1205 RETURN

3000 OPEN D$(0) + "FOOT." + YN$ FOR RANDOM AS #1 LEN = 934
     Y = LOF(1)
     CLOSE 1
     NU = Y / 934
     NU = NU + 1
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

