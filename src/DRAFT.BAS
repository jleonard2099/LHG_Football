'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Team Draft"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim teamItems$(500), V%(500), C2(15), C3(15)
Dim A0%(9), A1%(9), A2%(9), A3%(9), A4%(5), A5%(5), A6%(3), A7%(3), A8%(3), A9%(2)
Dim C0%(9), C1%(9), C2%(15), C3%(9), C4%(5), C5%(5), C6%(3), C7%(3), C8%(3), C9%(2)
Dim B%(2), B1%(2), B3%(2), B6%(7), B4%(1), B5%(1), B7%(1), B8%(1)
Dim D4%(1), D5%(1), D7%(1), D8%(1), QX%(3), D2%(2), FI$(9), FI%(9), DI$(9), DI%(9)
Dim D%(2), D1%(2), D3%(2), D6%(7), D$(1), menuItems$(4), PA%(4), QA%(4), SX%(3)
Dim B2%(2), Q$(300), FS%(14), DS$(14), DS%(14), FS$(14)
'Dim F$(500), Q6%(1), T1%(1), K3%(1, 6), C$(1)
Dim A$(1), A1$(9), A2$(5), A3$(3), A4$(2), A5$(2), A6$(2), A7$(1)
Dim C1$(9), C2$(5), C3$(3), C4$(2), C5$(2), C6$(2), C7$(1)

10 Clear

Open "DEV.BAS" For Input As #1
Input #1, D$(0)
Close #1

Call TITLE
menuItems$(1) = "DRAFT A NEW TEAM         "
menuItems$(2) = "DRAFT TO AN EXISTING TEAM"
menuItems$(3) = "EXIT TO MAIN MENU        "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 11, 50, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SELECTION% = 3 Then 15
If SELECTION% = 1 Then 205
If SELECTION% = 2 Then GoSub 530: GoSub 1200: GoTo 600

15 Run "HELLO.EXE 2"

205 Call TITLE: GoSub 9900: Locate 1, 1: Color 15, 0

400 Locate 3: Input " 1. ENTER DISK ID FOR THIS TEAM "; YN$: If C1 = 1 Then 420

401 Locate 4: Input " 2. TEAM # (0 FOR PROGRAM TO ASSIGN) "; NU: If C1 = 1 Then 420

402 Locate 5: Input " 3. TEAM NAME "; A$: If C1 = 1 Then 420

403 Locate 6: Input " 4. TEAM NICKNAME "; AA$: If C1 = 1 Then 420

404 Locate 7: Input " 5. DEF RUSH ADJ "; B6%(0): If C1 = 1 Then 420

405 Locate 8: Input " 6. DEF COMP % ADJ "; B6%(1): If C1 = 1 Then 420

406 Locate 9: Input " 7. DEF INT % ADJ "; B6%(2): If C1 = 1 Then 420

407 Locate 10: Input " 8. FUM % "; B6%(3): If C1 = 1 Then 420

408 Locate 11: Input " 9. DEF FUM % "; B6%(4): If C1 = 1 Then 420

409 Locate 12: Input "10. OFF SACKED % "; B6%(5): If C1 = 1 Then 420

410 Locate 13: Input "11. DEF SACK ADJ "; B6%(6): If C1 = 1 Then 420

411 Locate 14: Input "12. DEF YD/COMP ADJ "; Q: If C1 = 1 Then 420

412 Locate 15: Input "13. TM PENALTY ADJ "; PA%(0): If C1 = 1 Then 420

413 Locate 16: Input "14. OFF RUN/PASS % "; B6%(7): If C1 = 1 Then 420

414 Locate 17: Input "15. TM STADIUM (0-OUTDOOR 1-DOME) "; PA%(3): If C1 = 1 Then 420

415 Locate 18: Input "16. POWER RATING "; PA%(4): If C1 = 1 Then 420

416 Locate 19: Input "17. TM BACKGROUND COLOR (0-7) "; PA%(1): If C1 = 1 Then 420

417 Locate 20: Input "18. TM TEXT COLOR (0-15) "; PA%(2): If C1 = 1 Then 420

418 Locate 21: Input "19. STADIUM NAME "; S$: If C1 = 1 Then 420

419 Locate 22: Input "20. AVERAGE ATTENDANCE "; ATT: If C1 = 1 Then 420

420 Locate 3, 40: Color PA%(2), PA%(1): Print "SAMPLE TEXT COLOR": Color 15, 0
C1 = 1
Locate 23: Input "# TO CHANGE (0 TO CONTINUE ) "; B9
On B9 GOTO 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419

500 Call TITLE: GoSub 9900: Locate 5
Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "R";: Color 3: Print "UNNING BACKS"
Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "W";: Color 3: Print "IDE RECEIVERS"
Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "Q";: Color 3: Print "UARTERBACKS"
Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "K";: Color 3: Print "ICKOFF RETURN"
Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "P";: Color 3: Print "UNT RETURN"
Locate , 5: Color 3: Print "DRAFT P";: Color 15: Print "U";: Color 3: Print "NTERS"
Locate , 5: Color 3: Print "DRAFT PLACE K";: Color 15: Print "I";: Color 3: Print "CKERS"
Locate , 5: Color 15: Print "E";: Color 3: Print "ND AND SAVE DRAFT"
502 I$ = GetKeyPress$
If UCase$(I$) = "E" Then 950
If UCase$(I$) = "R" Then 150
If UCase$(I$) = "W" Then 250
If UCase$(I$) = "Q" Then 350
If UCase$(I$) = "K" Then 450
If UCase$(I$) = "P" Then 550
If UCase$(I$) = "U" Then 650
If UCase$(I$) = "I" Then 750
GoTo 502

600 Call TITLE: GoSub 9900: Locate 1, 1: Color 15, 0
Locate 3: Print " 1. ENTER DISK ID FOR THIS TEAM "; YN$
Locate 4: Print " 2. TEAM # "; NU
Locate 5: Print " 3. TEAM NAME "; A$
Locate 6: Print " 4. TEAM NICKNAME "; AA$
Locate 7: Print " 5. DEF RUSH ADJ "; B6%(0)
Locate 8: Print " 6. DEF COMP % ADJ "; B6%(1)
Locate 9: Print " 7. DEF INT % ADJ "; B6%(2)
Locate 10: Print " 8. FUM % "; B6%(3)
Locate 11: Print " 9. DEF FUM % "; B6%(4)
Locate 12: Print "10. OFF SACKED % "; B6%(5)
Locate 13: Print "11. DEF SACK ADJ "; B6%(6)
Locate 14: Print "12. DEF YD/COMP ADJ "; Q
Locate 15: Print "13. TM PENALTY ADJ "; PA%(0)
Locate 16: Print "14. OFF RUN/PASS % "; B6%(7)
Locate 17: Print "15. TM STADIUM (0-OUTDOOR 1-DOME) "; PA%(3)
Locate 18: Print "16. POWER RATING "; PA%(4)
Locate 19: Print "17. TM BACKGROUND COLOR (0-7) "; PA%(1)
Locate 20: Print "18. TM TEXT COLOR (0-15) "; PA%(2)
Locate 21: Print "19. STADIUM NAME "; S$
Locate 22: Print "20. AVERAGE ATTENDANCE "; ATT
GoTo 420

530 Color 15, 0
Cls
JJ = 7
Locate 6, 8:
Color 15, 4
'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To JJ
	Locate 6 + II, 8
	Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 532
YY$ = NY$
ERASE teamItems$
GoSub 1100

532 If BO% = 1 Then BO% = 0: GoTo 530

533 Locate 2, 33: Color QA%(2), QA%(1): Print N$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 530
Color 15, 0
Return

150 GoSub 530

151 Call TITLE: GoSub 9900: Locate 2, 33: Color 15, 0: Print "RUNNING BACKS"
Locate 3, 1
Color PA%(2), PA%(1): Print A$;: Color 7, 0
Print Tab(17); "RUN AVG CX% RC AVG CX%";
Print Tab(41);: Color QA%(2), QA%(1): Print N$;: Color 7, 0
Print Tab(57); "RUN AVG CX% RC AVG CX%"
Color 15, 0
For I = 0 To 9: Locate I + 4, 1
    Print I; " "; A1$(I); Tab(19);: Print Using "##.#  ###   ##  ###"; A1%(I) * .3; A0%(I); A3%(I) * 3; A2%(I)
    Locate I + 4, 41
    Print I; " "; C1$(I); Tab(59);: Print Using "##.#  ###   ##  ###"; C1%(I) * .3; C0%(I); C3%(I) * 3; C2%(I)
Next I

155 Locate 16, 5: Print "SLOT TO DRAFT # (0-9, 10=NONE) ";: Input P
If P < 0 Or P > 10 Then 155
If P = 10 Then 500

157 Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
If P1 < 0 Or P1 > 9 Then 157
A1$(P) = C1$(P1)
A0%(P) = C0%(P1): A1%(P) = C1%(P1): A2%(P) = C2%(P1)
A3%(P) = C3%(P1)
GoTo 151

250 GoSub 530

251 GoSub 9900: Locate 2, 33: Color 15, 0: Print "WIDE RECEIVERS"
Locate 3, 1
Color PA%(2), PA%(1): Print A$;: Color 7, 0
Print Tab(17); "REC AVG  CX% ";
Print Tab(41);: Color QA%(2), QA%(1): Print N$;: Color 7, 0
Print Tab(57); "REC AVG  CX%"
Color 15, 0
For I = 0 To 5: Locate I + 4, 1: Print I; " "; A2$(I);
    Print Tab(17); Using "  ##    ###"; A5%(I) * 3; A4%(I)
    Locate I + 4, 41
    Print I; " "; C2$(I);
    Print Tab(57); Using "  ##    ###"; C5%(I) * 3; C4%(I)
Next I

255 Locate 16, 5: Print "SLOT TO DRAFT # (0-5, 6=NONE) ";: Input P
If P < 0 Or P > 6 Then 255
If P = 6 Then 500

257 Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
If P1 < 0 Or P1 > 5 Then 257
A2$(P) = C2$(P1)
A4%(P) = C4%(P1): A5%(P) = C5%(P1)
GoTo 251

350 GoSub 530

351 GoSub 9900: Locate 2, 34: Color 15, 0: Print "QUARTERBACKS"
Locate 3, 1
Color PA%(2), PA%(1): Print A$;: Color 7, 0
Print Tab(17); " C%  I%   ARM  CX% ";
Print Tab(41);: Color QA%(2), QA%(1): Print N$;: Color 7, 0
Print Tab(57); " C%  I%   ARM  CX%"
Color 15, 0
For I = 0 To 3: Locate I + 4, 1
    Print I; " "; A3$(I);: Print Tab(17); Using "###  ##  ###  ###"; A7%(I); A8%(I); QX%(I); A6%(I)
    Locate I + 4, 41
    Print I; " "; C3$(I);: Print Tab(57); Using "###  ##  ###  ###"; C7%(I); C8%(I); SX%(I); C6%(I)
Next I

355 Locate 16, 5: Print "SLOT TO DRAFT # (0-3, 4=NONE) ";: Input P
If P < 0 Or P > 4 Then 355
If P = 4 Then 500

357 Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
If P1 < 0 Or P1 > 3 Then 357
A3$(P) = C3$(P1)
A6%(P) = C6%(P1): A7%(P) = C7%(P1): A8%(P) = C8%(P1): QX%(P) = SX%(P1)
GoTo 351

450 GoSub 530

451 GoSub 9900: Locate 2, 34: Color 15, 0: Print "KICKOFF RETURNERS"
Locate 3, 1
Color PA%(2), PA%(1): Print A$;: Color 7, 0
Print Tab(17); "RET AVG  CX%";
Print Tab(41);: Color QA%(2), QA%(1): Print N$;: Color 7, 0
Print Tab(57); "RET AVG  CX%"
Color 15, 0
For I = 0 To 2: Locate I + 4, 1
    Print I; " "; A4$(I);
    Print Tab(17); Using "  ##     ###"; B%(I) * 3; A9%(I)
    Locate I + 4, 41
    Print I; " "; C4$(I);
    Print Tab(57); Using "  ##     ###"; D%(I) * 3; C9%(I)
Next I

455 Locate 16, 5: Print "SLOT TO DRAFT (0-2, 3=NONE) ";: Input P
If P < 0 Or P > 3 Then 455
If P = 3 Then 500

457 Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
If P1 < 0 Or P1 > 2 Then 457
A4$(P) = C4$(P1)
A9%(P) = C9%(P1): B%(P) = D%(P1)
GoTo 451

550 GoSub 530

551 GoSub 9900: Locate 2, 34: Color 15, 0: Print "PUNT RETURNERS"
Locate 3, 1
Color PA%(2), PA%(1): Print A$;: Color 7, 0
Print Tab(17); "RET AVG  CX%";
Print Tab(41);: Color QA%(2), QA%(1): Print N$;: Color 7, 0
Print Tab(57); "RET AVG  CX%"
Color 15, 0
For I = 0 To 2
    Locate I + 4, 1
    Print I; " "; A5$(I);
    Print Tab(17); Using "  ##    ###"; B2%(I) * 3; B1%(I)
    Locate I + 4, 41
    Print I; " "; C5$(I);
    Print Tab(57); Using "  ##    ###"; D2%(I) * 3; D1%(I)
Next I

555 Locate 16, 5: Print "SLOT TO DRAFT (0-2, 3=NONE) ";: Input P
If P < 0 Or P > 3 Then 555
If P = 3 Then 500

557 Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
If P1 < 0 Or P1 > 2 Then 557
A5$(P) = C5$(P1)
B1%(P) = D1%(P1): B2%(P) = D2%(P1)
GoTo 551

650 GoSub 530

651 GoSub 9900: Locate 2, 37: Color 15, 0: Print "PUNTER"
Locate 3, 1
Color PA%(2), PA%(1): Print A$;: Color 7, 0
Print Tab(17); "PUNT AVG";
Print Tab(41);: Color QA%(2), QA%(1): Print N$;: Color 7, 0
Print Tab(57); "PUNT AVG"
Color 15, 0
Locate 4, 1
Print 0; " "; A6$(0);
Print Tab(17); Using "   ##"; B3%(0) * 2 + 28
Locate 4, 41
Print 0; " "; C6$(0);
Print Tab(57); Using "   ##"; D3%(0) * 2 + 28

655 Locate 16, 5: Print "PLAYER # (0, 1=NONE) ";: Input P
If P < 0 Or P > 1 Then 655
If P = 1 Then 500
GoSub 900
A6$(0) = C6$(0)
B3%(0) = D3%(0)
GoTo 651

750 GoSub 530

751 GoSub 9900: Locate 2, 34: Color 15, 0: Print "PLACE KICKERS"
Locate 3, 1
Color PA%(2), PA%(1): Print A$;: Color 7, 0
Print Tab(17); "FG%  CX   EP%  CX";
Print Tab(41);: Color QA%(2), QA%(1): Print N$;: Color 7, 0
Print Tab(57); "FG%  CX   EP%  CX"
Color 15, 0
For I = 0 To 1: Locate I + 4, 1
    Print I; " "; A7$(I);
    Print Tab(17); Using "### ###   ### ###"; B4%(I) * 5 + 25; B5%(I); B7%(I); B8%(I)
    Locate I + 4, 41
    Print I; " "; C7$(I);
    Print Tab(57); Using "### ###   ### ###"; D4%(I) * 5 + 25; D5%(I); D7%(I); D8%(I)
Next I

755 Locate 16, 5: Print "PLAYER # (0-1, 2=NONE) ";: Input P
If P < 0 Or P > 2 Then 755
If P = 2 Then 500

757 Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
If P1 < 0 Or P1 > 1 Then 757
A7$(P) = C7$(P1)
B4%(P) = D4%(P1): B5%(P) = D5%(P1): B7%(P) = D7%(P1): B8%(P) = D8%(P1)
GoTo 751

900 Locate 20, 1: Print "ANY KEY TO CONTINUE"
901 I$ = GetKeyPress$
Return

950 GoSub 9900
If UCase$(Left$(D$(0), 1)) <> "A" And UCase$(Left$(D$(0), 1)) <> "B" Then 953
Locate 3, 1: Print "ENTER DISK TO SAVE TEAM ON AND HIT ANY KEY"

952 U$ = GetKeyPress$

953 GoSub 9600
GoTo 10

4091 Locate 23, 1: Print "HIT ANY KEY TO CONTINUE"
4092 I$ = GetKeyPress$
Return

1100 NT = 0
Open D$(0) + "FOOT." + YY$ For Random As #1 Len = 934
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 934
    Get #1, I1
    For I2 = 1 To 15
        If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1105
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): V%(NT) = I1

1105 Next I1
Close 1
TS% = Int((NT - 1) / 80)

1102 SELECTION% = 1
Count% = NT
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14
Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    
_MouseShow "DEFAULT"
20 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T2 = V%(SELECTION%)
Open D$(0) + "FOOT." + YY$ For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$
Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$
Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$
Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$
Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$
Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$
Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$
Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)
Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)
Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)
Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)
    FIELD #1, 200 AS Q$, 200 AS Q$, 128 AS Q$, 12 AS Q$(122), 2 AS Q$(123), 12 AS Q$(124), 2 AS Q$(125), 2 AS Q$(126), 2 AS Q$(127), 2 AS Q$(128), 12 AS Q$(129), 2 AS Q$(130), 2 AS Q$(131), 2 AS Q$(132), 2 AS Q$(133), 2 AS Q$(134), 200 AS Q$, 150  _
AS Q$
Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)
Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)
Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)
Get #1, T2
N$ = Q$(1): CC$ = Q$(2)
For I = 0 To 7: D6%(I) = CVI(Q$(I + 3)): Next I
For I = 0 To 4: QA%(I) = CVI(Q$(I + 11)): Next I
For I = 0 To 9
    C1$(I) = Q$(I * 5 + 16)
    C0%(I) = CVI(Q$(I * 5 + 17))
    C1%(I) = CVI(Q$(I * 5 + 18))
    C2%(I) = CVI(Q$(I * 5 + 19))
    C3%(I) = CVI(Q$(I * 5 + 20))
Next I
For I = 0 To 5
    C2$(I) = Q$(I * 3 + 66)
    C4%(I) = CVI(Q$(I * 3 + 67))
    C5%(I) = CVI(Q$(I * 3 + 68))
Next I
For I = 0 To 3
    C3$(I) = Q$(I * 5 + 84)
    C6%(I) = CVI(Q$(I * 5 + 85))
    C7%(I) = CVI(Q$(I * 5 + 86))
    C8%(I) = CVI(Q$(I * 5 + 87))
    SX%(I) = CVI(Q$(I * 5 + 88))
Next I
For I = 0 To 2
    C4$(I) = Q$(I * 3 + 104)
    C9%(I) = CVI(Q$(I * 3 + 105))
    D%(I) = CVI(Q$(I * 3 + 106))
Next I
For I = 0 To 2
    C5$(I) = Q$(I * 3 + 113)
    D1%(I) = CVI(Q$(I * 3 + 114))
    D2%(I) = CVI(Q$(I * 3 + 115))
Next I
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
For I = 0 To 14
    FS$(I) = Q$(I * 2 + 135)
    FS%(I) = CVI(Q$(I * 2 + 136))
Next I
For I = 0 To 9
    FI$(I) = Q$(I * 2 + 165)
    FI%(I) = CVI(Q$(I * 2 + 166))
Next I
Close #1

9400 For I = 0 To 15: C2(I) = 0: C3(I) = 0: Next
U = 0
For I = 1 To 9
    If C0%(I) = 0 Then U = U + 1: GoTo 9403
    C3(I) = C0%(I) - C0%(I - 1 - U): U = 0

9403 Next
For I = 1 To 9: C0%(I) = C3(I): Next
For I = 0 To 5: C2%(I + 10) = C4%(I): Next
U = 0
For I = 1 To 15
    If C2%(I) = 0 Then U = U + 1: GoTo 9408
    C2(I) = C2%(I) - C2%(I - 1 - U): U = 0

9408 Next
For I = 1 To 15: C2%(I) = C2(I): Next
For I = 0 To 5: C4%(I) = C2%(I + 10): Next
For I = 3 To 1 Step -1
    If C6%(I) = 0 Then 9414
    C6%(I) = C6%(I) - C6%(I - 1)

9414 Next
For I = 2 To 1 Step -1
    If C9%(I) = 0 Then 9418
    C9%(I) = C9%(I) - C9%(I - 1)

9418 Next
For I = 2 To 1 Step -1
    If D1%(I) = 0 Then 9422
    D1%(I) = D1%(I) - D1%(I - 1)

9422 Next
For I = 1 To 1 Step -1
    If D5%(I) = 0 Then 9426
    D5%(I) = D5%(I) - D5%(I - 1)

9426 Next
For I = 1 To 1 Step -1
    If D8%(I) = 0 Then 9430
    D8%(I) = D8%(I) - D8%(I - 1)

9430 Next
Return

9600 Rem
S0 = 0: S1 = 0: S2 = 0: S3 = 0
For I = 0 To 9: S0 = S0 + A0%(I): S2 = S2 + A2%(I): Next
S4 = 100 / S0
For I = 0 To 9: If A0%(I) <> 0 Then A0%(I) = Int(S4 * A0%(I) + .5) + S1: S1 = A0%(I)
    If A0%(I) > 100 Then A0%(I) = 100
Next I
For I = 0 To 5: S2 = S2 + A4%(I): Next
S5 = 100 / S2
For I = 0 To 9: If A2%(I) <> 0 Then A2%(I) = Int(S5 * A2%(I) + .5) + S3: S3 = A2%(I)
    If A2%(I) > 100 Then A2%(I) = 100
Next I
For I = 0 To 5: If A4%(I) <> 0 Then A4%(I) = Int(S5 * A4%(I) + .5) + S3: S3 = A4%(I)
    If A4%(I) > 100 Then A4%(I) = 100
Next I
S6 = 0: S7 = 0
For I = 0 To 3: S6 = S6 + A6%(I): Next
S8 = 100 / S6
For I = 0 To 3: If A6%(I) <> 0 Then A6%(I) = Int(S8 * A6%(I) + .5) + S7: S7 = A6%(I)
    If A6%(I) > 100 Then A6%(I) = 100
Next
G0 = 0: G1 = 0: G3 = 0: G4 = 0
For I = 0 To 2: G0 = G0 + A9%(I): G3 = G3 + B1%(I): Next
G2 = 100 / G0: G5 = 100 / G3
For I = 0 To 2: If A9%(I) <> 0 Then A9%(I) = Int(G2 * A9%(I) + .5) + G1: G1 = A9%(I)
    If A9%(I) > 100 Then A9%(I) = 100
Next
For I = 0 To 2: If B1%(I) <> 0 Then B1%(I) = Int(G5 * B1%(I) + .5) + G4: G4 = B1%(I)
    If B1%(I) > 100 Then B1%(I) = 100
Next
G0 = 0: G1 = 0: G3 = 0: G4 = 0
For I = 0 To 1: G0 = G0 + B5%(I): G3 = G3 + B8%(I): Next
G2 = 100 / G0: G5 = 100 / G3
For I = 0 To 1: If B5%(I) <> 0 Then B5%(I) = Int(G2 * B5%(I) + .5) + G1: G1 = B5%(I)
    If B5%(I) > 100 Then B5%(I) = 100
Next
For I = 0 To 1: If B8%(I) <> 0 Then B8%(I) = Int(G5 * B8%(I) + .5) + G4: G4 = B8%(I)
    If B8%(I) > 100 Then B8%(I) = 100
Next
For I = 9 To 0 Step -1: If A0%(I) <> 0 Then A0%(I) = 100: GoTo 9776
Next

9776 For I = 5 To 0 Step -1: If A4%(I) <> 0 Then A4%(I) = 100: GoTo 9778
Next

9778 For I = 3 To 0 Step -1: If A6%(I) <> 0 Then A6%(I) = 100: GoTo 9780
Next

9780 For I = 2 To 0 Step -1: If A9%(I) <> 0 Then A9%(I) = 100: GoTo 9782
Next

9782 For I = 2 To 0 Step -1: If B1%(I) <> 0 Then B1%(I) = 100: GoTo 9784
Next

9784 For I = 1 To 0 Step -1: If B5%(I) <> 0 Then B5%(I) = 100: GoTo 9786
Next

9786 For I = 1 To 0 Step -1: If B8%(I) <> 0 Then B8%(I) = 100: GoTo 9800
Next

9800 X = Int((Q / .3) + .5): If X < 0 Then X = 100 + X
YC% = X
If NU = 0 Then GoSub 3000
Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$
Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$
Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$
Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$
Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$
Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$
Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$
Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)
Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)
Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)
Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)
    FIELD #1, 200 AS Q$, 200 AS Q$, 128 AS Q$, 12 AS Q$(122), 2 AS Q$(123), 12 AS Q$(124), 2 AS Q$(125), 2 AS Q$(126), 2 AS Q$(127), 2 AS Q$(128), 12 AS Q$(129), 2 AS Q$(130), 2 AS Q$(131), 2 AS Q$(132), 2 AS Q$(133), 2 AS Q$(134), 200 AS Q$, 150  _
AS Q$
Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)
Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)
Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)
LSet Q$(1) = A$: LSet Q$(2) = AA$
For I = 0 To 7: LSet Q$(I + 3) = MKI$(B6%(I)): Next I
For I = 0 To 4: LSet Q$(I + 11) = MKI$(PA%(I)): Next I
For I = 0 To 9
    LSet Q$(I * 5 + 16) = A1$(I)
    LSet Q$(I * 5 + 17) = MKI$(A0%(I))
    LSet Q$(I * 5 + 18) = MKI$(A1%(I))
    LSet Q$(I * 5 + 19) = MKI$(A2%(I))
    LSet Q$(I * 5 + 20) = MKI$(A3%(I))
Next I
For I = 0 To 5
    LSet Q$(I * 3 + 66) = A2$(I)
    LSet Q$(I * 3 + 67) = MKI$(A4%(I))
    LSet Q$(I * 3 + 68) = MKI$(A5%(I))
Next I
For I = 0 To 3
    LSet Q$(I * 5 + 84) = A3$(I)
    LSet Q$(I * 5 + 85) = MKI$(A6%(I))
    LSet Q$(I * 5 + 86) = MKI$(A7%(I))
    LSet Q$(I * 5 + 87) = MKI$(A8%(I))
    LSet Q$(I * 5 + 88) = MKI$(QX%(I))
Next I
For I = 0 To 2
    LSet Q$(I * 3 + 104) = A4$(I)
    LSet Q$(I * 3 + 105) = MKI$(A9%(I))
    LSet Q$(I * 3 + 106) = MKI$(B%(I))
Next I
For I = 0 To 2
    LSet Q$(I * 3 + 113) = A5$(I)
    LSet Q$(I * 3 + 114) = MKI$(B1%(I))
    LSet Q$(I * 3 + 115) = MKI$(B2%(I))
Next I
LSet Q$(122) = A6$(0)
LSet Q$(123) = MKI$(B3%(0))
LSet Q$(124) = A7$(0)
LSet Q$(125) = MKI$(B4%(0))
LSet Q$(126) = MKI$(B5%(0))
LSet Q$(127) = MKI$(B7%(0))
LSet Q$(128) = MKI$(B8%(0))
LSet Q$(129) = A7$(1)
LSet Q$(130) = MKI$(B4%(1))
LSet Q$(131) = MKI$(B5%(1))
LSet Q$(132) = MKI$(B7%(1))
LSet Q$(133) = MKI$(B8%(1))
LSet Q$(134) = MKI$(YC%)
For I = 0 To 14
    LSet Q$(I * 2 + 135) = DS$(I)
    LSet Q$(I * 2 + 136) = MKI$(DS%(I))
Next I
For I = 0 To 9
    LSet Q$(I * 2 + 165) = DI$(I)
    LSet Q$(I * 2 + 166) = MKI$(DI%(I))
Next I
Put #1, NU
Close #1

Open D$(0) + "STADIUMS." + YN$ For Random As #2 Len = 50
Field #2, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)
LSet Q$(1) = A$
LSet Q$(2) = S$
LSet Q$(3) = MKL$(ATT)
LSet Q$(4) = "x"
Put #2, NU
Close 2

Return

9900 Call TITLE: For XY = 2 To 23: Locate XY, 1: Print Tab(79);: Next: Return

1200 A$ = N$: AA$ = CC$
For I = 0 To 7: B6%(I) = D6%(I): Next I
For I = 0 To 4: PA%(I) = QA%(I): Next I
For I = 0 To 9
    A1$(I) = C1$(I)
    A0%(I) = C0%(I)
    A1%(I) = C1%(I)
    A2%(I) = C2%(I)
    A3%(I) = C3%(I)
Next I
For I = 0 To 5
    A2$(I) = C2$(I)
    A4%(I) = C4%(I)
    A5%(I) = C5%(I)
Next I
For I = 0 To 3
    A3$(I) = C3$(I)
    A6%(I) = C6%(I)
    A7%(I) = C7%(I)
    A8%(I) = C8%(I)
    QX%(I) = SX%(I)
Next I
For I = 0 To 2
    A4$(I) = C4$(I)
    A9%(I) = C9%(I)
    B%(I) = D%(I)
Next I
For I = 0 To 2
    A5$(I) = C5$(I)
    B1%(I) = D1%(I)
    B2%(I) = D2%(I)
Next I
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
For I = 0 To 14
    DS$(I) = FS$(I)
    DS%(I) = FS%(I)
Next I
For I = 0 To 9
    DI$(I) = FI$(I)
    DI%(I) = FI%(I)
Next I
NU = T2
YN$ = YY$
If Not _FileExists(D$(0) + "STADIUMS." + YN$) Then S$ = "": ATT = 0: GoTo 1205

Open D$(0) + "STADIUMS." + YN$ For Random As #2 Len = 50
Field #2, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)
Get #2, NU
S$ = RTrim$(Q$(1))
ATT = CVL(Q$(3))
Z$ = Q$(4)
Close #2

If Z$ <> "x" Then S$ = "": ATT = 0

1205 Return

3000 Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
Y = LOF(1)
Close 1

NU = Y / 934
NU = NU + 1
Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 32: Color 12
    Print "3 IN 1 FOOTBALL V6.0"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
