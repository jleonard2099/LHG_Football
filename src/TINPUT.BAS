'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Team Input"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

Dim Q$(205), B3(1), menuItems$(3)
Dim A1$(9), A2$(5), A3$(3), A4$(2), A5$(2), A6$(2), A7$(2), DS$(14), DI$(9), DS%(14), DI%(9)
Dim B6%(7), A0%(9), A1(9), A1%(9), A2%(9), A3%(9), A4%(5), A5%(5), A6%(3), A7%(3), A8%(3), A9%(2), B%(2), B1%(2), B2%(2), B3%(2)
Dim PA%(4), QX%(3), B5%(1), B4%(1), B7%(1), B8%(1)
'Dim A(11, 18), N$(11)

Open "DEV.BAS" For Input As #1
Input #1, G$
Close 1

10 Call TITLE
menuItems$(1) = "INPUT A NEW TEAM                "
menuItems$(2) = "RETURN TO 3 IN 1 FOOTBALL   MENU"
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 10, 57, 2, 31)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 2
I1 = SELECTION%
If I1 = 2 Then 633
Color 15, 1: Cls

400 Locate 1: Input " 1. DISK ID  "; YN$

401 Locate 2: Input " 2. TEAM # (ENTER FOR PROGRAM TO ASSIGN) "; NU: If C1 = 1 Then 420

402 Locate 3: Input " 3. TEAM NAME (IE. 94 ARKANSAS) "; A$: If C1 = 1 Then 420

403 Locate 4: Input " 4. TEAM NICKNAME (IE. PANTHERS) "; AA$: If C1 = 1 Then 420

404 Locate 5: Input " 5. DEF RUSH ADJ "; B6%(0): If C1 = 1 Then 420

405 Locate 6: Input " 6. DEF COMP % ADJ "; B6%(1): If C1 = 1 Then 420

406 Locate 7: Input " 7. DEF INT % ADJ "; B6%(2): If C1 = 1 Then 420

407 Locate 8: Input " 8. FUM % (AVG=4) "; B6%(3): If C1 = 1 Then 420

408 Locate 9: Input " 9. DEF FUM % (AVG=0) "; B6%(4): If C1 = 1 Then 420

409 Locate 10: Input " 10. OFF SACKED % (AVG=8) "; B6%(5): If C1 = 1 Then 420

410 Locate 11: Input "11. DEF SACK ADJ "; B6%(6): If C1 = 1 Then 420

411 Locate 12: Input "12. DEF YD/COMP ADJ "; Q: If C1 = 1 Then 420

412 Locate 13: Input "13. TM PENALTY ADJ "; PA%(0): If C1 = 1 Then 420

413 Locate 14: Input "14. OFF RUN/PASS % "; B6%(7): If C1 = 1 Then 420

414 Locate 15: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; PA%(3): If C1 = 1 Then 420

415 Locate 16: Input "16. POWER RATING "; PA%(4): If C1 = 1 Then 420

416 Locate 17: Input "17. BACKGROUND COLOR (0-7) "; PA%(1): If C1 = 1 Then 420

417 Locate 18: Input "18. TEXT COLOR (0-15) "; PA%(2): If C1 = 1 Then 420

418 Locate 19: Input "19. OFF POWER ADJ "; O9: If C1 = 1 Then 420

419 Locate 20: Input "20. DEF POWER ADJ "; D9: If C1 = 1 Then 420

421 Locate 21: Input "21. STADIUM NAME "; S$: If C1 = 1 Then 420

422 Locate 22: Input "22. AVERAGE ATTENDANCE "; ATT: If C1 = 1 Then 420

420 Locate 1, 40: Color PA%(2), PA%(1): Print "SAMPLE TEXT COLOR": Color 15, 0
C1 = 1
Locate 23: Input "# TO CHANGE (0 TO CONTINUE ) "; B9: On B9 GOTO 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 421, 422
C1 = 0
Cls
For I = 0 To 9: Cls
    Locate 3: Print "RUNNING BACKS-"; I + 1

    426 Locate 5: Input "1. NAME "; A1$(I): If C1 = 1 Then 432
    If A1$(I) = "END" Then 440

    428 Locate 6: Input "2. # OF RUSHES "; A0%(I): If C1 = 1 Then 432

    429 Locate 7: Input "3. RUSHING AVERAGE "; A1(I): If C1 = 1 Then 432

    430 Locate 8: Input "4. # OF RECEPTIONS "; A2%(I): If C1 = 1 Then 432

    431 Locate 9: Input "5. RECEIVING AVERAGE "; A3%(I): If C1 = 1 Then 432

    432 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE)"; B9: On B9 GOTO 426, 428, 429, 430, 431
    C1 = 0
Next I

440 Cls
For I = 0 To 5: Cls
    Locate 3: Print "WIDE RECEIVERS/TIGHT ENDS -"; I + 1

    443 Locate 5: Input "1. NAME "; A2$(I): If C1 = 1 Then 450
    If A2$(I) = "END" Then 460

    445 Locate 6: Input "2. # OF RECEPTIONS "; A4%(I): If C1 = 1 Then 450

    446 Locate 7: Input "3. RECEIVING AVERAGE "; A5%(I): If C1 = 1 Then 450

    450 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 443, 445, 446
    C1 = 0
Next I

460 Cls
For I = 0 To 3: Cls
    Locate 3: Print "QUARTERBACKS -"; I + 1

    463 Locate 5: Input "1. NAME "; A3$(I): If C1 = 1 Then 470
    If A3$(I) = "END" Then 480

    465 Locate 6: Input "2. PASSES ATTEMPTED "; A6%(I): If C1 = 1 Then 470

    466 Locate 7: Input "3. COMPLETION % "; A7%(I): If C1 = 1 Then 470

    467 Locate 8: Input "4. INTERCEPTION % "; A8%(I): If C1 = 1 Then 470

    468 Locate 9: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; QX%(I): If C1 = 1 Then 470

    470 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 463, 465, 466, 467, 468
    C1 = 0
Next I

480 Cls
For I = 0 To 2: Cls
    Locate 3: Print "KICKOFF RETURNMEN -"; I + 1

    483 Locate 5: Input "1. NAME "; A4$(I): If C1 = 1 Then 490
    If A4$(I) = "END" Then 500

    485 Locate 6: Input "2. KICKOFFS RETURNED "; A9%(I): If C1 = 1 Then 490

    486 Locate 7: Input "3. RETURN AVERAGE "; B%(I): If C1 = 1 Then 490

    490 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 483, 485, 486
    C1 = 0
Next I

500 Cls
For I = 0 To 2: Cls
    Locate 3: Print "PUNT RETURNMEN -"; I + 1

    503 Locate 5: Input "1. NAME "; A5$(I): If C1 = 1 Then 510
    If A5$(I) = "END" Then 520

    505 Locate 6: Input "2. PUNTS RETURNED "; B1%(I): If C1 = 1 Then 510

    506 Locate 7: Input "3. PUNT RETURN AVERAGE "; B2%(I): If C1 = 1 Then 510

    510 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 503, 505, 506
    C1 = 0
Next I

520 Cls

521 Locate 3: Input "1. PUNTER NAME "; A6$(0): If C1 = 1 Then 525

522 Locate 4: Input "2. PUNT AVERAGE "; B3(0): If C1 = 1 Then 525

525 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 521, 522
C1 = 0: Cls
For I = 0 To 1: Cls
    Locate 3: Print "PLACE KICKER-"; I + 1

    529 Locate 5: Input "1. NAME "; A7$(I): If C1 = 1 Then 535
    If A7$(I) = "END" Then 537

    531 Locate 6: Input "2. FGA "; B5%(I): If C1 = 1 Then 535

    532 Locate 7: Input "3. FG% "; B4%(I): If C1 = 1 Then 535

    533 Locate 8: Input "4. PAT ATTEMPTED "; B8%(I): If C1 = 1 Then 535

    534 Locate 9: Input "5. PAT% "; B7%(I): If C1 = 1 Then 535

    535 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 529, 531, 532, 533, 534
C1 = 0: Next

537 B3%(0) = Int((B3(0) - 28) / 2! + .5)
For I = 0 To 1
    B4%(I) = Int((B4%(I) / 5) + .5) - 5
    If B4%(I) < 1 Then B4%(I) = 1
Next: GoTo 2000

543 SM = 0: ST = 0: RM = 0: RT = 0
For I = 0 To 9: SM = SM + A0%(I): RM = RM + A2%(I): Next I
SK = 100 / SM
GoSub 1000
For I = 0 To 9: A1%(I) = Int(A1(I) / .3 + .5): Next I
For I = 0 To 9: If O9 <> 0 And A1%(I) <> 0 Then A1%(I) = A1%(I) - O9
    'IF A1%(I) <  = 0 THEN A1%(I) = 1
Next I
For I = 0 To 5: RM = RM + A4%(I): Next I
RK = 100 / RM
For I = 0 To 9: If A2%(I) <> 0 Then A2%(I) = Int(RK * A2%(I) + .5) + RT: RT = A2%(I): If A2%(I) > 100 Then A2%(I) = 100
Next I
For I = 0 To 5: If A4%(I) <> 0 Then A4%(I) = Int(RK * A4%(I) + .5) + RT: RT = A4%(I): If A4%(I) > 100 Then A4%(I) = 100
Next I
For I = 0 To 9: A3%(I) = Int(A3%(I) / 3 + .5): Next I
For I = 0 To 5: A5%(I) = Int(A5%(I) / 3 + .5): Next I
PM = 0: PT = 0
For I = 0 To 3: PM = PM + A6%(I): Next I
PK = 100 / PM
For I = 0 To 3: If A6%(I) <> 0 Then A6%(I) = Int(PK * A6%(I) + .5) + PT: PT = A6%(I): If A6%(I) > 100 Then A6%(I) = 100
Next I
KM = 0: KT = 0: QM = 0: QT = 0
For I = 0 To 3: If O9 <> 0 And A7%(I) <> 0 Then A7%(I) = A7%(I) - O9
Next I
For I = 0 To 2: KM = KM + A9%(I): QM = QM + B1%(I): B%(I) = Int(B%(I) / 3! + .5): B2%(I) = Int(B2%(I) / 3 + .5): Next I
KK = 100 / KM: QK = 100 / QM
For I = 0 To 2: If A9%(I) <> 0 Then A9%(I) = Int(KK * A9%(I) + .5) + KT: KT = A9%(I): If A9%(I) > 100 Then A9%(I) = 100
Next I
For I = 0 To 2: If B1%(I) <> 0 Then B1%(I) = Int(QK * B1%(I) + .5) + QT: QT = B1%(I): If B1%(I) > 100 Then B1%(I) = 100
Next I: PM = 0: PT = 0: B5%(0) = Int(B5%(0) / (B5%(0) + B5%(1)) * 100 + .5): B5%(1) = 100: B8%(0) = Int(B8%(0) / (B8%(0) + B8%(1)) * 100 + .5): B8%(1) = 100
For I = 9 To 0 Step -1: If A0%(I) <> 0 Then A0%(I) = 100: GoTo 576
Next I

576 For I = 5 To 0 Step -1: If A4%(I) <> 0 Then A4%(I) = 100: GoTo 578
Next I

578 For I = 3 To 0 Step -1: If A6%(I) <> 0 Then A6%(I) = 100: GoTo 580
Next I

580 For I = 2 To 0 Step -1: If A9%(I) <> 0 Then A9%(I) = 100: GoTo 582
Next I

582 For I = 2 To 0 Step -1: If B1%(I) <> 0 Then B1%(I) = 100: GoTo 584
Next I

584 QV = 0
X = Int((Q / .3) + .5): If X < 0 Then X = 100 + X
YC% = X
If D9 > 0 Then B6%(0) = B6%(0) + D9: B6%(1) = B6%(1) + D9
GoSub 9500
For I = 0 To 7: B6%(I) = 0: Next I
For I = 0 To 9: A1$(I) = "": A0%(I) = 0: A1(I) = 0: A1%(I) = 0: A2%(I) = 0: A3%(I) = 0: Next I
For I = 0 To 5: A2$(I) = "": A4%(I) = 0: A5%(I) = 0: Next I
For I = 0 To 3: A3$(I) = "": A6%(I) = 0: A7%(I) = 0: A8%(I) = 0: Next I
For I = 0 To 2: A4$(I) = "": A9%(I) = 0: B%(I) = 0: Next I
For I = 0 To 2: A5$(I) = "": B1%(I) = 0: B2%(I) = 0: Next I
A6$(0) = "": B3%(0) = 0
For I = 0 To 1: A7$(I) = "": B4%(I) = 0: B7%(I) = 0: B5%(I) = 0: B8%(I) = 0: Next
For I = 0 To 9: DI$(I) = "": DI%(I) = 0: Next
For I = 0 To 14: DS$(I) = "": DS%(I) = 0: Next

3900 GoSub 4600
U$ = "INPUT ANOTHER TEAM (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
631 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 10

633 Run "HELLO.EXE 2"
End

4600 JJ = 3
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

1000 For I = 0 To 9: If A0%(I) <> 0 Then A0%(I) = Int(SK * A0%(I) + .5) + ST: ST = A0%(I)
    If A0%(I) > 100 Then A0%(I) = 100
Next I
Return

2000 Cls
For I = 0 To 9: Cls
    Locate 3: Print "PLAYERS WITH INTERCEPTIONS -"; I + 1

    2003 Locate 5: Input "1. NAME "; DI$(I): If C1 = 1 Then 2006
    If DI$(I) = "END" Then 2009

    2005 Locate 6: Input "2. # OF INTERCEPTIONS "; DI%(I): If C1 = 1 Then 2006

    2006 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 2003, 2005
    C1 = 0
Next I

2009 Cls
For I = 0 To 14: Cls
    Locate 3: Print "PLAYERS WITH SACKS -"; I + 1

    2013 Locate 5: Input "1. NAME "; DS$(I): If C1 = 1 Then 2016
    If DS$(I) = "END" Then 2099

    2015 Locate 6: Input "2. # OF SACKS "; DS%(I): If C1 = 1 Then 2016

    2016 C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 2013, 2015
    C1 = 0
Next I

2099 GoTo 543

3000 Open G$ + "FOOT." + YN$ For Random As #1 Len = 934
Y = LOF(1)
Close 1
NU = Y / 934
NU = NU + 1
Return

9500 If NU = 0 Then GoSub 3000

Open G$ + "FOOT." + YN$ For Random As #1 Len = 934
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

Open G$ + "STADIUMS." + YN$ For Random As #2 Len = 50
Field #2, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)
LSet Q$(1) = A$
LSet Q$(2) = S$
LSet Q$(3) = MKL$(ATT)
LSet Q$(4) = "x"
Put #2, NU
Close 2

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
