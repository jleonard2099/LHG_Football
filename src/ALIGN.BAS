'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Align Stats"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

Dim teamItems$(500)
Dim menuItems$(3), D$(1), AR$(1), A$(1), GIN%(9, 2), GSN%(15, 1), LFN$(1)
Dim Z2(14, 18), Z(38), Z1(38), DS$(15)
'Dim GI$(1, 10), DI%(10), APR%(1, 120), CRDR(120), SITER$(120)
'Dim GIR$(1, 10), DIR%(10),
Dim GIR%(9, 2), LRR$(9, 1), LR$(9, 1), LCR$(15, 1), LPR$(3, 1), LP$(3, 1), LKR$(2, 1)
Dim GI%(9, 2), LRN$(9, 1), LCN$(15, 1), LPN$(3, 1), LKN$(2, 1), PKN$(2, 1)
Dim LK$(2, 1), PK$(2, 1), PKR$(2, 1), AMR(1, 4), AM(1, 4), AFR(1, 4), AF(1, 4), DI$(9)
Dim V%(500), Q$(500), PA%(4)
Dim AMN(1, 4), AFN(1, 4), DIN$(9), DSN$(14)
'Dim AP%(1, 120), CRD(120), SITE$(120)
Dim A1$(9), A2$(5), A3$(3), A4$(2), A5$(2), A6$(3), A7$(1)
Dim AN1$(9), AN2$(5), AN3$(3), AN4$(2), AN5$(2), AN6$(3), AN7$(1)
Dim AR1$(9), AR2$(5), AR3$(3), AR4$(2), AR5$(2), AR6$(3), AR7$(1)
Dim GS%(15, 1), LC(15), LC$(15, 1), LFR(1), LFR$(1), LF(1), LF$(1), LFN(1)
Dim ZR2(14, 18), ZR(38), ZR1(38), DSR$(15), DRI$(9)
Dim GSR%(15, 1), LCR(15), ZN2(14, 18), LCN(15)
Dim LP(3), LPR(3), LK(2), LKR(2), PK(2), PKR(2)
Dim LPN(3), LKN(2), PKN(2)
Dim LR(9), LI(9), LI$(9, 1), LRR(9), LIR(9), LIR$(9, 1)
Dim LRN(9), LIN(9), LIN$(9, 1)
'Dim O%(120), ORD%(120), O$(120), ORD$(120), T%(120), TR%(120), H$(120), HR$(120)

1 Cls

Open "DEV.BAS" For Input As #1: For I = 0 To 1: Input #1, D$(I): Next: Close #1

TMR = 6420: NB = -1: NC = -1: ND = -1: NE = -1: NF = -1: NG = -1: NH = -1: NI = -1: NJ = 0: NK = 0: D = 0: JX = 0: VV = 0

Color , 2
Locate 1, 1
Print Tab(80);
Locate , 26: Color 0
Print "3 IN 1 FOOTBALL UTILITIES"
Locate 24, 1
Print Tab(80);
Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 2, 0
menuItems$(1) = "ALIGN STATISTICAL FILES           "
menuItems$(2) = "EXIT TO MAIN MENU                 "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 10, 59, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
RN = SELECTION%
If SCANCODE% = 27 Then RN = 2
If RN = 1 Then 11
Run "MERGE.EXE"

11 Color 12, 0: Cls: Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"
Color 15: Locate 4, 5: Print "EXAMPLE - A:"
Locate , 5: Print "          C:\HAFFNER"
Locate , 5: Print "          C:\HAFFNER\1980"
Locate , 5: Print "          D:\FOOTBALL"

27 For J = 10 To 13: Locate J
Print String$(72, " "): Next
Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$
Locate , 5: Locate , 5: Line Input "CURRENT STATS:  "; PC$

Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 27

GoSub 3056
B$ = A$: GoSub 6585
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
Cls
Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; PR$
Locate , 15: Print "MASTER STATS ARE ON DRIVE "; PC$
Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$

Locate 14, 15: Print "ANY CHANGE (YN)"
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 11

110 If Not _FileExists(PR$ + "\" + Z$) Then 950
Open PR$ + "\" + Z$ For Input As #1
Input #1, AR$(PX)
For I = 0 To 9: Input #1, AR1$(I): Next I
For I = 0 To 5: Input #1, AR2$(I): Next I
For I = 0 To 3: Input #1, AR3$(I): Next I
For I = 0 To 2: Input #1, AR4$(I): Next I
For I = 0 To 2: Input #1, AR5$(I): Next I
Input #1, AR6$(0)
For I = 0 To 1: Input #1, AR7$(I): Next I
For J = 0 To 13
    For K = 0 To 17: Input #1, ZR2(J, K): Next K
Next J
For L = 0 To 38: Input #1, ZR(L): Next L
For L = 0 To 38: Input #1, ZR1(L): Next L
For J = 0 To 9: Input #1, DRI$(J): For J1 = 0 To 2: Input #1, GIR%(J, J1): Next: Next
For J = 0 To 14: Input #1, DSR$(J): For J1 = 0 To 1: Input #1, GSR%(J, J1): Next: Next
For I = 0 To 9: Input #1, LRR(I): Input #1, LRR$(I, 0): Input #1, LRR$(I, 1): Next
For I = 0 To 15: Input #1, LCR(I): Input #1, LCR$(I, 0): Input #1, LCR$(I, 1): Next
For I = 0 To 3: Input #1, LPR(I): Input #1, LPR$(I, 0): Input #1, LPR$(I, 1): Next
For I = 0 To 2: Input #1, LKR(I): Input #1, LKR$(I, 0): Input #1, LKR$(I, 1): Next
For I = 0 To 2: Input #1, PKR(I): Input #1, PKR$(I, 0): Input #1, PKR$(I, 1): Next
For I = 0 To 1: Input #1, LFR(I): Input #1, LFR$(I): For I1 = 0 To 4
Input #1, AMR(I, I1): Input #1, AFR(I, I1): Next: Next
For I = 0 To 9: Input #1, LIR(I): Input #1, LIR$(I, 0)
Input #1, LIR$(I, 1): Next
Close #1

215 If Not _FileExists(PC$ + "\" + Z$) Then 950
Open PC$ + "\" + Z$ For Input As #1
Input #1, A$(PX)
For I = 0 To 9: Input #1, A1$(I): Next I
For I = 0 To 5: Input #1, A2$(I): Next I
For I = 0 To 3: Input #1, A3$(I): Next I
For I = 0 To 2: Input #1, A4$(I): Next I
For I = 0 To 2: Input #1, A5$(I): Next I
Input #1, A6$(0)
For I = 0 To 1: Input #1, A7$(I): Next I
For J = 0 To 13
    For K = 0 To 17: Input #1, Z2(J, K): Next K
Next J
For L = 0 To 38: Input #1, Z(L): Next L
For L = 0 To 38: Input #1, Z1(L): Next L
For J = 0 To 9: Input #1, DI$(J): For J1 = 0 To 2: Input #1, GI%(J, J1): Next: Next
For J = 0 To 14: Input #1, DS$(J): For J1 = 0 To 1: Input #1, GS%(J, J1): Next: Next
For I = 0 To 9: Input #1, LR(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
For I = 0 To 15: Input #1, LC(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
For I = 0 To 3: Input #1, LP(I): Input #1, LP$(I, 0): Input #1, LP$(I, 1): Next
For I = 0 To 2: Input #1, LK(I): Input #1, LK$(I, 0): Input #1, LK$(I, 1): Next
For I = 0 To 2: Input #1, PK(I): Input #1, PK$(I, 0): Input #1, PK$(I, 1): Next
For I = 0 To 1: Input #1, LF(I): Input #1, LF$(I): For I1 = 0 To 4: Input #1, AM(I, I1): Input #1, AF(I, I1): Next: Next
For I = 0 To 9: Input #1, LI(I): Input #1, LI$(I, 0): Input #1, LI$(I, 1): Next
Close 1

400 GoTo 1000

404 N = 0

405 For J = 0 To 9

    406 If A1$(N) = AR1$(J) Then 407 Else GoTo 410

    407 AN1$(N) = AR1$(J): LRN(N) = LRR(J): LCN(N) = LCR(J)
    For I = 0 To 1
        LRN$(N, I) = LRR$(J, I): LCN$(N, I) = LCR$(J, I)
    Next

    408 For K = 0 To 5: ZN2(N, K) = ZR2(J, K)

    409 Next K

410 Next J

411 N = N + 1

412 If N = 10 GoTo 414 Else GoTo 405

414 GoSub 550: N = 0

415 For J = 0 To 5

    416 If A2$(N) = AR2$(J) Then 417 Else GoTo 420

    417 AN2$(N) = AR2$(J): LCN(N + 10) = LCR(J + 10)
    LCN$(N + 10, 0) = LCR$(J + 10, 0)
    LCN$(N + 10, 1) = LCR$(J + 10, 1)

    418 For K = 6 To 8: ZN2(N, K) = ZR2(J, K)

    419 Next K

420 Next J

421 N = N + 1

422 If N = 6 GoTo 424 Else GoTo 415

424 N = 0

425 For J = 0 To 3

    426 If A3$(N) = AR3$(J) Then 427 Else GoTo 430

    427 AN3$(N) = AR3$(J): LPN(N) = LPR(J)
    LPN$(N, 0) = LPR$(J, 0)
    LPN$(N, 1) = LPR$(J, 1)

    428 For K = 9 To 13: ZN2(N, K) = ZR2(J, K)

    429 Next K

430 Next J

431 N = N + 1

432 If N = 4 GoTo 434 Else GoTo 425

434 N = 0

435 For J = 0 To 2

    436 If A4$(N) = AR4$(J) Then 437 Else GoTo 440

    437 AN4$(N) = AR4$(J): LKN(N) = LKR(J)
    LKN$(N, 0) = LKR$(J, 0)
    LKN$(N, 1) = LKR$(J, 1)

    438 For K = 14 To 16: ZN2(N, K) = ZR2(J, K)

    439 Next K

440 Next J

441 N = N + 1

442 If N = 3 GoTo 444 Else GoTo 435

444 N = 0

445 For J = 0 To 2

    446 If A5$(N) = AR5$(J) Then 447 Else GoTo 450

    447 AN5$(N) = AR5$(J): PKN(N) = PKR(J)
    PKN$(N, 1) = PKR$(J, 1)
    PKN$(N, 1) = PKR$(J, 1)

    448 For K = 14 To 16: ZN2(N + 3, K) = ZR2(J + 3, K)

    449 Next K

450 Next J

451 N = N + 1

452 If N = 3 GoTo 460 Else GoTo 445

460 If A6$(0) = AR6$(0) Then 461 Else GoTo 470

461 AN6$(0) = AR6$(0)

470 N = 0

475 For J = 0 To 1

    476 If A7$(N) = AR7$(J) Then 477 Else GoTo 485

    477 AN7$(N) = AR7$(J): LFN(N) = LFR(J): LFN$(N) = LFR$(J)

    478 If N = J Then 480

    479 If N <> J Then 481

    480 For L = 3 To 6: ZN2(L, 17) = ZR2(L, 17): Next L
    For L = 7 To 10: ZN2(L, 17) = ZR2(L, 17): Next L: GoTo 482

    481 For L = 3 To 6: ZN2(L, 17) = ZR2(L + 4, 17): Next L
    For L = 7 To 10: ZN2(L, 17) = ZR2(L - 4, 17): Next L: GoTo 482

    482 For J1 = 0 To 4: AMN(N, J1) = AMR(J, J1): AFN(N, J1) = AFR(J, J1): Next J1

485 Next J

486 N = N + 1

487 If N = 2 GoTo 500 Else GoTo 475

500 N = 0

501 For J = 0 To 9

    502 If DI$(N) = DRI$(J) Then 503 Else GoTo 505

    503 DIN$(N) = DRI$(J)
    For J1 = 0 To 2
        GIN%(J, J1) = GIR%(J, J1)
    Next

505 Next J

506 N = N + 1

507 If N = 10 Then GoTo 510 Else GoTo 501

510 N = 0

511 For J = 0 To 14

    512 If DS$(N) = DSR$(J) Then 513 Else GoTo 515

    513 DSN$(N) = DSR$(J)
    For J1 = 0 To 1
        GSN%(J, J1) = GSR%(J, J1)
    Next

515 Next J

516 N = N + 1

517 If N = 15 Then GoTo 820 Else GoTo 511

550 N = 0

551 For J = 0 To 3

    552 If A3$(N) = AR3$(J) Then 553 Else GoTo 556

    553 AN3$(N) = AR3$(J)

    554 For K = 0 To 2: ZN2(N + 10, K) = ZR2(J + 10, K)

    555 Next K

556 Next J

557 N = N + 1

558 If N = 4 GoTo 559 Else GoTo 551

559 Return

820 KKK = 1: GoTo 1300

830 Open PR$ + "\" + Z$ For Output As #1

831 Write #1, AR$(PX)

832 For I = 0 To 9: Write #1, AN1$(I): Next I
For I = 0 To 5: Write #1, AN2$(I): Next I
For I = 0 To 3: Write #1, AN3$(I): Next I

833 For I = 0 To 2: Write #1, AN4$(I): Next I
For I = 0 To 2: Write #1, AN5$(I): Next I
Write #1, AN6$(0)
For I = 0 To 1: Write #1, AN7$(I): Next I

834 For J = 0 To 13

    835 For K = 0 To 17: Write #1, ZN2(J, K): Next K

836 Next J

837 For L = 0 To 38: Write #1, ZR(L): Next L

838 For L = 0 To 38: Write #1, ZR1(L): Next L
For J = 0 To 9: Write #1, DIN$(J): For J1 = 0 To 2: Write #1, GIN%(J, J1): Next: Next
For J = 0 To 14: Write #1, DSN$(J): For J1 = 0 To 1: Write #1, GSN%(J, J1): Next: Next
For I = 0 To 9: Write #1, LRN(I): Write #1, LRN$(I, 0): Write #1, LRN$(I, 1): Next
For I = 0 To 15: Write #1, LCN(I): Write #1, LCN$(I, 0): Write #1, LCN$(I, 1): Next
For I = 0 To 3: Write #1, LPN(I): Write #1, LPN$(I, 0): Write #1, LPN$(I, 1): Next
For I = 0 To 2: Write #1, LKN(I): Write #1, LKN$(I, 0): Write #1, LKN$(I, 1): Next
For I = 0 To 2: Write #1, PKN(I): Write #1, PKN$(I, 0): Write #1, PKN$(I, 1): Next
For I = 0 To 1: Write #1, LFN(I): Write #1, LFN$(I): For I1 = 0 To 4: Write #1, AMN(I, I1): Write #1, AFN(I, I1): Next: Next
For I = 0 To 9: Write #1, LIN(I): Write #1, LIN$(I, 0): Write #1, LIN$(I, 1): Next

860 Close #1

880 Cls: Color 15: Locate 10, 20: Print "RE-ALIGNMENT COMPLETED"

881 Print: Locate 15, 20: Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

900
GoTo 1

950 Cls: Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"

951 Print: Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

952 GoTo 11

1000 Cls: PX = 0: Locate 1, 32: Color 12: Print "STAT ROSTERS"

1001 Print: Color 15: Print "CURRENT  ";: Color 14: Print "- RUSHERS -";: Color 7: Print " ROAD";: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "- RECEIVERS -";: Color 7: Print " ROAD"
For I = 0 To 9: Locate I + 4: Color 12: Print I; " ";: Color 15: Print A1$(I);: Color 12: Print Tab(20); I; " ";: Color 7: Print AR1$(I): Next
For I = 0 To 5: Locate I + 4, 40: Color 12: Print I; " ";: Color 15: Print A2$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AR2$(I): Next
Locate 11, 40: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "- KICK RET. -";: Color 7: Print " ROAD"
For I = 0 To 2: Locate I + 12, 40: Color 12: Print I; " ";: Color 15: Print A4$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AR4$(I): Next
Locate 15: Color 15: Print "CURRENT  ";: Color 14: Print "-   QB'S  -";: Color 7: Print " ROAD"
For I = 0 To 3: Locate I + 16: Color 12: Print I; " ";: Color 15: Print A3$(I);: Color 12: Print Tab(20); I; " ";: Color 7: Print AR3$(I): Next
Locate 16, 40: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "- PUNT RET. -";: Color 7: Print " ROAD"
For I = 0 To 2: Locate I + 17, 40: Color 12: Print I; " ";: Color 15: Print A5$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AR5$(I): Next

Color 15: Locate 23: Print "HIT ANY KEY TO CONTINUE"
I$ = GetKeyPress$

Cls: Locate 1, 32: Color 12: Print "STAT ROSTERS"
Print
Color 15: Print "CURRENT  ";: Color 14: Print "- PUNTERS -";: Color 7: Print " ROAD";: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "-  KICKERS  -";: Color 7: Print " ROAD"
Locate 4: Color 12: Print "0 ";: Color 15: Print A6$(0);: Color 12: Print Tab(20); "0 ";: Color 7: Print AR6$(0)
For I = 0 To 1: Locate I + 4, 40: Color 12: Print I; " ";: Color 15: Print A7$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AR7$(I): Next
Locate 7: Color 15: Print "CURRENT  ";: Color 14: Print "-INTERCEPTORS-";: Color 7: Print " ROAD";: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "-  SACKERS  -";: Color 7: Print " ROAD"
For I = 0 To 9: Locate I + 8: Color 12: Print Using "##"; I;: Print " ";: Color 15: Print DI$(I);: Color 12: Print Tab(20); Using "##"; I;: Print " ";: Color 7: Print DRI$(I): Next
For I = 0 To 14: Locate I + 8, 40: Color 12: Print Using "##"; I;: Print " ";: Color 15: Print DS$(I);: Color 12: Print Tab(63); Using "##"; I;: Print " ";: Color 7: Print DSR$(I): Next

1005 Color 11: Locate 20: Line Input "HIT ENTER TO ALIGN"; DUMMY$

1006 GoTo 1206

1010 Cls: PX = 0: Locate 1, 32: Color 12: Print "STAT ROSTERS"

1011 Print: Color 15: Print "CURRENT  ";: Color 14: Print "- RUSHERS -";: Color 7: Print " ROAD";: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "- RECEIVERS -";: Color 7: Print " ROAD"
For I = 0 To 9: Locate I + 4: Color 12: Print I; " ";: Color 15: Print A1$(I);: Color 12: Print Tab(20); I; " ";: Color 7: Print AN1$(I): Next
For I = 0 To 5: Locate I + 4, 40: Color 12: Print I; " ";: Color 15: Print A2$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AN2$(I): Next
Locate 11, 40: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "- KICK RET. -";: Color 7: Print " ROAD"
For I = 0 To 2: Locate I + 12, 40: Color 12: Print I; " ";: Color 15: Print A4$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AN4$(I): Next
Locate 15: Color 15: Print "CURRENT  ";: Color 14: Print "-   QB'S  -";: Color 7: Print " ROAD"
For I = 0 To 3: Locate I + 16: Color 12: Print I; " ";: Color 15: Print A3$(I);: Color 12: Print Tab(20); I; " ";: Color 7: Print AN3$(I): Next
Locate 16, 40: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "- PUNT RET. -";: Color 7: Print " ROAD"
For I = 0 To 2: Locate I + 17, 40: Color 12: Print I; " ";: Color 15: Print A5$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AN5$(I): Next

Color 15: Locate 23: Print "HIT ANY KEY TO CONTINUE"
1012 I$ = GetKeyPress$

Cls: Locate 1, 32: Color 12: Print "STAT ROSTERS"
Print
Color 15: Print "CURRENT  ";: Color 14: Print "- PUNTERS -";: Color 7: Print " ROAD";: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "-  KICKERS  -";: Color 7: Print " ROAD"
Locate 4: Color 12: Print "0 ";: Color 15: Print A6$(0);: Color 12: Print Tab(20); "0 ";: Color 7: Print AN6$(0)
For I = 0 To 1: Locate I + 4, 40: Color 12: Print I; " ";: Color 15: Print A7$(I);: Color 12: Print Tab(63); I; " ";: Color 7: Print AN7$(I): Next
Locate 7: Color 15: Print "CURRENT  ";: Color 14: Print "-INTERCEPTORS-";: Color 7: Print " ROAD";: Color 15: Print Tab(45); "CURRENT  ";: Color 14: Print "-  SACKERS  -";: Color 7: Print " ROAD"
For I = 0 To 9: Locate I + 8: Color 12: Print Using "##"; I;: Print " ";: Color 15: Print DI$(I);: Color 12: Print Tab(20); Using "##"; I;: Print " ";: Color 7: Print DIN$(I): Next
For I = 0 To 14: Locate I + 8, 40: Color 12: Print Using "##"; I;: Print " ";: Color 15: Print DS$(I);: Color 12: Print Tab(63); Using "##"; I;: Print " ";: Color 7: Print DSN$(I): Next

1015 Color 11: Locate 20: Line Input "HIT ENTER TO ALIGN"; DUMMY$

1016 GoTo 830

1206 Locate 20: Print String$(26, " ")

1209 Color 11: Locate 20: Print "ALIGNING FILES . . ."

1212 GoTo 404

1300 Locate 20: Print String$(26, " ")

1301 SEQ = 0

1302 For I = 0 To 9

    1303 If AN1$(I) = A1$(I) Then 1305 Else GoTo 1304

    1304 SEQ = SEQ + 1

1305 Next

1306 For I = 0 To 5

    1307 If AN2$(I) = A2$(I) Then 1309 Else GoTo 1308

    1308 SEQ = SEQ + 1

1309 Next

1311 For I = 0 To 3

    1312 If AN3$(I) = A3$(I) Then 1314 Else GoTo 1313

    1313 SEQ = SEQ + 1

1314 Next

1315 For I = 0 To 2

    1316 If AN4$(I) = A4$(I) Then 1318 Else GoTo 1317

    1317 SEQ = SEQ + 1

1318 Next

1319 For I = 0 To 2

    1320 If AN5$(I) = A5$(I) Then 1322 Else GoTo 1321

    1321 SEQ = SEQ + 1

1322 Next

1324 If AN6$(I) = A6$(I) Then 1326 Else GoTo 1325

1325 SEQ = SEQ + 1

1326 For I = 0 To 1

    1327 If AN7$(I) = A7$(I) Then 1329 Else GoTo 1328

    1328 SEQ = SEQ + 1

1329 Next

1390 If SEQ = 0 Then 1010 Else GoTo 1400

1400 Cls: Locate 20: Color 14: Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"

1402 Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."

1405 Locate 23, 30: Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

1406 GoTo 900

1704 Return

3056 Color 15, 0: Cls
JJ = 6
SN = 1
Locate 6, 8
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
Print
Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
If I$ = Chr$(27) Then Z$ = "END": Clear: GoTo 1
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060

Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0
If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$

ERASE teamItems$
GoSub 1101

3060 If BO% = 1 Then BO% = 0: GoTo 3056

3058 Locate 2, 33: Color PA%(2), PA%(1): Print A$

Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3056
    
Color 15, 0
Return

1101 NTMS = 0
Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 934
    Get #1, I1
    For I2 = 1 To 15
        If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1105
    Next I2
    NTMS = NTMS + 1
    teamItems$(NTMS) = Q$(0): V%(NTMS) = I1

1105 Next I1
Close 1

1102 SELECTION% = 1
Count% = NTMS
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
    
21 _MouseShow "DEFAULT"
MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 21
If EXITCODE% = -79 Then Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GoTo 21
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 21
T1 = V%(SELECTION%)
    
Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Get #1, T1
A$ = Q$(1)
For I1 = 0 To 4: PA%(I1) = CVI(Q$(I1 + 11)): Next I1
Close 1

9006 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9006
Return

6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Return

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
