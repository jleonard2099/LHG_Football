DECLARE FUNCTION FNP(X)
DECLARE SUB ADDBACKS ()
DECLARE SUB ADDTIME ()
DECLARE SUB BLITZ ()
DECLARE SUB BLITZ2 ()
DECLARE SUB BOX2SCREEN ()
DECLARE SUB BOXDRAW (J)
DECLARE SUB CHANGEPOSS ()
DECLARE SUB CHANGEQB()
DECLARE SUB COMPCHEAT ()
DECLARE SUB DBOX ()
DECLARE SUB DECIDE ()
DECLARE SUB DEFENSE ()
DECLARE SUB DEFMENU ()
DECLARE SUB DISPFIELD ()
DECLARE SUB DISPWEATHER ()
DECLARE SUB FINDSPACE ()
DECLARE SUB FINDTEAMS ()
DECLARE SUB GETSPEED ()
DECLARE SUB GETTEAM ()
DECLARE SUB GOALLINE ()
DECLARE SUB HALFSTART ()
DECLARE SUB INCOMPLETE ()
DECLARE SUB KICKER ()
DECLARE SUB LGPBP ()
DECLARE SUB MARKER ()
DECLARE SUB MARKER1 ()
DECLARE SUB MEASUREMENT ()
DECLARE SUB MOVEBALL ()
DECLARE SUB NAMEEDIT ()
DECLARE SUB OFFMENU ()
DECLARE SUB OVERUSE ()
DECLARE SUB PASSMENU ()
DECLARE SUB PASSPBP ()
DECLARE SUB PBOX ()
DECLARE SUB PBP ()
DECLARE SUB PBPINT ()
DECLARE SUB PCPBP ()
DECLARE SUB PLACEPBP ()
DECLARE SUB PRINTBOX ()
DECLARE SUB PUNTYARDS ()
DECLARE SUB QBBOX ()
DECLARE SUB QBSNEAK ()
DECLARE SUB SAVESTATS ()
DECLARE SUB SCOREBOARD ()
DECLARE SUB SCOUT ()
DECLARE SUB SCOUTREPT ()
DECLARE SUB SELECTTEAM ()
DECLARE SUB SETSCREEN ()
'DECLARE SUB SETUP ()
DECLARE SUB SETWEATHER ()
DECLARE SUB STATS2SCREEN ()
DECLARE SUB STUFFED ()
DECLARE SUB TICKER ()
DECLARE SUB TICKUPDATE ()
DECLARE SUB YARDLINE ()

Dim scheduleFile$

Dim Shared A$, A1$, A2$, A3$, A4$, A5$, A6$, A7$, AA$, D2$, DI$, DN$, DR$, DS$, DV$
Dim Shared F$, G$, strG9$, I$, LO$, NM$, NN$, PS$, Q$, RP$, RV$, NY$
Dim Shared SD$, SITE$, ST$, SX$, U$, U5$, X$, Y8$, Y9$, YN$
 
Dim Shared A$(1), A1$(1, 10), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2), A7$(1, 1)
Dim Shared AA$(1), B$(1), D$(15), D1$(11), D2$(15), DI$(1, 9), DN$(4), DR$(1), DS$(1, 14), diskPaths$(1)
Dim Shared G$(3), strG9$(5), H$(120), HO$(120), strIR$(1, 9), strKR$(1, 2)
Dim Shared strLC$(1, 20, 1), LF$(1, 1), LI$(1, 9, 1), LK$(1, 2, 1), LO$(2), LP$(1, 3, 1), strLR$(1, 9, 1)
Dim Shared O$(50), NN$(1), P$(2), PK$(1, 2, 1), strPR$(1, 2), PS$(9), Q$(203), strQB$(1, 3)
Dim Shared R$(14), strRB$(1, 17), RP$(30), SD$(3), SITE$(120), SX$(32, 1)
Dim Shared U5$(3), teamNames$(500), strWR$(1, 20), Y$(1), Y8$(3), Y9$(5), YN$(1), YR%(1)

Dim Shared A, A2, A3, A4, A5, A6, A7, A8, A9, AP, ATT
Dim Shared B, B1, B2, B3, B4, B5, B7, B8, BW, CP, CT
Dim Shared D, D1, D2, D3, DDI, DDS, DI, DR, DS, DT, E, EY
Dim Shared F2, F8, F9, G, G9, I1, I2, I3, I4, I5, I6, I7, I8, I9, I
Dim Shared J, J6, JJ, K3, N, NT, O, OT, P1, P2, PN, PQ, PR, PW, Q, QB, R1, RP, RY
Dim Shared S, S2, S3, S6, SN, SY, T, T1, TE, TMT, TP, W5, WE, WS
Dim Shared X, X1, X2, XD, XE, Y, Y1, Y9, YC, YF, YL, YT, Z1

Dim Shared pbpDelay!

Dim Shared A(1, 9), A1(1, 9), A2(1, 9), A3(1, 9), A4(1, 9), A5(1, 9), A6(1, 9), A7(1, 9), A8(1, 9), A9(1, 2)
Dim Shared AF(1, 1, 4), AM(1, 1, 4), B(1, 2), B1(1, 2), B2(1, 2), B3(1, 2), B4(1, 1), B5(1, 1), B6(1, 7), B7(1, 1), B8(1, 1)
Dim Shared C(50), CRD(120), DDI(1), DDS(1), DI(1, 9), DS(1, 14), F(4), F1(4), FA(1, 1, 4), FL(1, 1), FM(1, 1, 4)
Dim Shared GI(1, 9, 2), GS(1, 14, 1), IR(1, 9), J6(1), K(1, 36), K1(50, 6), K2(1, 13, 17), K3(1, 6), KR(1, 2)
Dim Shared LC(1, 20), LF(1, 1), LI(1, 9), LK(1, 2), LP(1, 3), LR(1, 9), OT(1), P(2), PK(1, 2), PR(1, 2)
Dim Shared QB(1, 3), RB(1, 17), S(1, 10), SI(1, 9, 2), SK(1, 14, 1), T(50), T1(1), TP(1)
Dim Shared W6(1, 1), WR(1, 20), XD(1), YC(1), Z(38), Z1(38), Z2(13, 17)

Dim Shared AP%(2), intB8%(2), BY%(38, 4), D3%(7, 10), intDI%(1), intDS%(1)
Dim Shared intG9%(1), GL%(31, 2), HB%(1)
Dim Shared NG%(20), O%(120), PA%(2, 9), PC%(1, 9), intPR%(1, 120), PS%(2, 21)
Dim Shared Q6%(1), Q7%(1), QR%(50, 2), QX%(1, 3), R9%(1), RM%(1, 14), RN%(1, 38), RQ%(1), RV%(1)
Dim Shared S1%(3, 10, 11), S2%(5, 10, 14), ST%(32), SX%(32, 2, 14), TE%(1), TF%(1), Time%(120)
Dim Shared teamIndex%(500), V4%(1, 3), WX%(6)

Dim Shared AP%, intB8%, BO%, C%, EG%, F%, GL%, H%, HB%, HT%, intI%, intDI%, intDS%
Dim Shared PA%, PC%, PS%, Q6%, Q7%, QBN%, QX%, R5%, RF%, S%, S1%, S2%, ST%, SX%, TS%
Dim Shared W%, WX%, intY8%, intYL%