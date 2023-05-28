Sub DRAFT ()

    _Title "3-in-1 Football - Team Draft"

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "DRAFT A NEW TEAM         "
        menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
        menuChoices$(3) = "EXIT TO MAIN MENU        "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 50, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%
                Case 1:

                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 1, 1: Color 15, 0

                    Call DraftChoices(N$, CC$, T2, 0)

                    Locate 23, 1: Print "HIT ANY KEY TO CONTINUE"
                    I$ = GetKeyPress$

                Case 2:
                    Call GetTeam_DRAFT(YN$, N$, CC$, T2)
                    Call DraftNewTeam(YN$, N$, CC$, T2)
                    Call DraftExistingTeam(YN$, T2)
                    Call DraftChoices(N$, CC$, T2, 1)

                Case 3: Cls

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       Something Subroutine
'----------------------------------------
'...explanation...
Sub DraftNewTeam (YN$, N$, CC$, T2)

    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%(), careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%(), careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared C0%(), C1%(), C2%(), C3%(), C4%(), C5%(), C6%(), C7%(), C8%(), C9%()
    Shared D%(), D1%(), D2%(), draftD3%(), D4%(), D5%(), D6%(), D7%(), D8%()
    Shared careerDI%(), careerDS%(), FI%(), FS%()
    Shared statsPA%(), draftSX%(), QA%(), careerQX%()

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()
    Shared C1$(), C2$(), C3$(), C4$(), C5$(), C6$(), C7$()
    Shared statsDI$(), statsDS$()
    Shared FI$(), FS$()

    A$ = N$
    AA$ = CC$

    For I = 0 To 7: careerB6%(I) = D6%(I): Next I

    For I = 0 To 4: statsPA%(I) = QA%(I): Next I

    For I = 0 To 9
        statsA1$(I) = C1$(I)
        careerA0%(I) = C0%(I)
        careerA1%(I) = C1%(I)
        careerA2%(I) = C2%(I)
        careerA3%(I) = C3%(I)
    Next I

    For I = 0 To 5
        statsA2$(I) = C2$(I)
        careerA4%(I) = C4%(I)
        careerA5%(I) = C5%(I)
    Next I

    For I = 0 To 3
        statsA3$(I) = C3$(I)
        careerA6%(I) = C6%(I)
        careerA7%(I) = C7%(I)
        careerA8%(I) = C8%(I)
        careerQX%(I) = draftSX%(I)
    Next I

    For I = 0 To 2
        statsA4$(I) = C4$(I)
        careerA9%(I) = C9%(I)
        careerB%(I) = D%(I)
    Next I

    For I = 0 To 2
        statsA5$(I) = C5$(I)
        careerB1%(I) = D1%(I)
        careerB2%(I) = D2%(I)
    Next I

    statsA6$(0) = C6$(0)
    careerB3%(0) = draftD3%(0)
    statsA7$(0) = C7$(0)
    careerB4%(0) = D4%(0)
    careerB5%(0) = D5%(0)
    careerB7%(0) = D7%(0)
    careerB8%(0) = D8%(0)
    statsA7$(1) = C7$(1)
    careerB4%(1) = D4%(1)
    careerB5%(1) = D5%(1)
    careerB7%(1) = D7%(1)
    careerB8%(1) = D8%(1)

    For I = 0 To 14
        statsDS$(I) = FS$(I)
        careerDS%(I) = FS%(I)
    Next I

    For I = 0 To 9
        statsDI$(I) = FI$(I)
        careerDI%(I) = FI%(I)
    Next I

    If Not _FileExists(diskPaths$(0) + "STADIUMS." + YN$) Then
        S$ = ""
        ATT = 0
    Else

        Open diskPaths$(0) + "STADIUMS." + YN$ For Random As #2 Len = 50
        Field #2, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)
        Get #2, T2
        S$ = RTrim$(Q$(1))
        ATT = CVL(Q$(3))
        Z$ = Q$(4)
        Close #2

        If Z$ <> "x" Then S$ = "": ATT = 0

    End If

End Sub


'----------------------------------------
'       DoTheDraft Subroutine
'----------------------------------------
'...explanation...
Sub DoTheDraft (YN$, NU)

    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%(), careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%(), careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared statsPA%(), careerQX%()

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()
    Shared statsDI$(), statsDS$()

    Shared careerDI%(), careerDS%()

    9600 '
    S0 = 0
    S1 = 0
    S2 = 0
    S3 = 0

    For I = 0 To 9:
        S0 = S0 + careerA0%(I)
        S2 = S2 + careerA2%(I)
    Next

    S4 = 100 / S0

    For I = 0 To 9:
        If careerA0%(I) <> 0 Then careerA0%(I) = Int(S4 * careerA0%(I) + .5) + S1: S1 = careerA0%(I)
        If careerA0%(I) > 100 Then careerA0%(I) = 100
    Next I

    For I = 0 To 5:
        S2 = S2 + careerA4%(I)
    Next

    S5 = 100 / S2

    For I = 0 To 9: If careerA2%(I) <> 0 Then careerA2%(I) = Int(S5 * careerA2%(I) + .5) + S3: S3 = careerA2%(I)
        If careerA2%(I) > 100 Then careerA2%(I) = 100
    Next I

    For I = 0 To 5: If careerA4%(I) <> 0 Then careerA4%(I) = Int(S5 * careerA4%(I) + .5) + S3: S3 = careerA4%(I)
        If careerA4%(I) > 100 Then careerA4%(I) = 100
    Next I

    S6 = 0
    S7 = 0

    For I = 0 To 3:
        S6 = S6 + careerA6%(I)
    Next

    S8 = 100 / S6

    For I = 0 To 3:
        If careerA6%(I) <> 0 Then careerA6%(I) = Int(S8 * careerA6%(I) + .5) + S7: S7 = careerA6%(I)
        If careerA6%(I) > 100 Then careerA6%(I) = 100
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 2: G0 = G0 + careerA9%(I): G3 = G3 + careerB1%(I): Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 2: If careerA9%(I) <> 0 Then careerA9%(I) = Int(G2 * careerA9%(I) + .5) + G1: G1 = careerA9%(I)
        If careerA9%(I) > 100 Then careerA9%(I) = 100
    Next

    For I = 0 To 2: If careerB1%(I) <> 0 Then careerB1%(I) = Int(G5 * careerB1%(I) + .5) + G4: G4 = careerB1%(I)
        If careerB1%(I) > 100 Then careerB1%(I) = 100
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 1:
        G0 = G0 + careerB5%(I)
        G3 = G3 + careerB8%(I)
    Next

    G2 = 100 / G0: G5 = 100 / G3

    For I = 0 To 1: If careerB5%(I) <> 0 Then careerB5%(I) = Int(G2 * careerB5%(I) + .5) + G1: G1 = careerB5%(I)
        If careerB5%(I) > 100 Then careerB5%(I) = 100
    Next

    For I = 0 To 1: If careerB8%(I) <> 0 Then careerB8%(I) = Int(G5 * careerB8%(I) + .5) + G4: G4 = careerB8%(I)
        If careerB8%(I) > 100 Then careerB8%(I) = 100
    Next

    For I = 9 To 0 Step -1:
        If careerA0%(I) <> 0 Then careerA0%(I) = 100: Exit For
    Next

    For I = 5 To 0 Step -1:
        If careerA4%(I) <> 0 Then careerA4%(I) = 100: Exit For
    Next

    For I = 3 To 0 Step -1:
        If careerA6%(I) <> 0 Then careerA6%(I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:
        If careerA9%(I) <> 0 Then careerA9%(I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:

        If careerB1%(I) <> 0 Then careerB1%(I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If careerB5%(I) <> 0 Then careerB5%(I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If careerB8%(I) <> 0 Then careerB8%(I) = 100: Exit For
    Next

    X = Int((Q / .3) + .5)
    If X < 0 Then X = 100 + X
    YC% = X

    If NU = 0 Then

        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

        fileLength& = LOF(1)

        Close 1

        NU = fileLength& / 934
        NU = NU + 1

    End If

    teamYear$ = RTrim$(YN$)
    
    Open diskPaths$(0) + "FOOT." + teamYear$ For Random As #1 Len = 934

    Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

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

    Field #1, 200 As Q$, 200 As Q$, 128 As Q$, 12 As Q$(122), 2 As Q$(123), 12 As Q$(124), 2 As Q$(125), 2 As Q$(126), 2 As Q$(127), 2 As Q$(128), 12 As Q$(129), 2 As Q$(130), 2 As Q$(131), 2 As Q$(132), 2 As Q$(133), 2 As Q$(134), 200 As Q$, 150 As Q$

    Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)

    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)

    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)

    Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)

    Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)

    LSet Q$(1) = A$
    LSet Q$(2) = AA$

    For I = 0 To 7: LSet Q$(I + 3) = MKI$(careerB6%(I)): Next 
    For I = 0 To 4: LSet Q$(I + 11) = MKI$(statsPA%(I)): Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = statsA1$(I)
        LSet Q$(I * 5 + 17) = MKI$(careerA0%(I))
        LSet Q$(I * 5 + 18) = MKI$(careerA1%(I))
        LSet Q$(I * 5 + 19) = MKI$(careerA2%(I))
        LSet Q$(I * 5 + 20) = MKI$(careerA3%(I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = statsA2$(I)
        LSet Q$(I * 3 + 67) = MKI$(careerA4%(I))
        LSet Q$(I * 3 + 68) = MKI$(careerA5%(I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = statsA3$(I)
        LSet Q$(I * 5 + 85) = MKI$(careerA6%(I))
        LSet Q$(I * 5 + 86) = MKI$(careerA7%(I))
        LSet Q$(I * 5 + 87) = MKI$(careerA8%(I))
        LSet Q$(I * 5 + 88) = MKI$(careerQX%(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = statsA4$(I)
        LSet Q$(I * 3 + 105) = MKI$(careerA9%(I))
        LSet Q$(I * 3 + 106) = MKI$(careerB%(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = statsA5$(I)
        LSet Q$(I * 3 + 114) = MKI$(careerB1%(I))
        LSet Q$(I * 3 + 115) = MKI$(careerB2%(I))
    Next I

    LSet Q$(122) = statsA6$(0)
    LSet Q$(123) = MKI$(careerB3%(0))
    LSet Q$(124) = statsA7$(0)
    LSet Q$(125) = MKI$(careerB4%(0))
    LSet Q$(126) = MKI$(careerB5%(0))
    LSet Q$(127) = MKI$(careerB7%(0))
    LSet Q$(128) = MKI$(careerB8%(0))
    LSet Q$(129) = statsA7$(1)
    LSet Q$(130) = MKI$(careerB4%(1))
    LSet Q$(131) = MKI$(careerB5%(1))
    LSet Q$(132) = MKI$(careerB7%(1))
    LSet Q$(133) = MKI$(careerB8%(1))
    LSet Q$(134) = MKI$(YC%)

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = statsDS$(I)
        LSet Q$(I * 2 + 136) = MKI$(careerDS%(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = statsDI$(I)
        LSet Q$(I * 2 + 166) = MKI$(careerDI%(I))
    Next I

    Put #1, NU

    Close #1

    Open diskPaths$(0) + "STADIUMS." + teamYear$ For Random As #2 Len = 50

    Field #2, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)

    LSet Q$(1) = A$
    LSet Q$(2) = S$
    LSet Q$(3) = MKL$(ATT)
    LSet Q$(4) = "x"

    Put #2, NU

    Close 2

End Sub


'----------------------------------------
'       GetTeam_DRAFT Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_DRAFT (YN$, N$, CC$, T2)

    Shared QA%()

    Do
        Color 15, 0

        Cls

        JJ = 7

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
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" Then
            Call SelectTeam_DRAFT(NY$, numberTeams, T2, N$, CC$)
        Else
            YN$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0

            Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

            fileLength& = LOF(1)

            Field #1, 15 As Q$(0), 640 As X$

            For I1 = 1 to fileLength& / 934
                Get #1, I1

                For I2 = 1 To 15
                    If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                Next I2

                If nullTeam <> 1 Then
                    numberTeams = numberTeams + 1
                    teamNames$(numberTeams) = Q$(0)
                    teamIndex%(numberTeams) = I1
                End If

            Next I1

            Close 1

            Call SelectTeam_DRAFT(YN$, numberTeams, T2, N$, CC$)

        End If

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color QA%(2), QA%(1): Print N$

            Color 15, 4

            Do
				Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

End Sub


'----------------------------------------
'       SelectTeam_DRAFT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_DRAFT (YY$, numberTeams, T2, N$, CC$)

    Selection% = 1
    Count% = numberTeams
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
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        T2 = teamIndex%(Selection%)
        teamYear$ = RTrim$(YY$)

        Open diskPaths$(0) + "FOOT." + teamYear$ For Random As #1 Len = 934

        Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

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

        Field #1, 200 As Q$, 200 As Q$, 128 As Q$, 12 As Q$(122), 2 As Q$(123), 12 As Q$(124), 2 As Q$(125), 2 As Q$(126), 2 As Q$(127), 2 As Q$(128), 12 As Q$(129), 2 As Q$(130), 2 As Q$(131), 2 As Q$(132), 2 As Q$(133), 2 As Q$(134), 200 As Q$, 150 As Q$

        Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)

        Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)

        Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)

        Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)

        Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)

        Get #1, T2

        N$ = RTrim$(Q$(1))
        CC$ = RTrim$(Q$(2))

        Call ReadTeam_DRAFT

    Else

        BO% = 1

    End If

End Sub

'----------------------------------------
'     ReadTeam_DRAFT Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam_DRAFT ()

    Dim C2!(15), C3!(15)

    Shared C0%(), C1%(), C2%(), C3%(), C4%(), C5%(), C6%(), C7%(), C8%(), C9%()
    Shared D%(), D1%(), D2%(), draftD3%(), D4%(), D5%(), D6%(), D7%(), D8%()
    Shared FI%(), FS%(), QA%(), draftSX%()

    Shared C1$(), C2$(), C3$(), C4$(), C5$(), C6$(), C7$()
    Shared FI$(), FS$()

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
        draftSX%(I) = CVI(Q$(I * 5 + 88))
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
    draftD3%(0) = CVI(Q$(123))
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

    For I = 0 To 15: C2!(I) = 0: C3!(I) = 0: Next

    U = 0

    For I = 1 To 9
        If C0%(I) = 0 Then
            U = U + 1
        Else
            C3!(I) = C0%(I) - C0%(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 9: C0%(I) = C3!(I): Next

    For I = 0 To 5: C2%(I + 10) = C4%(I): Next

    U = 0

    For I = 1 To 15
        If C2%(I) = 0 Then
            U = U + 1
        Else
            C2!(I) = C2%(I) - C2%(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 15: C2%(I) = C2!(I): Next

    For I = 0 To 5: C4%(I) = C2%(I + 10): Next

    For I = 3 To 1 Step -1
        If C6%(I) <> 0 Then
            C6%(I) = C6%(I) - C6%(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If C9%(I) <> 0 Then
            C9%(I) = C9%(I) - C9%(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If D1%(I) <> 0 Then
            D1%(I) = D1%(I) - D1%(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If D5%(I) <> 0 Then
            D5%(I) = D5%(I) - D5%(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If D8%(I) <> 0 Then
            D8%(I) = D8%(I) - D8%(I - 1)
        End If
    Next

End Sub

'----------------------------------------
'     DraftExistingTeam Subroutine
'----------------------------------------
'...explanation...
Sub DraftExistingTeam (YN$, NU)

    Shared careerB6%()
    Shared statsPA%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 1, 1: Color 15, 0
    Locate 3: Print " 1. ENTER DISK ID FOR THIS TEAM "; YN$
    Locate 4: Print " 2. TEAM # "; NU
    Locate 5: Print " 3. TEAM NAME "; A$
    Locate 6: Print " 4. TEAM NICKNAME "; AA$
    Locate 7: Print " 5. DEF RUSH ADJ "; careerB6%(0)
    Locate 8: Print " 6. DEF COMP % ADJ "; careerB6%(1)
    Locate 9: Print " 7. DEF INT % ADJ "; careerB6%(2)
    Locate 10: Print " 8. FUM % "; careerB6%(3)
    Locate 11: Print " 9. DEF FUM % "; careerB6%(4)
    Locate 12: Print "10. OFF SACKED % "; careerB6%(5)
    Locate 13: Print "11. DEF SACK ADJ "; careerB6%(6)
    Locate 14: Print "12. DEF YD/COMP ADJ "; Q
    Locate 15: Print "13. TM PENALTY ADJ "; statsPA%(0)
    Locate 16: Print "14. OFF RUN/PASS % "; careerB6%(7)
    Locate 17: Print "15. TM STADIUM (0-OUTDOOR 1-DOME) "; statsPA%(3)
    Locate 18: Print "16. POWER RATING "; statsPA%(4)
    Locate 19: Print "17. TM BACKGROUND COLOR (0-7) "; statsPA%(1)
    Locate 20: Print "18. TM TEXT COLOR (0-15) "; statsPA%(2)
    Locate 21: Print "19. STADIUM NAME "; S$
    Locate 22: Print "20. AVERAGE ATTENDANCE "; ATT

End Sub


'----------------------------------------
'       DraftChoices Subroutine
'----------------------------------------
'...explanation...
Sub DraftChoices (N$, CC$, T2, existingTeam)

    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%(), careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%(), careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared C0%(), C1%(), C2%(), C3%(), C4%(), C5%(), C6%(), C7%(), C8%(), C9%()
    Shared D%(), D1%(), D2%(), draftD3%(), D4%(), D5%(), D7%(), D8%()
    Shared statsPA%(), draftSX%(), QA%(), careerQX%()

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()
    Shared C1$(), C2$(), C3$(), C4$(), C5$(), C6$(), C7$()

    If existingTeam = 0 Then
        400 Locate 3: Input " 1. ENTER DISK ID FOR THIS TEAM "; YN$: If C1 = 1 Then 420
        401 Locate 4: Input " 2. TEAM # (0 FOR PROGRAM TO ASSIGN) "; NU: If C1 = 1 Then 420
        402 Locate 5: Input " 3. TEAM NAME "; A$: If C1 = 1 Then 420
        403 Locate 6: Input " 4. TEAM NICKNAME "; AA$: If C1 = 1 Then 420
        404 Locate 7: Input " 5. DEF RUSH ADJ "; careerB6%(0): If C1 = 1 Then 420
        405 Locate 8: Input " 6. DEF COMP % ADJ "; careerB6%(1): If C1 = 1 Then 420
        406 Locate 9: Input " 7. DEF INT % ADJ "; careerB6%(2): If C1 = 1 Then 420
        407 Locate 10: Input " 8. FUM % "; careerB6%(3): If C1 = 1 Then 420
        408 Locate 11: Input " 9. DEF FUM % "; careerB6%(4): If C1 = 1 Then 420
        409 Locate 12: Input "10. OFF SACKED % "; careerB6%(5): If C1 = 1 Then 420
        410 Locate 13: Input "11. DEF SACK ADJ "; careerB6%(6): If C1 = 1 Then 420
        411 Locate 14: Input "12. DEF YD/COMP ADJ "; Q: If C1 = 1 Then 420
        412 Locate 15: Input "13. TM PENALTY ADJ "; statsPA%(0): If C1 = 1 Then 420
        413 Locate 16: Input "14. OFF RUN/PASS % "; careerB6%(7): If C1 = 1 Then 420
        414 Locate 17: Input "15. TM STADIUM (0-OUTDOOR 1-DOME) "; statsPA%(3): If C1 = 1 Then 420
        415 Locate 18: Input "16. POWER RATING "; statsPA%(4): If C1 = 1 Then 420
        416 Locate 19: Input "17. TM BACKGROUND COLOR (0-7) "; statsPA%(1): If C1 = 1 Then 420
        417 Locate 20: Input "18. TM TEXT COLOR (0-15) "; statsPA%(2): If C1 = 1 Then 420
        418 Locate 21: Input "19. STADIUM NAME "; S$: If C1 = 1 Then 420
        419 Locate 22: Input "20. AVERAGE ATTENDANCE "; ATT: If C1 = 1 Then 420
    End If

    420 Locate 3, 40
    Color statsPA%(2), statsPA%(1): Print "SAMPLE TEXT COLOR"
    Color 15, 0

    C1 = 1

    Locate 23: Input "# TO CHANGE (0 TO CONTINUE ) "; B9
    On B9 GOTO 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419

    '0 = continue
    500 '
    Call TITLE

    Do

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Locate 5
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "R";: Color 3: Print "UNNING BACKS"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "W";: Color 3: Print "IDE RECEIVERS"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "Q";: Color 3: Print "UARTERBACKS"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "K";: Color 3: Print "ICKOFF RETURN"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "P";: Color 3: Print "UNT RETURN"
        Locate , 5: Color 3: Print "DRAFT P";: Color 15: Print "U";: Color 3: Print "NTERS"
        Locate , 5: Color 3: Print "DRAFT PLACE K";: Color 15: Print "I";: Color 3: Print "CKERS"
        Locate , 5: Color 15: Print "E";: Color 3: Print "ND AND SAVE DRAFT"

        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "R", "W", "Q", "K", "P", "U", "I": Call GetTeam_DRAFT(YN$, N$, CC$, NU)
        End Select

        Select Case UCase$(I$)

            Case "R":
                150 '

                Do
                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 2, 33
                    Color 15, 0: Print "RUNNING BACKS"
                    Locate 3, 1

                    Color statsPA%(2), statsPA%(1): Print A$;
                    Color 7, 0
                    Print Tab(17); "RUN AVG CX% RC AVG CX%";

                    Print Tab(41);

                    Color QA%(2), QA%(1): Print N$;
                    Color 7, 0
                    Print Tab(57); "RUN AVG CX% RC AVG CX%"

                    Color 15, 0

                    For I = 0 To 9: Locate I + 4, 1
                        Print I; " "; statsA1$(I); Tab(19);
                        Print Using "##.#  ###   ##  ###"; careerA1%(I) * .3; careerA0%(I); careerA3%(I) * 3; careerA2%(I)
                        Locate I + 4, 41
                        Print I; " "; C1$(I); Tab(59);
                        Print Using "##.#  ###   ##  ###"; C1%(I) * .3; C0%(I); C3%(I) * 3; C2%(I)
                    Next I

                    Do
                        Locate 16,
                        Print "SLOT TO DRAFT # (0-9, 10=NONE) ";: Input P
                    Loop Until P >= 0 And P <= 10

                    If P <> 10 Then
                        Do
                            Locate 16, 45
                            Print "PLAYER # TO DRAFT ";: Input P1
                        Loop Until P1 >= 0 And P1 <= 9

                        statsA1$(P) = C1$(P1)
                        careerA0%(P) = C0%(P1)
                        careerA1%(P) = C1%(P1)
                        careerA2%(P) = C2%(P1)
                        careerA3%(P) = C3%(P1)

                    End If

                Loop Until P = 10

            Case "W":
                250 '

                Do
                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 2, 33
                    Color 15, 0: Print "WIDE RECEIVERS"
                    Locate 3, 1

                    Color statsPA%(2), statsPA%(1): Print A$;
                    Color 7, 0
                    Print Tab(17); "REC AVG  CX% ";

                    Print Tab(41);

                    Color QA%(2), QA%(1): Print N$;
                    Color 7, 0
                    Print Tab(57); "REC AVG  CX%"

                    Color 15, 0

                    For I = 0 To 5:
                        Locate I + 4, 1: Print I; " "; statsA2$(I);
                        Print Tab(17); Using "  ##    ###"; careerA5%(I) * 3; careerA4%(I)
                        Locate I + 4, 41
                        Print I; " "; C2$(I);
                        Print Tab(57); Using "  ##    ###"; C5%(I) * 3; C4%(I)
                    Next I

                    Do
                        255 '
                        Locate 16, 5
                        Print "SLOT TO DRAFT # (0-5, 6=NONE) ";: Input P
                    Loop Until P >= 0 And P <= 6

                    If P <> 6 Then
                        Do
                            Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
                        Loop Until P1 >= 0 And P1 <= 5

                        statsA2$(P) = C2$(P1)
                        careerA4%(P) = C4%(P1)
                        careerA5%(P) = C5%(P1)

                    End If

                Loop Until P = 6

            Case "Q":

                350 '

                Do
                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 2, 34
                    Color 15, 0: Print "QUARTERBACKS"
                    Locate 3, 1

                    Color statsPA%(2), statsPA%(1): Print A$;
                    Color 7, 0: Print Tab(17); " C%  I%   ARM  CX% ";

                    Print Tab(41);

                    Color QA%(2), QA%(1): Print N$;

                    Color 7, 0: Print Tab(57); " C%  I%   ARM  CX%"

                    Color 15, 0

                    For I = 0 To 3: Locate I + 4, 1
                        Print I; " "; statsA3$(I);
                        Print Tab(17); Using "###  ##  ###  ###"; careerA7%(I); careerA8%(I); careerQX%(I); careerA6%(I)
                        Locate I + 4, 41
                        Print I; " "; C3$(I);
                        Print Tab(57); Using "###  ##  ###  ###"; C7%(I); C8%(I); draftSX%(I); C6%(I)
                    Next I

                    355 '
                    Do
                        Locate 16, 5
                        Print "SLOT TO DRAFT # (0-3, 4=NONE) ";: Input P
                    Loop Until P >= 0 And P <= 4

                    If P <> 4 Then

                        Do
                            Locate 16, 45
                            Print "PLAYER # TO DRAFT ";: Input P1
                        Loop Until P1 >= 0 And P1 <= 3

                        statsA3$(P) = C3$(P1)
                        careerA6%(P) = C6%(P1)
                        careerA7%(P) = C7%(P1)
                        careerA8%(P) = C8%(P1)
                        careerQX%(P) = draftSX%(P1)

                    End If

                Loop Until P = 4

            Case "K":
                450 '
                Do
                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 2, 34
                    Color 15, 0: Print "KICKOFF RETURNERS"
                    Locate 3, 1

                    Color statsPA%(2), statsPA%(1): Print A$;
                    Color 7, 0
                    Print Tab(17); "RET AVG  CX%";

                    Print Tab(41);

                    Color QA%(2), QA%(1): Print N$;
                    Color 7, 0
                    Print Tab(57); "RET AVG  CX%"

                    Color 15, 0

                    For I = 0 To 2: Locate I + 4, 1
                        Print I; " "; statsA4$(I);
                        Print Tab(17); Using "  ##     ###"; careerB%(I) * 3; careerA9%(I)
                        Locate I + 4, 41
                        Print I; " "; C4$(I);
                        Print Tab(57); Using "  ##     ###"; D%(I) * 3; C9%(I)
                    Next I

                    455 '
                    Do
                        Locate 16, 5
                        Print "SLOT TO DRAFT (0-2, 3=NONE) ";: Input P
                    Loop Until P >= 0 And P <= 3

                    If P <> 3 Then

                        457 '
                        Do
                            Locate 16, 45
                            Print "PLAYER # TO DRAFT ";: Input P1
                        Loop Until P1 >= 0 And P1 <= 2

                        statsA4$(P) = C4$(P1)
                        careerA9%(P) = C9%(P1)
                        careerB%(P) = D%(P1)

                    End If

                Loop Until P = 3

            Case "P":
                550 '
                Do
                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 2, 34
                    Color 15, 0: Print "PUNT RETURNERS"
                    Locate 3, 1

                    Color statsPA%(2), statsPA%(1): Print A$;
                    Color 7, 0
                    Print Tab(17); "RET AVG  CX%";

                    Print Tab(41);

                    Color QA%(2), QA%(1): Print N$;
                    Color 7, 0
                    Print Tab(57); "RET AVG  CX%"

                    Color 15, 0

                    For I = 0 To 2
                        Locate I + 4, 1
                        Print I; " "; statsA5$(I);
                        Print Tab(17); Using "  ##    ###"; careerB2%(I) * 3; careerB1%(I)
                        Locate I + 4, 41
                        Print I; " "; C5$(I);
                        Print Tab(57); Using "  ##    ###"; D2%(I) * 3; D1%(I)
                    Next I

                    Do
                        Locate 16, 5
                        Print "SLOT TO DRAFT (0-2, 3=NONE) ";: Input P
                    Loop Until P >= 0 And P <= 3

                    If P <> 3 Then
                        Do
                            Locate 16, 45
                            Print "PLAYER # TO DRAFT ";: Input P1
                        Loop Until P1 >= 0 And P1 <= 2

                        statsA5$(P) = C5$(P1)
                        careerB1%(P) = D1%(P1)
                        careerB2%(P) = D2%(P1)

                    End If

                Loop Until P = 3

            Case "U":
                650 '
                Do
                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 2, 37
                    Color 15, 0: Print "PUNTER"
                    Locate 3, 1

                    Color statsPA%(2), statsPA%(1): Print A$;
                    Color 7, 0
                    Print Tab(17); "PUNT AVG";

                    Print Tab(41);

                    Color QA%(2), QA%(1): Print N$;
                    Color 7, 0
                    Print Tab(57); "PUNT AVG"

                    Color 15, 0

                    Locate 4, 1
                    Print 0; " "; statsA6$(0);
                    Print Tab(17); Using "   ##"; careerB3%(0) * 2 + 28

                    Locate 4, 41
                    Print 0; " "; C6$(0);
                    Print Tab(57); Using "   ##"; draftD3%(0) * 2 + 28

                    Do
                        Locate 16, 5
                        Print "PLAYER # (0, 1=NONE) ";: Input P
                    Loop Until P >= 0 And P <= 1

                    If P <> 1 Then
                        Locate 20, 1: Print "ANY KEY TO CONTINUE"
                        I$ = GetKeyPress$

                        statsA6$(0) = C6$(0)
                        careerB3%(0) = draftD3%(0)
                    End If

                Loop Until P = 1

            Case "I":
                750 '
                Do
                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 2, 34
                    Color 15, 0: Print "PLACE KICKERS"
                    Locate 3, 1

                    Color statsPA%(2), statsPA%(1): Print A$;
                    Color 7, 0
                    Print Tab(17); "FG%  CX   EP%  CX";

                    Print Tab(41);

                    Color QA%(2), QA%(1): Print N$;
                    Color 7, 0
                    Print Tab(57); "FG%  CX   EP%  CX"

                    Color 15, 0

                    For I = 0 To 1:
                        Locate I + 4, 1
                        Print I; " "; statsA7$(I);
                        Print Tab(17); Using "### ###   ### ###"; careerB4%(I) * 5 + 25; careerB5%(I); careerB7%(I); careerB8%(I)
                        Locate I + 4, 41
                        Print I; " "; C7$(I);
                        Print Tab(57); Using "### ###   ### ###"; D4%(I) * 5 + 25; D5%(I); D7%(I); D8%(I)
                    Next I

                    Do
                        Locate 16, 5
                        Print "PLAYER # (0-1, 2=NONE) ";: Input P
                    Loop Until P >= 0 And P <= 2

                    If P <> 2 Then
                        Do
                            Locate 16, 45
                            Print "PLAYER # TO DRAFT ";: Input P1
                        Loop Until P1 >= 0 And P1 <= 1

                        statsA7$(P) = C7$(P1)
                        careerB4%(P) = D4%(P1)
                        careerB5%(P) = D5%(P1)
                        careerB7%(P) = D7%(P1)
                        careerB8%(P) = D8%(P1)

                    End If

                Loop Until P = 2

            Case "E":
                Call TITLE

                For XY = 2 To 23:
                    Locate XY, 1: Print Tab(79);
                Next

                Call DoTheDraft(YN$, T2)

        End Select

    Loop Until UCase$(I$) = "E"

End Sub
