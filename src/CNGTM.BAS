Sub CNGTM ()

    _Title "3-in-1 Football - Change Team"

    ReDim menuChoices$(1 To 4)

    Do

        Call TITLE
        menuChoices$(1) = "CHANGE OR EDIT AN EXISTING  TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER             "
        menuChoices$(3) = "VIEW A TEAM ROSTER              "
        menuChoices$(4) = "EXIT TO MAIN MENU               "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 12, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        'In this case, RN is actually used later
        'to control flow / operations
        'It should probably be broken into
        'different functions based on value
        RN = Selection%

        If ExitCode% <> 27 Then

            Call GetTeam_CNGTM(YN$)

            Select Case RN
                Case 1
                    Call ChangeTeam(YN$)

                Case 2
                    Call PrintRoster

                Case 3
                    Call ViewRoster
            End Select

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub

'----------------------------------------
'      GetTeam_CNGTM Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CNGTM (YN$)

    Shared PA%()

    Do
        Color 15, 0
        Cls
        JJ = 7

        Locate 6, 8: Color 15, 4
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
            Call SelectTeam_CNGTM(NTMS, YN$)
        Else
            YN$ = NY$
            Erase teamNames$

            NTMS = 0
            Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
            Y = LOF(1)
            Field #1, 15 As Q$(0), 640 As X$

            For I1 = 1 To Y / 934
                Get #1, I1
                For I2 = 1 To 15
                    If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam% = 1: Exit For
                Next I2
                If nullTeam% <> 1 Then
                    NTMS = NTMS + 1
                    teamNames$(NTMS) = Q$(0): teamIndex%(NTMS) = I1
                End If
            Next I1

            Close 1
            'TS% = Int((NTMS - 1) / 80)

            Call SelectTeam_CNGTM(NTMS, YN$)

        End If

        A$ = Q$(1)
        AA$ = Q$(2)

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33: Color PA%(2), PA%(1): Print A$
            Color 15, 4
            Locate , 33: Print "ANY CHANGE (YN)";
            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

End Sub

'----------------------------------------
'      SelectTeam_CNGTM Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CNGTM (NTMS, YN$)

    Selection% = 1
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

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% = 27 Then BO% = 1: Exit Sub

    B8% = teamIndex%(Selection%)

    Call ReadTeam_CNGTM(YN$, B8%)

End Sub

'----------------------------------------
'      ReadTeam_CNGTM Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam_CNGTM (YN$, B8%)

    Shared A1$(), A2$(), A3$(), A4$()
    Shared A5$(), A6$(), A7$()
    Shared DI$(), DS$()
    Shared A0%(), A1%(), A2%(), A3%(), A4%()
    Shared A5%(), A6%(), A7%(), A8%(), A9%()
    Shared B%(), B1%(), B2%(), B3%(), B4%()
    Shared B5%(), B6%(), B7%(), B8%()
    Shared DI%(), DS%(), PA%(), QX%()

    Dim C2(15), C3(15)

    Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
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
    Get #1, B8%

    For I = 0 To 7: B6%(I) = CVI(Q$(I + 3)): Next I

    For I = 0 To 4: PA%(I) = CVI(Q$(I + 11)): Next I

    For I = 0 To 9
        A1$(I) = Q$(I * 5 + 16)
        A0%(I) = CVI(Q$(I * 5 + 17))
        A1%(I) = CVI(Q$(I * 5 + 18))
        A2%(I) = CVI(Q$(I * 5 + 19))
        A3%(I) = CVI(Q$(I * 5 + 20))
    Next I

    For I = 0 To 5
        A2$(I) = Q$(I * 3 + 66)
        A4%(I) = CVI(Q$(I * 3 + 67))
        A5%(I) = CVI(Q$(I * 3 + 68))
    Next I

    For I = 0 To 3
        A3$(I) = Q$(I * 5 + 84)
        A6%(I) = CVI(Q$(I * 5 + 85))
        A7%(I) = CVI(Q$(I * 5 + 86))
        A8%(I) = CVI(Q$(I * 5 + 87))
        QX%(I) = CVI(Q$(I * 5 + 88))
    Next I

    For I = 0 To 2
        A4$(I) = Q$(I * 3 + 104)
        A9%(I) = CVI(Q$(I * 3 + 105))
        B%(I) = CVI(Q$(I * 3 + 106))
    Next I

    For I = 0 To 2
        A5$(I) = Q$(I * 3 + 113)
        B1%(I) = CVI(Q$(I * 3 + 114))
        B2%(I) = CVI(Q$(I * 3 + 115))
    Next I

    A6$(0) = Q$(122)
    B3%(0) = CVI(Q$(123))
    A7$(0) = Q$(124)
    B4%(0) = CVI(Q$(125))
    B5%(0) = CVI(Q$(126))
    B7%(0) = CVI(Q$(127))
    B8%(0) = CVI(Q$(128))
    A7$(1) = Q$(129)
    B4%(1) = CVI(Q$(130))
    B5%(1) = CVI(Q$(131))
    B7%(1) = CVI(Q$(132))
    B8%(1) = CVI(Q$(133))
    YC! = CVI(Q$(134))

    For I = 0 To 14
        DS$(I) = Q$(I * 2 + 135)
        DS%(I) = CVI(Q$(I * 2 + 136))
    Next I

    For I = 0 To 9
        DI$(I) = Q$(I * 2 + 165)
        DI%(I) = CVI(Q$(I * 2 + 166))
    Next I

    Close #1

    If _FileExists(diskPaths$(0) + "STADIUMS." + YN$) Then

        Open diskPaths$(0) + "STADIUMS." + YN$ For Random As #2 Len = 50
        Field #2, 15 As X$, 30 As QQ$(1), 4 As QQ$(3), 1 As QQ$(4)
        Get #2, B8%
        S$ = RTrim$(QQ$(1))
        ATT& = CVL(QQ$(3))
        Z$ = QQ$(4)
        Close #2

        If Z$ <> "x" Then S$ = "": ATT& = 0

    Else
        S$ = ""
        ATT& = 0
    End If

    For I = 0 To 15: C2(I) = 0: C3(I) = 0: Next

    U = 0

    For I = 1 To 9
        If A0%(I) = 0 Then
            U = U + 1
        Else
            C3(I) = A0%(I) - A0%(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 9: A0%(I) = C3(I): Next
    For I = 0 To 5: A2%(I + 10) = A4%(I): Next

    U = 0

    For I = 1 To 15
        If A2%(I) = 0 Then
            U = U + 1
        Else
            C2(I) = A2%(I) - A2%(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 15: A2%(I) = C2(I): Next
    For I = 0 To 5: A4%(I) = A2%(I + 10): Next

    For I = 3 To 1 Step -1
        If A6%(I) <> 0 Then
            A6%(I) = A6%(I) - A6%(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If A9%(I) <> 0 Then
            A9%(I) = A9%(I) - A9%(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If B1%(I) <> 0 Then
            B1%(I) = B1%(I) - B1%(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If B5%(I) <> 0 Then
            B5%(I) = B5%(I) - B5%(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If B8%(I) <> 0 Then
            B8%(I) = B8%(I) - B8%(I - 1)
        End If
    Next

End Sub

'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub PrintRoster ()

    Shared A1$(), A2$(), A3$(), A4$()
    Shared A5$(), A6$(), A7$()

    Shared A0%(), A1%(), A2%(), A3%(), A4%()
    Shared A5%(), A6%(), A7%(), A8%(), A9%()

    Shared B%(), B1%(), B2%(), B3%(), B4%()
    Shared B5%(), B6%(), B7%(), B8%()
    Shared PA%(), QX%()

    ' *** PRINT TEAM ROSTER ***
    LPrint "TEAM ROSTER FOR "; A$; " "; AA$; " TEAM NUMBER ="; B8%: LPrint
    LPrint Using "DEF RUSH ADJ   = ###"; B6%(0);: LPrint Tab(35); Using "SACKED %         = ##"; B6%(5)
    LPrint Using "DEF COMP % ADJ = ###"; B6%(1);: LPrint Tab(35); Using "DEF SACK ADJ     = ##"; B6%(6)
    LPrint Using "DEF INT % ADJ  = ###"; B6%(2);: LPrint Tab(35); Using "RUN/PASS %       = ##"; B6%(7)
    LPrint Using "OFF FUMBLE  %  = ###"; B6%(3);: LPrint Tab(35); "DEF YDS/COMP ADJ = ";

    If YC! < 50 Then LPrint Using "+#.#"; YC! / 100 * 30
    If YC! > 50 Then LPrint Using "+#.#"; (100 - YC!) / 100 * -30

    LPrint Using "DEF FUMBLE  %  = ###"; B6%(4);: LPrint Tab(35); Using "PENALTY ADJ      = ##"; PA%(0)
    LPrint Using "STAD TYPE      = ###"; PA%(3);: LPrint Tab(35); Using "GEOGRAPH REGION  = ##"; PA%(4)
    LPrint Using "BACKGRND COLOR = ###"; PA%(1);: LPrint Tab(35); Using "FOREGRND COLOR   = ##"; PA%(2)
    LPrint
    LPrint "RUNNING BACKS-"
    LPrint "NAME-               RUSH AVG   CX%  REC AVG   CX%"

    For I = 0 To 9:
        If Left$(A1$(I), 1) <> " " And Left$(A1$(I), 3) <> "END" Then
            LPrint A1$(I); Tab(23);
            LPrint Using "##.#     ###    ##     ###"; A1%(I) * .3; A0%(I); A3%(I) * 3; A2%(I)
        End If
    Next

    LPrint
    LPrint "WIDE RECEIVERS-"
    LPrint "NAME-              REC AVG     CX%"

    For I = 0 To 5:
        If Left$(A2$(I), 1) <> " " And Left$(A2$(I), 3) <> "END" Then
            LPrint A2$(I); Tab(23);
            LPrint Using "##       ###"; A5%(I) * 3; A4%(I)
        End If
    Next

    LPrint
    LPrint "QUARTERBACKS-                     YDS/CMP"
    LPrint "NAME-               COMP%   INT%    ADJ     CX%"

    For I = 0 To 3:
        If Left$(A3$(I), 1) <> " " And Left$(A3$(I), 3) <> "END" Then
            LPrint A3$(I); Tab(21);
            LPrint Using "###     ##     ###      ###"; A7%(I); A8%(I); QX%(I); A6%(I)
        End If
    Next

    LPrint
    LPrint "KICKOFF RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:
        If Left$(A4$(I), 1) <> " " And Left$(A4$(I), 3) <> "END" Then
            LPrint A4$(I); Tab(23);
            LPrint Using "##       ###"; B%(I) * 3; A9%(I)
        End If
    Next

    LPrint
    LPrint "PUNT RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:
        If Left$(A5$(I), 1) <> " " And Left$(A5$(I), 3) <> "END" Then
            LPrint A5$(I); Tab(23);
            LPrint Using "##       ###"; B2%(I) * 3; B1%(I)
        End If
    Next

    LPrint
    LPrint "PLACE KICKERS-"
    LPrint "NAME-                FG%  CX%   EP%  CX%"

    For I = 0 To 1:
        If Left$(A7$(I), 1) <> " " And Left$(A7$(I), 3) <> "END" Then
            LPrint A7$(I); Tab(21);
            LPrint Using "###   ###   ###  ###"; B4%(I) * 5 + 25; B5%(I); B7%(I); B8%(I)
        End If
    Next

    LPrint
    LPrint "PUNTER-"
    LPrint "NAME-              AVG"
    LPrint A6$(0); Tab(21);: LPrint Using "##"; B3%(0) * 2 + 28
    LPrint Chr$(12)

End Sub

'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub ChangeTeam (YN$)

    Shared A1$(), A2$(), A3$(), A4$()
    Shared A5$(), A6$(), A7$()

    Shared DI$(), DS$()
    Shared A1%(), A3%(), A5%(), A7%(), A8%()
    Shared B%(), B1%(), B2%(), B3%(), B4%()
    Shared B5%(), B6%(), B7%(), B8%()
    Shared DI%(), DS%()
    Shared PA%(), QX%()

    Dim A1!(9), A3!(9), A5!(9)
    Dim B!(9), B2!(1), B3!(2), B4!(2)

    Do
        1000 ' *** CHANGE EXISTING TEAM FILES ***
        Cls
        U$ = "CHANGE/EDIT AN EXISTING TEAM"
        Locate 1, 40 - Len(U$) / 2
        Color 14: Print U$
        Locate 3, 1
        Color 12: Print "(1) ";
        Color 15: Print "TEAM RATINGS"
        Color 12: Print "(2) ";
        Color 15: Print "RUNNING BACKS"
        Color 12: Print "(3) ";
        Color 15: Print "WIDE RECEIVERS"
        Color 12: Print "(4) ";
        Color 15: Print "QUARTERBACKS"
        Color 12: Print "(5) ";
        Color 15: Print "KICK-OFF RETURNMEN"
        Color 12: Print "(6) ";
        Color 15: Print "PUNT RETURNMEN"
        Color 12: Print "(7) ";
        Color 15: Print "PUNTER"
        Color 12: Print "(8) ";
        Color 15: Print "PLACE KICKERS"
        Color 12: Print "(9) ";
        Color 15: Print "PLAYERS W/SACKS"
        Color 12: Print "(A) ";
        Color 15: Print "PLAYERS W/INTERCEPTIONS"
        Color 12: Print "(B) ";
        Color 15: Print "RETURN TO TEAM MANAGEMENT MENU"
        Color 12: Print "(S) ";
        Color 15: Print "SAVE CHANGES"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
            If UCase$(I$) = "A" Then I = 10
            If UCase$(I$) = "B" Then I = 11
            'If UCase$(I$) = "S" Then I = 12
        Loop Until I >= 1 And I <= 11 Or UCase$(I$) = "S"

        Select Case I

            Case 1

                Do
                    1100 '
                    Cls
                    U$ = "CHANGE TEAM RATINGS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Color 15
                    Locate 2: Print " 1. TEAM NUMBER "; B8%
                    Locate 3: Print " 2. TEAM NAME "; A$
                    Locate 4: Print " 3. NICKNAME "; AA$
                    Locate 5: Print " 4. DEF RUSHING ADJ "; B6%(0)
                    Locate 6: Print " 5. DEF COMPLETION % ADJ "; B6%(1)
                    Locate 7: Print " 6. DEF INTERCEPTION % ADJ "; B6%(2)
                    Locate 8: Print " 7. OFF FUMBLE % "; B6%(3)
                    Locate 9: Print " 8. DEF FUMBLE % "; B6%(4)
                    Locate 10: Print " 9. OFF SACK ADJ % "; B6%(5)
                    Locate 11: Print "10. DEF SACK ADJ % "; B6%(6)
                    Locate 12: Print "11. DEF YDS/COMP ADJ ";
                    If YC! < 50 Then Print Using "+#.#"; YC! / 100 * 30
                    If YC! > 50 Then Print Using "+#.#"; (100 - YC!) / 100 * -30
                    Locate 13: Print "12. TM PENALTY ADJ "; PA%(0)
                    Locate 14: Print "13. OFF RUN/PASS % "; B6%(7)
                    Locate 15: Print "14. TM STADIUM (0-OUTDOOR 1-DOME) "; PA%(3)
                    Locate 16: Print "15. POWER INDEX "; PA%(4)
                    Locate 17: Print "16. TM BACKGROUND COLOR (0-7) "; PA%(1)
                    Locate 18: Print "17. TM TEXT COLOR (0-15) "; PA%(2)
                    Locate 19: Print "18. DISK ID "; YN$
                    Locate 20: Print "19. STADIUM NAME "; S$
                    Locate 21: Print "20. AVERAGE ATTENDANCE "; ATT&
                    Locate 22: Color PA%(2), PA%(1): Print "SAMPLE TEAM COLOR"
                    Color 15, 0

                    1170
                    Locate 22: Color PA%(2), PA%(1): Print "SAMPLE TEAM COLOR"
                    Color 15, 0
                    C1 = 1
                    Do
                        Locate 23: Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 20

                    Select Case B9

                        Case 1
                            Locate 2: Input " 1. TEAM NUMBER "; B8%
                            If C1 = 1 Then 1170

                        Case 2
                            Locate 3: Input " 2. TEAM NAME "; A$
                            If C1 = 1 Then 1170

                        Case 3
                            Locate 4: Input " 3. NICKNAME "; AA$
                            If C1 = 1 Then 1170

                        Case 4
                            Locate 5: Input " 4. DEF RUSHING ADJ "; B6%(0)
                            If C1 = 1 Then 1170

                        Case 5
                            Locate 6: Input " 5. DEF COMPLETION % ADJ "; B6%(1)
                            If C1 = 1 Then 1170

                        Case 6
                            Locate 7: Input " 6. DEF INTERCEPTION % ADJ "; B6%(2)
                            If C1 = 1 Then 1170

                        Case 7
                            Locate 8: Input " 7. OFF FUMBLE % "; B6%(3)
                            If C1 = 1 Then 1170

                        Case 8
                            Locate 9: Input " 8. DEF FUMBLE % "; B6%(4)
                            If C1 = 1 Then 1170

                        Case 9
                            Locate 10: Input " 9. OFF SACK ADJ % "; B6%(5)
                            If C1 = 1 Then 1170

                        Case 10
                            Locate 11: Input "10. DEF SACK ADJ % "; B6%(6)
                            If C1 = 1 Then 1170

                        Case 11
                            Locate 12: Input "11. DEF YDS/COMP ADJ "; YC!
                            If YC! <= 0 Then YC! = 100 + (YC! * 10 / 3): GoTo 1170

                        Case 12
                            Locate 13: Input "12. TM PENALTY ADJ "; PA%(0)
                            If C1 = 1 Then 1170

                        Case 13
                            Locate 14: Input "13. OFF RUN/PASS % "; B6%(7)
                            If C1 = 1 Then 1170

                        Case 14
                            Locate 15: Input "14. TM STADIUM (0-OUTDOOR 1-DOME) "; PA%(3)
                            If C1 = 1 Then 1170

                        Case 15
                            Locate 16: Input "15. POWER INDEX "; PA%(4)
                            If C1 = 1 Then 1170

                        Case 16
                            Locate 17: Input "16. TM BACKGROUND COLOR (0-7) "; PA%(1)
                            If C1 = 1 Then 1170

                        Case 17
                            Locate 18: Input "17. TM TEXT COLOR (0-15) "; PA%(2)
                            If C1 = 1 Then 1170

                        Case 18
                            Locate 19: Input "18. DISK ID "; YN$
                            If C1 = 1 Then 1170

                        Case 19
                            Locate 20: Input "19. STADIUM NAME "; S$
                            If C1 = 1 Then 1170

                        Case 20
                            Locate 21: Input "20. AVERAGE ATTENDANCE "; ATT&
                    End Select

                Loop Until B9 = 0

                Cls

                U$ = "CHANGE TEAM RATINGS"
                Locate 1, 40 - Len(U$) / 2
                Color 14: Print U$
                Color 15
                YC! = 10 / 3 * YC!

            Case 2
                1200 '
                Do
                    Cls
                    U$ = "EDIT RUNNING BACKS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "RUSH AVG  REC AVG"

                    For I = 0 To 9:
                        Print Using "## "; I + 1;
                        Print " "; A1$(I); Tab(20);: Print Using "##.#       ## ##"; A1%(I) * .3; A3%(I) * 3
                    Next

                    Locate 13

                    Do
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 10

                    If B9 <> 0 Then

                        B9 = B9 - 1
                        I1 = B9
                        Cls

                        U$ = "RUNNING BACK RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Color 15: Locate 2
                        Print "1. NAME "; A1$(I1)
                        Print "2. RUSHING AVG ";: Print Using "##.#"; A1%(I1) * .3
                        Print "3. RECEIVING AVG ";: Print Using "##"; A3%(I1) * 3
                        C1 = 1

                        Do
                            Do
                                Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO RB SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 3

                            Select Case C9
                                Case 1
                                    Locate 2: Input "1. NAME "; A1$(I1)

                                Case 2
                                    Locate 3: Input "2. RUSHING AVG "; A1!(I1)
                                    A1%(I1) = Int(A1!(I1) / .3 + .5)

                                Case 3
                                    Locate 4: Input "3. RECEIVING AVG "; A3!(I1)
                                    A3%(I1) = Int(A3!(I1) / 3 + .5)

                            End Select

                        Loop Until C9 = 0

                    End If

                Loop Until B9 = 0

            Case 3

                Do
                    1300 '
                    Cls
                    U$ = "EDIT WIDE RECEIVERS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "REC AVG"

                    For I = 0 To 5: Print Using "## "; I + 1;: Print " "; A2$(I); Tab(20);: Print Using "   ##"; A5%(I) * 3: Next

                    Do
                        Locate 9: Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 6

                    If B9 <> 0 Then

                        B9 = B9 - 1
                        I1 = B9

                        Cls
                        U$ = "WIDE RECEIVER RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Color 15: Locate 2
                        Print "1. NAME "; A2$(I1)
                        Print "2. RECEIVING AVG ";: Print Using "##"; A5%(I1) * 3

                        1325 '
                        C1 = 1
                        Do
                            Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO WR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9
                            Case 1
                                Locate 2: Input "1. NAME "; A2$(I1): If C1 = 1 Then 1325

                            Case 2
                                Locate 3: Input "2. RECEIVING AVG "; A5!(I1): A5%(I1) = Int(A5!(I1) / 3 + .5): If C1 = 1 Then 1325

                        End Select

                    End If

                Loop Until B9 = 0

            Case 4
                1400 '
                Do
                    Cls
                    U$ = "EDIT QUARTERBACKS"
                    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "COMP%   INT%   YDS/COMP ADJ"

                    For I = 0 To 3:
                        Print Using "## "; I + 1;
                        Print " "; A3$(I); Tab(20);: Print Using " ##      ##       +##"; A7%(I); A8%(I); QX%(I)
                    Next

                    Do
                        Locate 13: Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    If B9 <> 0 Then
                        B9 = B9 - 1
                        I1 = B9

                        Cls
                        U$ = "QUARTERBACK RATINGS"
                        Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                        Color 15: Locate 2
                        Print "1. NAME "; A3$(I1)
                        Print "2. COMPLETION % "; A7%(I1)
                        Print "3. INTERCEPTION % "; A8%(I1)
                        Print "4. YDS/COMP ADJ "; QX%(I1)

                        1425 '
                        Do
                            C1 = 1
                            Locate 7: Input "ENTER # TO CHANGE (0-RETURN TO QB SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 4

                        Select Case C9
                            Case 1
                                Locate 2: Input "1. NAME "; A3$(I1)
                                If C1 = 1 Then 1425

                            Case 2
                                Locate 3: Input "2. COMPLETION % "; A7%(I1)
                                If C1 = 1 Then 1425

                            Case 3
                                Locate 4: Input "3. INTERCEPTION % "; A8%(I1)
                                If C1 = 1 Then 1425

                            Case 4
                                Locate 5: Input "4. YDS/COMP ADJ "; QX%(I1)
                                If C1 = 1 Then 1425

                        End Select

                    End If

                Loop Until B9 = 0

            Case 5

                1500 '
                Do
                    Cls
                    U$ = "EDIT KICK-OFF RETURNMEN"
                    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "RET AVG"

                    For I = 0 To 2: Print Using "## "; I + 1;: Print " "; A4$(I); Tab(20);: Print Using "  ##"; B%(I) * 3: Next

                    Do
                        Locate 9: Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 3

                    If B9 <> 0 Then
                        B9 = B9 - 1
                        I1 = B9
                        Cls
                        U$ = "KICK-OFF RETURNMEN RATINGS"
                        Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                        Color 15: Locate 2
                        Print "1. NAME "; A4$(I1)
                        Print "2. RETURN AVG ";: Print Using "##"; B%(I1) * 3

                        1525 C1 = 1

                        Do
                            Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO KOR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9
                            Case 1
                                Locate 2: Input "1. NAME "; A4$(I1)
                                If C1 = 1 Then 1525

                            Case 2
                                Locate 3: Input "2. RETURN AVG "; B!(I1)
                                B%(I1) = Int(B!(I1) / 3 + .5)
                                If C1 = 1 Then 1525
                        End Select

                    End If

                Loop Until B9 = 0

            Case 6

                1600 '
                Do
                    Cls
                    U$ = "EDIT PUNT RETURNMEN"
                    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "RET AVG"

                    For I = 0 To 2: Print Using "## "; I + 1;: Print " "; A5$(I); Tab(20);: Print Using "  ##"; B2%(I) * 3: Next

                    Do
                        Locate 9: Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 3

                    If B9 <> 0 Then

                        B9 = B9 - 1
                        I1 = B9
                        Cls
                        U$ = "PUNT RETURNMEN RATINGS"
                        Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                        Color 15: Locate 2
                        Print "1. NAME "; A5$(I1)
                        Print "2. RETURN AVG ";: Print Using "##"; B2%(I1) * 3

                        1625 C1 = 1

                        Do
                            Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO PR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9
                            Case 1
                                Locate 2: Input "1. NAME "; A5$(I1)
                                If C1 = 1 Then 1625

                            Case 2
                                Locate 3: Input "2. RETURN AVG "; B2!(I1): B2%(I1) = Int(B2!(I1) / 3 + .5)
                                If C1 = 1 Then 1625

                        End Select

                    End If

                Loop Until B9 = 0

            Case 7

                1700 '
                Do

                    Cls
                    U$ = "EDIT PUNTER"
                    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "AVG"

                    For I = 0 To 0: Print Using "## "; I + 1;: Print " "; A6$(I); Tab(20);: Print Using "##"; B3%(I) * 2 + 28: Next

                    Do
                        Locate 9: Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 1

                    If B9 <> 0 Then
                        B9 = B9 - 1
                        I1 = B9
                        Cls
                        U$ = "PUNTER RATINGS"
                        Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                        Color 15: Locate 2
                        Print "1. NAME "; A6$(I1)
                        Print "2. PUNTING AVG ";: Print Using "##"; B3%(I1) * 2 + 28

                        1725 C1 = 1

                        Do
                            Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO PUNTER SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9
                            Case 1
                                Locate 2: Input "1. NAME "; A6$(I1)
                                If C1 = 1 Then 1725

                            Case 2
                                Locate 3: Input "2. PUNTING AVG "; B3!(I1): B3%(I1) = Int(((B3!(I1) - 28) / 2) + .5)
                                If C1 = 1 Then 1725
                        End Select

                    End If

                Loop Until B9 = 0

            Case 8

                1800 '
                Do
                    Cls
                    U$ = "EDIT PLACE KICKERS"
                    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "FG%   EP%"

                    For I = 0 To 1: Print Using "## "; I + 1;: Print " "; A7$(I); Tab(20);: Print Using "###    ###"; B4%(I) * 5 + 25; B7%(I): Next

                    Do
                        Locate 13: Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 2

                    If B9 <> 0 Then

                        B9 = B9 - 1: I1 = B9
                        Cls: U$ = "PLACE KICKER RATINGS": Locate 1, 40 - Len(U$) / 2: Color 14: Print U$: Color 15: Locate 2
                        Print "1. NAME "; A7$(I1)
                        Print "2. FIELD GOAL % ";: Print B4%(I1) * 5 + 25
                        Print "3. EXTRA POINT % ";: Print B7%(I1)
                        1825 C1 = 1

                        Do
                            Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO PK SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 3

                        Select Case C9
                            Case 1
                                Locate 2: Input "1. NAME "; A7$(I1)
                                If C1 = 1 Then 1825

                            Case 2
                                Locate 3: Input "2. FIELD GOAL % "; B4!(I1)
                                B4%(I1) = Int((B4!(I1) / 5) + .5) - 5
                                If C1 = 1 Then 1825

                            Case 3
                                Locate 4: Input "3. EXTRA POINT % "; B7%(I1)
                                If C1 = 1 Then 1825
                        End Select

                    End If

                Loop Until B9 = 0

            Case 9

                1900 '
                Cls
                U$ = "EDIT PLAYERS WITH SACKS"
                Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                Color 15
                Print "   NAME"; Tab(20); "SACKS"
                Locate 3
                TSR% = 0

                For I = 0 To 14: Print Using "## "; I + 1;: Print " "; DS$(I); Tab(20); DS%(I): Next

                Do
                    Locate 20: Input "ENTER PLAYER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                Loop Until B9 >= 0 And B9 <= 15

                If B9 <> 0 Then
                    B9 = B9 - 1
                    I1 = B9
                    Cls
                    U$ = "SACK RATINGS"
                    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                    Color 15: Locate 2

                    Print "1. NAME "; DS$(I1)
                    Print "2. SACKS "; DS%(I1)

                    1925 C1 = 1

                    Do
                        Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO SACKS SELECTION) "; C9
                    Loop Until C9 >= 0 And C9 <= 2

                    Select Case C9
                        Case 1
                            Locate 2: Input "1. NAME "; DS$(I1)
                            If C1 = 1 Then 1925

                        Case 2
                            Locate 3: Input "2. SACKS "; DS%(I1)
                            If C1 = 1 Then 1925

                    End Select

                End If

            Case 10

                1950 '
                Do

                    Cls
                    U$ = "EDIT PLAYERS WITH INTERCEPTIONS"
                    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                    Color 15
                    Print "   NAME"; Tab(20); "INTERCEPTIONS"
                    Locate 3

                    For I = 0 To 9: Print Using "## "; I + 1;: Print " "; DI$(I); Tab(27); DI%(I): Next

                    Do
                        Locate 20: Input "ENTER PLAYER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 10

                    If B9 <> 0 Then
                        B9 = B9 - 1
                        I1 = B9
                        Cls
                        U$ = "INTERCEPTION RATINGS"
                        Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
                        Color 15: Locate 2
                        Print "1. NAME "; DI$(I1)
                        Print "2. INTERCEPTIONS "; DI%(I1)

                        1975 C1 = 1

                        Do
                            Locate 6: Input "ENTER # TO CHANGE (0-RETURN TO INT SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9
                            Case 1
                                Locate 2: Input "1. NAME "; DI$(I1)
                                If C1 = 1 Then 1975

                            Case 2
                                Locate 3: Input "2. INTERCEPTIONS "; DI%(I1)
                                If C1 = 1 Then 1975

                        End Select

                    End If

                Loop Until B9 = 0

            Case 11
                Cls
                Print "SAVE CHANGES (YN)";
                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then Call SaveChangedTeam

        End Select

        If UCase$(I$) = "S" Then Call SaveChangedTeam

    Loop Until I = 11 Or UCase$(I$) = "S"

End Sub

'----------------------------------------
'           SaveChangedTeam Subroutine
'----------------------------------------
'...explanation...
Sub SaveChangedTeam ()

    Shared A1$(), A2$(), A3$(), A4$()
    Shared A5$(), A6$(), A7$()

    Shared DI$(), DS$()

    Shared A0%(), A1%(), A2%(), A3%(), A4%()
    Shared A5%(), A6%(), A7%(), A8%(), A9%()
    Shared B%(), B1%(), B2%(), B3%(), B4%()
    Shared B5%(), B6%(), B7%(), B8%()

    Shared DI%(), DS%(), PA%(), QX%()

    '-- Why do we do this???
    '-- We lose accuracy here!!!
    '-- To change it though means updating
    '-- our routines for reading/saving teams
    '-- Not only that, but we have to
    '-- update ever team file to accomodate
    YC% = YC!

    Call CheckContributions

    S0 = 0
    S1 = 0
    S2 = 0
    S3 = 0

    For I = 0 To 9: S0 = S0 + A0%(I): S2 = S2 + A2%(I): Next

    S4 = 100 / S0

    For I = 0 To 9:
        If A0%(I) <> 0 Then A0%(I) = Int(S4 * A0%(I) + .5) + S1: S1 = A0%(I)
        If A0%(I) > 100 Then A0%(I) = 100
    Next I

    For I = 0 To 5: S2 = S2 + A4%(I): Next

    S5 = 100 / S2

    For I = 0 To 9:
        If A2%(I) <> 0 Then A2%(I) = Int(S5 * A2%(I) + .5) + S3: S3 = A2%(I)
        If A2%(I) > 100 Then A2%(I) = 100
    Next I

    For I = 0 To 5:
        If A4%(I) <> 0 Then A4%(I) = Int(S5 * A4%(I) + .5) + S3: S3 = A4%(I)
        If A4%(I) > 100 Then A4%(I) = 100
    Next I

    S6 = 0: S7 = 0

    For I = 0 To 3: S6 = S6 + A6%(I): Next

    S8 = 100 / S6

    For I = 0 To 3:
        If A6%(I) <> 0 Then A6%(I) = Int(S8 * A6%(I) + .5) + S7: S7 = A6%(I)
        If A6%(I) > 100 Then A6%(I) = 100
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 2: G0 = G0 + A9%(I): G3 = G3 + B1%(I): Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 2:
        If A9%(I) <> 0 Then A9%(I) = Int(G2 * A9%(I) + .5) + G1: G1 = A9%(I)
        If A9%(I) > 100 Then A9%(I) = 100
    Next

    For I = 0 To 2:
        If B1%(I) <> 0 Then B1%(I) = Int(G5 * B1%(I) + .5) + G4: G4 = B1%(I)
        If B1%(I) > 100 Then B1%(I) = 100
    Next

    G0 = 0: G1 = 0: G3 = 0: G4 = 0

    For I = 0 To 1: G0 = G0 + B5%(I): G3 = G3 + B8%(I): Next

    G2 = 100 / G0: G5 = 100 / G3

    For I = 0 To 1:
        If B5%(I) <> 0 Then B5%(I) = Int(G2 * B5%(I) + .5) + G1: G1 = B5%(I)
        If B5%(I) > 100 Then B5%(I) = 100
    Next

    For I = 0 To 1: If B8%(I) <> 0 Then B8%(I) = Int(G5 * B8%(I) + .5) + G4: G4 = B8%(I)
        If B8%(I) > 100 Then B8%(I) = 100
    Next

    For I = 9 To 0 Step -1:
        If A0%(I) <> 0 Then A0%(I) = 100: Exit For
    Next

    For I = 5 To 0 Step -1:
        If A4%(I) <> 0 Then A4%(I) = 100: Exit For
    Next

    For I = 3 To 0 Step -1:
        If A6%(I) <> 0 Then A6%(I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:
        If A9%(I) <> 0 Then A9%(I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:
        If B1%(I) <> 0 Then B1%(I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If B5%(I) <> 0 Then B5%(I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If B8%(I) <> 0 Then B8%(I) = 100: Exit For
    Next

    Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
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

    Put #1, B8%

    Close #1

    Open diskPaths$(0) + "STADIUMS." + YN$ For Random As #2 Len = 50
    Field #2, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)
    LSet Q$(1) = A$
    LSet Q$(2) = S$
    LSet Q$(3) = MKL$(ATT&)
    LSet Q$(4) = "x"
    Put #2, B8%
    Close 2

    S$ = ""
    ATT& = 0

    For I = 0 To 9
        A0%(I) = 0
        A1%(I) = 0
        A2%(I) = 0
        A3%(I) = 0
        A1$(I) = ""
    Next

    For I = 0 To 5
        A4%(I) = 0
        A5%(I) = 0
        A2$(I) = ""
    Next

    For I = 0 To 3
        A6%(I) = 0
        A7%(I) = 0
        A8%(I) = 0
        QX%(I) = 0
        A3$(I) = ""
    Next

    For I = 0 To 2
        A9%(I) = 0
        B%(I) = 0
        B1%(I) = 0
        B2%(I) = 0
        A4$(I) = ""
        A5$(I) = ""
    Next

    A6$(0) = ""

    For I = 0 To 1
        B3%(I) = 0
        B4%(I) = 0
        B5%(I) = 0
        B7%(I) = 0
        B8%(I) = 0
        A7$(I) = ""
    Next

End Sub

'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub ViewRoster ()

    Shared A1$(), A2$(), A3$(), A4$()
    Shared A5$(), A6$(), A7$()
    Shared A0%(), A1%(), A2%(), A3%(), A4%()
    Shared A5%(), A6%(), A7%(), A8%(), A9%()
    Shared B%(), B1%(), B2%(), B3%(), B4%()
    Shared B5%(), B6%(), B7%(), B8%()

    Shared PA%()
    Shared QX%()

    Cls
    Color 11: Print "TEAM ROSTER FOR ";: Color PA%(2), PA%(1): Print A$;
    Color 11, 0: Print "   TEAM NUMBER =";
    Color 14: Print B8%
    Print
    Color 15: Print Using "DEF RUSH ADJ   = ###"; B6%(0);
    Print Tab(35); Using "SACKED %         = ##"; B6%(5)
    Print Using "DEF COMP % ADJ = ###"; B6%(1);
    Print Tab(35); Using "DEF SACK ADJ     = ##"; B6%(6)
    Print Using "DEF INT % ADJ  = ###"; B6%(2);
    Print Tab(35); Using "RUN/PASS %       = ##"; B6%(7)
    Print Using "OFF FUMBLE  %  = ###"; B6%(3);
    Print Tab(35); "DEF YDS/COMP ADJ = ";

    If YC! < 50 Then Print Using "+#.#"; YC! / 100 * 30
    If YC! > 50 Then Print Using "+#.#"; (100 - YC!) / 100 * -30

    Print Using "DEF FUMBLE  %  = ###"; B6%(4);
    Print Tab(35); Using "PENALTY ADJ      = ##"; PA%(0)
    Print Using "STAD TYPE      = ###"; PA%(3)
    Print Using "BACKGRND COLOR = ###"; PA%(1);
    Print Tab(35); Using "FOREGRND COLOR   = ##"; PA%(2)
    Print
    Color 12: Print "RUNNING BACKS-"
    Color 14: Print "NAME-               RUSH AVG   CX%  REC AVG   CX%"

    For I = 0 To 9:
        If Left$(A1$(I), 1) <> " " And Left$(A1$(I), 3) <> "END" Then
            Color 3: Print A1$(I);
            Color 15
            Locate , 23: Print Using "##.#     ###    ##     ###"; A1%(I) * .3; A0%(I); A3%(I) * 3; A2%(I)
        End If
    Next

    Locate 23
    Color 15: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

    Cls
    Color 12: Print "WIDE RECEIVERS-"
    Color 14: Print "NAME-              REC AVG     CX%"

    For I = 0 To 5:
        If Left$(A2$(I), 1) <> " " And Left$(A2$(I), 3) <> "END" Then
            Color 3: Print A2$(I);
            Color 15: Locate , 23: Print Using "##       ###"; A5%(I) * 3; A4%(I)
        End If
    Next

    Print
    Color 12: Print "QUARTERBACKS-                     YDS/CMP"
    Color 14: Print "NAME-               COMP%   INT%    ADJ     CX%"
    For I = 0 To 3:
        If Left$(A3$(I), 1) <> " " And Left$(A3$(I), 3) <> "END" Then
            Color 3: Print A3$(I);
            Color 15: Locate , 21: Print Using "###     ##     ###      ###"; A7%(I); A8%(I); QX%(I); A6%(I)
        End If
    Next

    Print
    Color 12: Print "KICKOFF RETURNMEN-"
    Color 14: Print "NAME-              RET AVG     CX%"

    For I = 0 To 2:
        If Left$(A4$(I), 1) <> " " And Left$(A4$(I), 3) <> "END" Then
            Color 3: Print A4$(I);
            Color 15: Locate , 23: Print Using "##       ###"; B%(I) * 3; A9%(I)
        End If
    Next

    Locate 21: Color 15: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$
    Cls
    Color 12: Print "PUNT RETURNMEN-"
    Color 14: Print "NAME-              RET AVG     CX%"

    For I = 0 To 2:
        If Left$(A5$(I), 1) <> " " And Left$(A5$(I), 3) <> "END" Then
            Color 3: Print A5$(I);
            Color 15: Locate , 23: Print Using "##       ###"; B2%(I) * 3; B1%(I)
        End If
    Next

    Print
    Color 12: Print "PLACE KICKERS-"
    Color 14: Print "NAME-                FG%  CX%   EP%  CX%"

    For I = 0 To 1:
        If Left$(A7$(I), 1) <> " " And Left$(A7$(I), 3) <> "END" Then
            Color 3: Print A7$(I);
            Color 15: Locate , 21: Print Using "###   ###   ###  ###"; B4%(I) * 5 + 25; B5%(I); B7%(I); B8%(I)
        End If
    Next

    Print
    Color 12: Print "PUNTER-"
    Color 14: Print "NAME-              AVG"
    Color 3: Print A6$(0);
    Color 15
    Locate , 21: Print Using "##"; B3%(0) * 2 + 28
    Locate 21: Color 15: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'     CheckContributions Subroutine
'----------------------------------------
'...explanation...
Sub CheckContributions

    Shared A1$(), A2$(), A3$(), A4$()
    Shared A5$(), A6$(), A7$()
    Shared A0%(), A1%(), A2%(), A3%(), A4%()
    Shared A5%(), A6%(), A7%(), A8%(), A9%()
    Shared B1%(), B5%(), B8%()

    9900 ' *** CHECK CONTRIBUTION RATINGS ***
    Cls
    U$ = "CONTRIBUTION RATINGS": Locate 1, 40 - Len(U$) / 2: Color 14: Print U$: Color 15: Locate 3
    Print "THE NEXT SCREENS WILL CYCLE THROUGH THE  TEAM ROSTER SHOWING THE  % OF PLAYING"
    Print "TIME EACH PLAYER IS CURRENTLY ALLOTTED. TOTAL CONTRIBUTION FOR EACH POSITION"
    Print "MUST TOTAL 100%"
    Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    '-----

    Cls
    U$ = "RUNNING BACK CONTRIBUTION RATINGS": Locate 1, 40 - Len(U$) / 2: Color 14: Print U$: Color 15: Locate 2
    Print "   NAME"; Tab(20); "CONTRIBUTION"

    Do
        9909 '
        Locate 3
        BC% = 0

        For I = 0 To 9:
            Print Using "## "; I + 1;
            Print " "; A1$(I); Tab(20);: Print Using "     ###"; A0%(I)
            BC% = BC% + A0%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print "11  PROCEED TO RECEIVERS"

        9911
        Do
            Locate 16: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 11

        B9 = B9 - 1
        I1 = B9

        If I1 <> 10 Then
            Locate I1 + 3, 24: Print "   "
            Locate I1 + 3, 24: Input A0%(I1)
        Else
            If BC% <> 100 Then
                Locate 18: Print "TOTAL IS NOT 100%!"
                Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                I$ = GetKeyPress$
                Locate 16: Print Space$(20)

            End If

        End If

    Loop Until I1 = 10 And BC% = 100

    '-----

    9917
    Cls
    U$ = "RECEIVING CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
    Color 15: Locate 2
    Print "   NAME"; Tab(20); "CONTRIBUTION"

    Do
        9919
        Locate 3
        BC% = 0
        For I = 0 To 9:
            Print Using "## "; I + 1;
            Print " "; A1$(I); Tab(20);: Print Using "      ##"; A2%(I)
            BC% = BC% + A2%(I)
        Next

        For I = 0 To 5
            Print Using "## "; I + 11;
            Print " "; A2$(I); Tab(20);: Print Using "      ##"; A4%(I)
            BC% = BC% + A4%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print "17  PROCEED TO QUARTERBACKS"

        9921 '
        Do
            Locate 21: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 17

        B9 = B9 - 1
        I1 = B9

        If I1 <> 16 Then
            Locate I1 + 3, 24: Print "   ": Locate I1 + 3, 24
            If I1 <= 9 Then
                Input A2%(I1)
            Else
                Input A4%(I1 - 10)
            End If
        Else
            If BC% <> 100 Then
                Locate 22: Print "TOTAL IS NOT 100%!"
                Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                I$ = GetKeyPress$
                Locate 22: Print Space$(20)
            End If

        End If

    Loop Until I1 = 16 And BC% = 100

    '-----

    9927 '
    Cls
    U$ = "QUARTERBACK CONTRIBUTION RATINGS": Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Color 15: Locate 2
    Print "   NAME"; Tab(20); "CONTRIBUTION"

    Do
        9929 '
        Locate 3
        BC% = 0

        For I = 0 To 3:
            Print Using "## "; I + 1;
            Print " "; A3$(I); Tab(20);
            Print Using "     ###"; A6%(I)
            BC% = BC% + A6%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 5  PROCEED TO KO RETURNMEN"

        9931 '
        Do
            Locate 10: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 5

        B9 = B9 - 1
        I1 = B9

        If I1 <> 4 Then
            Locate I1 + 3, 24: Print "   ": Locate I1 + 3, 24
            Input A6%(I1)
        Else
            If BC% <> 100 Then
                Locate 16: Print "TOTAL IS NOT 100%!"
                Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                I$ = GetKeyPress$
                Locate 16: Print Space$(20)
            End If

        End If

    Loop Until I1 = 4 And BC% = 100

    '-----

    9937 '
    Cls
    U$ = "KICK-OFF RETURN CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
    Color 15: Locate 2
    Print "   NAME"; Tab(20); "CONTRIBUTION"

    Do
        9939 '
        Locate 3
        BC% = 0
        For I = 0 To 2
            Print Using "## "; I + 1;
            Print " "; A4$(I); Tab(20);
            Print Using "     ###"; A9%(I)
            BC% = BC% + A9%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 4  PROCEED TO PUNT RETURNMEN"

        9941 '
        Do
            Locate 10: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 4

        B9 = B9 - 1
        I1 = B9

        If I1 <> 3 Then
            Locate I1 + 3, 24
            Print "   ": Locate I1 + 3, 24
            Input A9%(I1)
        Else
            If BC% <> 100 Then
                Locate 16: Print "TOTAL IS NOT 100%!"
                Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                I$ = GetKeyPress$
                Locate 16: Print Space$(20)
            End If

        End If

    Loop Until I1 = 3 And BC% = 100

    '-----

    9947 '
    Cls
    U$ = "PUNT RETURN CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
    Color 15: Locate 2
    Print "   NAME"; Tab(20); "CONTRIBUTION"

    Do
        9949 '
        Locate 3: BC% = 0

        For I = 0 To 2:
            Print Using "## "; I + 1;
            Print " "; A5$(I); Tab(20);
            Print Using "     ###"; B1%(I)
            BC% = BC% + B1%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 4  PROCEED TO FG KICKERS"

        9951 '
        Do
            Locate 10: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 4

        B9 = B9 - 1
        I1 = B9

        If I1 <> 3 Then
            Locate I1 + 3, 24: Print "   ": Locate I1 + 3, 24
            Input B1%(I1)
        Else
            If BC% <> 100 Then
                Locate 16: Print "TOTAL IS NOT 100%!"
                Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                I$ = GetKeyPress$
                Locate 16: Print Space$(20)
            End If

        End If

    Loop Until I1 = 3 And BC% = 100

    '-----

    9957 '
    Cls
    U$ = "FIELD GOAL CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2: Color 14: Print U$
    Color 15: Locate 2
    Print "   NAME"; Tab(20); "CONTRIBUTION"

    Do
        9959 '
        Locate 3
        BC% = 0

        For I = 0 To 1:
            Print Using "## "; I + 1;
            Print " "; A7$(I); Tab(20);
            Print Using "     ###"; B5%(I)
            BC% = BC% + B5%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 3  PROCEED TO PAT/EP KICKERS"

        9961 '
        Do
            Locate 10: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 3

        B9 = B9 - 1
        I1 = B9

        If I1 <> 2 Then
            Locate I1 + 3, 24: Print "   ": Locate I1 + 3, 24
            Input B5%(I1)
        Else
            If BC% <> 100 Then
                Locate 16: Print "TOTAL IS NOT 100%!"
                Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                I$ = GetKeyPress$
                Locate 16: Print Space$(20)
            End If

        End If

    Loop Until I1 = 2 And BC% = 100

    '-----

    9967
    Cls
    U$ = "PAT/EXTRA POINT CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Color 15: Locate 2
    Print "   NAME"; Tab(20); "CONTRIBUTION"

    Do
        9969 '
        Locate 3
        BC% = 0

        For I = 0 To 1
            Print Using "## "; I + 1;
            Print " "; A7$(I); Tab(20);
            Print Using "     ###"; B8%(I): BC% = BC% + B8%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 3  PROCEED"

        9971 '
        Do
            Locate 10: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 3

        B9 = B9 - 1
        I1 = B9

        If I1 <> 2 Then
            Locate I1 + 3, 24: Print "   "
            Locate I1 + 3, 24

            Input B8%(I1)
        Else
            If BC% <> 100 Then
                Locate 16: Print "TOTAL IS NOT 100%!"
                Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                I$ = GetKeyPress$
                Locate 16: Print Space$(20)
            End If

        End If

    Loop Until I1 = 2 And BC% = 100

End Sub
