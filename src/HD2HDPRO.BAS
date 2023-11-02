'----------------------------------------
'          HD2HDPRO Subroutine
'----------------------------------------
'This subroutine presents the user a menu
'of options for viewing Head-to-Head team
'comparisons.
Sub HD2HDPRO ()

    _Title "3-in-1 Football - Head-to-Head League Stats"

    Color 15, 0
    Cls

    Call TITLE_UTILS

    Locate 4, 1
    Color 2, 0

    Call MenuBox (8, 8, 61, 6, 15, 1)
    
    Do

        Locate 10, 15
        Color 3

        Locate , 20: Print "SELECT OPTION:"
        Locate , 20: Color 15: Print "O";
        Color 7: Print "NE LEAGUE / CONFERENCE"
        Locate , 20: Color 15: Print "T";
        Color 7: Print "WO LEAGUES / CONFERENCES WITH COMPETING PLAY"
        Locate , 20: Color 15: Print "R";
        Color 7: Print "ETURN TO MAIN MENU"

        I$ = GetKeyPress$

        If UCase$(I$) = "O" Then X2 = 0
        If UCase$(I$) = "T" Then X2 = 1
        If UCase$(I$) = "O" Or UCase$(I$) = "T" Then
            Call Hd2HdPro_League(X2)
        End If

    Loop Until UCase$(I$) = "R" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'     Hd2HdPro_League Subroutine
'----------------------------------------
'This subroutine assists the user with the
'selection of a team for comparison, then
'loads the associated stats, calculates
'the comparisons and presents output options
Sub Hd2HdPro_League (X2)

    Shared div1$, div2$, div3$, div1_2$, div2_2$, div3_2$

    Shared AL!(), AL2!(), AW!(), AW2!()
    Shared G1!(), G12!(), G2!(), G22!()
    Shared G3!(), G32!(), G4!(), G42!()
    Shared HL!(), HL2!(), HT!(), HT2!(), HW!(), HW2!()
    Shared RT2!(), RT!()
    Shared TAL!(), TAL2!(), TAW!(), TAW2!()
    Shared TG1!(), TG12!(), TG2!(), TG22!()
    Shared TG3!(), TG32!(), TG4!(), TG42!()
    Shared THL!(), THL2!(), THW!(), THW2!()
    Shared THT!(), THT2!(), TRT!(), TRT2!()

    Shared statsO%(), compT%(), statsZ1%()

    Dim Z12%(30)

    Shared DV$(), DV2$(), statsH$(), statsO$()
    Shared statsZ1$(), Z12$(), statsZ2$(), Z22$()

    newTeam = 1

    Do

        If newTeam = 1 Then

            Do

                Call ChooseTeam(YN$, NTMS, teamNbr, team$)
                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

                    MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    J$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                End If

            Loop Until _FileExists(diskPaths$(1) + statFile$ + "S")

            Open diskPaths$(1) + statFile$ + "S" For Input As #1

            Input #1, YY%

            For I = 1 To NUM_STATRECORDS: Input #1, statsO%(I): Next I

            For I = 1 To NUM_STATRECORDS: Input #1, statsO$(I): Next I

            For I = 1 To NUM_STATRECORDS: Input #1, compT%(I): Next I

            For I = 1 To NUM_STATRECORDS: Input #1, statsH$(I): Next I

            Close #1

            'Check if same league
            If sameLeague <> 1 Then

                leag1$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "LEAGUE", ExitCode%)

                If X2 <> 0 Then
                    leag2$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "LEAGUE", ExitCode%)
                End If

            End If

        End If

        Color 7, 0
        Cls

        Call MenuBox (8, 8, 61, 5, 15, 1)

        Do
            Locate 10, 32
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "F";: Color 7: Print "ILE"

            J$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(J$) = "F" Or UCase$(J$) = "P" Or UCase$(J$) = "S"

        VPP = 0
        If UCase$(J$) = "P" Then X1 = 1
        If UCase$(J$) = "S" Then VPP = 1

        Open diskPaths$(2) + leag1$ + ".LIB" For Input As #2

        Input #2, X%

        If X% = 0 Then

            Input #2, leag1$
            Input #2, div1$
            Input #2, div2$

            For X = 1 To 15:
                Input #2, statsZ1$(X)
                Input #2, statsZ1%(X)
                Input #2, statsZ2$(X)
            Next

            NT = 15

        End If

        If X% = 1 Then
            Input #2, leag1$
            Input #2, div1$
            Input #2, div2$
            Input #2, div3$

            For X = 1 To 30:
                Input #2, statsZ1$(X)
                Input #2, statsZ1%(X)
                Input #2, statsZ2$(X)
            Next

            NT = 30

        End If

        Close #2

        If X2 = 1 Then

            Open diskPaths$(2) + leag2$ + ".LIB" For Input As #3

            Input #3, X%

            If X% = 0 Then
                Input #3, leag2$
                Input #3, div1_2$
                Input #3, div2_2$

                For X = 1 To 15:
                    Input #3, Z12$(X)
                    Input #3, Z12%(X)
                    Input #3, Z22$(X)
                Next

                NT = 15

            End If

            If X% = 1 Then
                Input #3, leag2$
                Input #3, div1_2$
                Input #3, div2_2$
                Input #3, div3_2$

                For X = 1 To 30:
                    Input #3, Z12$(X)
                    Input #3, Z12%(X)
                    Input #3, Z22$(X)
                Next

                NT = 30

            End If

        End If

        Close #3

        Color 15
        Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

        'Initialization for 1st league
        Erase AL!, AW!
        Erase G1!, G2!, G3!, G4!
        Erase HL!, HT!, HW!, RT!
        Erase TAL!, TAW!, THL!, TRT!
        Erase TG1!, TG2!, TG3!, TG4!

        'Calculations for 1st league
        For K = 1 To 30

            If statsZ1$(K) <> "XXX" Then

                For I = 1 To YY%
                    If statsO%(I) > compT%(I) And Left$(statsH$(I), 1) = "A" And statsO$(I) = statsZ1$(K) Then AW!(K) = AW!(K) + 1
                    If statsO%(I) > compT%(I) And Left$(statsH$(I), 1) = "H" And statsO$(I) = statsZ1$(K) Then HW!(K) = HW!(K) + 1
                    If statsO%(I) < compT%(I) And Left$(statsH$(I), 1) = "A" And statsO$(I) = statsZ1$(K) Then AL!(K) = AL!(K) + 1
                    If statsO%(I) < compT%(I) And Left$(statsH$(I), 1) = "H" And statsO$(I) = statsZ1$(K) Then HL!(K) = HL!(K) + 1
                    If statsO%(I) = compT%(I) And Left$(statsH$(I), 1) = "A" And statsO$(I) = statsZ1$(K) Then RT!(K) = RT!(K) + 1
                    If statsO%(I) = compT%(I) And Left$(statsH$(I), 1) = "H" And statsO$(I) = statsZ1$(K) Then HT!(K) = HT!(K) + 1
                    If statsO$(I) = statsZ1$(K) And Left$(statsH$(I), 1) = "H" Then G1!(K) = G1!(K) + statsO%(I)
                    If statsO$(I) = statsZ1$(K) And Left$(statsH$(I), 1) = "H" Then G2!(K) = G2!(K) + compT%(I)
                    If statsO$(I) = statsZ1$(K) And Left$(statsH$(I), 1) = "A" Then G3!(K) = G3!(K) + statsO%(I)
                    If statsO$(I) = statsZ1$(K) And Left$(statsH$(I), 1) = "A" Then G4!(K) = G4!(K) + compT%(I)
                Next I

            End If

        Next K

        'If we're doing a 2nd league..
        If X2 = 1 Then
        
            'Initialization for 2nd league
            Erase AL2!, AW2!, G12!, G22!, G32!, G42!
            Erase HL2!, HT2!, HW2!
            Erase RT2!
            Erase TAL2!, TAW2!, THL2!, THT2!, THW2!, TRT2!
            Erase TG12!, TG22!, TG32!, TG42!

            'Calculations for 2nd league
            For K = 1 To 30

                If Z12$(K) <> "XXX" Then

                    For I = 1 To YY%

                        If statsO%(I) > compT%(I) And Left$(statsH$(I), 1) = "A" And statsO$(I) = Z12$(K) Then AW2!(K) = AW2!(K) + 1
                        If statsO%(I) > compT%(I) And Left$(statsH$(I), 1) = "H" And statsO$(I) = Z12$(K) Then HW2!(K) = HW2!(K) + 1
                        If statsO%(I) < compT%(I) And Left$(statsH$(I), 1) = "A" And statsO$(I) = Z12$(K) Then AL2!(K) = AL2!(K) + 1
                        If statsO%(I) < compT%(I) And Left$(statsH$(I), 1) = "H" And statsO$(I) = Z12$(K) Then HL2!(K) = HL2!(K) + 1
                        If statsO%(I) = compT%(I) And Left$(statsH$(I), 1) = "A" And statsO$(I) = Z12$(K) Then RT2!(K) = RT2!(K) + 1
                        If statsO%(I) = compT%(I) And Left$(statsH$(I), 1) = "H" And statsO$(I) = Z12$(K) Then HT2!(K) = HT2!(K) + 1
                        If statsO$(I) = Z12$(K) And Left$(statsH$(I), 1) = "H" Then G12!(K) = G12!(K) + statsO%(I)
                        If statsO$(I) = Z12$(K) And Left$(statsH$(I), 1) = "H" Then G22!(K) = G22!(K) + compT%(I)
                        If statsO$(I) = Z12$(K) And Left$(statsH$(I), 1) = "A" Then G32!(K) = G32!(K) + statsO%(I)
                        If statsO$(I) = Z12$(K) And Left$(statsH$(I), 1) = "A" Then G42!(K) = G42!(K) + compT%(I)

                    Next I

                End If

            Next K

        End If

        'Generate output
        If VPP <> 1 Then
            Call Hd2HdPro_Report(team$, leag1$, leag2$, DV$(), DV2$())
        Else
            Call Hd2HdPro_Screen(team$, leag1$, leag2$, DV$(), DV2$())
        End If

        If X1 = 1 Then
            'If the option to send to printer was chosen,
            'print the pre-generated report
            Open "REPORT" For Input As #4

            Do While Not EOF(2)
                Line Input #4, X$
                LPrint X$
            Loop

            Close #4

            LPrint Chr$(12)
            X1 = 0

        End If

        Cls

        Call TITLE_UTILS

        Call MenuBox (8, 8, 61, 6, 15, 1)

        Do

            Locate 10, 15

            Color 3: Locate , 25: Print "SELECT OPTION:"
            Color 7: Locate , 25: Print "VIEW ";
            Color 15: Print "S";
            Color 7: Print "AME TEAM AGAIN"
            Color 7: Locate , 25: Print "VIEW ";
            Color 15: Print "A";
            Color 7: Print "NOTHER TEAM IN THE SAME LEAGUE"
            Locate , 25: Color 15: Print "R";
            Color 7: Print "ETURN TO MAIN MENU"

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "A"
                    sameLeague = 1
                    VPP = 0
                    newTeam = 1

                Case "R"
                    sameLeague = 0
                    VPP = 0

                Case "S"

                    'Re-initialize for the next loop
                    Erase TAL!, TAW!, THL!, THT!, THW!, TRT!
                    Erase TG1!, TG2!, TG3!, TG4!
                    Erase TAL2!, TAW2!, THL2!, THT2!, THW2!, TRT2!
                    Erase TG12!, TG22!, TG32!, TG42!

                    newTeam = 0

            End Select

        Loop Until UCase$(I$) = "S" Or UCase$(I$) = "A" Or UCase$(I$) = "R" Or I$ = Chr$(27)

    Loop Until UCase$(I$) = "R" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'     Hd2HdPro_Screen Subroutine
'----------------------------------------
'This subroutine outputs the calculated 
'comparison values to the end user display
Sub Hd2HdPro_Screen (A$, leag1$, leag2$, DV$(), DV2$())

    Shared div1$, div2$, div3$, div1_2$, div2_2$, div3_2$

    Shared teamRatings%()

    Shared AL!(), AL2!(), AW!(), AW2!()
    Shared G1!(), G12!(), G2!(), G22!()
    Shared G3!(), G32!(), G4!(), G42!()
    Shared HL!(), HL2!(), HT!(), HT2!(), HW!(), HW2!()
    Shared RT2!(), RT!()
    Shared TAL!(), TAL2!(), TAW!(), TAW2!()
    Shared TG1!(), TG12!(), TG2!(), TG22!()
    Shared TG3!(), TG32!(), TG4!(), TG42!()
    Shared THL!(), THL2!(), THW!(), THW2!()
    Shared THT!(), THT2!(), TRT!(), TRT2!()

    Shared DV$(), DV2$()
    Shared statsZ1$(), Z12$()

    DV$(1) = div1$
    DV$(2) = div2$
    DV$(3) = div3$

    RDD = 0

    Do

        Cls

        Locate 2, 35
        Color teamRatings%(2), teamRatings%(1): Print A$
        Color 3, 0

        Locate 4
        Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
        Locate 5
        Print Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF"; Tab(32); "PTA"; Tab(35); Chr$(179); Tab(37); "W"; Tab(40); "L"; Tab(43); "T"; Tab(46); "PTF"; Tab(50); "PTA"; Tab(53); Chr$(179); Tab(55); "W"; Tab(58); "L"; Tab(61); "T"; Tab(64); "PTF"; Tab(68); "PTA"

        RDD = RDD + 1

        DD = ((RDD - 1) * 10) + 1
        EE = RDD * 10

        If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then RDD = 3

        Color 11
        Print leag1$; " "; DV$(RDD);

        Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)

        For K = DD To EE

            If statsZ1$(K) <> "XXX" And statsZ1$(K) <> A$ And statsZ1$(K) <> "" Then

                Color 15
                Locate , 1: Print Left$(statsZ1$(K), 15);

                Color 7
                Locate , 17: Print Using "##"; HW!(K);
                Locate , 20: Print Using "##"; HL!(K);
                Locate , 23: Print Using "##"; HT!(K);
                Locate , 27: Print Using "###"; G1!(K);
                Locate , 32: Print Using "###"; G2!(K);

                Color 3: Print Tab(35); Chr$(179);

                Color 7
                Locate , 36: Print Using "##"; AW!(K);
                Locate , 39: Print Using "##"; AL!(K);
                Locate , 42: Print Using "##"; RT!(K);
                Locate , 47: Print Using "##"; G3!(K);
                Locate , 51: Print Using "##"; G4!(K);

                Color 3: Print Tab(53); Chr$(179);

                Color 7
                Locate , 54: Print Using "##"; HW!(K) + AW!(K);
                Locate , 57: Print Using "##"; HL!(K) + AL!(K);
                Locate , 60: Print Using "##"; HT!(K) + RT!(K);
                Locate , 64: Print Using "###"; G1!(K) + G3!(K);
                Locate , 68: Print Using "###"; G2!(K) + G4!(K)

                Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

                THW!(RDD) = THW!(RDD) + HW!(K)
                THL!(RDD) = THL!(RDD) + HL!(K)
                TAW!(RDD) = TAW!(RDD) + AW!(K)
                TAL!(RDD) = TAL!(RDD) + AL!(K)
                TRT!(RDD) = TRT!(RDD) + RT!(K)
                THT!(RDD) = THT!(RDD) + HT!(K)
                TG1!(RDD) = TG1!(RDD) + G1!(K)
                TG2!(RDD) = TG2!(RDD) + G2!(K)
                TG3!(RDD) = TG3!(RDD) + G3!(K)
                TG4!(RDD) = TG4!(RDD) + G4!(K)

            End If

        Next K

        Print

        Color 11
        Locate , 2: Print "TOTALS";
        Locate , 17: Print Using "##"; THW!(RDD);
        Locate , 20: Print Using "##"; THL!(RDD);
        Locate , 23: Print Using "##"; THT!(RDD);
        Locate , 27: Print Using "###"; TG1!(RDD);
        Locate , 32: Print Using "###"; TG2!(RDD);

        Color 3: Print Tab(35); Chr$(179);

        Color 11
        Locate , 36: Print Using "##"; TAW!(RDD);
        Locate , 39: Print Using "##"; TAL!(RDD);
        Locate , 42: Print Using "##"; TRT!(RDD);
        Locate , 46: Print Using "###"; TG3!(RDD);
        Locate , 50: Print Using "###"; TG4!(RDD);

        Color 3: Print Tab(53); Chr$(179);

        Color 11
        Locate , 54: Print Using "##"; THW!(RDD) + TAW!(RDD);
        Locate , 57: Print Using "##"; THL!(RDD) + TAL!(RDD);
        Locate , 60: Print Using "##"; THT!(RDD) + TRT!(RDD);
        Locate , 64: Print Using "###"; TG1!(RDD) + TG3!(RDD);
        Locate , 68: Print Using "###"; TG2!(RDD) + TG4!(RDD)

        Color 14
        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

    Loop Until RDD = 3

    'Second League
    If X2 = 1 Then

        DV2$(1) = div1_2$
        DV2$(2) = div2_2$
        DV2$(3) = div3_2$

        RDD = 0

        Do

            RDD = RDD + 1

            DD = ((RDD - 1) * 10) + 1
            EE = RDD * 10

            If DV2$(RDD) = "XXX" Or DV2$(RDD) = "" Then RDD = 3

            Cls

            Locate 2, 35
            Color teamRatings%(2), teamRatings%(1): Print A$

            Color 3, 0

            Locate 4
            Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
            Locate 5
            Print Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF"; Tab(32); "PTA"; Tab(35); Chr$(179); Tab(37); "W"; Tab(40); "L"; Tab(43); "T"; Tab(46); "PTF"; Tab(50); "PTA"; Tab(53); Chr$(179); Tab(55); "W"; Tab(58); "L"; Tab(61); "T"; Tab(64); "PTF"; Tab(68); "PTA"

            Color 11
            Print leag2$; " "; DV2$(RDD);

            Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)

            For K = DD To EE

                If Z12$(K) <> "XXX" And Z12$(K) <> A$ Then

                    Color 15
                    Locate , 1: Print Left$(Z12$(K), 15);
                    Color 7
                    Locate , 17: Print Using "##"; HW2!(K);
                    Locate , 20: Print Using "##"; HL2!(K);
                    Locate , 23: Print Using "##"; HT2!(K);
                    Locate , 27: Print Using "###"; G12!(K);
                    Locate , 32: Print Using "###"; G22!(K);

                    Color 3: Print Tab(35); Chr$(179);

                    Color 7
                    Locate , 36: Print Using "##"; AW2!(K);
                    Locate , 39: Print Using "##"; AL2!(K);
                    Locate , 42: Print Using "##"; RT2!(K);
                    Locate , 47: Print Using "##"; G32!(K);
                    Locate , 51: Print Using "##"; G42!(K);

                    Color 3: Print Tab(53); Chr$(179);

                    Color 7
                    Locate , 54: Print Using "##"; HW2!(K) + AW2!(K);
                    Locate , 57: Print Using "##"; HL2!(K) + AL2!(K);
                    Locate , 60: Print Using "##"; HT2!(K) + RT2!(K);
                    Locate , 64: Print Using "###"; G12!(K) + G32!(K);
                    Locate , 68: Print Using "###"; G22!(K) + G42!(K)

                    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

                    THW2!(RDD) = THW2!(RDD) + HW2!(K)
                    THL2!(RDD) = THL2!(RDD) + HL2!(K)
                    TAW2!(RDD) = TAW2!(RDD) + AW2!(K)
                    TAL2!(RDD) = TAL2!(RDD) + AL2!(K)
                    TRT2!(RDD) = TRT2!(RDD) + RT2!(K)
                    THT2!(RDD) = THT2!(RDD) + HT2!(K)
                    TG12!(RDD) = TG12!(RDD) + G12!(K)
                    TG22!(RDD) = TG22!(RDD) + G22!(K)
                    TG32!(RDD) = TG32!(RDD) + G32!(K)
                    TG42!(RDD) = TG42!(RDD) + G42!(K)

                End If

            Next K

            Print

            Color 11
            Locate , 2: Print "TOTALS";
            Locate , 17: Print Using "##"; THW2!(RDD);
            Locate , 20: Print Using "##"; THL2!(RDD);
            Locate , 23: Print Using "##"; THT2!(RDD);
            Locate , 27: Print Using "###"; TG12!(RDD);
            Locate , 32: Print Using "###"; TG22!(RDD);

            Color 3: Print Tab(35); Chr$(179);

            Color 11
            Locate , 36: Print Using "##"; TAW2!(RDD);
            Locate , 39: Print Using "##"; TAL2!(RDD);
            Locate , 42: Print Using "##"; TRT2!(RDD);
            Locate , 46: Print Using "###"; TG32!(RDD);
            Locate , 50: Print Using "###"; TG42!(RDD);

            Color 3: Print Tab(53); Chr$(179);

            Color 11
            Locate , 54: Print Using "##"; THW2!(RDD) + TAW2!(RDD);
            Locate , 57: Print Using "##"; THL2!(RDD) + TAL2!(RDD);
            Locate , 60: Print Using "##"; THT2!(RDD) + TRT2!(RDD);
            Locate , 64: Print Using "###"; TG12!(RDD) + TG32!(RDD);
            Locate , 68: Print Using "###"; TG22!(RDD) + TG42!(RDD)

            Color 14
            Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

        Loop Until RDD = 3

    End If

    Cls

    Locate 2, 35
    Color teamRatings%(2), teamRatings%(1): Print A$
    Color 3, 0

    Locate 4
    Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
    Locate 5
    Print Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF"; Tab(32); "PTA"; Tab(35); Chr$(179); Tab(37); "W"; Tab(40); "L"; Tab(43); "T"; Tab(46); "PTF"; Tab(50); "PTA"; Tab(53); Chr$(179); Tab(55); "W"; Tab(58); "L"; Tab(61); "T"; Tab(64); "PTF"; Tab(68); "PTA"

    Color 11
    Print leag1$;

    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)

    For W = 1 To 3

        If DV$(W) <> "XXX" And DV$(W) <> "" Then

            Color 15
            'We have to limit this to 16 because we start stats on column 17
            Locate , 1: Print Left$(DV$(W), 16);

            Color 7
            Locate , 17: Print Using "##"; THW!(W);
            Locate , 20: Print Using "##"; THL!(W);
            Locate , 23: Print Using "##"; THT!(W);
            Locate , 27: Print Using "###"; TG1!(W);
            Locate , 32: Print Using "###"; TG2!(W);

            Color 3: Print Tab(35); Chr$(179);

            Color 7
            Locate , 36: Print Using "##"; TAW!(W);
            Locate , 39: Print Using "##"; TAL!(W);
            Locate , 42: Print Using "##"; TRT!(W);
            Locate , 46: Print Using "###"; TG3!(W);
            Locate , 50: Print Using "###"; TG4!(W);

            Color 3: Print Tab(53); Chr$(179);

            Color 7
            Locate , 54: Print Using "##"; THW!(W) + TAW!(W);
            Locate , 57: Print Using "##"; THL!(W) + TAL!(W);
            Locate , 60: Print Using "##"; THT!(W) + TRT!(W);
            Locate , 64: Print Using "###"; TG1!(W) + TG3!(W);
            Locate , 68: Print Using "###"; TG2!(W) + TG4!(W)

            Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

        End If

    Next W

    THWW = 0
    THLL = 0
    THTT = 0
    TAWW = 0
    TALL = 0
    TRTT = 0

    For I = 1 To 4
        THWW = THWW + THW!(I)
        THLL = THLL + THL!(I)
        THTT = THTT + THT!(I)
        TAWW = TAWW + TAW!(I)
        TALL = TALL + TAL!(I)
        TRTT = TRTT + TRT!(I)
    Next

    TG11 = 0
    TG22 = 0
    TG33 = 0
    TG44 = 0

    For I = 1 To 4
        TG11 = TG11 + TG1!(I)
        TG22 = TG22 + TG2!(I)
        TG33 = TG33 + TG3!(I)
        TG44 = TG44 + TG4!(I)
    Next

    Print

    Color 11
    Locate , 2: Print "TOTALS";
    Locate , 17: Print Using "##"; THWW;
    Locate , 20: Print Using "##"; THLL;
    Locate , 23: Print Using "##"; THTT;
    Locate , 27: Print Using "###"; TG11;
    Locate , 32: Print Using "###"; TG22;

    Color 3: Print Tab(35); Chr$(179);

    Color 11
    Locate , 36: Print Using "##"; TAWW;
    Locate , 39: Print Using "##"; TALL;
    Locate , 42: Print Using "##"; TRTT;
    Locate , 46: Print Using "###"; TG33;
    Locate , 50: Print Using "###"; TG44;

    Color 3: Print Tab(53); Chr$(179);

    Color 11
    Locate , 54: Print Using "##"; THWW + TAWW;
    Locate , 57: Print Using "##"; THLL + TALL;
    Locate , 60: Print Using "##"; THTT + TRTT;
    Locate , 64: Print Using "###"; TG11 + TG33;
    Locate , 68: Print Using "###"; TG22 + TG44

    If X2 = 1 Then

        Print
        Print leag2$

        For W = 1 To 3

            If DV2$(W) <> "XXX" And DV2$(W) <> "" Then

                Color 15
                Locate , 2: Print DV2$(W);

                Color 7
                Locate , 17: Print Using "##"; THW2!(W);
                Locate , 20: Print Using "##"; THL2!(W);
                Locate , 23: Print Using "##"; THT2!(W);
                Locate , 27: Print Using "###"; TG12!(W);
                Locate , 32: Print Using "###"; TG22!(W);

                Color 3: Print Tab(35); Chr$(179);

                Color 7
                Locate , 36: Print Using "##"; TAW2!(W);
                Locate , 39: Print Using "##"; TAL2!(W);
                Locate , 42: Print Using "##"; TRT2!(W);
                Locate , 46: Print Using "###"; TG32!(W);
                Locate , 50: Print Using "###"; TG42!(W);

                Color 3: Print Tab(53); Chr$(179);

                Color 7
                Locate , 54: Print Using "##"; THW2!(W) + TAW2!(W);
                Locate , 57: Print Using "##"; THL2!(W) + TAL2!(W);
                Locate , 60: Print Using "##"; THT2!(W) + TRT2!(W);
                Locate , 64: Print Using "###"; TG12!(W) + TG32!(W);
                Locate , 68: Print Using "###"; TG22!(W) + TG42!(W)

                Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

            End If

        Next W

        THWW2 = 0
        THLL2 = 0
        THTT2 = 0
        TAWW2 = 0
        TALL2 = 0
        TRTT2 = 0

        For I = 1 To 4
            THWW2 = THWW2 + THW2!(I)
            THLL2 = THLL2 + THL2!(I)
            THTT2 = THTT2 + THT2!(I)
            TAWW2 = TAWW2 + TAW2!(I)
            TALL2 = TALL2 + TAL2!(I)
            TRTT2 = TRTT2 + TRT2!(I)
        Next

        TG112 = 0
        TG222 = 0
        TG332 = 0
        TG442 = 0

        For I = 1 To 4
            TG112 = TG112 + TG12!(I)
            TG222 = TG222 + TG22!(I)
            TG332 = TG332 + TG32!(I)
            TG442 = TG442 + TG42!(I)
        Next

        Print

        Color 11
        Locate , 2: Print "TOTALS";
        Locate , 17: Print Using "##"; THWW2;
        Locate , 20: Print Using "##"; THLL2;
        Locate , 23: Print Using "##"; THTT2;
        Locate , 27: Print Using "###"; TG112;
        Locate , 32: Print Using "###"; TG222;

        Color 3: Print Tab(35); Chr$(179);

        Color 11
        Locate , 36: Print Using "##"; TAWW2;
        Locate , 39: Print Using "##"; TALL2;
        Locate , 42: Print Using "##"; TRTT2;
        Locate , 46: Print Using "###"; TG332;
        Locate , 50: Print Using "###"; TG442;

        Color 3: Print Tab(53); Chr$(179);

        Color 11
        Locate , 54: Print Using "##"; THWW2 + TAWW2;
        Locate , 57: Print Using "##"; THLL2 + TALL2;
        Locate , 60: Print Using "##"; THTT2 + TRTT2;
        Locate , 64: Print Using "###"; TG112 + TG332;
        Locate , 68: Print Using "###"; TG222 + TG442

    End If

    Color 14
    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       Hd2HdPro_Report Subroutine
'----------------------------------------
'This subroutine outputs the calculated 
'comparison values to a report file
Sub Hd2HdPro_Report (A$, leag1$, leag2$, DV$(), DV2$())

    Shared div1$, div2$, div3$

    Shared AL!(), AL2!(), AW!(), AW2!()
    Shared G1!(), G12!(), G2!(), G22!()
    Shared G3!(), G32!(), G4!(), G42!()
    Shared HL!(), HL2!(), HT!(), HT2!(), HW!(), HW2!()
    Shared RT2!(), RT!()
    Shared TAL!(), TAL2!(), TAW!(), TAW2!()
    Shared TG1!(), TG12!(), TG2!(), TG22!()
    Shared TG3!(), TG32!(), TG4!(), TG42!()
    Shared THL!(), THL2!(), THW!(), THW2!()
    Shared THT!(), THT2!(), TRT!(), TRT2!()

    Shared statsZ1$(), Z12$()

    Do

        If X1 = 1 Then
            reportFile$ = "REPORT"
        Else

            Color 15, 0
            Cls

            Call MenuBox (6, 8, 61, 7, 15, 1)

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR FILE: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
            Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
            Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"

            Locate 7, 37: Input reportFile$

            If reportFile$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        End If

    Loop Until UCase$(I$) = "N" And reportFile$ <> ""

    Color 15, 0
    
    reportFile$ = reportFile$ + ".RPT"

    Open reportFile$ For Output As #1
    
    DV$(1) = div1$
    DV$(2) = div2$
    DV$(3) = div3$

    RDD = 0

    Print #1, Tab(48); A$
    Print #1,

    Print #1, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
    Print #1, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
    Print #1, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
    Print #1, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
    Print #1, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"

    Do

        RDD = RDD + 1

        DD = ((RDD - 1) * 10) + 1
        EE = RDD * 10

        If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then

            Print #1, leag1$; " "; DV$(RDD)

            For K = DD To EE

                If statsZ1$(K) <> "XXX" And statsZ1$(K) <> A$ Then
                    Print #1, Left$(statsZ1$(K), 15);
                    Print #1, Tab(17); Using "##"; HW!(K);
                    Print #1, Tab(20); Using "##"; HL!(K);
                    Print #1, Tab(23); Using "##"; HT!(K);
                    Print #1, Tab(27); Using "###"; G1!(K);
                    Print #1, Tab(32); Using "###"; G2!(K);
                    Print #1, Tab(38); Using "##"; AW!(K);
                    Print #1, Tab(41); Using "##"; AL!(K);
                    Print #1, Tab(44); Using "##"; RT!(K);
                    Print #1, Tab(48); Using "###"; G3!(K);
                    Print #1, Tab(52); Using "###"; G4!(K);
                    Print #1, Tab(58); Using "##"; HW!(K) + AW!(K);
                    Print #1, Tab(61); Using "##"; HL!(K) + AL!(K);
                    Print #1, Tab(64); Using "##"; HT!(K) + RT!(K);
                    Print #1, Tab(68); Using "###"; G1!(K) + G3!(K);
                    Print #1, Tab(72); Using "###"; G2!(K) + G4!(K)
                    THW!(RDD) = THW!(RDD) + HW!(K)
                    THL!(RDD) = THL!(RDD) + HL!(K)
                    TAW!(RDD) = TAW!(RDD) + AW!(K)
                    TAL!(RDD) = TAL!(RDD) + AL!(K)
                    TRT!(RDD) = TRT!(RDD) + RT!(K)
                    THT!(RDD) = THT!(RDD) + HT!(K)
                    TG1!(RDD) = TG1!(RDD) + G1!(K)
                    TG2!(RDD) = TG2!(RDD) + G2!(K)
                    TG3!(RDD) = TG3!(RDD) + G3!(K)
                    TG4!(RDD) = TG4!(RDD) + G4!(K)
                End If

            Next K

            Print #1,
            Print #1, "TOTALS";
            Print #1, Tab(17); Using "##"; THW!(RDD);
            Print #1, Tab(20); Using "##"; THL!(RDD);
            Print #1, Tab(23); Using "##"; THT!(RDD);
            Print #1, Tab(27); Using "###"; TG1!(RDD);
            Print #1, Tab(32); Using "###"; TG2!(RDD);
            Print #1, Tab(38); Using "##"; TAW!(RDD);
            Print #1, Tab(41); Using "##"; TAL!(RDD);
            Print #1, Tab(44); Using "##"; TRT!(RDD);
            Print #1, Tab(48); Using "###"; TG3!(RDD);
            Print #1, Tab(52); Using "###"; TG4!(RDD);
            Print #1, Tab(58); Using "##"; THW!(RDD) + TAW!(RDD);
            Print #1, Tab(61); Using "##"; THL!(RDD) + TAL!(RDD);
            Print #1, Tab(64); Using "##"; THT!(RDD) + TRT!(RDD);
            Print #1, Tab(68); Using "###"; TG1!(RDD) + TG3!(RDD);
            Print #1, Tab(72); Using "###"; TG2!(RDD) + TG4!(RDD)
            Print #1,
            Print #1,

        End If

    Loop Until RDD = 3

    If X2 = 1 Then

        DV2$(1) = div1_2$
        DV2$(2) = div2_2$
        DV2$(3) = div3_2$
        RDD = 0

        Do

            Print #1, Tab(48); A$
            Print #1,

            Print #1, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
            Print #1, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
            Print #1, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
            Print #1, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
            Print #1, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"

            RDD = RDD + 1

            DD = ((RDD - 1) * 10) + 1
            EE = RDD * 10

            If DV2$(RDD) <> "XXX" And DV2$(RDD) <> "" Then RDD = 3

            Print #1, leag2$; " "; DV2$(RDD)

            For K = DD To EE

                If Z12$(K) <> "XXX" And Z12$(K) <> A$ Then

                    Print #1, Left$(Z12$(K), 15);
                    Print #1, Tab(17); Using "##"; HW2!(K);
                    Print #1, Tab(20); Using "##"; HL2!(K);
                    Print #1, Tab(23); Using "##"; HT2!(K);
                    Print #1, Tab(27); Using "###"; G12!(K);
                    Print #1, Tab(32); Using "###"; G22!(K);
                    Print #1, Tab(38); Using "##"; AW2!(K);
                    Print #1, Tab(41); Using "##"; AL2!(K);
                    Print #1, Tab(44); Using "##"; RT2!(K);
                    Print #1, Tab(48); Using "###"; G32!(K);
                    Print #1, Tab(52); Using "###"; G42!(K);
                    Print #1, Tab(58); Using "##"; HW2!(K) + AW2!(K);
                    Print #1, Tab(61); Using "##"; HL2!(K) + AL2!(K);
                    Print #1, Tab(64); Using "##"; HT2!(K) + RT2!(K);
                    Print #1, Tab(68); Using "###"; G12!(K) + G32!(K);
                    Print #1, Tab(72); Using "###"; G22!(K) + G42!(K)

                    THW2!(RDD) = THW2!(RDD) + HW2!(K)
                    THL2!(RDD) = THL2!(RDD) + HL2!(K)
                    TAW2!(RDD) = TAW2!(RDD) + AW2!(K)
                    TAL2!(RDD) = TAL2!(RDD) + AL2!(K)
                    TRT2!(RDD) = TRT2!(RDD) + RT2!(K)
                    THT2!(RDD) = THT2!(RDD) + HT2!(K)
                    TG12!(RDD) = TG12!(RDD) + G12!(K)
                    TG22!(RDD) = TG22!(RDD) + G22!(K)
                    TG32!(RDD) = TG32!(RDD) + G32!(K)
                    TG42!(RDD) = TG42!(RDD) + G42!(K)

                End If

            Next K

            Print #1,
            Print #1, "TOTALS";
            Print #1, Tab(17); Using "##"; THW2!(RDD);
            Print #1, Tab(20); Using "##"; THL2!(RDD);
            Print #1, Tab(23); Using "##"; THT2!(RDD);
            Print #1, Tab(27); Using "###"; TG12!(RDD);
            Print #1, Tab(32); Using "###"; TG22!(RDD);
            Print #1, Tab(38); Using "##"; TAW2!(RDD);
            Print #1, Tab(41); Using "##"; TAL2!(RDD);
            Print #1, Tab(44); Using "##"; TRT2!(RDD);
            Print #1, Tab(48); Using "###"; TG32!(RDD);
            Print #1, Tab(52); Using "###"; TG42!(RDD);
            Print #1, Tab(58); Using "##"; THW2!(RDD) + TAW2!(RDD);
            Print #1, Tab(61); Using "##"; THL2!(RDD) + TAL2!(RDD);
            Print #1, Tab(64); Using "##"; THT2!(RDD) + TRT2!(RDD);
            Print #1, Tab(68); Using "###"; TG12!(RDD) + TG32!(RDD);
            Print #1, Tab(72); Using "###"; TG22!(RDD) + TG42!(RDD)
            Print #1,
            Print #1,

        Loop Until RDD = 3

    End If

    Print #1, Tab(48); A$
    Print #1, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
    Print #1, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
    Print #1, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
    Print #1, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
    Print #1, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"
    Print #1, leag1$

    For W = 1 To 3

        Print #1, DV$(W);
        Print #1, Tab(17); Using "##"; THW!(W);
        Print #1, Tab(20); Using "##"; THL!(W);
        Print #1, Tab(23); Using "##"; THT!(W);
        Print #1, Tab(27); Using "###"; TG1!(W);
        Print #1, Tab(32); Using "###"; TG2!(W);
        Print #1, Tab(38); Using "##"; TAW!(W);
        Print #1, Tab(41); Using "##"; TAL!(W);
        Print #1, Tab(44); Using "##"; TRT!(W);
        Print #1, Tab(48); Using "###"; TG3!(W);
        Print #1, Tab(52); Using "###"; TG4!(W);
        Print #1, Tab(58); Using "##"; THW!(W) + TAW!(W);
        Print #1, Tab(61); Using "##"; THL!(W) + TAL!(W);
        Print #1, Tab(64); Using "##"; THT!(W) + TRT!(W);
        Print #1, Tab(68); Using "###"; TG1!(W) + TG3!(W);
        Print #1, Tab(72); Using "###"; TG2!(W) + TG4!(W)

    Next W

    THWW = 0
    THLL = 0
    THTT = 0
    TAWW = 0
    TALL = 0
    TRTT = 0

    For I = 1 To 4
        THWW = THWW + THW!(I)
        THLL = THLL + THL!(I)
        THTT = THTT + THT!(I)
        TAWW = TAWW + TAW!(I)
        TALL = TALL + TAL!(I)
        TRTT = TRTT + TRT!(I)
    Next

    TG11 = 0
    TG22 = 0
    TG33 = 0
    TG44 = 0

    For I = 1 To 4
        TG11 = TG11 + TG1!(I)
        TG22 = TG22 + TG2!(I)
        TG33 = TG33 + TG3!(I)
        TG44 = TG44 + TG4!(I)
    Next

    Print #1,
    Print #1, "TOTALS";
    Print #1, Tab(17); Using "##"; THWW;
    Print #1, Tab(20); Using "##"; THLL;
    Print #1, Tab(23); Using "##"; THTT;
    Print #1, Tab(27); Using "###"; TG11;
    Print #1, Tab(32); Using "###"; TG22;
    Print #1, Tab(38); Using "##"; TAWW;
    Print #1, Tab(41); Using "##"; TALL;
    Print #1, Tab(44); Using "##"; TRTT;
    Print #1, Tab(48); Using "###"; TG33;
    Print #1, Tab(52); Using "###"; TG44;
    Print #1, Tab(58); Using "##"; THWW + TAWW;
    Print #1, Tab(61); Using "##"; THLL + TALL;
    Print #1, Tab(64); Using "##"; THTT + TRTT;
    Print #1, Tab(68); Using "###"; TG11 + TG33;
    Print #1, Tab(72); Using "###"; TG22 + TG44

    If X2 = 1 Then

        Print #1,
        Print #1, leag2$

        For W = 1 To 3
            Print #1, DV2$(W);
            Print #1, Tab(17); Using "##"; THW2!(W);
            Print #1, Tab(20); Using "##"; THL2!(W);
            Print #1, Tab(23); Using "##"; THT2!(W);
            Print #1, Tab(27); Using "###"; TG12!(W);
            Print #1, Tab(32); Using "###"; TG22!(W);
            Print #1, Tab(38); Using "##"; TAW2!(W);
            Print #1, Tab(41); Using "##"; TAL2!(W);
            Print #1, Tab(44); Using "##"; TRT2!(W);
            Print #1, Tab(48); Using "###"; TG32!(W);
            Print #1, Tab(52); Using "###"; TG42!(W);
            Print #1, Tab(58); Using "##"; THW2!(W) + TAW2!(W);
            Print #1, Tab(61); Using "##"; THL2!(W) + TAL2!(W);
            Print #1, Tab(64); Using "##"; THT2!(W) + TRT2!(W);
            Print #1, Tab(68); Using "###"; TG12!(W) + TG32!(W);
            Print #1, Tab(72); Using "###"; TG22!(W) + TG42!(W)
        Next W

        THWW2 = 0
        THLL2 = 0
        THTT2 = 0
        TAWW2 = 0
        TALL2 = 0
        TRTT2 = 0

        For I = 1 To 4
            THWW2 = THWW2 + THW2!(I)
            THLL2 = THLL2 + THL2!(I)
            THTT2 = THTT2 + THT2!(I)
            TAWW2 = TAWW2 + TAW2!(I)
            TALL2 = TALL2 + TAL2!(I)
            TRTT2 = TRTT2 + TRT2!(I)
        Next

        TG112 = 0
        TG222 = 0
        TG332 = 0
        TG442 = 0

        For I = 1 To 4
            TG112 = TG112 + TG12!(I)
            TG222 = TG222 + TG22!(I)
            TG332 = TG332 + TG32!(I)
            TG442 = TG442 + TG42!(I)
        Next

        Print #1,
        Print #1, "TOTALS";
        Print #1, Tab(17); Using "##"; THWW2;
        Print #1, Tab(20); Using "##"; THLL2;
        Print #1, Tab(23); Using "##"; THTT2;
        Print #1, Tab(27); Using "###"; TG112;
        Print #1, Tab(32); Using "###"; TG222;
        Print #1, Tab(38); Using "##"; TAWW2;
        Print #1, Tab(41); Using "##"; TALL2;
        Print #1, Tab(44); Using "##"; TRTT2;
        Print #1, Tab(48); Using "###"; TG332;
        Print #1, Tab(52); Using "###"; TG442;
        Print #1, Tab(58); Using "##"; THWW2 + TAWW2;
        Print #1, Tab(61); Using "##"; THLL2 + TALL2;
        Print #1, Tab(64); Using "##"; THTT2 + TRTT2;
        Print #1, Tab(68); Using "###"; TG112 + TG332;
        Print #1, Tab(72); Using "###"; TG222 + TG442

    End If

    Close #1

End Sub
