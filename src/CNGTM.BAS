Sub CNGTM ()

    _Title "3-in-1 Football - Change Team"

    ReDim menuChoices$(1 To 4)

    Do

        Call TITLE
        menuChoices$(1) = "EDIT AN EXISTING TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER  "
        menuChoices$(3) = "VIEW A TEAM ROSTER   "
        menuChoices$(4) = "EXIT TO MAIN MENU    "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 12, 46, 2, 15, 1)

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

            Call GetTeam_CNGTM(A$, AA$, YN$, S$, teamNbr, YC!)

            Select Case RN

                Case 1
                    Call EditTeam(A$, AA$, YN$, S$, teamNbr, YC!)

                Case 2
                    Call PrintRoster(A$, AA$, teamNbr)

                Case 3
                    Call ViewRoster(A$, AA$, teamNbr)

            End Select

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub PrintRoster (A$, AA$, B8%)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$()
    Shared statsA5$(), statsA6$(), statsA7$()

    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%()
    Shared careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()

    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared statsPA%(), careerQX%()

    Color 0, 15

    ' *** PRINT TEAM ROSTER ***
    LPrint "TEAM ROSTER FOR "; A$; " "; AA$; " TEAM NUMBER ="; B8%
    LPrint
    LPrint Using "DEF RUSH ADJ   = ###"; careerB6%(0);
    LPrint Tab(35); Using "SACKED %         = ##"; careerB6%(5)
    LPrint Using "DEF COMP % ADJ = ###"; careerB6%(1);
    LPrint Tab(35); Using "DEF SACK ADJ     = ##"; careerB6%(6)
    LPrint Using "DEF INT % ADJ  = ###"; careerB6%(2);
    LPrint Tab(35); Using "RUN/PASS %       = ##"; careerB6%(7)
    LPrint Using "OFF FUMBLE  %  = ###"; careerB6%(3);
    LPrint Tab(35); "DEF YDS/COMP ADJ = ";

    If YC! < 50 Then LPrint Using "+#.#"; YC! / 100 * 30
    If YC! > 50 Then LPrint Using "+#.#"; (100 - YC!) / 100 * -30

    LPrint Using "DEF FUMBLE  %  = ###"; careerB6%(4);
    LPrint Tab(35); Using "PENALTY ADJ      = ##"; statsPA%(0)
    LPrint Using "STAD TYPE      = ###"; statsPA%(3);
    LPrint Tab(35); Using "GEOGRAPH REGION  = ##"; statsPA%(4)
    LPrint Using "BACKGRND COLOR = ###"; statsPA%(1);
    LPrint Tab(35); Using "FOREGRND COLOR   = ##"; statsPA%(2)
    LPrint
    LPrint "RUNNING BACKS-"
    LPrint "NAME-               RUSH AVG   CX%  REC AVG   CX%"

    For I = 0 To 9:

        If Left$(statsA1$(I), 1) <> " " And Left$(statsA1$(I), 3) <> "END" Then
            LPrint statsA1$(I); Tab(23);
            LPrint Using "##.#     ###    ##     ###"; careerA1%(I) * .3; careerA0%(I); careerA3%(I) * 3; careerA2%(I)
        End If

    Next

    LPrint
    LPrint "WIDE RECEIVERS-"
    LPrint "NAME-              REC AVG     CX%"

    For I = 0 To 5:

        If Left$(statsA2$(I), 1) <> " " And Left$(statsA2$(I), 3) <> "END" Then
            LPrint statsA2$(I); Tab(23);
            LPrint Using "##       ###"; careerA5%(I) * 3; careerA4%(I)
        End If

    Next

    LPrint
    LPrint "QUARTERBACKS-                     YDS/CMP"
    LPrint "NAME-               COMP%   INT%    ADJ     CX%"

    For I = 0 To 3:

        If Left$(statsA3$(I), 1) <> " " And Left$(statsA3$(I), 3) <> "END" Then
            LPrint statsA3$(I); Tab(21);
            LPrint Using "###     ##     ###      ###"; careerA7%(I); careerA8%(I); careerQX%(I); careerA6%(I)
        End If

    Next

    LPrint
    LPrint "KICKOFF RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:

        If Left$(statsA4$(I), 1) <> " " And Left$(statsA4$(I), 3) <> "END" Then
            LPrint statsA4$(I); Tab(23);
            LPrint Using "##       ###"; careerB%(I) * 3; careerA9%(I)
        End If

    Next

    LPrint
    LPrint "PUNT RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:

        If Left$(statsA5$(I), 1) <> " " And Left$(statsA5$(I), 3) <> "END" Then
            LPrint statsA5$(I); Tab(23);
            LPrint Using "##       ###"; careerB2%(I) * 3; careerB1%(I)
        End If

    Next

    LPrint
    LPrint "PLACE KICKERS-"
    LPrint "NAME-                FG%  CX%   EP%  CX%"

    For I = 0 To 1:

        If Left$(statsA7$(I), 1) <> " " And Left$(statsA7$(I), 3) <> "END" Then
            LPrint statsA7$(I); Tab(21);
            LPrint Using "###   ###   ###  ###"; careerB4%(I) * 5 + 25; careerB5%(I); careerB7%(I); careerB8%(I)
        End If

    Next

    LPrint
    LPrint "PUNTER-"
    LPrint "NAME-              AVG"
    LPrint statsA6$(0); Tab(21);
    LPrint Using "##"; careerB3%(0) * 2 + 28

    LPrint Chr$(12)

End Sub

'----------------------------------------
'          EditTeam Subroutine
'----------------------------------------
'Steps user through screens for adjusting
'team data and players
'
'NOTE: <ENTER> key is valued as 0 for when
'a user is prompted to press keys to CONTINUE
'This improvement over the original makes it
'faster and easier to proceed through screens
Sub EditTeam (A$, AA$, YN$, S$, teamNbr, YC!)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$()
    Shared statsA5$(), statsA6$(), statsA7$()

    Shared statsDI$(), statsDS$()
    Shared careerA1%(), careerA3%(), careerA5%(), careerA7%(), careerA8%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared careerDI%(), careerDS%()
    Shared statsPA%(), careerQX%()

    Dim A1!(9), A3!(9), A5!(9)
    Dim B!(9), B2!(1), B3!(2), B4!(2)

    Do

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
            prompt$ = GetKeyPress$
            choice = Val(prompt$)
            If UCase$(prompt$) = "A" Then choice = 10
            If UCase$(prompt$) = "B" Then choice = 11
            'If UCase$(I$) = "S" Then I = 12
        Loop Until choice >= 1 And choice <= 11 Or UCase$(prompt$) = "S"

        Select Case choice

            Case 1
                Call ChangeTeamRatings(A$, AA$, YN$, S$, teamNbr, YC!)

            Case 2

                Do

                    Cls
                    U$ = "EDIT RUNNING BACKS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "   NAME"; Tab(20); "RUSH AVG  REC AVG"
                    Color 15

                    For I = 0 To 9:
                        Print Using "## "; I + 1;
                        Print " "; statsA1$(I); Tab(20);
                        Print Using "##.#       ## ##"; careerA1%(I) * .3; careerA3%(I) * 3
                    Next

                    Do
                        Locate 15
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 10

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls

                        U$ = "RUNNING BACK RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME "; statsA1$(I1)
                        Print "2. RUSHING AVG ";: Print Using "##.#"; careerA1%(I1) * .3
                        Print "3. RECEIVING AVG ";: Print Using "##"; careerA3%(I1) * 3

                        Do

                            Do
                                Locate 7
                                Input "ENTER # TO CHANGE (0-RETURN TO RB SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 3

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; statsA1$(I1)

                                Case 2
                                    Locate 4: Input "2. RUSHING AVG "; A1!(I1)
                                    careerA1%(I1) = Int(A1!(I1) / .3 + .5)

                                Case 3
                                    Locate 5: Input "3. RECEIVING AVG "; A3!(I1)
                                    careerA3%(I1) = Int(A3!(I1) / 3 + .5)

                            End Select

                        Loop Until C9 = 0

                    End If

                Loop Until B9 = 0

            Case 3

                Do

                    Cls
                    U$ = "EDIT WIDE RECEIVERS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "   NAME"; Tab(20); "REC AVG"
                    Color 15

                    For I = 0 To 5:
                        Print Using "## "; I + 1;
                        Print " "; statsA2$(I); Tab(20);
                        Print Using "   ##"; careerA5%(I) * 3
                    Next

                    Do
                        Locate 11
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 6

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls
                        U$ = "WIDE RECEIVER RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME "; statsA2$(I1)
                        Print "2. RECEIVING AVG ";: Print Using "##"; careerA5%(I1) * 3

                        Do

                            Do
                                Locate 6
                                Input "ENTER # TO CHANGE (0-RETURN TO WR SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 2

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; statsA2$(I1)

                                Case 2
                                    Locate 4: Input "2. RECEIVING AVG "; A5!(I1): careerA5%(I1) = Int(A5!(I1) / 3 + .5)

                            End Select

                        Loop Until C9 = 0

                    End If

                Loop Until B9 = 0

            Case 4

                Do
                    Cls
                    U$ = "EDIT QUARTERBACKS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "   NAME"; Tab(20); "COMP%   INT%   YDS/COMP ADJ"
                    Color 15

                    For I = 0 To 3:
                        Print Using "## "; I + 1;
                        Print " "; statsA3$(I); Tab(20);
                        Print Using " ##      ##       +##"; careerA7%(I); careerA8%(I); careerQX%(I)
                    Next

                    Do
                        Locate 9
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls

                        U$ = "QUARTERBACK RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME "; statsA3$(I1)
                        Print "2. COMPLETION % "; careerA7%(I1)
                        Print "3. INTERCEPTION % "; careerA8%(I1)
                        Print "4. YDS/COMP ADJ "; careerQX%(I1)

                        Do

                            Do
                                Locate 8
                                Input "ENTER # TO CHANGE (0-RETURN TO QB SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 4

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; statsA3$(I1)

                                Case 2
                                    Locate 4: Input "2. COMPLETION % "; careerA7%(I1)

                                Case 3
                                    Locate 5: Input "3. INTERCEPTION % "; careerA8%(I1)

                                Case 4
                                    Locate 6: Input "4. YDS/COMP ADJ "; careerQX%(I1)

                            End Select

                        Loop Until C9 = 0

                    End If

                Loop Until B9 = 0

            Case 5

                Do

                    Cls
                    U$ = "EDIT KICK-OFF RETURNMEN"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "   NAME"; Tab(20); "RET AVG"
                    Color 15

                    For I = 0 To 2:
                        Print Using "## "; I + 1;
                        Print " "; statsA4$(I); Tab(20);
                        Print Using "  ##"; careerB%(I) * 3
                    Next

                    Do
                        Locate 8
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 3

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls
                        U$ = "KICK-OFF RETURNMEN RATINGS"
                        Locate 1, 40 - Len(U$) / 2

                        Color 14: Print U$
                        Color 15
                        Print
                        Print "1. NAME "; statsA4$(I1)
                        Print "2. RETURN AVG ";: Print Using "##"; careerB%(I1) * 3

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO KOR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; statsA4$(I1)

                            Case 2
                                Locate 4: Input "2. RETURN AVG "; B!(I1)
                                careerB%(I1) = Int(B!(I1) / 3 + .5)
                        End Select

                    End If

                Loop Until B9 = 0

            Case 6

                Do
                    Cls
                    U$ = "EDIT PUNT RETURNMEN"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "   NAME"; Tab(20); "RET AVG"
                    Color 15

                    For I = 0 To 2:
                        Print Using "## "; I + 1;
                        Print " "; statsA5$(I); Tab(20);
                        Print Using "  ##"; careerB2%(I) * 3
                    Next

                    Do
                        Locate 8
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 3

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls
                        U$ = "PUNT RETURNMEN RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Color 15
                        Print
                        Print "1. NAME "; statsA5$(I1)
                        Print "2. RETURN AVG ";: Print Using "##"; careerB2%(I1) * 3

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO PR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; statsA5$(I1)

                            Case 2
                                Locate 4: Input "2. RETURN AVG "; B2!(I1): careerB2%(I1) = Int(B2!(I1) / 3 + .5)

                        End Select

                    End If

                Loop Until B9 = 0

            Case 7

                Do

                    Cls
                    U$ = "EDIT PUNTER"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 15
                    Print "   NAME"; Tab(20); "AVG"

                    For I = 0 To 0:
                        Print Using "## "; I + 1;
                        Print " "; statsA6$(I); Tab(20);
                        Print Using "##"; careerB3%(I) * 2 + 28
                    Next

                    Do
                        Locate 6
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 1

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls
                        U$ = "PUNTER RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME "; statsA6$(I1)
                        Print "2. PUNTING AVG ";: Print Using "##"; careerB3%(I1) * 2 + 28

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO PUNTER SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; statsA6$(I1)

                            Case 2
                                Locate 4: Input "2. PUNTING AVG "; B3!(I1): careerB3%(I1) = Int(((B3!(I1) - 28) / 2) + .5)

                        End Select

                    End If

                Loop Until B9 = 0

            Case 8

                Do
                    Cls
                    U$ = "EDIT PLACE KICKERS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "   NAME"; Tab(20); "FG%   EP%"
                    Color 15

                    For I = 0 To 1:
                        Print Using "## "; I + 1;
                        Print " "; statsA7$(I); Tab(20);
                        Print Using "###    ###"; careerB4%(I) * 5 + 25; careerB7%(I)
                    Next

                    Do
                        Locate 7
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 2

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls
                        U$ = "PLACE KICKER RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME "; statsA7$(I1)
                        Print "2. FIELD GOAL % ";: Print careerB4%(I1) * 5 + 25
                        Print "3. EXTRA POINT % ";: Print careerB7%(I1)

                        Do
                            Locate 7
                            Input "ENTER # TO CHANGE (0-RETURN TO PK SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 3

                        Select Case C9
                            Case 1
                                Locate 3: Input "1. NAME "; statsA7$(I1)

                            Case 2
                                Locate 4: Input "2. FIELD GOAL % "; B4!(I1)
                                careerB4%(I1) = Int((B4!(I1) / 5) + .5) - 5

                            Case 3
                                Locate 5: Input "3. EXTRA POINT % "; careerB7%(I1)

                        End Select

                    End If

                Loop Until B9 = 0

            Case 9

                Cls
                U$ = "EDIT PLAYERS WITH SACKS"
                Locate 1, 40 - Len(U$) / 2
                Color 14: Print U$
                Print
                Color 12
                Print "   NAME"; Tab(20); "SACKS"
                Color 15

                Locate 4
                TSR% = 0

                For I = 0 To 14:
                    Print Using "## "; I + 1;
                    Print " "; statsDS$(I); Tab(20); careerDS%(I)
                Next

                Do
                    Locate 20
                    Input "ENTER PLAYER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                Loop Until B9 >= 0 And B9 <= 15

                If B9 <> 0 Then

                    I1 = B9 - 1

                    Cls
                    U$ = "SACK RATINGS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 15
                    Print "1. NAME "; statsDS$(I1)
                    Print "2. SACKS "; careerDS%(I1)

                    Do
                        Locate 6
                        Input "ENTER # TO CHANGE (0-RETURN TO SACKS SELECTION) "; C9
                    Loop Until C9 >= 0 And C9 <= 2

                    Select Case C9

                        Case 1
                            Locate 3: Input "1. NAME "; statsDS$(I1)

                        Case 2
                            Locate 4: Input "2. SACKS "; careerDS%(I1)

                    End Select

                End If

            Case 10

                Do

                    Cls
                    U$ = "EDIT PLAYERS WITH INTERCEPTIONS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "   NAME"; Tab(20); "INTERCEPTIONS"
                    Color 15

                    Locate 4

                    For I = 0 To 9:
                        Print Using "## "; I + 1;
                        Print " "; statsDI$(I); Tab(27); careerDI%(I)
                    Next

                    Do
                        Locate 15
                        Input "ENTER PLAYER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
                    Loop Until B9 >= 0 And B9 <= 10

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls
                        U$ = "INTERCEPTION RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME "; statsDI$(I1)
                        Print "2. INTERCEPTIONS "; careerDI%(I1)

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO INT SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; statsDI$(I1)

                            Case 2
                                Locate 4: Input "2. INTERCEPTIONS "; careerDI%(I1)

                        End Select

                    End If

                Loop Until B9 = 0

            Case 11
                Cls
                Print "SAVE CHANGES (YN)";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End Select

    Loop Until choice = 11 Or UCase$(prompt$) = "S"
    
    If (choice = 11 And UCase$(I$) = "Y") Or UCase$(prompt$) = "S" Then

        Call CheckContributions

        '-- Why do we do this???
        '-- We lose accuracy here!!!
        '-- To change it though means updating
        '-- our routines for reading/saving teams
        '-- Not only that, but we have to
        '-- update every team file to accomodate
        YC% = YC!

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

        For I = 0 To 9:
            If careerA2%(I) <> 0 Then careerA2%(I) = Int(S5 * careerA2%(I) + .5) + S3: S3 = careerA2%(I)
            If careerA2%(I) > 100 Then careerA2%(I) = 100
        Next I

        For I = 0 To 5:
            If careerA4%(I) <> 0 Then careerA4%(I) = Int(S5 * careerA4%(I) + .5) + S3: S3 = careerA4%(I)
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

        For I = 0 To 2:
            G0 = G0 + careerA9%(I)
            G3 = G3 + careerB1%(I)
        Next

        G2 = 100 / G0
        G5 = 100 / G3

        For I = 0 To 2:
            If careerA9%(I) <> 0 Then careerA9%(I) = Int(G2 * careerA9%(I) + .5) + G1: G1 = careerA9%(I)
            If careerA9%(I) > 100 Then careerA9%(I) = 100
        Next

        For I = 0 To 2:
            If careerB1%(I) <> 0 Then careerB1%(I) = Int(G5 * careerB1%(I) + .5) + G4: G4 = careerB1%(I)
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

        G2 = 100 / G0
        G5 = 100 / G3

        For I = 0 To 1:
            If careerB5%(I) <> 0 Then careerB5%(I) = Int(G2 * careerB5%(I) + .5) + G1: G1 = careerB5%(I)
            If careerB5%(I) > 100 Then careerB5%(I) = 100
        Next

        For I = 0 To 1:
            If careerB8%(I) <> 0 Then careerB8%(I) = Int(G5 * careerB8%(I) + .5) + G4: G4 = careerB8%(I)
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

        Call SaveChangedTeam(A$, AA$, YN$, S$, teamNbr, YC!)

    End If


End Sub


'----------------------------------------
'       ChangeTeamRatings Subroutine
'----------------------------------------
'...explanation...
Sub ChangeTeamRatings (A$, AA$, YN$, S$, teamNbr, YC!)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$()
    Shared statsA5$(), statsA6$(), statsA7$()

    Shared statsDI$(), statsDS$()
    Shared careerA1%(), careerA3%(), careerA5%(), careerA7%(), careerA8%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared careerDI%(), careerDS%()
    Shared statsPA%(), careerQX%()

    Cls

    U$ = "CHANGE TEAM RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    YC! = 10 / 3 * YC!

    Color 15
    Locate 2: Print " 1. TEAM NUMBER "; teamNbr
    Locate 3: Print " 2. TEAM NAME "; A$
    Locate 4: Print " 3. NICKNAME "; AA$
    Locate 5: Print " 4. DEF RUSHING ADJ "; careerB6%(0)
    Locate 6: Print " 5. DEF COMPLETION % ADJ "; careerB6%(1)
    Locate 7: Print " 6. DEF INTERCEPTION % ADJ "; careerB6%(2)
    Locate 8: Print " 7. OFF FUMBLE % "; careerB6%(3)
    Locate 9: Print " 8. DEF FUMBLE % "; careerB6%(4)
    Locate 10: Print " 9. OFF SACK ADJ % "; careerB6%(5)
    Locate 11: Print "10. DEF SACK ADJ % "; careerB6%(6)
    Locate 12: Print "11. DEF YDS/COMP ADJ ";
    If YC! < 50 Then Print Using "+#.#"; YC! / 100 * 30
    If YC! > 50 Then Print Using "+#.#"; (100 - YC!) / 100 * -30

    Locate 13: Print "12. TM PENALTY ADJ "; statsPA%(0)
    Locate 14: Print "13. OFF RUN/PASS % "; careerB6%(7)
    Locate 15: Print "14. TM STADIUM (0-OUTDOOR 1-DOME) "; statsPA%(3)
    Locate 16: Print "15. POWER INDEX "; statsPA%(4)
    Locate 17: Print "16. TM BACKGROUND COLOR (0-7) "; statsPA%(1)
    Locate 18: Print "17. TM TEXT COLOR (0-15) "; statsPA%(2)
    Locate 19: Print "18. DISK ID "; YN$
    Locate 20: Print "19. STADIUM NAME "; S$
    Locate 21: Print "20. AVERAGE ATTENDANCE "; teamAttendance&

    If statsPA%(1) >= 0 And statsPA%(2) >= 0 Then
        Locate 22, 1
        Print "TEAM APPEARANCE: ";
        Color statsPA%(2), statsPA%(1): Print A$ + " " + AA$
    End If

    Color 15, 0

    Do

        Do
            Locate 23
            Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
        Loop Until B9 >= 0 And B9 <= 20

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 2: Input " 1. TEAM NUMBER "; B8%

                Case 2
                    Locate 3: Input " 2. TEAM NAME "; A$

                Case 3
                    Locate 4: Input " 3. NICKNAME "; AA$

                Case 4
                    Locate 5: Input " 4. DEF RUSHING ADJ "; careerB6%(0)

                Case 5
                    Locate 6: Input " 5. DEF COMPLETION % ADJ "; careerB6%(1)

                Case 6
                    Locate 7: Input " 6. DEF INTERCEPTION % ADJ "; careerB6%(2)

                Case 7
                    Locate 8: Input " 7. OFF FUMBLE % "; careerB6%(3)

                Case 8
                    Locate 9: Input " 8. DEF FUMBLE % "; careerB6%(4)

                Case 9
                    Locate 10: Input " 9. OFF SACK ADJ % "; careerB6%(5)

                Case 10
                    Locate 11: Input "10. DEF SACK ADJ % "; careerB6%(6)

                Case 11
                    Locate 12: Input "11. DEF YDS/COMP ADJ "; YC!
                    If YC! <= 0 Then YC! = 100 + (YC! * 10 / 3)

                Case 12
                    Locate 13: Input "12. TM PENALTY ADJ "; statsPA%(0)

                Case 13
                    Locate 14: Input "13. OFF RUN/PASS % "; careerB6%(7)

                Case 14
                    Locate 15: Input "14. TM STADIUM (0-OUTDOOR 1-DOME) "; statsPA%(3)

                Case 15
                    Locate 16: Input "15. POWER INDEX "; statsPA%(4)

                Case 16
                    Locate 17: Input "16. TM BACKGROUND COLOR (0-7) "; statsPA%(1)
                    If statsPA%(1) >= 0 And statsPA%(2) >= 0 Then
                        Locate 22, 1
                        Color statsPA%(2), statsPA%(1): Print "SAMPLE TEAM COLOR"
                    End If

                Case 17
                    Locate 18: Input "17. TM TEXT COLOR (0-15) "; statsPA%(2)
                    If statsPA%(1) >= 0 And statsPA%(2) >= 0 Then
                        Locate 22, 1
                        Color statsPA%(2), statsPA%(1): Print "SAMPLE TEAM COLOR"
                    End If

                Case 18
                    Locate 19: Input "18. DISK ID "; YN$

                Case 19
                    Locate 20: Input "19. STADIUM NAME "; S$

                Case 20
                    Locate 21: Input "20. AVERAGE ATTENDANCE "; teamAttendance&
            End Select

        End If

    Loop Until B9 = 0

    Cls

End Sub


'----------------------------------------
'       SaveChangedTeam Subroutine
'----------------------------------------
'...explanation...
Sub SaveChangedTeam (A$, AA$, YN$, S$, teamNbr, YC!)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$()
    Shared statsA5$(), statsA6$(), statsA7$()

    Shared statsDI$(), statsDS$()

    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%()
    Shared careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()

    Shared careerDI%(), careerDS%(), statsPA%(), careerQX%()

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

    For I = 0 To 7:
        LSet Q$(I + 3) = MKI$(careerB6%(I))
    Next I

    For I = 0 To 4:
        LSet Q$(I + 11) = MKI$(statsPA%(I))
    Next I

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

    LSet Q$(122) = RTrim$(statsA6$(0))
    LSet Q$(123) = MKI$(careerB3%(0))
    LSet Q$(124) = RTrim$(statsA7$(0))
    LSet Q$(125) = MKI$(careerB4%(0))
    LSet Q$(126) = MKI$(careerB5%(0))
    LSet Q$(127) = MKI$(careerB7%(0))
    LSet Q$(128) = MKI$(careerB8%(0))
    LSet Q$(129) = RTrim$(statsA7$(1))
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

    Put #1, teamNbr

    Close #1

    Open diskPaths$(0) + "STADIUMS." + teamYear$ For Random As #2 Len = 50

    Field #2, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)

    LSet Q$(1) = A$
    LSet Q$(2) = S$
    LSet Q$(3) = MKL$(teamAttendance&)
    LSet Q$(4) = "x"

    Put #2, teamNbr

    Close 2

    S$ = ""
    teamAttendance& = 0

    For I = 0 To 9
        careerA0%(I) = 0
        careerA1%(I) = 0
        careerA2%(I) = 0
        careerA3%(I) = 0
        statsA1$(I) = ""
    Next

    For I = 0 To 5
        careerA4%(I) = 0
        careerA5%(I) = 0
        statsA2$(I) = ""
    Next

    For I = 0 To 3
        careerA6%(I) = 0
        careerA7%(I) = 0
        careerA8%(I) = 0
        careerQX%(I) = 0
        statsA3$(I) = ""
    Next

    For I = 0 To 2
        careerA9%(I) = 0
        careerB%(I) = 0
        careerB1%(I) = 0
        careerB2%(I) = 0
        statsA4$(I) = ""
        statsA5$(I) = ""
    Next

    statsA6$(0) = ""

    For I = 0 To 1
        careerB3%(I) = 0
        careerB4%(I) = 0
        careerB5%(I) = 0
        careerB7%(I) = 0
        careerB8%(I) = 0
        statsA7$(I) = ""
    Next

End Sub

'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub ViewRoster (A$, AA$, B8%)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$()
    Shared statsA5$(), statsA6$(), statsA7$()
    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%()
    Shared careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()

    Shared statsPA%()
    Shared careerQX%()

    Do
        Cls
        Color 11
        Print "TEAM ROSTER FOR ";
        Color statsPA%(2), statsPA%(1): Print A$;

        Color 11, 0: Print "   TEAM NUMBER =";
        Color 14: Print B8%

        Print

        Color 15: Print Using "DEF RUSH ADJ   = ###"; careerB6%(0);
        Print Tab(35); Using "SACKED %         = ##"; careerB6%(5)
        Print Using "DEF COMP % ADJ = ###"; careerB6%(1);
        Print Tab(35); Using "DEF SACK ADJ     = ##"; careerB6%(6)
        Print Using "DEF INT % ADJ  = ###"; careerB6%(2);
        Print Tab(35); Using "RUN/PASS %       = ##"; careerB6%(7)
        Print Using "OFF FUMBLE  %  = ###"; careerB6%(3);
        Print Tab(35); "DEF YDS/COMP ADJ = ";

        If YC! < 50 Then Print Using "+#.#"; YC! / 100 * 30
        If YC! > 50 Then Print Using "+#.#"; (100 - YC!) / 100 * -30

        Print Using "DEF FUMBLE  %  = ###"; careerB6%(4);
        Print Tab(35); Using "PENALTY ADJ      = ##"; statsPA%(0)
        Print Using "STAD TYPE      = ###"; statsPA%(3)
        Print Using "BACKGRND COLOR = ###"; statsPA%(1);
        Print Tab(35); Using "FOREGRND COLOR   = ##"; statsPA%(2)
        Print
        Color 12: Print "RUNNING BACKS-"
        Color 14: Print "NAME-               RUSH AVG   CX%  REC AVG   CX%"

        For I = 0 To 9:
            If Left$(statsA1$(I), 1) <> " " And Left$(statsA1$(I), 3) <> "END" Then
                Color 3: Print statsA1$(I);
                Color 15
                Locate , 23: Print Using "##.#     ###    ##     ###"; careerA1%(I) * .3; careerA0%(I); careerA3%(I) * 3; careerA2%(I)
            End If
        Next

        Locate 23
        Color 15: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

        Cls
        Color 12: Print "WIDE RECEIVERS-"
        Color 14: Print "NAME-              REC AVG     CX%"

        For I = 0 To 5:
            If Left$(statsA2$(I), 1) <> " " And Left$(statsA2$(I), 3) <> "END" Then
                Color 3: Print statsA2$(I);
                Color 15: Locate , 23: Print Using "##       ###"; careerA5%(I) * 3; careerA4%(I)
            End If
        Next

        Print
        Color 12: Print "QUARTERBACKS-                     YDS/CMP"
        Color 14: Print "NAME-               COMP%   INT%    ADJ     CX%"
        For I = 0 To 3:
            If Left$(statsA3$(I), 1) <> " " And Left$(statsA3$(I), 3) <> "END" Then
                Color 3: Print statsA3$(I);
                Color 15: Locate , 21: Print Using "###     ##     ###      ###"; careerA7%(I); careerA8%(I); careerQX%(I); careerA6%(I)
            End If
        Next

        Print
        Color 12: Print "KICKOFF RETURNMEN-"
        Color 14: Print "NAME-              RET AVG     CX%"

        For I = 0 To 2:
            If Left$(statsA4$(I), 1) <> " " And Left$(statsA4$(I), 3) <> "END" Then
                Color 3: Print statsA4$(I);
                Color 15: Locate , 23: Print Using "##       ###"; careerB%(I) * 3; careerA9%(I)
            End If
        Next

        Locate 21: Color 15: Print "HIT ANY KEY TO CONTINUE"

        I$ = GetKeyPress$
        Cls
        Color 12: Print "PUNT RETURNMEN-"
        Color 14: Print "NAME-              RET AVG     CX%"

        For I = 0 To 2:
            If Left$(statsA5$(I), 1) <> " " And Left$(statsA5$(I), 3) <> "END" Then
                Color 3: Print statsA5$(I);
                Color 15: Locate , 23: Print Using "##       ###"; careerB2%(I) * 3; careerB1%(I)
            End If
        Next

        Print
        Color 12: Print "PLACE KICKERS-"
        Color 14: Print "NAME-                FG%  CX%   EP%  CX%"

        For I = 0 To 1:
            If Left$(statsA7$(I), 1) <> " " And Left$(statsA7$(I), 3) <> "END" Then
                Color 3: Print statsA7$(I);
                Color 15: Locate , 21: Print Using "###   ###   ###  ###"; careerB4%(I) * 5 + 25; careerB5%(I); careerB7%(I); careerB8%(I)
            End If
        Next

        Print
        Color 12: Print "PUNTER-"
        Color 14: Print "NAME-              AVG"
        Color 3: Print statsA6$(0);
        Color 15
        Locate , 21: Print Using "##"; careerB3%(0) * 2 + 28

        Do
            Locate 21
            Color 15
            Print "VIEW AGAIN (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"



End Sub

'----------------------------------------
'     CheckContributions Subroutine
'----------------------------------------
'...explanation...
Sub CheckContributions

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$()
    Shared statsA5$(), statsA6$(), statsA7$()
    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%()
    Shared careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()
    Shared careerB1%(), careerB5%(), careerB8%()

    Cls
    U$ = "CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Color 15

    Locate 3
    Print "THE NEXT SCREENS WILL CYCLE THROUGH THE  TEAM ROSTER SHOWING THE  % OF PLAYING"
    Print "TIME EACH PLAYER IS CURRENTLY ALLOTTED. TOTAL CONTRIBUTION FOR EACH POSITION"
    Print "MUST TOTAL 100%"
    Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    '-----

    Cls
    U$ = "RUNNING BACK CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Print
    Color 12
    Print "   NAME"; Tab(20); "CONTRIBUTION"
    Color 15

    Do

        Locate 4, 1
        BC% = 0

        For I = 0 To 9:
            Print Using "## "; I + 1;
            Print " "; statsA1$(I); Tab(20);
            Print Using "     ###"; careerA0%(I)
            BC% = BC% + careerA0%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print "11  PROCEED TO RECEIVERS"

        Do
            Locate 17
            Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 11

        If B9 <> 0 Then

            I1 = B9 - 1

            If I1 <> 10 Then
                Locate I1 + 4, 24: Print "   "
                Locate I1 + 4, 24: Input careerA0%(I1)
            Else
                If BC% <> 100 Then
                    Locate 19: Print "TOTAL IS NOT 100%!"
                    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    Locate 24, 1: Print Space$(25);

                End If

            End If

        End If

    Loop Until (B9 = 0 Or B9 = 11) And BC% = 100

    '-----

    Cls
    U$ = "RECEIVING CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Print
    Color 12
    Print "   NAME"; Tab(20); "CONTRIBUTION"
    Color 15

    Do

        Locate 4, 1
        BC% = 0

        For I = 0 To 9:
            Print Using "## "; I + 1;
            Print " "; statsA1$(I); Tab(20);: Print Using "      ##"; careerA2%(I)
            BC% = BC% + careerA2%(I)
        Next

        For I = 0 To 5
            Print Using "## "; I + 11;
            Print " "; statsA2$(I); Tab(20);: Print Using "      ##"; careerA4%(I)
            BC% = BC% + careerA4%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print "17  PROCEED TO QUARTERBACKS"

        Do
            Locate 22: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 17

        If B9 <> 0 Then

            I1 = B9 - 1

            If I1 <> 17 Then

                Locate I1 + 4, 24
                Print "   "
                Locate I1 + 4, 24

                If I1 <= 9 Then
                    Input careerA2%(I1)
                Else
                    Input careerA4%(I1 - 10)
                End If

            Else

                If BC% <> 100 Then
                    Locate 23: Print "TOTAL IS NOT 100%!"
                    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    Locate 24, 1: Print Space$(25);
                End If

            End If

        End If

    Loop Until (B9 = 0 Or B9 = 17) And BC% = 100

    '-----

    Cls
    U$ = "QUARTERBACK CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Print
    Color 12
    Print "   NAME"; Tab(20); "CONTRIBUTION"
    Color 15

    Do

        Locate 4, 1
        BC% = 0

        For I = 0 To 3:
            Print Using "## "; I + 1;
            Print " "; statsA3$(I); Tab(20);
            Print Using "     ###"; careerA6%(I)
            BC% = BC% + careerA6%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 5  PROCEED TO KO RETURNMEN"

        Do
            Locate 11: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 5

        If B9 <> 0 Then

            I1 = B9 - 1

            If I1 <> 4 Then
                Locate I1 + 4, 24
                Print "   "
                Locate I1 + 4, 24
                Input careerA6%(I1)
            Else
                If BC% <> 100 Then
                    Locate 13: Print "TOTAL IS NOT 100%!"
                    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    Locate 24, 1: Print Space$(25);
                End If

            End If

        End If

    Loop Until (B9 = 0 Or B9 = 5) And BC% = 100

    '-----

    Cls
    U$ = "KICK-OFF RETURN CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Print
    Color 12
    Print "   NAME"; Tab(20); "CONTRIBUTION"
    Color 15

    Do

        Locate 4, 1
        BC% = 0

        For I = 0 To 2
            Print Using "## "; I + 1;
            Print " "; statsA4$(I); Tab(20);
            Print Using "     ###"; careerA9%(I)
            BC% = BC% + careerA9%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 4  PROCEED TO PUNT RETURNMEN"

        Do
            Locate 10
            Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 4

        If B9 <> 0 Then

            I1 = B9 - 1

            If I1 <> 3 Then

                Locate I1 + 4, 24
                Print "   "
                Locate I1 + 4, 24
                Input careerA9%(I1)

            Else

                If BC% <> 100 Then
                    Locate 12: Print "TOTAL IS NOT 100%!"
                    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    Locate 24, 1: Print Space$(25);
                End If

            End If

        End If

    Loop Until (B9 = 0 Or B9 = 4) And BC% = 100


    '-----

    Cls
    U$ = "PUNT RETURN CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Print
    Color 12
    Print "   NAME"; Tab(20); "CONTRIBUTION"
    Color 15

    Do

        Locate 4, 1
        BC% = 0

        For I = 0 To 2:
            Print Using "## "; I + 1;
            Print " "; statsA5$(I); Tab(20);
            Print Using "     ###"; careerB1%(I)
            BC% = BC% + careerB1%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 4  PROCEED TO FG KICKERS"

        Do
            Locate 11: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 4

        If B9 <> 0 Then

            I1 = B9 - 1

            If I1 <> 3 Then

                Locate I1 + 4, 24
                Print "   "
                Locate I1 + 4, 24
                Input careerB1%(I1)

            Else

                If BC% <> 100 Then
                    Locate 13: Print "TOTAL IS NOT 100%!"
                    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    Locate 24, 1: Print Space$(25);
                End If

            End If

        End If

    Loop Until (B9 = 0 Or B9 = 4) And BC% = 100

    '-----

    Cls
    U$ = "FIELD GOAL CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Print
    Color 12
    Print "   NAME"; Tab(20); "CONTRIBUTION"
    Color 15

    Do

        Locate 4, 1
        BC% = 0

        For I = 0 To 1:
            Print Using "## "; I + 1;
            Print " "; statsA7$(I); Tab(20);
            Print Using "     ###"; careerB5%(I)
            BC% = BC% + careerB5%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 3  PROCEED TO PAT/EP KICKERS"

        Do
            Locate 10: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 3

        If B9 <> 0 Then

            I1 = B9 - 1

            If I1 <> 2 Then

                Locate I1 + 4, 24
                Print "   "
                Locate I1 + 4, 24
                Input careerB5%(I1)

            Else

                If BC% <> 100 Then
                    Locate 12: Print "TOTAL IS NOT 100%!"
                    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    Locate 24, 1: Print Space$(25);
                End If

            End If

        End If

    Loop Until (B9 = 0 Or B9 = 3) And BC% = 100

    '-----

    Cls
    U$ = "PAT/EXTRA POINT CONTRIBUTION RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    Print
    Color 12
    Print "   NAME"; Tab(20); "CONTRIBUTION"
    Color 15

    Do

        Locate 4, 1
        BC% = 0

        For I = 0 To 1
            Print Using "## "; I + 1;
            Print " "; statsA7$(I); Tab(20);
            Print Using "     ###"; careerB8%(I): BC% = BC% + careerB8%(I)
        Next

        Print "    TOTAL"; Tab(20);: Print Using "     ###"; BC%
        Print " 3  PROCEED"

        Do
            Locate 10: Input "ENTER #: "; B9
        Loop Until B9 >= 0 And B9 <= 3

        If B9 <> 0 Then

            I1 = B9 - 1

            If I1 <> 2 Then

                Locate I1 + 4, 24
                Print "   "
                Locate I1 + 4, 24
                Input careerB8%(I1)

            Else

                If BC% <> 100 Then
                    Locate 12: Print "TOTAL IS NOT 100%!"
                    Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
                    I$ = GetKeyPress$
                    Locate 24, 1: Print Space$(25);
                End If

            End If

        End If

    Loop Until (B9 = 0 Or B9 = 3) And BC% = 100

End Sub


'----------------------------------------
'      GetTeam_CNGTM Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CNGTM (A$, AA$, YN$, S$, teamNbr, YC!)

    Shared statsPA%()

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
            Call SelectTeam_CNGTM(NTMS, teamNbr, YN$, YC!)
        Else
            YN$ = RTrim$(NY$)
            Erase teamNames$
            NTMS = 0

            Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

            fileLength& = LOF(1)

            Field #1, 15 As Q$(0), 640 As X$

            For I1 = 1 To fileLength& / 934

                Get #1, I1

                For I2 = 1 To 15
                    If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam% = 1: Exit For
                Next I2

                If nullTeam% <> 1 Then
                    NTMS = NTMS + 1
                    teamNames$(NTMS) = Q$(0)
                    teamIndex%(NTMS) = I1
                End If

            Next I1

            Close 1

            Call SelectTeam_CNGTM(NTMS, teamNbr, YN$, YC!)

        End If

        A$ = Q$(1)
        AA$ = Q$(2)

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color statsPA%(2), statsPA%(1): Print A$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
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
Sub SelectTeam_CNGTM (NTMS, teamNbr, YN$, YC!)

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

    teamNbr = teamIndex%(Selection%)
    teamYear$ = RTrim$(YN$)

    Call ReadTeam_CNGTM(teamYear$, teamNbr, YC!)

End Sub


'----------------------------------------
'      ReadTeam_CNGTM Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam_CNGTM (YN$, B8%, YC!)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$()
    Shared statsA5$(), statsA6$(), statsA7$()
    Shared statsDI$(), statsDS$()
    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%()
    Shared careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()
    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared careerDI%(), careerDS%(), statsPA%(), careerQX%()

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

    For I = 0 To 7: careerB6%(I) = CVI(Q$(I + 3)): Next I

    For I = 0 To 4: statsPA%(I) = CVI(Q$(I + 11)): Next I

    For I = 0 To 9
        statsA1$(I) = Q$(I * 5 + 16)
        careerA0%(I) = CVI(Q$(I * 5 + 17))
        careerA1%(I) = CVI(Q$(I * 5 + 18))
        careerA2%(I) = CVI(Q$(I * 5 + 19))
        careerA3%(I) = CVI(Q$(I * 5 + 20))
    Next I

    For I = 0 To 5
        statsA2$(I) = Q$(I * 3 + 66)
        careerA4%(I) = CVI(Q$(I * 3 + 67))
        careerA5%(I) = CVI(Q$(I * 3 + 68))
    Next I

    For I = 0 To 3
        statsA3$(I) = Q$(I * 5 + 84)
        careerA6%(I) = CVI(Q$(I * 5 + 85))
        careerA7%(I) = CVI(Q$(I * 5 + 86))
        careerA8%(I) = CVI(Q$(I * 5 + 87))
        careerQX%(I) = CVI(Q$(I * 5 + 88))
    Next I

    For I = 0 To 2
        statsA4$(I) = Q$(I * 3 + 104)
        careerA9%(I) = CVI(Q$(I * 3 + 105))
        careerB%(I) = CVI(Q$(I * 3 + 106))
    Next I

    For I = 0 To 2
        statsA5$(I) = Q$(I * 3 + 113)
        careerB1%(I) = CVI(Q$(I * 3 + 114))
        careerB2%(I) = CVI(Q$(I * 3 + 115))
    Next I

    statsA6$(0) = RTrim$(Q$(122))
    careerB3%(0) = CVI(Q$(123))
    statsA7$(0) = RTrim$(Q$(124))
    careerB4%(0) = CVI(Q$(125))
    careerB5%(0) = CVI(Q$(126))
    careerB7%(0) = CVI(Q$(127))
    careerB8%(0) = CVI(Q$(128))
    statsA7$(1) = RTrim$(Q$(129))
    careerB4%(1) = CVI(Q$(130))
    careerB5%(1) = CVI(Q$(131))
    careerB7%(1) = CVI(Q$(132))
    careerB8%(1) = CVI(Q$(133))
    YC! = CVI(Q$(134))

    For I = 0 To 14
        statsDS$(I) = Q$(I * 2 + 135)
        careerDS%(I) = CVI(Q$(I * 2 + 136))
    Next I

    For I = 0 To 9
        statsDI$(I) = Q$(I * 2 + 165)
        careerDI%(I) = CVI(Q$(I * 2 + 166))
    Next I

    Close #1

    If _FileExists(diskPaths$(0) + "STADIUMS." + YN$) Then

        Open diskPaths$(0) + "STADIUMS." + YN$ For Random As #2 Len = 50

        Field #2, 15 As X$, 30 As QQ$(1), 4 As QQ$(3), 1 As QQ$(4)

        Get #2, B8%

        S$ = RTrim$(QQ$(1))
        teamAttendance& = CVL(QQ$(3))
        Z$ = QQ$(4)

        Close #2

        If Z$ <> "x" Then S$ = "": teamAttendance& = 0

    Else

        S$ = ""
        teamAttendance& = 0

    End If

    For I = 0 To 15:
        C2(I) = 0: C3(I) = 0
    Next

    U = 0

    For I = 1 To 9
        If careerA0%(I) = 0 Then
            U = U + 1
        Else
            C3(I) = careerA0%(I) - careerA0%(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 9:
        careerA0%(I) = C3(I)
    Next

    For I = 0 To 5:
        careerA2%(I + 10) = careerA4%(I)
    Next

    U = 0

    For I = 1 To 15
        If careerA2%(I) = 0 Then
            U = U + 1
        Else
            C2(I) = careerA2%(I) - careerA2%(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 15:
        careerA2%(I) = C2(I)
    Next

    For I = 0 To 5:
        careerA4%(I) = careerA2%(I + 10)
    Next

    For I = 3 To 1 Step -1
        If careerA6%(I) <> 0 Then
            careerA6%(I) = careerA6%(I) - careerA6%(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If careerA9%(I) <> 0 Then
            careerA9%(I) = careerA9%(I) - careerA9%(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If careerB1%(I) <> 0 Then
            careerB1%(I) = careerB1%(I) - careerB1%(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If careerB5%(I) <> 0 Then
            careerB5%(I) = careerB5%(I) - careerB5%(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If careerB8%(I) <> 0 Then
            careerB8%(I) = careerB8%(I) - careerB8%(I - 1)
        End If
    Next

End Sub

