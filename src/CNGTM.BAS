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

            'Call GetTeam_CNGTM(A$, YN$, teamNbr, )
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then

                Select Case RN

                    Case 1
                        Call EditTeam(team$, YN$, teamNbr)

                    Case 2
                        Call PrintRoster(team$, teamNbr)

                    Case 3
                        Call ViewRoster(team$, teamNbr)

                End Select
                
            End If

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub PrintRoster (teamName$, teamIdx)

    Shared mascot$, stadium$
    
    Shared YC!
    
    Shared teamAttendance&

    Shared leagueRatings%(), teamRatings%()
    
    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Color 0, 15

    ' *** PRINT TEAM ROSTER ***
    LPrint "TEAM ROSTER FOR "; teamName; " "; mascot$; " TEAM NUMBER ="; teamIdx
    LPrint
    LPrint Using "DEF RUSH ADJ   = ###"; leagueRatings%(0);
    LPrint Tab(35); Using "SACKED %         = ##"; leagueRatings%(5)
    LPrint Using "DEF COMP % ADJ = ###"; leagueRatings%(1);
    LPrint Tab(35); Using "DEF SACK ADJ     = ##"; leagueRatings%(6)
    LPrint Using "DEF INT % ADJ  = ###"; leagueRatings%(2);
    LPrint Tab(35); Using "RUN/PASS %       = ##"; leagueRatings%(7)
    LPrint Using "OFF FUMBLE  %  = ###"; leagueRatings%(3);
    LPrint Tab(35); "DEF YDS/COMP ADJ = ";

    If YC! < 50 Then LPrint Using "+#.#"; YC! / 100 * 30
    If YC! > 50 Then LPrint Using "+#.#"; (100 - YC!) / 100 * -30

    LPrint Using "DEF FUMBLE  %  = ###"; leagueRatings%(4);
    LPrint Tab(35); Using "PENALTY ADJ      = ##"; teamRatings%(0)
    LPrint Using "STAD TYPE      = ###"; teamRatings%(3);
    LPrint Tab(35); Using "GEOGRAPH REGION  = ##"; teamRatings%(4)
    LPrint Using "BACKGRND COLOR = ###"; teamRatings%(1);
    LPrint Tab(35); Using "FOREGRND COLOR   = ##"; teamRatings%(2)
    LPrint
    LPrint "RUNNING BACKS-"
    LPrint "NAME-               RUSH AVG   CX%  REC AVG   CX%"

    For I = 0 To 9:

        If Left$(rbName$(I), 1) <> " " And Left$(rbName$(I), 3) <> "END" Then
            LPrint rbName$(I); Tab(23);
            LPrint Using "##.#     ###    ##     ###"; rushAvg(I) * .3; rushAtt(I); rbRecAvg(I) * 3; rbRecepts(I)
        End If

    Next

    LPrint
    LPrint "WIDE RECEIVERS-"
    LPrint "NAME-              REC AVG     CX%"

    For I = 0 To 5:

        If Left$(wrName$(I), 1) <> " " And Left$(wrName$(I), 3) <> "END" Then
            LPrint wrName$(I); Tab(23);
            LPrint Using "##       ###"; wrRecAvg(I) * 3; wrRecept(I)
        End If

    Next

    LPrint
    LPrint "QUARTERBACKS-                     YDS/CMP"
    LPrint "NAME-               COMP%   INT%    ADJ     CX%"

    For I = 0 To 3:

        If Left$(qbName$(I), 1) <> " " And Left$(qbName$(I), 3) <> "END" Then
            LPrint qbName$(I); Tab(21);
            LPrint Using "###     ##     ###      ###"; compPct(I); qbIntPct(I); armRating(I); passAtt(I)
        End If

    Next

    LPrint
    LPrint "KICKOFF RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:

        If Left$(krName$(I), 1) <> " " And Left$(krName$(I), 3) <> "END" Then
            LPrint krName$(I); Tab(23);
            LPrint Using "##       ###"; krRetAvg(I) * 3; krRet(I)
        End If

    Next

    LPrint
    LPrint "PUNT RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:

        If Left$(prName$(I), 1) <> " " And Left$(prName$(I), 3) <> "END" Then
            LPrint prName$(I); Tab(23);
            LPrint Using "##       ###"; prRetAvg(I) * 3; prRet(I)
        End If

    Next

    LPrint
    LPrint "PLACE KICKERS-"
    LPrint "NAME-                FG%  CX%   EP%  CX%"

    For I = 0 To 1:

        If Left$(pkName$(I), 1) <> " " And Left$(pkName$(I), 3) <> "END" Then
            LPrint pkName$(I); Tab(21);
            LPrint Using "###   ###   ###  ###"; pkFGA(I) * 5 + 25; pkFGPct(I); pkPAT(I); pkPATPct(I)
        End If

    Next

    LPrint
    LPrint "PUNTER-"
    LPrint "NAME-              AVG"
    LPrint punterName$(0); Tab(21);
    LPrint Using "##"; puntAvg(0) * 2 + 28

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
Sub EditTeam (teamName$, YN$, teamNbr)

    Shared mascot$, stadium$
    
    Shared YC!
    
    Shared teamAttendance&

    Shared rbName$(), wrName$(), qbName$(), krName$()
    Shared prName$(), punterName$(), pkName$()

    Shared intName$(), sackName$()
    Shared rushAvg(), rbRecAvg(), wrRecAvg(), compPct(), qbIntPct()
    Shared krRetAvg(), prRet(), prRetAvg(), puntAvg(), pkFGA()
    Shared pkFGPct(), leagueRatings%(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks()
    Shared teamRatings%(), armRating()

    Dim A1!(9), A3!(9), A5!(9)
    Dim B!(9), B2!(1), B3!(2), B4!(2)

    Do

        Color 15, 0
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
                Call ChangeTeamRatings(teamName$, YN$, teamNbr)

            Case 2

                Do

                    Cls
                    U$ = "EDIT RUNNING BACKS"
                    Locate 1, 40 - Len(U$) / 2
                    Color 14: Print U$
                    Print
                    Color 12
                    Print "    NAME"; Tab(20); "RUSH AVG  REC AVG"
                    Color 15

                    For I = 0 To 9:
                        Print Using "## "; I + 1;
                        Print " "; rbName$(I); Tab(20);
                        Print Using "##.#       ## ##"; rushAvg(I) * .3; rbRecAvg(I) * 3
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
                        Print "1. NAME "; rbName$(I1)
                        Print "2. RUSHING AVG ";: Print Using "##.#"; rushAvg(I1) * .3
                        Print "3. RECEIVING AVG ";: Print Using "##"; rbRecAvg(I1) * 3

                        Do

                            Do
                                Locate 7
                                Input "ENTER # TO CHANGE (0-RETURN TO RB SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 3

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; rbName$(I1)

                                Case 2
                                    Locate 4: Input "2. RUSHING AVG "; A1!(I1)
                                    rushAvg(I1) = Int(A1!(I1) / .3 + .5)

                                Case 3
                                    Locate 5: Input "3. RECEIVING AVG "; A3!(I1)
                                    rbRecAvg(I1) = Int(A3!(I1) / 3 + .5)

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
                    Print "    NAME"; Tab(20); "REC AVG"
                    Color 15

                    For I = 0 To 5:
                        Print Using "## "; I + 1;
                        Print " "; wrName$(I); Tab(20);
                        Print Using "   ##"; wrRecAvg(I) * 3
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
                        Print "1. NAME "; wrName$(I1)
                        Print "2. RECEIVING AVG ";: Print Using "##"; wrRecAvg(I1) * 3

                        Do

                            Do
                                Locate 6
                                Input "ENTER # TO CHANGE (0-RETURN TO WR SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 2

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; wrName$(I1)

                                Case 2
                                    Locate 4: Input "2. RECEIVING AVG "; A5!(I1): wrRecAvg(I1) = Int(A5!(I1) / 3 + .5)

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
                    Print "    NAME"; Tab(20); "COMP%   INT%   YDS/COMP ADJ"
                    Color 15

                    For I = 0 To 3:
                        Print Using "## "; I + 1;
                        Print " "; qbName$(I); Tab(20);
                        Print Using " ##      ##       +##"; compPct(I); qbIntPct(I); armRating(I)
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
                        Print "1. NAME "; qbName$(I1)
                        Print "2. COMPLETION % "; compPct(I1)
                        Print "3. INTERCEPTION % "; qbIntPct(I1)
                        Print "4. YDS/COMP ADJ "; armRating(I1)

                        Do

                            Do
                                Locate 8
                                Input "ENTER # TO CHANGE (0-RETURN TO QB SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 4

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; qbName$(I1)

                                Case 2
                                    Locate 4: Input "2. COMPLETION % "; compPct(I1)

                                Case 3
                                    Locate 5: Input "3. INTERCEPTION % "; qbIntPct(I1)

                                Case 4
                                    Locate 6: Input "4. YDS/COMP ADJ "; armRating(I1)

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
                    Print "    NAME"; Tab(20); "RET AVG"
                    Color 15

                    For I = 0 To 2:
                        Print Using "## "; I + 1;
                        Print " "; krName$(I); Tab(20);
                        Print Using "  ##"; krRetAvg(I) * 3
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
                        Print "1. NAME "; krName$(I1)
                        Print "2. RETURN AVG ";: Print Using "##"; krRetAvg(I1) * 3

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO KOR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; krName$(I1)

                            Case 2
                                Locate 4: Input "2. RETURN AVG "; B!(I1)
                                krRetAvg(I1) = Int(B!(I1) / 3 + .5)
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
                    Print "    NAME"; Tab(20); "RET AVG"
                    Color 15

                    For I = 0 To 2:
                        Print Using "## "; I + 1;
                        Print " "; prName$(I); Tab(20);
                        Print Using "  ##"; prRetAvg(I) * 3
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
                        Print "1. NAME "; prName$(I1)
                        Print "2. RETURN AVG ";: Print Using "##"; prRetAvg(I1) * 3

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO PR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; prName$(I1)

                            Case 2
                                Locate 4: Input "2. RETURN AVG "; B2!(I1): prRetAvg(I1) = Int(B2!(I1) / 3 + .5)

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
                    Print "    NAME"; Tab(20); "AVG"

                    For I = 0 To 0:
                        Print Using "## "; I + 1;
                        Print " "; punterName$(I); Tab(20);
                        Print Using "##"; puntAvg(I) * 2 + 28
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
                        Print "1. NAME "; punterName$(I1)
                        Print "2. PUNTING AVG ";: Print Using "##"; puntAvg(I1) * 2 + 28

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO PUNTER SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; punterName$(I1)

                            Case 2
                                Locate 4: Input "2. PUNTING AVG "; B3!(I1): puntAvg(I1) = Int(((B3!(I1) - 28) / 2) + .5)

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
                    Print "    NAME"; Tab(20); "FG%   EP%"
                    Color 15

                    For I = 0 To 1:
                        Print Using "## "; I + 1;
                        Print " "; pkName$(I); Tab(20);
                        Print Using "###    ###"; pkFGA(I) * 5 + 25; pkPAT(I)
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
                        Print "1. NAME "; pkName$(I1)
                        Print "2. FIELD GOAL % ";: Print pkFGA(I1) * 5 + 25
                        Print "3. EXTRA POINT % ";: Print pkPAT(I1)

                        Do
                            Locate 7
                            Input "ENTER # TO CHANGE (0-RETURN TO PK SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 3

                        Select Case C9
                            Case 1
                                Locate 3: Input "1. NAME "; pkName$(I1)

                            Case 2
                                Locate 4: Input "2. FIELD GOAL % "; B4!(I1)
                                pkFGA(I1) = Int((B4!(I1) / 5) + .5) - 5

                            Case 3
                                Locate 5: Input "3. EXTRA POINT % "; pkPAT(I1)

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
                Print "    NAME"; Tab(20); "SACKS"
                Color 15

                Locate 4
                TSR% = 0

                For I = 0 To 14:
                    Print Using "## "; I + 1;
                    Print " "; sackName$(I); Tab(20); numSacks(I)
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
                    Print "1. NAME "; sackName$(I1)
                    Print "2. SACKS "; numSacks(I1)

                    Do
                        Locate 6
                        Input "ENTER # TO CHANGE (0-RETURN TO SACKS SELECTION) "; C9
                    Loop Until C9 >= 0 And C9 <= 2

                    Select Case C9

                        Case 1
                            Locate 3: Input "1. NAME "; sackName$(I1)

                        Case 2
                            Locate 4: Input "2. SACKS "; numSacks(I1)

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
                    Print "    NAME"; Tab(20); "INTERCEPTIONS"
                    Color 15

                    Locate 4

                    For I = 0 To 9:
                        Print Using "## "; I + 1;
                        Print " "; intName$(I); Tab(27); numInts(I)
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
                        Print "1. NAME "; intName$(I1)
                        Print "2. INTERCEPTIONS "; numInts(I1)

                        Do
                            Locate 6
                            Input "ENTER # TO CHANGE (0-RETURN TO INT SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; intName$(I1)

                            Case 2
                                Locate 4: Input "2. INTERCEPTIONS "; numInts(I1)

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
            S0 = S0 + rushAtt(I)
            S2 = S2 + rbRecepts(I)
        Next

        S4 = 100 / S0

        For I = 0 To 9:
            If rushAtt(I) <> 0 Then rushAtt(I) = Int(S4 * rushAtt(I) + .5) + S1: S1 = rushAtt(I)
            If rushAtt(I) > 100 Then rushAtt(I) = 100
        Next I

        For I = 0 To 5:
            S2 = S2 + wrRecept(I)
        Next

        S5 = 100 / S2

        For I = 0 To 9:
            If rbRecepts(I) <> 0 Then rbRecepts(I) = Int(S5 * rbRecepts(I) + .5) + S3: S3 = rbRecepts(I)
            If rbRecepts(I) > 100 Then rbRecepts(I) = 100
        Next I

        For I = 0 To 5:
            If wrRecept(I) <> 0 Then wrRecept(I) = Int(S5 * wrRecept(I) + .5) + S3: S3 = wrRecept(I)
            If wrRecept(I) > 100 Then wrRecept(I) = 100
        Next I

        S6 = 0
        S7 = 0

        For I = 0 To 3:
            S6 = S6 + passAtt(I)
        Next

        S8 = 100 / S6

        For I = 0 To 3:
            If passAtt(I) <> 0 Then passAtt(I) = Int(S8 * passAtt(I) + .5) + S7: S7 = passAtt(I)
            If passAtt(I) > 100 Then passAtt(I) = 100
        Next

        G0 = 0
        G1 = 0
        G3 = 0
        G4 = 0

        For I = 0 To 2:
            G0 = G0 + krRet(I)
            G3 = G3 + prRet(I)
        Next

        G2 = 100 / G0
        G5 = 100 / G3

        For I = 0 To 2:
            If krRet(I) <> 0 Then krRet(I) = Int(G2 * krRet(I) + .5) + G1: G1 = krRet(I)
            If krRet(I) > 100 Then krRet(I) = 100
        Next

        For I = 0 To 2:
            If prRet(I) <> 0 Then prRet(I) = Int(G5 * prRet(I) + .5) + G4: G4 = prRet(I)
            If prRet(I) > 100 Then prRet(I) = 100
        Next

        G0 = 0
        G1 = 0
        G3 = 0
        G4 = 0

        For I = 0 To 1:
            G0 = G0 + pkFGPct(I)
            G3 = G3 + pkPATPct(I)
        Next

        G2 = 100 / G0
        G5 = 100 / G3

        For I = 0 To 1:
            If pkFGPct(I) <> 0 Then pkFGPct(I) = Int(G2 * pkFGPct(I) + .5) + G1: G1 = pkFGPct(I)
            If pkFGPct(I) > 100 Then pkFGPct(I) = 100
        Next

        For I = 0 To 1:
            If pkPATPct(I) <> 0 Then pkPATPct(I) = Int(G5 * pkPATPct(I) + .5) + G4: G4 = pkPATPct(I)
            If pkPATPct(I) > 100 Then pkPATPct(I) = 100
        Next

        For I = 9 To 0 Step -1:
            If rushAtt(I) <> 0 Then rushAtt(I) = 100: Exit For
        Next

        For I = 5 To 0 Step -1:
            If wrRecept(I) <> 0 Then wrRecept(I) = 100: Exit For
        Next

        For I = 3 To 0 Step -1:
            If passAtt(I) <> 0 Then passAtt(I) = 100: Exit For
        Next

        For I = 2 To 0 Step -1:
            If krRet(I) <> 0 Then krRet(I) = 100: Exit For
        Next

        For I = 2 To 0 Step -1:
            If prRet(I) <> 0 Then prRet(I) = 100: Exit For
        Next

        For I = 1 To 0 Step -1:
            If pkFGPct(I) <> 0 Then pkFGPct(I) = 100: Exit For
        Next

        For I = 1 To 0 Step -1:
            If pkPATPct(I) <> 0 Then pkPATPct(I) = 100: Exit For
        Next

        Call SaveChangedTeam(teamName$, YN$, teamNbr, YC%)

    End If


End Sub


'----------------------------------------
'       ChangeTeamRatings Subroutine
'----------------------------------------
'...explanation...
Sub ChangeTeamRatings (teamName$, YN$, teamNbr)

    Shared mascot$, stadium$
    
    Shared YC!
    
    Shared teamAttendance&

    Shared rbName$(), wrName$(), qbName$(), krName$()
    Shared prName$(), punterName$(), pkName$()

    Shared intName$(), sackName$()
    Shared rushAvg(), rbRecAvg(), wrRecAvg(), compPct(), qbIntPct()
    Shared krRetAvg(), prRet(), prRetAvg(), puntAvg(), pkFGA()
    Shared pkFGPct(), leagueRatings%(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks()
    Shared teamRatings%(), armRating()

    Cls

    U$ = "CHANGE TEAM RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    YC! = 10 / 3 * YC!

    Color 15
    Locate 2: Print " 1. TEAM NUMBER "; teamNbr
    Locate 3: Print " 2. DISK ID "; YN$
    Locate 4: Print " 3. TEAM NAME "; teamName$
    Locate 5: Print " 4. TEAM NICKNAME "; mascot$
    Locate 6: Print " 5. DEF RUSHING ADJ "; leagueRatings%(0)
    Locate 7: Print " 6. DEF COMPLETION % ADJ "; leagueRatings%(1)
    Locate 8: Print " 7. DEF INTERCEPTION % ADJ "; leagueRatings%(2)
    Locate 9: Print " 8. OFF FUMBLE % "; leagueRatings%(3)
    Locate 10: Print " 9. DEF FUMBLE % "; leagueRatings%(4)
    Locate 11: Print "10. OFF SACK ADJ % "; leagueRatings%(5)
    Locate 12: Print "11. DEF SACK ADJ % "; leagueRatings%(6)
    Locate 13: Print "12. DEF YDS/COMP ADJ ";
    If YC! < 50 Then Print Using "+#.#"; YC! / 100 * 30
    If YC! > 50 Then Print Using "+#.#"; (100 - YC!) / 100 * -30

    Locate 14: Print "13. TM PENALTY ADJ "; teamRatings%(0)
    Locate 15: Print "14. OFF RUN/PASS % "; leagueRatings%(7)
    Locate 16: Print "15. TM STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)
    Locate 17: Print "16. POWER INDEX "; teamRatings%(4)
    Locate 18: Print "17. TEXT COLOR (0-15) "; teamRatings%(2)
    Locate 19: Print "18. BACKGROUND COLOR (0-7) "; teamRatings%(1)
    Locate 20: Print "19. STADIUM NAME "; stadium$
    Locate 21: Print "20. AVERAGE ATTENDANCE "; teamAttendance&

    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
        Locate 22, 1
        Print "TEAM APPEARANCE: ";
        Color teamRatings%(2), teamRatings%(1): Print RTRim$(team$) + " " + RTrim$(mascot$)
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
                    Locate 2: Input " 1. TEAM NUMBER "; teamNbr

                Case 2
                    Locate 3: Input " 2. DISK ID "; YN$

                Case 3
                    Locate 4: Input " 3. TEAM NAME "; teamName$

                Case 4
                    Locate 5: Input " 4. TEAM NICKNAME "; mascot$

                Case 5
                    Locate 6: Input " 5. DEF RUSHING ADJ "; leagueRatings%(0)

                Case 6
                    Locate 7: Input " 6. DEF COMPLETION % ADJ "; leagueRatings%(1)

                Case 7
                    Locate 8: Input " 7. DEF INTERCEPTION % ADJ "; leagueRatings%(2)

                Case 8
                    Locate 9: Input " 8. OFF FUMBLE % "; leagueRatings%(3)

                Case 9
                    Locate 10: Input " 9. DEF FUMBLE % "; leagueRatings%(4)

                Case 10
                    Locate 11: Input "10. OFF SACK ADJ % "; leagueRatings%(5)

                Case 11
                    Locate 12: Input "11. DEF SACK ADJ % "; leagueRatings%(6)

                Case 12
                    Locate 13: Input "12. DEF YDS/COMP ADJ "; YC!
                    If YC! <= 0 Then YC! = 100 + (YC! * 10 / 3)

                Case 13
                    Locate 14: Input "13. TM PENALTY ADJ "; teamRatings%(0)

                Case 14
                    Locate 15: Input "14. OFF RUN/PASS % "; leagueRatings%(7)

                Case 15
                    Locate 16: Input "15. TM STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)

                Case 16
                    Locate 17: Input "16. POWER INDEX "; teamRatings%(4)

                Case 17
                    Locate 18: Input "17. TEXT COLOR (0-15) "; teamRatings%(2)
                    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                        Locate 22, 1
                        Print "TEAM APPEARANCE: ";
                        Color teamRatings%(2), teamRatings%(1): Print RTRim$(team$) + " " + RTrim$(mascot$)
                    End If
                    Color 15, 0

                Case 18
                    Locate 19: Input "18. BACKGROUND COLOR (0-7) "; teamRatings%(1)
                    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                        Locate 22, 1
                        Print "TEAM APPEARANCE: ";
                        Color teamRatings%(2), teamRatings%(1): Print RTRim$(team$) + " " + RTrim$(mascot$)
                    End If
                    Color 15, 0

                Case 19
                    Locate 20: Input "19. STADIUM NAME "; stadium$

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
Sub SaveChangedTeam (team$, YN$, teamNbr, YC%)

    Shared mascot$, stadium$
    
    Shared YC!
    
    Shared teamAttendance&

    Shared leagueRatings%(), teamRatings%()
    
    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    teamYear$ = RTrim$(YN$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

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

    LSet Q$(1) = team$
    LSet Q$(2) = mascot$

    For I = 0 To 7:
        LSet Q$(I + 3) = MKI$(leagueRatings%(I))
    Next I

    For I = 0 To 4:
        LSet Q$(I + 11) = MKI$(teamRatings%(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = rbName$(I)
        LSet Q$(I * 5 + 17) = MKI$(rushAtt(I))
        LSet Q$(I * 5 + 18) = MKI$(rushAvg(I))
        LSet Q$(I * 5 + 19) = MKI$(rbRecepts(I))
        LSet Q$(I * 5 + 20) = MKI$(rbRecAvg(I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = wrName$(I)
        LSet Q$(I * 3 + 67) = MKI$(wrRecept(I))
        LSet Q$(I * 3 + 68) = MKI$(wrRecAvg(I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = qbName$(I)
        LSet Q$(I * 5 + 85) = MKI$(passAtt(I))
        LSet Q$(I * 5 + 86) = MKI$(compPct(I))
        LSet Q$(I * 5 + 87) = MKI$(qbIntPct(I))
        LSet Q$(I * 5 + 88) = MKI$(armRating(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = krName$(I)
        LSet Q$(I * 3 + 105) = MKI$(krRet(I))
        LSet Q$(I * 3 + 106) = MKI$(krRetAvg(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = prName$(I)
        LSet Q$(I * 3 + 114) = MKI$(prRet(I))
        LSet Q$(I * 3 + 115) = MKI$(prRetAvg(I))
    Next I

    LSet Q$(122) = RTrim$(punterName$(0))
    LSet Q$(123) = MKI$(puntAvg(0))
    LSet Q$(124) = RTrim$(pkName$(0))
    LSet Q$(125) = MKI$(pkFGA(0))
    LSet Q$(126) = MKI$(pkFGPct(0))
    LSet Q$(127) = MKI$(pkPAT(0))
    LSet Q$(128) = MKI$(pkPATPct(0))
    LSet Q$(129) = RTrim$(pkName$(1))
    LSet Q$(130) = MKI$(pkFGA(1))
    LSet Q$(131) = MKI$(pkFGPct(1))
    LSet Q$(132) = MKI$(pkPAT(1))
    LSet Q$(133) = MKI$(pkPATPct(1))
    LSet Q$(134) = MKI$(YC%)

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = sackName$(I)
        LSet Q$(I * 2 + 136) = MKI$(numSacks(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = intName$(I)
        LSet Q$(I * 2 + 166) = MKI$(numInts(I))
    Next I

    Put #1, teamNbr

    Close #1

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = DATA_SIZE_BYTES

    Field #2, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)

    LSet Q$(1) = team$
    LSet Q$(2) = stadium$
    LSet Q$(3) = MKL$(teamAttendance&)
    LSet Q$(4) = "x"

    Put #2, teamNbr

    Close 2

    stadium$ = ""
    teamAttendance& = 0

    For I = 0 To 9
        rushAtt(I) = 0
        rushAvg(I) = 0
        rbRecepts(I) = 0
        rbRecAvg(I) = 0
        rbName$(I) = ""
    Next

    For I = 0 To 5
        wrRecept(I) = 0
        wrRecAvg(I) = 0
        wrName$(I) = ""
    Next

    For I = 0 To 3
        passAtt(I) = 0
        compPct(I) = 0
        qbIntPct(I) = 0
        armRating(I) = 0
        qbName$(I) = ""
    Next

    For I = 0 To 2
        krRet(I) = 0
        krRetAvg(I) = 0
        prRet(I) = 0
        prRetAvg(I) = 0
        krName$(I) = ""
        prName$(I) = ""
    Next

    punterName$(0) = ""

    For I = 0 To 1
        puntAvg(I) = 0
        pkFGA(I) = 0
        pkFGPct(I) = 0
        pkPAT(I) = 0
        pkPATPct(I) = 0
        pkName$(I) = ""
    Next

End Sub

'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub ViewRoster (team$, teamNbr)

    Shared mascot$, stadium$
    
    Shared YC!
    
    Shared teamAttendance&

    Shared leagueRatings%(), teamRatings%()
    
    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Do
        Cls
        Color 11
        Print "TEAM ROSTER FOR ";
        Color teamRatings%(2), teamRatings%(1): Print team$;

        Color 11, 0: Print "   TEAM NUMBER =";
        Color 14: Print teamNbr

        Print

        Color 15: Print Using "DEF RUSH ADJ   = ###"; leagueRatings%(0);
        Print Tab(35); Using "SACKED %         = ##"; leagueRatings%(5)
        Print Using "DEF COMP % ADJ = ###"; leagueRatings%(1);
        Print Tab(35); Using "DEF SACK ADJ     = ##"; leagueRatings%(6)
        Print Using "DEF INT % ADJ  = ###"; leagueRatings%(2);
        Print Tab(35); Using "RUN/PASS %       = ##"; leagueRatings%(7)
        Print Using "OFF FUMBLE  %  = ###"; leagueRatings%(3);
        Print Tab(35); "DEF YDS/COMP ADJ = ";

        If YC! < 50 Then Print Using "+#.#"; YC! / 100 * 30
        If YC! > 50 Then Print Using "+#.#"; (100 - YC!) / 100 * -30

        Print Using "DEF FUMBLE  %  = ###"; leagueRatings%(4);
        Print Tab(35); Using "PENALTY ADJ      = ##"; teamRatings%(0)
        Print Using "STAD TYPE      = ###"; teamRatings%(3)
        Print Using "BACKGRND COLOR = ###"; teamRatings%(1);
        Print Tab(35); Using "FOREGRND COLOR   = ##"; teamRatings%(2)
        Print
        Color 12: Print "RUNNING BACKS-"
        Color 14: Print "NAME-               RUSH AVG   CX%  REC AVG   CX%"

        For I = 0 To 9:
            If Left$(rbName$(I), 1) <> " " And Left$(rbName$(I), 3) <> "END" Then
                Color 3: Print rbName$(I);
                Color 15
                Locate , 23: Print Using "##.#     ###    ##     ###"; rushAvg(I) * .3; rushAtt(I); rbRecAvg(I) * 3; rbRecepts(I)
            End If
        Next

        Locate 23
        Color 15: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

        Cls
        Color 12: Print "WIDE RECEIVERS-"
        Color 14: Print "NAME-              REC AVG     CX%"

        For I = 0 To 5:
            If Left$(wrName$(I), 1) <> " " And Left$(wrName$(I), 3) <> "END" Then
                Color 3: Print wrName$(I);
                Color 15: Locate , 23: Print Using "##       ###"; wrRecAvg(I) * 3; wrRecept(I)
            End If
        Next

        Print
        Color 12: Print "QUARTERBACKS-                     YDS/CMP"
        Color 14: Print "NAME-               COMP%   INT%    ADJ     CX%"
        For I = 0 To 3:
            If Left$(qbName$(I), 1) <> " " And Left$(qbName$(I), 3) <> "END" Then
                Color 3: Print qbName$(I);
                Color 15: Locate , 21: Print Using "###     ##     ###      ###"; compPct(I); qbIntPct(I); armRating(I); passAtt(I)
            End If
        Next

        Print
        Color 12: Print "KICKOFF RETURNMEN-"
        Color 14: Print "NAME-              RET AVG     CX%"

        For I = 0 To 2:
            If Left$(krName$(I), 1) <> " " And Left$(krName$(I), 3) <> "END" Then
                Color 3: Print krName$(I);
                Color 15: Locate , 23: Print Using "##       ###"; krRetAvg(I) * 3; krRet(I)
            End If
        Next

        Locate 21: Color 15: Print "HIT ANY KEY TO CONTINUE"

        I$ = GetKeyPress$
        Cls
        Color 12: Print "PUNT RETURNMEN-"
        Color 14: Print "NAME-              RET AVG     CX%"

        For I = 0 To 2:
            If Left$(prName$(I), 1) <> " " And Left$(prName$(I), 3) <> "END" Then
                Color 3: Print prName$(I);
                Color 15: Locate , 23: Print Using "##       ###"; prRetAvg(I) * 3; prRet(I)
            End If
        Next

        Print
        Color 12: Print "PLACE KICKERS-"
        Color 14: Print "NAME-                FG%  CX%   EP%  CX%"

        For I = 0 To 1:
            If Left$(pkName$(I), 1) <> " " And Left$(pkName$(I), 3) <> "END" Then
                Color 3: Print pkName$(I);
                Color 15: Locate , 21: Print Using "###   ###   ###  ###"; pkFGA(I) * 5 + 25; pkFGPct(I); pkPAT(I); pkPATPct(I)
            End If
        Next

        Print
        Color 12: Print "PUNTER-"
        Color 14: Print "NAME-              AVG"
        Color 3: Print punterName$(0);
        Color 15
        Locate , 21: Print Using "##"; puntAvg(0) * 2 + 28

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

    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

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
            Print " "; rbName$(I); Tab(20);
            Print Using "     ###"; rushAtt(I)
            BC% = BC% + rushAtt(I)
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
                Locate I1 + 4, 24: Input rushAtt(I1)
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
            Print " "; rbName$(I); Tab(20);: Print Using "      ##"; rbRecepts(I)
            BC% = BC% + rbRecepts(I)
        Next

        For I = 0 To 5
            Print Using "## "; I + 11;
            Print " "; wrName$(I); Tab(20);: Print Using "      ##"; wrRecept(I)
            BC% = BC% + wrRecept(I)
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
                    Input rbRecepts(I1)
                Else
                    Input wrRecept(I1 - 10)
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
            Print " "; qbName$(I); Tab(20);
            Print Using "     ###"; passAtt(I)
            BC% = BC% + passAtt(I)
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
                Input passAtt(I1)
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
            Print " "; krName$(I); Tab(20);
            Print Using "     ###"; krRet(I)
            BC% = BC% + krRet(I)
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
                Input krRet(I1)

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
            Print " "; prName$(I); Tab(20);
            Print Using "     ###"; prRet(I)
            BC% = BC% + prRet(I)
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
                Input prRet(I1)

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
            Print " "; pkName$(I); Tab(20);
            Print Using "     ###"; pkFGPct(I)
            BC% = BC% + pkFGPct(I)
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
                Input pkFGPct(I1)

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
            Print " "; pkName$(I); Tab(20);
            Print Using "     ###"; pkPATPct(I): BC% = BC% + pkPATPct(I)
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
                Input pkPATPct(I1)

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
