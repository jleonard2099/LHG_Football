'----------------------------------------
'            CNGTM Subroutine
'----------------------------------------
'This subroutine presents a menu of all
'the options related to team rosters
Sub CNGTM ()

    _Title "3-in-1 Football - Change Team"

    ReDim menuChoices$(1 To 4)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT AN EXISTING TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER  "
        menuChoices$(3) = "VIEW A TEAM ROSTER   "
        menuChoices$(4) = "EXIT TO MAIN MENU    "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        'In this case, RN is actually used later
        'to control flow / operations
        'It should probably be broken into
        'different functions based on value
        RN = Selection%

        If ExitCode% <> 27 Then

            teamNbr = 0
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
'         PrintRoster Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team roster to the user's display
Sub PrintRoster (teamName$, teamIdx)

    Shared mascot$, stadium$

    Shared YC!

    Shared teamAttendance&

    Shared leagueRatings%(), teamRatings%()

    Shared rushContrib(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared recContrib(), wrRecAvg()
    Shared passContrib(), compPct(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGContrib(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Color 0, 15

    ' *** PRINT TEAM ROSTER ***
    LPrint "TEAM ROSTER FOR "; teamName$; " "; mascot$; " TEAM NUMBER ="; teamIdx
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
            LPrint Using "##.#     ###    ##     ###"; rushAvg(I) * .3; rushContrib(I); rbRecAvg(I) * 3; rbRecepts(I)
        End If

    Next

    LPrint
    LPrint "WIDE RECEIVERS-"
    LPrint "NAME-              REC AVG     CX%"

    For I = 0 To 5:

        If Left$(wrName$(I), 1) <> " " And Left$(wrName$(I), 3) <> "END" Then
            LPrint wrName$(I); Tab(23);
            LPrint Using "##       ###"; wrRecAvg(I) * 3; recContrib(I)
        End If

    Next

    LPrint
    LPrint "QUARTERBACKS-                     YDS/CMP"
    LPrint "NAME-               COMP%   INT%    ADJ     CX%"

    For I = 0 To 3:

        If Left$(qbName$(I), 1) <> " " And Left$(qbName$(I), 3) <> "END" Then
            LPrint qbName$(I); Tab(21);
            LPrint Using "###     ##     ###      ###"; compPct(I); qbIntPct(I); armRating(I); passContrib(I)
        End If

    Next

    LPrint
    LPrint "KICKOFF RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:

        If Left$(krName$(I), 1) <> " " And Left$(krName$(I), 3) <> "END" Then
            LPrint krName$(I); Tab(23);
            LPrint Using "##       ###"; krRetAvg(I) * 3; krContrib(I)
        End If

    Next

    LPrint
    LPrint "PUNT RETURNMEN-"
    LPrint "NAME-              RET AVG     CX%"

    For I = 0 To 2:

        If Left$(prName$(I), 1) <> " " And Left$(prName$(I), 3) <> "END" Then
            LPrint prName$(I); Tab(23);
            LPrint Using "##       ###"; prRetAvg(I) * 3; prContrib(I)
        End If

    Next

    LPrint
    LPrint "PLACE KICKERS-"
    LPrint "NAME-                FG%  CX%   EP%  CX%"

    For I = 0 To 1:

        If Left$(pkName$(I), 1) <> " " And Left$(pkName$(I), 3) <> "END" Then
            LPrint pkName$(I); Tab(21);
            LPrint Using "###   ###   ###  ###"; pkFGA(I) * 5 + 25; pkFGContrib(I); pkPAT(I); pkPATContrib(I)
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
'This subroutine steps the user through
'screens for adjusting team data and players
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

    Shared rushContrib(), rushAvg(), rbRecAvg(), rbRecepts()
    Shared wrRecAvg(), recContrib()
    Shared armRating(), compPct(), passContrib(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg(), puntAvg(), pkFGA()
    Shared pkFGContrib(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks()
    Shared leagueRatings%(), teamRatings%()

    Dim A1!(9), A3!(9), A5!(9)
    Dim B!(9), B2!(1), B3!(2), B4!(2)
    Dim contrib2(0 To 15), contrib3(0 To 15)

    'Calculate contribution %s for each individual
    '-- this should technically be done each time
    '-- this is called because of the Dim ????
    'Erase contrib2, contrib3

    U = 0

    For I = 1 To 9
        If rushContrib(I) = 0 Then
            U = U + 1
        Else
            contrib3(I) = rushContrib(I) - rushContrib(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 9:
        rushContrib(I) = contrib3(I)
    Next

    For I = 0 To 5:
        rbRecepts(I + 10) = recContrib(I)
    Next

    U = 0

    For I = 1 To 15
        If rbRecepts(I) = 0 Then
            U = U + 1
        Else
            contrib2(I) = rbRecepts(I) - rbRecepts(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 15:
        rbRecepts(I) = contrib2(I)
    Next

    For I = 0 To 5:
        recContrib(I) = rbRecepts(I + 10)
    Next

    For I = 3 To 1 Step -1
        If passContrib(I) <> 0 Then
            passContrib(I) = passContrib(I) - passContrib(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If krContrib(I) <> 0 Then
            krContrib(I) = krContrib(I) - krContrib(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If prContrib(I) <> 0 Then
            prContrib(I) = prContrib(I) - prContrib(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If pkFGContrib(I) <> 0 Then
            pkFGContrib(I) = pkFGContrib(I) - pkFGContrib(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If pkPATContrib(I) <> 0 Then
            pkPATContrib(I) = pkPATContrib(I) - pkPATContrib(I - 1)
        End If
    Next

    'Present menu
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
                        Locate 15, 1
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
                        Print "1. NAME   "; rbName$(I1)
                        Print "2. RUSHING AVG  ";: Print Using "##.#"; rushAvg(I1) * .3
                        Print "3. RECEIVING AVG  ";: Print Using "##"; rbRecAvg(I1) * 3

                        Do

                            Do
                                Locate 7, 1
                                Input "ENTER # TO CHANGE (0-RETURN TO RB SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 3

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; rbName$(I1)
                                    Locate 3, 8: Print " "

                                Case 2
                                    Locate 4, 15: Print Space$(10)
                                    Locate 4: Input "2. RUSHING AVG "; A1!(I1)
                                    Locate 4, 15: Print " "

                                    rushAvg(I1) = Int(A1!(I1) / .3 + .5)

                                Case 3
                                    Locate 5, 17: Print Space$(10)
                                    Locate 5: Input "3. RECEIVING AVG "; A3!(I1)
                                    Locate 5, 17: Print " "

                                    rbRecAvg(I1) = Int(A3!(I1) / 3 + .5)

                            End Select

                            Locate 7, 46: Print Space$(10)

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
                        Locate 11, 1
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU)"; B9
                    Loop Until B9 >= 0 And B9 <= 6

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls
                        U$ = "WIDE RECEIVER RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME   "; wrName$(I1)
                        Print "2. RECEIVING AVG  ";: Print Using "##"; wrRecAvg(I1) * 3

                        Do

                            Do
                                Locate 6, 1
                                Input "ENTER # TO CHANGE (0-RETURN TO WR SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 2

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; wrName$(I1)
                                    Locate 3, 8: Print " "

                                Case 2
                                    Locate 4, 17: Print Space$(10)
                                    Locate 4: Input "2. RECEIVING AVG "; A5!(I1): wrRecAvg(I1) = Int(A5!(I1) / 3 + .5)
                                    Locate 4, 17: Print " "

                            End Select


                        Loop Until C9 = 0

                        Locate 6, 46: Print Space$(10)

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
                        Locate 9, 1
                        Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU)"; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    If B9 <> 0 Then

                        I1 = B9 - 1

                        Cls

                        U$ = "QUARTERBACK RATINGS"
                        Locate 1, 40 - Len(U$) / 2
                        Color 14: Print U$
                        Print
                        Color 15
                        Print "1. NAME  "; qbName$(I1)
                        Print "2. COMPLETION %  "; compPct(I1)
                        Print "3. INTERCEPTION %  "; qbIntPct(I1)
                        Print "4. YDS/COMP ADJ  "; armRating(I1)

                        Do

                            Do
                                Locate 8, 1
                                Input "ENTER # TO CHANGE (0-RETURN TO QB SELECTION) "; C9
                            Loop Until C9 >= 0 And C9 <= 4

                            Select Case C9

                                Case 1
                                    Locate 3: Input "1. NAME "; qbName$(I1)
                                    Locate 3, 8: Print " "

                                Case 2
                                    Locate 4, 16: Print Space$(10)
                                    Locate 4: Input "2. COMPLETION % "; compPct(I1)
                                    Locate 4, 16: Print " "

                                Case 3
                                    Locate 5, 18: Print Space$(10)
                                    Locate 5: Input "3. INTERCEPTION % "; qbIntPct(I1)
                                    Locate 5, 18: Print " "

                                Case 4
                                    Locate 6, 16: Print Space$(10)
                                    Locate 6: Input "4. YDS/COMP ADJ "; armRating(I1)
                                    Locate 6, 16: Print " "

                            End Select

                            Locate 8, 46: Print Space$(10)

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
                        Print "1. NAME   "; krName$(I1)
                        Print "2. RETURN AVG  ";: Print Using "##"; krRetAvg(I1) * 3

                        Do
                            Locate 6, 1
                            Input "ENTER # TO CHANGE (0-RETURN TO KOR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; krName$(I1)
                                Locate 3, 8: Print " "

                            Case 2
                                Locate 4, 14: Print Space$(10)
                                Locate 4: Input "2. RETURN AVG "; B!(I1)
                                Locate 4, 14: Print " "

                                krRetAvg(I1) = Int(B!(I1) / 3 + .5)

                        End Select

                        Locate 6, 47: Print Space$(10)

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
                        Print "1. NAME   "; prName$(I1)
                        Print "2. RETURN AVG  ";: Print Using "##"; prRetAvg(I1) * 3

                        Do
                            Locate 6, 1
                            Input "ENTER # TO CHANGE (0-RETURN TO PR SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; prName$(I1)
                                Locate 3, 8: Print " "

                            Case 2
                                Locate 4, 14: Print Space$(10)
                                Locate 4: Input "2. RETURN AVG "; B2!(I1)
                                Locate 4, 14: Print " "

                                prRetAvg(I1) = Int(B2!(I1) / 3 + .5)

                        End Select

                        Locate 6, 46: Print Space$(10)

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
                        Locate 6, 1
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
                        Print "1. NAME   "; punterName$(I1)
                        Print "2. PUNTING AVG  ";: Print Using "##"; puntAvg(I1) * 2 + 28

                        Do
                            Locate 6, 1
                            Input "ENTER # TO CHANGE (0-RETURN TO PUNTER SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; punterName$(I1)
                                Locate 3, 8: Print " "

                            Case 2
                                Locate 4, 15: Print Space$(10)
                                Locate 4: Input "2. PUNTING AVG "; B3!(I1)
                                Locate 4, 15: Print " "
                                
                                puntAvg(I1) = Int(((B3!(I1) - 28) / 2) + .5)

                        End Select

                        Locate 6, 50: Print Space$(10)

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
                        Locate 7, 1
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
                        Print "1. NAME   "; pkName$(I1)
                        Print "2. FIELD GOAL %  ";: Print pkFGA(I1) * 5 + 25
                        Print "3. EXTRA POINT %  ";: Print pkPAT(I1)

                        Do
                            Locate 7, 1
                            Input "ENTER # TO CHANGE (0-RETURN TO PK SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 3

                        Select Case C9
                            Case 1
                                Locate 3: Input "1. NAME "; pkName$(I1)
                                Locate 3, 8: Print " "

                            Case 2
                                Locate 4, 16: Print Space$(10)
                                Locate 4: Input "2. FIELD GOAL % "; B4!(I1)
                                Locate 4, 16: Print " "

                                pkFGA(I1) = Int((B4!(I1) / 5) + .5) - 5

                            Case 3
                                Locate 5, 16: Print Space$(10)
                                Locate 5: Input "3. EXTRA POINT % "; pkPAT(I1)
                                Locate 5, 16: Print " "

                        End Select

                        Locate 7, 46: Print Space$(10)

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
                    Print "1. NAME   "; sackName$(I1)
                    Print "2. SACKS  "; numSacks(I1)

                    Do
                        Locate 6, 1
                        Input "ENTER # TO CHANGE (0-RETURN TO SACKS SELECTION) "; C9
                    Loop Until C9 >= 0 And C9 <= 2

                    Select Case C9

                        Case 1
                            Locate 3: Input "1. NAME "; sackName$(I1)
                            Locate 3, 8: Print " "

                        Case 2
                            Locate 4, 9: Print Space$(10)
                            Locate 4: Input "2. SACKS "; numSacks(I1)
                            Locate 4, 9: Print " "

                    End Select

                    Locate 6, 49: Print Space$(10)

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
                        Locate 15, 1
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
                        Print "1. NAME   "; intName$(I1)
                        Print "2. INTERCEPTIONS  "; numInts(I1)

                        Do
                            Locate 6, 1
                            Input "ENTER # TO CHANGE (0-RETURN TO INT SELECTION) "; C9
                        Loop Until C9 >= 0 And C9 <= 2

                        Select Case C9

                            Case 1
                                Locate 3: Input "1. NAME "; intName$(I1)
                                Locate 3, 8: Print " "

                            Case 2
                                Locate 4, 17: Print Space$(10)
                                Locate 4: Input "2. INTERCEPTIONS "; numInts(I1)
                                Locate 4, 17: Print " "

                        End Select

                        Locate 6, 47: Print Space$(10)

                    End If

                Loop Until B9 = 0

            Case 11
                Cls
                Print "SAVE CHANGES? (Y/N)";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End Select

    Loop Until choice = 11 Or UCase$(prompt$) = "S"

    If (choice = 11 And UCase$(I$) = "Y") Or UCase$(prompt$) = "S" Then

        Call CheckContributions

        '-- Why do we make this converstion and lose accuracy?
        '-- We lose accuracy here!!!
        '-- To change it means updating routines for team files,
        '-- and updating every team file to accomodate
        YC% = YC!

        'Convert individual contribution %'s
        'Back to the scaled / cumulative #'s
        S0 = 0
        S1 = 0
        S2 = 0
        S3 = 0

        For I = 0 To 9:
            S0 = S0 + rushContrib(I)
            S2 = S2 + rbRecepts(I)
        Next

        S4 = 100 / S0

        For I = 0 To 9:

            If rushContrib(I) <> 0 Then 
                rushContrib(I) = Int(S4 * rushContrib(I) + .5) + S1
                S1 = rushContrib(I)
            End If

            If rushContrib(I) > 100 Then 
                rushContrib(I) = 100
            End If

        Next I

        For I = 0 To 5:
            S2 = S2 + recContrib(I)
        Next

        S5 = 100 / S2

        For I = 0 To 9:

            If rbRecepts(I) <> 0 Then 
                rbRecepts(I) = Int(S5 * rbRecepts(I) + .5) + S3
                S3 = rbRecepts(I)
            End If

            If rbRecepts(I) > 100 Then 
                rbRecepts(I) = 100
            End If

        Next I

        For I = 0 To 5:

            If recContrib(I) <> 0 Then 
                recContrib(I) = Int(S5 * recContrib(I) + .5) + S3
                S3 = recContrib(I)
            End If

            If recContrib(I) > 100 Then 
                recContrib(I) = 100
            End If

        Next I

        S6 = 0
        S7 = 0

        For I = 0 To 3:
            S6 = S6 + passContrib(I)
        Next

        S8 = 100 / S6

        For I = 0 To 3:

            If passContrib(I) <> 0 Then 
                passContrib(I) = Int(S8 * passContrib(I) + .5) + S7
                S7 = passContrib(I)
            End If

            If passContrib(I) > 100 Then 
                passContrib(I) = 100
            End If

        Next

        G0 = 0
        G1 = 0
        G3 = 0
        G4 = 0

        For I = 0 To 2:
            G0 = G0 + krContrib(I)
            G3 = G3 + prContrib(I)
        Next

        G2 = 100 / G0
        G5 = 100 / G3

        For I = 0 To 2:

            If krContrib(I) <> 0 Then 
                krContrib(I) = Int(G2 * krContrib(I) + .5) + G1
                G1 = krContrib(I)
            End If

            If krContrib(I) > 100 Then 
                krContrib(I) = 100
            End If

        Next

        For I = 0 To 2:

            If prContrib(I) <> 0 Then 
                prContrib(I) = Int(G5 * prContrib(I) + .5) + G4
                G4 = prContrib(I)
            End If

            If prContrib(I) > 100 Then 
                prContrib(I) = 100
            End If

        Next

        G0 = 0
        G1 = 0
        G3 = 0
        G4 = 0

        For I = 0 To 1:
            G0 = G0 + pkFGContrib(I)
            G3 = G3 + pkPATContrib(I)
        Next

        G2 = 100 / G0
        G5 = 100 / G3

        For I = 0 To 1:

            If pkFGContrib(I) <> 0 Then 
                pkFGContrib(I) = Int(G2 * pkFGContrib(I) + .5) + G1
                G1 = pkFGContrib(I)
            End If

            If pkFGContrib(I) > 100 Then 
                pkFGContrib(I) = 100
            End If

        Next

        For I = 0 To 1:

            If pkPATContrib(I) <> 0 Then 
                pkPATContrib(I) = Int(G5 * pkPATContrib(I) + .5) + G4
                G4 = pkPATContrib(I)
            End If

            If pkPATContrib(I) > 100 Then 
                pkPATContrib(I) = 100
            End If

        Next

        For I = 9 To 0 Step -1:
            If rushContrib(I) <> 0 Then 
                rushContrib(I) = 100: Exit For
            End If
        Next

        For I = 5 To 0 Step -1:
            If recContrib(I) <> 0 Then 
                recContrib(I) = 100: Exit For
            End If
        Next

        For I = 3 To 0 Step -1:
            If passContrib(I) <> 0 Then 
                passContrib(I) = 100: Exit For
            End If
        Next

        For I = 2 To 0 Step -1:
            If krContrib(I) <> 0 Then 
                krContrib(I) = 100: Exit For
            End If
        Next

        For I = 2 To 0 Step -1:
            If prContrib(I) <> 0 Then 
                prContrib(I) = 100: Exit For
            End If
        Next

        For I = 1 To 0 Step -1:
            If pkFGContrib(I) <> 0 Then 
                pkFGContrib(I) = 100: Exit For
            End If
        Next

        For I = 1 To 0 Step -1:
            If pkPATContrib(I) <> 0 Then 
                pkPATContrib(I) = 100: Exit For
            End If
        Next

        Call SaveChangedTeam(teamName$, YN$, teamNbr, YC%)

    End If


End Sub


'----------------------------------------
'       ChangeTeamRatings Subroutine
'----------------------------------------
'This subroutine is focused on walking the
'user through modifying team ratings
Sub ChangeTeamRatings (teamName$, teamYear$, teamNbr)

    Shared mascot$, stadium$

    Shared YC!

    Shared teamAttendance&

    Shared rbName$(), wrName$(), qbName$(), krName$()
    Shared prName$(), punterName$(), pkName$()

    Shared intName$(), sackName$()
    Shared rushAvg(), rbRecAvg(), wrRecAvg(), compPct(), qbIntPct()
    Shared krRetAvg(), prContrib(), prRetAvg(), puntAvg(), pkFGA()
    Shared pkFGContrib(), leagueRatings%(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks()
    Shared teamRatings%(), armRating()

    Cls

    U$ = "CHANGE TEAM RATINGS"
    Locate 1, 40 - Len(U$) / 2
    Color 14: Print U$
    YC! = 10 / 3 * YC!

    Color 15
    Locate 2: Print " 1. TEAM NUMBER  "; teamNbr
    Locate 3: Print " 2. DISK ID   "; teamYear$
    Locate 4: Print " 3. TEAM NAME   "; teamName$
    Locate 5: Print " 4. TEAM NICKNAME   "; mascot$
    Locate 6: Print " 5. DEF RUSHING ADJ  "; leagueRatings%(0)
    Locate 7: Print " 6. DEF COMPLETION % ADJ  "; leagueRatings%(1)
    Locate 8: Print " 7. DEF INTERCEPTION % ADJ  "; leagueRatings%(2)
    Locate 9: Print " 8. OFF FUMBLE %  "; leagueRatings%(3)
    Locate 10: Print " 9. DEF FUMBLE %  "; leagueRatings%(4)
    Locate 11: Print "10. OFF SACK ADJ %  "; leagueRatings%(5)
    Locate 12: Print "11. DEF SACK ADJ %  "; leagueRatings%(6)
    Locate 13: Print "12. DEF YDS/COMP ADJ  ";
    If YC! < 50 Then Print Using "+#.#"; YC! / 100 * 30
    If YC! > 50 Then Print Using "+#.#"; (100 - YC!) / 100 * -30

    Locate 14: Print "13. TM PENALTY ADJ  "; teamRatings%(0)
    Locate 15: Print "14. OFF RUN/PASS %  "; leagueRatings%(7)
    Locate 16: Print "15. TM STADIUM (0-OUTDOOR 1-DOME)  "; teamRatings%(3)
    Locate 17: Print "16. POWER INDEX  "; teamRatings%(4)
    Locate 18: Print "17. TEXT COLOR (0-15)  "; teamRatings%(2)
    Locate 19: Print "18. BACKGROUND COLOR (0-7)  "; teamRatings%(1)
    Locate 20: Print "19. STADIUM NAME   "; stadium$
    Locate 21: Print "20. AVERAGE ATTENDANCE  "; teamAttendance&

    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
        Locate 22, 1
        Print "TEAM APPEARANCE: ";
        Color teamRatings%(2), teamRatings%(1): Print RTrim$(teamName$) + " " + RTrim$(mascot$)
    End If

    Color 15, 0

    Do

        Do
            Locate 23, 15
            Input "ENTER # TO CHANGE (0-RETURN TO POSITION MENU) "; B9
        Loop Until B9 >= 0 And B9 <= 20

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 2, 17: Print Space$(10)
                    Locate 2, 1: Input " 1. TEAM NUMBER "; teamNbr
                    Locate 2, 17: Print " "

                Case 2
                    Locate 3, 1: Input " 2. DISK ID "; teamYear$
                    Locate 3, 13: Print " "

                Case 3
                    Locate 4, 1: Input " 3. TEAM NAME "; teamName$
                    Locate 4, 15: Print " "

                Case 4
                    Locate 5, 1: Input " 4. TEAM NICKNAME "; mascot$
                    Locate 5, 19: Print " "

                Case 5
                    Locate 6, 21: Print Space$(10)
                    Locate 6, 1: Input " 5. DEF RUSHING ADJ "; leagueRatings%(0)
                    Locate 6, 21: Print " "

                Case 6
                    Locate 7, 26: Print Space$(10)
                    Locate 7, 1: Input " 6. DEF COMPLETION % ADJ "; leagueRatings%(1)
                    Locate 7, 26: Print " "

                Case 7
                    Locate 8, 28: Print Space$(10)
                    Locate 8, 1: Input " 7. DEF INTERCEPTION % ADJ "; leagueRatings%(2)
                    Locate 8, 28: Print " "

                Case 8
                    Locate 9, 18: Print Space$(10)
                    Locate 9, 1: Input " 8. OFF FUMBLE % "; leagueRatings%(3)
                    Locate 9, 18: Print " "

                Case 9
                    Locate 10, 18: Print Space$(10)
                    Locate 10, 1: Input " 9. DEF FUMBLE % "; leagueRatings%(4)
                    Locate 10, 18: Print " "

                Case 10
                    Locate 11, 20: Print Space$(10)
                    Locate 11, 1: Input "10. OFF SACK ADJ % "; leagueRatings%(5)
                    Locate 11, 20: Print " "

                Case 11
                    Locate 12, 20: Print Space$(10)
                    Locate 12, 1: Input "11. DEF SACK ADJ % "; leagueRatings%(6)
                    Locate 12, 20: Print " "

                Case 12
                    Locate 13, 22: Print Space$(10)
                    Locate 13, 1: Input "12. DEF YDS/COMP ADJ "; YC!
                    Locate 13, 22: Print " "

                    If YC! <= 0 Then YC! = 100 + (YC! * 10 / 3)

                Case 13
                    Locate 14, 20: Print Space$(10)
                    Locate 14, 1: Input "13. TM PENALTY ADJ "; teamRatings%(0)
                    Locate 14, 20: Print " "

                Case 14
                    Locate 15, 20: Print Space$(10)
                    Locate 15, 1: Input "14. OFF RUN/PASS % "; leagueRatings%(7)
                    Locate 15, 20: Print " "

                Case 15
                    Locate 16, 35: Print Space$(10)
                    Locate 16, 1: Input "15. TM STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)
                    Locate 16, 35: Print " "

                Case 16
                    Locate 17, 17: Print Space$(10)
                    Locate 17, 1: Input "16. POWER INDEX "; teamRatings%(4)
                    Locate 17, 17: Print " "

                Case 17
                    Locate 18, 23: Print Space$(10)
                    Locate 18, 1: Input "17. TEXT COLOR (0-15) "; teamRatings%(2)
                    Locate 18, 23: Print " "

                    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                        Locate 22, 1
                        Print "TEAM APPEARANCE: ";
                        Color teamRatings%(2), teamRatings%(1): Print RTrim$(teamName$) + " " + RTrim$(mascot$)
                        Color 15, 0
                    End If

                Case 18
                    Locate 20, 28: Print Space$(10)
                    Locate 19, 1: Input "18. BACKGROUND COLOR (0-7) "; teamRatings%(1)
                    Locate 19, 28: Print " "

                    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                        Locate 22, 1
                        Print "TEAM APPEARANCE: ";
                        Color teamRatings%(2), teamRatings%(1): Print RTrim$(teamName$) + " " + RTrim$(mascot$)
                        Color 15, 0
                    End If

                Case 19
                    Locate 20, 18: Print Space$(10)
                    Locate 20, 1: Input "19. STADIUM NAME "; stadium$
                    Locate 20, 18: Print " "

                Case 20
                    Locate 21, 24: Print Space$(10)
                    Locate 21, 1: Input "20. AVERAGE ATTENDANCE "; teamAttendance&
                    Locate 21, 24: Print " "

            End Select

        End If

        Locate 23, 61
        Print Space$(10)

    Loop Until B9 = 0

    Cls

End Sub


'----------------------------------------
'         ViewRoster Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team roster to the user's display
Sub ViewRoster (team$, teamNbr)

    Shared mascot$, stadium$

    Shared YC!

    Shared teamAttendance&

    Shared leagueRatings%(), teamRatings%()

    Shared rushContrib(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared recContrib(), wrRecAvg()
    Shared passContrib(), compPct(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGContrib(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Do
        Color 15, 0
        Cls

        Color 11
        Print "TEAM ROSTER FOR ";
        Color teamRatings%(2), teamRatings%(1)
        Print team$;

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
                Color 3
                Print rbName$(I);
                Color 15
                Locate , 23
                Print Using "##.#     ###    ##     ###"; rushAvg(I) * .3; rushContrib(I); rbRecAvg(I) * 3; rbRecepts(I)
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
                Color 3
                Print wrName$(I);
                Color 15
                Locate , 23
                Print Using "##       ###"; wrRecAvg(I) * 3; recContrib(I)
            End If
        Next

        Print
        Color 12: Print "QUARTERBACKS-                     YDS/CMP"
        Color 14: Print "NAME-               COMP%   INT%    ADJ     CX%"
        For I = 0 To 3:
            If Left$(qbName$(I), 1) <> " " And Left$(qbName$(I), 3) <> "END" Then
                Color 3
                Print qbName$(I);
                Color 15
                Locate , 21
                Print Using "###     ##     ###      ###"; compPct(I); qbIntPct(I); armRating(I); passContrib(I)
            End If
        Next

        Print
        Color 12: Print "KICKOFF RETURNMEN-"
        Color 14: Print "NAME-              RET AVG     CX%"

        For I = 0 To 2:
            If Left$(krName$(I), 1) <> " " And Left$(krName$(I), 3) <> "END" Then
                Color 3
                Print krName$(I);
                Color 15
                Locate , 23
                Print Using "##       ###"; krRetAvg(I) * 3; krContrib(I)
            End If
        Next

        Locate 23
        Color 15: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

        Cls
        Color 12: Print "PUNT RETURNMEN-"
        Color 14: Print "NAME-              RET AVG     CX%"

        For I = 0 To 2:
            If Left$(prName$(I), 1) <> " " And Left$(prName$(I), 3) <> "END" Then
                Color 3
                Print prName$(I);
                Color 15
                Locate , 23
                Print Using "##       ###"; prRetAvg(I) * 3; prContrib(I)
            End If
        Next

        Print
        Color 12: Print "PLACE KICKERS-"
        Color 14: Print "NAME-                FG%  CX%   EP%  CX%"

        For I = 0 To 1:
            If Left$(pkName$(I), 1) <> " " And Left$(pkName$(I), 3) <> "END" Then
                Color 3
                Print pkName$(I);
                Color 15
                Locate , 21
                Print Using "###   ###   ###  ###"; pkFGA(I) * 5 + 25; pkFGContrib(I); pkPAT(I); pkPATContrib(I)
            End If
        Next

        Print
        Color 12: Print "PUNTER-"
        Color 14: Print "NAME-              AVG"
        Color 3: Print punterName$(0);
        Color 15
        Locate , 21: Print Using "##"; puntAvg(0) * 2 + 28

        Do
            Locate 23
            Color 15
            Print "VIEW AGAIN? (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'     CheckContributions Subroutine
'----------------------------------------
'This subroutine assists the user with
'reviewing contribution %'s for each
'team category before saving the changes.
Sub CheckContributions

    Shared rushContrib(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared recContrib(), wrRecAvg()
    Shared passContrib(), compPct(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGContrib(), pkPAT(), pkPATContrib()
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
            Print Using "     ###"; rushContrib(I)
            BC% = BC% + rushContrib(I)
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
                Locate I1 + 4, 24: Input rushContrib(I1)
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
            Print " "; wrName$(I); Tab(20);: Print Using "      ##"; recContrib(I)
            BC% = BC% + recContrib(I)
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
                    Input recContrib(I1 - 10)
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
            Print Using "     ###"; passContrib(I)
            BC% = BC% + passContrib(I)
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
                Input passContrib(I1)
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
            Print Using "     ###"; krContrib(I)
            BC% = BC% + krContrib(I)
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
                Input krContrib(I1)

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
            Print Using "     ###"; prContrib(I)
            BC% = BC% + prContrib(I)
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
                Input prContrib(I1)

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
            Print Using "     ###"; pkFGContrib(I)
            BC% = BC% + pkFGContrib(I)
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
                Input pkFGContrib(I1)

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
            Print Using "     ###"; pkPATContrib(I): BC% = BC% + pkPATContrib(I)
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
                Input pkPATContrib(I1)

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
'       SaveChangedTeam Subroutine
'----------------------------------------
'This routine saves the modified data back
'to the original team file
Sub SaveChangedTeam (team$, teamYear$, teamNbr, YC%)

    Shared mascot$, stadium$

    Shared YC!

    Shared teamAttendance&

    Shared leagueRatings%(), teamRatings%()

    Shared rushContrib(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared recContrib(), wrRecAvg()
    Shared passContrib(), compPct(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGContrib(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Call CSMsgBox("SAVING TEAM FILES", 20, Pass)

    teamYear$ = RTrim$(teamYear$)

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
        LSet Q$(I * 5 + 17) = MKI$(rushContrib(I))
        LSet Q$(I * 5 + 18) = MKI$(rushAvg(I))
        LSet Q$(I * 5 + 19) = MKI$(rbRecepts(I))
        LSet Q$(I * 5 + 20) = MKI$(rbRecAvg(I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = wrName$(I)
        LSet Q$(I * 3 + 67) = MKI$(recContrib(I))
        LSet Q$(I * 3 + 68) = MKI$(wrRecAvg(I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = qbName$(I)
        LSet Q$(I * 5 + 85) = MKI$(passContrib(I))
        LSet Q$(I * 5 + 86) = MKI$(compPct(I))
        LSet Q$(I * 5 + 87) = MKI$(qbIntPct(I))
        LSet Q$(I * 5 + 88) = MKI$(armRating(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = krName$(I)
        LSet Q$(I * 3 + 105) = MKI$(krContrib(I))
        LSet Q$(I * 3 + 106) = MKI$(krRetAvg(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = prName$(I)
        LSet Q$(I * 3 + 114) = MKI$(prContrib(I))
        LSet Q$(I * 3 + 115) = MKI$(prRetAvg(I))
    Next I

    LSet Q$(122) = RTrim$(punterName$(0))
    LSet Q$(123) = MKI$(puntAvg(0))
    LSet Q$(124) = RTrim$(pkName$(0))
    LSet Q$(125) = MKI$(pkFGA(0))
    LSet Q$(126) = MKI$(pkFGContrib(0))
    LSet Q$(127) = MKI$(pkPAT(0))
    LSet Q$(128) = MKI$(pkPATContrib(0))
    LSet Q$(129) = RTrim$(pkName$(1))
    LSet Q$(130) = MKI$(pkFGA(1))
    LSet Q$(131) = MKI$(pkFGContrib(1))
    LSet Q$(132) = MKI$(pkPAT(1))
    LSet Q$(133) = MKI$(pkPATContrib(1))
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

    Close #2

    stadium$ = ""
    teamAttendance& = 0

    Erase armRating, compPct, passContrib, qbIntPct
    Erase krRetAvg, krContrib, prRetAvg, prContrib
    Erase numInts, numSacks
    Erase pkFGA, pkFGContrib, pkPAT, pkPATContrib, puntAvg
    Erase rushContrib, rushAvg, rbRecepts, rbRecAvg
    Erase wrRecAvg, recContrib
    Erase intName$, sackName$
    Erase krName$, pkName$, prName$, rbName$, wrName$, qbName$

    punterName$(0) = ""

    Call CSMsgBox("", 0, Pass)

End Sub

