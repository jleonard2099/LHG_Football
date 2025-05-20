'----------------------------------------
'           SEECON Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'conference stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEECON ()

    _Title "3-in-1 Football - View Conference Stats"

    leaderFile$ = _OpenFileDialog$("Choose Conference Stats file", diskPaths$(2), "*.LDR", "Conference Stats files [.LDR]")
    'ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If leaderFile$ <> "" Then

        Call LoadLeaderFiles_Coll(leaderFile$, conf$)
        Call StatOptions_Coll(conf$)

    End If

End Sub


'----------------------------------------
'    LoadLeaderFiles_Coll Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_Coll (targetFile$, confName$)

    Shared divLosses(), divTies(), divWins()
    Shared leaderVal1!(), leaderVal2!()
    Shared leagPtsAgnst!(), leagPtsFor!()
    Shared totalTeamVal!()

    Shared leagRecText$(), divTeamNames$()
    Shared leaderName$(), leaderTeam$()
    Shared totalTeamName$()

    Shared div1Name$, div2Name$

    Call ReadConferenceStats(targetFile$, 0)

    div1Name$ = PadRight(div1Name$, 8)
    div2Name$ = PadRight(div2Name$, 8)

    Cls

    Locate 9, 6
    Print "PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open targetFile$ For Input As #2

    For X = 1 To (2 * TEAMS_PER_CONFERENCE)
        Input #2, divTeamNames$(X), divWins(X), divLosses(X), divTies(X), leagPtsFor!(X), leagPtsAgnst!(X)
    Next

    For X = 1 To 240
        Input #2, leaderName$(X), leaderTeam$(X), leaderVal1!(X), leaderVal2!(X)
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Input #2, totalTeamVal!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Input #2, totalTeamName$(I, I1)
        Next
    Next

    Close #2

    For NT = 1 To 4

        AA = ((NT - 1) * TEAMS_PER_DIV_COL) + 1
        ZZ = NT * TEAMS_PER_DIV_COL

        D1 = divWins(AA) - divLosses(AA)

        For P = AA To ZZ

            If divTeamNames$(P) = "*" Or divTeamNames$(P) = "" Then
                leagRecText$(P) = ""
            Else
                leagRecText$(P) = PadRight$(divTeamNames$(P), 12)

                rightStr$ = Right$("     " + Str$(Int(divWins(P))), 3)
                leagRecText$(P) = PadRight$(leagRecText$(P) + rightStr$, 16)

                rightStr$ = Right$("     " + Str$(Int(divLosses(P))), 3)
                leagRecText$(P) = PadRight$(leagRecText$(P) + rightStr$, 20)

                rightStr$ = Right$("     " + Str$(Int(divTies(P))), 3)
                leagRecText$(P) = PadRight$(leagRecText$(P) + rightStr$, 24)

                A = 0

                If divWins(P) + divLosses(P) > 0 Then
                    A = Int((divWins(P) + .5 * divTies(P)) / (divWins(P) + divLosses(P) + divTies(P)) * 1000 + .5) / 1000
                End If

                A = CInt(1000 * A)

                If A = 1000 Then
                    leagRecText$(P) = leagRecText$(P) + "1.000 "
                Else
                    rightStr$ = Str$(A)
                    L = Len(rightStr$)
                    rightStr$ = Right$(rightStr$, L - 1)

                    Do While (Len(rightStr$) < 3)
                        rightStr$ = "0" + rightStr$
                    Loop

                    leagRecText$(P) = leagRecText$(P) + " ." + Right$(rightStr$, 3)

                End If

                leagRecText$(P) = PadRight$(leagRecText$(P), 30)

                A = leagPtsFor!(P)
                leagRecText$(P) = leagRecText$(P) + Right$("     " + Str$(A), 3)

                A = leagPtsAgnst!(P)
                leagRecText$(P) = leagRecText$(P) + Right$("     " + Str$(A), 4)

            End If

        Next P

    Next NT

End Sub

'----------------------------------------
'       StatOptions_Coll Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions_Coll (confName$)

    Do

        Cls

        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SaveFileDialog$("Save Conference Report", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call CollLeaders_Report(confName$, 0, userFile$)
                    End If

                Case "P":
                    Call CollLeaders_Report(confName$, 1, "REPORT")

                Case "S":
                    Call CollegeStdgs_Screen(confName$)
                    Call Leaders_Screen                        

            End Select

            Color 15, 0
            Cls

            Call box0(6, 18, 10, 62, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW CONFERENCE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or userKey$ = Chr$(27)

End Sub


'----------------------------------------
'     CollLeaders_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file
'that is then looped through to send to
'the default printer
Sub CollLeaders_Report (confName$, printDest, destFile$)

    Shared divLosses(), divTies(), divWins()
    Shared leaderVal1!(), leaderVal2!()
    Shared leagPtsAgnst!(), leagPtsFor!()
    Shared totalTeamVal!()

    Shared leagRecText$(), divTeamNames$()
    Shared totalTeamName$()
    Shared leaderName$(), leaderTeam$()

    Shared div1Name$, div2Name$

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, confName$ + " OVERALL RECORD"

    If div1Name$ <> "" Then Print #1, div1Name$; " DIVISION"

    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 1 To 15

        If leagRecText$(P) <> "" Then
            Print #1, divTeamNames$(P); Tab(14); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins(P) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If
            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1,
    If div2Name$ <> "" Then Print #1, div2Name$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 16 To 30

        If leagRecText$(P) <> "" Then
            Print #1, divTeamNames$(P); Tab(14); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins(P) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If
            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1,
    Print #1, confName$ + " CONFERENCE RECORD"

    If div1Name$ <> "" Then Print #1, div1Name$; " DIVISION"

    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 31 To 45

        If leagRecText$(P) <> "" Then
            Print #1, divTeamNames$(P); Tab(14); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins(P) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If
            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1,
    If div2Name$ <> "" Then Print #1, div2Name$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 46 To 60

        If leagRecText$(P) <> "" Then
            Print #1, divTeamNames$(P); Tab(14); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins(P) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If
            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "RUSHING LEADERS (MIN 10 ATT/G)"; Tab(41); "RECEIVING LEADERS (MIN 3 REC/G)"
    Print #1, "                            AVG  ATT"; Tab(41); "                            AVG  REC"

    For P = 1 To 20
        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.# ####"; leaderVal1!(P); leaderVal2!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 20) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 20) + ", " + leaderTeam$(P + 20);
            Print #1, Tab(68);
            Print #1, Using "##.# ####"; leaderVal1!(P + 20); leaderVal2!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "RUSHING YARDS TOTAL"; Tab(41); "RECEIVING YARDS TOTAL"

    For P = 41 To 50
        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; leaderVal1!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "RUSHING ATTEMPTS"; Tab(41); "TOTAL RECEPTIONS"

    For P = 81 To 90
        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; leaderVal1!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "ALL PURPOSE YARDAGE"; Tab(41); "SCORING NON-KICKERS"

    For P = 101 To 110

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; leaderVal1!(P + 10)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "RUSHING TOUCHDOWNS"; Tab(41); "RECEIVING TOUCHDOWNS"

    For P = 61 To 70

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; leaderVal1!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "COMPLETION PERCENTAGE (MIN 15 COMP/GAME)"; Tab(41); "QUARTERBACK RATING (MIN 15 COMP/GAME)"

    For P = 121 To 130

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#"; leaderVal1!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###.#"; leaderVal1!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "PASSING YARDAGE"; Tab(41); "PASSING TOUCHDOWNS"

    For P = 141 To 150

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; leaderVal1!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "KO RETURN AVERAGE                 #"; Tab(41); "PUNT RETURN AVERAGE               #"

    For P = 161 To 170

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ##"; leaderVal1!(P); leaderVal2!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##.#  ##"; leaderVal1!(P + 10); leaderVal2!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "PUNTING AVERAGE"; Tab(41); "SCORING (KICKERS)"

    For P = 181 To 190

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ###"; leaderVal1!(P); leaderVal2!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###"; leaderVal1!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "FIELD GOAL %                     FGA"; Tab(41); "EXTRA POINT %                   EPA"

    For P = 201 To 210

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#  ###"; leaderVal1!(P); leaderVal2!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###.#  ##"; leaderVal1!(P + 10); leaderVal2!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "INTERCEPTIONS"; Tab(41); "SACKS"

    For P = 221 To 230

        If leaderName$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##"; leaderVal1!(P);
            Print #1, Tab(41);
        End If

        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##"; leaderVal1!(P + 10)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"; Tab(41); "TOTAL DEFENSE   G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 1

        If totalTeamName$(CA, P) = "*" Or totalTeamName$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5);
            Print #1, Tab(41);
        End If

        CA = 2

        If totalTeamName$(CA, P) = "*" Or totalTeamName$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, totalTeamName$(CA, P); Tab(56);
            Print #1, Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5)
        End If

    Next

    Print #1,
    Print #1, "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"; Tab(41); "RUSHING DEF     G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 3

        If totalTeamName$(CA, P) = "*" Or totalTeamName$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5);
            Print #1, Tab(41);
        End If

        CA = 4

        If totalTeamName$(CA, P) = "*" Or totalTeamName$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, totalTeamName$(CA, P); Tab(56);
            Print #1, Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"

    CA = 7

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5) * 100; totalTeamVal!(CA, P, 6); totalTeamVal!(CA, P, 7); totalTeamVal!(CA, P, 8)
        End If
    Next

    Print #1,
    Print #1, "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
    CA = 8

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5) * 100; totalTeamVal!(CA, P, 6); totalTeamVal!(CA, P, 7); totalTeamVal!(CA, P, 8)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "SCORING OFF    G  POINTS    AVG"; Tab(41); "SCORING DEF     G  POINTS    AVG"

    For P = 1 To 20

        CA = 5

        If totalTeamName$(CA, P) = "*" Or totalTeamName$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "##   ####   ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3);
            Print #1, Tab(41);
        End If

        CA = 6

        If totalTeamName$(CA, P) = "*" Or totalTeamName$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, totalTeamName$(CA, P); Tab(56);
            Print #1, Using "##   ####   ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3)
        End If

    Next

    Print #1,
    Print #1, "PUNTING        NO     AVG"

    CA = 9

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "###    ##.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
    Print #1, "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "

    CA = 10

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print #1, totalTeamName$(CA, P); Tab(18);
            Print #1, Using "### ###  ###    ### ###  ###       ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5); totalTeamVal!(CA, P, 6); totalTeamVal!(CA, P, 7)
        End If
    Next

    Close #1

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub


'----------------------------------------
'    CollegeStdgs_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub CollegeStdgs_Screen (confName$)

    Shared divLosses(), divTies(), divWins()
    Shared leagPtsAgnst!(), leagPtsFor!()

    Shared leagRecText$(), divTeamNames$()

    Shared div1Name$, div2Name$

    Color 15, 0

    Cls
    Color 12

    Locate 2, 33
    Print confName$ + "OVERALL RECORD"

    Color 2
    Locate 4, 1
    Print RTrim$(div1Name$); " DIVISION"

    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 1 To 15:

        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P + 5 - RT)
            Print divTeamNames$(P); Tab(14); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);

            'Win %
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print "  .000";
                    Else
                        'Ties count as 1/2 win
                        Print Using "  .###"; (divWins(P) + (divTies(P) * .5)) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If

            Print Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)

        End If

    Next

    RT = 0

    'If RTrim$(div2Name$) <> "" Then

    Locate 4, 41

    Color 2
    Print RTrim$(div2Name$); " DIVISION"

    Locate , 41
    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 16 To 30

        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 10 - RT), 41
            Print divTeamNames$(P); Tab(54); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);

            'Win %
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print "  .000";
                    Else
                        'Ties count as 1/2 win
                        Print Using "  .###"; (divWins(P) + (divTies(P) * .5)) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If

            Print Tab(71); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)

        End If

    Next

    RT = 0

    'End If

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Locate 2, 33
    Print confName$ + "CONFERENCE RECORD"

    Locate 4, 1
    Color 2
    Print RTrim$(div1Name$); " DIVISION"

    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 31 To 45

        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 25 - RT)
            Print divTeamNames$(P); Tab(14); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);

            'Win %
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print "  .000";
                    Else
                        'Ties count as 1/2 win
                        Print Using "  .###"; (divWins(P) + (divTies(P) * .5)) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If

            Print Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)

        End If

    Next

    RT = 0

    'If RTrim$(div2Name$) <> "" Then

    Locate 4, 41
    Color 2
    Print RTrim$(div2Name$); " DIVISION"

    Locate , 41
    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 46 To 60
        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 40 - RT), 41
            Print divTeamNames$(P); Tab(54); Using "##  ##  ##"; divWins(P); divLosses(P); divTies(P);

            'Win %
            If divWins(P) + divLosses(P) + divTies(P) = 0 Then
                Print "  .---";
            Else
                If divLosses(P) + divTies(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins(P) = 0 Then
                        Print "  .000";
                    Else
                        'Ties count as 1/2 win
                        Print Using "  .###"; (divWins(P) + (divTies(P) * .5)) / (divWins(P) + divLosses(P) + divTies(P));
                    End If
                End If
            End If

            Print Tab(71); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If
    Next

    RT = 0

    'End If

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub
