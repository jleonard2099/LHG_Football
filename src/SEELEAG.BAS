'----------------------------------------
'            SEELEAG Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEELEAG ()

    _Title "3-in-1 Football - View League Stats"

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadLeaderFiles_Pro(league$, leaderFile$)
        Call StatOptions_Pro(league$)

    End If

End Sub

'----------------------------------------
'     LoadLeaderFiles_Pro Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_Pro (league$, targetFile$)

    Shared leagRecText$(), leagLdrTeams$()
    Shared leaderName$(), leaderTeam$()
    Shared totalTeamName$()

    Shared divLosses!(), divTies!(), divWins!()
    Shared leaderVal1!(), leaderVal2!()
    Shared leagPtsAgnst!(), leagPtsFor!()
    Shared totalTeamVal!()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Call ReadConferenceStats (targetFile$, 1)

    div1Name$ = PadRight$(div1Name$, 8)
    div2Name$ = PadRight$(div2Name$, 8)
    div3Name$ = PadRight$(div3Name$, 8)
    div4Name$ = PadRight$(div4Name$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + targetFile$ + ".LDR" For Input As #2

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, leagLdrTeams$(X), divWins!(X), divLosses!(X), divTies!(X), leagPtsFor!(X), leagPtsAgnst!(X)
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

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        D1 = divWins!(AA) - divLosses!(AA)

        For P = AA To ZZ

            If leagLdrTeams$(P) = "*" Or leagLdrTeams$(P) = "" Then
                leagRecText$(P) = ""
            Else

                leagRecText$(P) = PadRight$(leagLdrTeams$(P), 12)

                rightStr$ = Right$("     " + Str$(Int(divWins!(P))), 3)
                leagRecText$(P) = PadRight$(leagRecText$(P) + rightStr$, 16)

                rightStr$ = Right$("     " + Str$(Int(divLosses!(P))), 3)
                leagRecText$(P) = PadRight$(leagRecText$(P) + rightStr$, 20)

                rightStr$ = Right$("     " + Str$(Int(divTies!(P))), 3)
                leagRecText$(P) = PadRight$(leagRecText$(P) + rightStr$, 24)

                A = 0

                If divWins!(P) + divLosses!(P) > 0 Then
                    A = Int((divWins!(P) + .5 * divTies!(P)) / (divWins!(P) + divLosses!(P) + divTies!(P)) * 1000 + .5) / 1000
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
'       StatOptions_Pro Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions_Pro (league$)

    Do

        Cls

        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SaveFileDialog$("Save League Report", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call ProLeaders_Report(league$, 0, userFile$)
                    End If

                Case "P":
                    Call ProLeaders_Report(league$, 1, "REPORT")

                Case "S":
                    Call ProLeaders_Screen

            End Select

            Color 15, 0
            Cls

            Call box0(6, 8, 11, 70, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW LEAGUE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or userKey$ = Chr$(27)

End Sub



'----------------------------------------
'     ProLeaders_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub ProLeaders_Screen ()

    Shared leagRecText$(), leagLdrTeams$()
    Shared leaderName$(), leaderTeam$()
    Shared totalTeamName$()

    Shared divLosses!(), divTies!(), divWins!()
    Shared leaderVal1!(), leaderVal2!()
    Shared leagPtsAgnst!(), leagPtsFor!()
    Shared totalTeamVal!()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Color 15, 0
    Cls

    Color 12
    If div1Name$ <> "" Then Print RTrim$(div1Name$) + " DIVISION"

    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 1 To 10:

        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P + 2 - RT)
            Print leagLdrTeams$(P); Tab(14); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (divWins!(P) + divTies!(P) * .5) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    RT = 0

    Locate 1, 41
    Color 12
    If div2Name$ <> "" Then Print RTrim$(div2Name$) + " DIVISION"
    Color 14

    Locate 2, 41
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 11 To 20:

        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 8 - RT), 41
            Print leagLdrTeams$(P); Tab(54); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (divWins!(P) + .5 * divTies!(P)) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print Tab(71); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If
    Next

    RT = 0

    Locate 13, 1

    Color 12
    If div3Name$ <> "" Then Print RTrim$(div3Name$) + " DIVISION"

    Locate 14, 1
    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 21 To 30:

        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 6 - RT)
            Print leagLdrTeams$(P); Tab(14); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (divWins!(P) + .5 * divTies!(P)) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)

        End If

    Next

    RT = 0

    Locate 13, 41

    Color 12
    If div4Name$ <> "" Then Print RTrim$(div4Name$) + " DIVISION"

    Locate 14, 41
    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 31 To 40:

        If leagRecText$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 16 - RT), 41
            Print leagLdrTeams$(P); Tab(54); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (divWins!(P) + .5 * divTies!(P)) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print Tab(71); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)

        End If

    Next

    RT = 0

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "RUSHING LEADERS (MIN 10 ATT/G)"
    Color 12
    Print "                            AVG  ATT"
    Color 15

    For P = 1 To 20
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "##.# ####"; leaderVal1!(P); leaderVal2!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RECEIVING LEADERS (MIN 3 REC/G)"
    Locate 2, 41

    Color 12
    Print "                            AVG  REC"
    Color 15

    For P = 21 To 40
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "##.#  ###"; leaderVal1!(P); leaderVal2!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "RUSHING YARDS TOTAL"
    Color 15

    For P = 41 To 50
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RECEIVING YARDS TOTAL"
    Color 15

    For P = 51 To 60
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "RUSHING ATTEMPTS"
    Color 15

    For P = 81 To 90
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "TOTAL RECEPTIONS"
    Color 15

    For P = 91 To 100
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "ALL PURPOSE YARDAGE"
    Color 15

    For P = 101 To 110
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "SCORING NON-KICKERS"
    Color 15

    For P = 111 To 120
        If leaderName$(P) <> "*" Then
            Locate , 41: Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "RUSHING TOUCHDOWNS"
    Color 15

    For P = 61 To 70
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "RECEIVING TOUCHDOWNS"
    Color 15

    For P = 71 To 80
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "COMPLETION PERCENTAGE (MIN 15 ATT/G)"
    Color 15

    For P = 121 To 130
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "###.#"; leaderVal1!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "QUARTERBACK RATING (MIN 15 ATT/G)"
    Color 15

    For P = 131 To 140
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "###.#"; leaderVal1!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "PASSING YARDAGE"
    Color 15

    For P = 141 To 150
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "PASSING TOUCHDOWNS"
    Color 15

    For P = 151 To 160
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "KO RETURN AVERAGE                 #"
    Color 15

    For P = 161 To 170
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "##.#  ##"; leaderVal1!(P); leaderVal2!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "PUNT RETURN AVERAGE               #"
    Color 15

    For P = 171 To 180
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "##.#  ##"; leaderVal1!(P); leaderVal2!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "PUNTING AVERAGE"
    Color 15

    For P = 181 To 190
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "##.#  ###"; leaderVal1!(P); leaderVal2!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "SCORING (KICKERS)"
    Color 15

    For P = 191 To 200
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "###"; leaderVal1!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "FIELD GOAL %                     FGA"
    Color 15

    For P = 201 To 210
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "###.#  ##"; leaderVal1!(P); leaderVal2!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "EXTRA POINT %                    EPA"
    Color 15

    For P = 211 To 220
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "###.#  ##"; leaderVal1!(P); leaderVal2!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "INTERCEPTIONS"
    Color 15

    For P = 221 To 230
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "##"; leaderVal1!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "SACKS"
    Color 15

    For P = 231 To 240
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "##"; leaderVal1!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 1

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "TOTAL DEFENSE  G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 2

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Locate , 41
            Print totalTeamName$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 3

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RUSHING DEF    G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 4

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Locate , 41
            Print totalTeamName$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "SCORING OFF    G  POINTS    AVG"
    Color 15

    CA = 5

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);
            Locate , 15
            Print Using "##   ####   ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "SCORING DEF    G  POINTS    AVG"
    Color 15

    CA = 6

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Locate , 41
            Print totalTeamName$(CA, P);
            Locate , 55
            Print Using "##   ####   ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
    Color 15

    CA = 7

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5) * 100; totalTeamVal!(CA, P, 6); totalTeamVal!(CA, P, 7); totalTeamVal!(CA, P, 8)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
    Color 15

    CA = 8

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5) * 100; totalTeamVal!(CA, P, 6); totalTeamVal!(CA, P, 7); totalTeamVal!(CA, P, 8)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "PUNTING        NO     AVG"
    Color 15

    CA = 9

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);: Locate , 15
            Print Using "###    ##.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
    Print "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "
    Color 15

    CA = 10

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);: Locate , 18
            Print Using "### ###  ###    ### ###  ###       ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5); totalTeamVal!(CA, P, 6); totalTeamVal!(CA, P, 7)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'       ProLeaders_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file
'that is then looped through to send to
'the default printer
Sub ProLeaders_Report (league$, printDest, destFile$)

    Shared leagRecText$(), leagLdrTeams$()
    Shared leaderName$(), leaderTeam$()
    Shared totalTeamName$()

    Shared divLosses!(), divTies!(), divWins!()
    Shared leaderVal1!(), leaderVal2!()
    Shared leagPtsAgnst!(), leagPtsFor!()
    Shared totalTeamVal!()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    Print #1, league$ + " OVERALL RECORD"
    Print #1, div1Name$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 1 To 10

        If leagRecText$(P) <> "" Then
            Print #1, leagLdrTeams$(P); Tab(14); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins!(P) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1,
    Print #1, div2Name$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 11 To 20

        If leagRecText$(P) <> "" Then
            Print #1, leagLdrTeams$(P); Tab(14); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins!(P) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1,
    Print #1, div3Name$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 21 To 30

        If leagRecText$(P) <> "" Then

            Print #1, leagLdrTeams$(P); Tab(14); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins!(P) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1,
    Print #1, div4Name$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 31 To 40

        If leagRecText$(P) <> "" Then

            Print #1, leagLdrTeams$(P); Tab(14); Using "##  ##  ##"; divWins!(P); divLosses!(P); divTies!(P);

            If divWins!(P) + divLosses!(P) + divTies!(P) = 0 Then
                Print #1, "  .---";
            Else
                If divLosses!(P) + divTies!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If divWins!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; divWins!(P) / (divWins!(P) + divLosses!(P) + divTies!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; leagPtsFor!(P); leagPtsAgnst!(P)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "RUSHING LEADERS (MIN 10 ATT/G)"; Tab(41); "RECEIVING LEADERS (MIN 4 REC/G)"
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
    Print #1, "COMPLETION PERCENTAGE"; Tab(41); "QUARTERBACK RATING"

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

