'----------------------------------------
'           SEENATL Subroutine
'----------------------------------------
'This subroutine lets the user choose an
'organization stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEENATL ()

    _Title "3-in-1 Football - View Org Stats"

    leaderFile$ = _OpenFileDialog$("Choose Organization Stats file", diskPaths$(2), "*.NLD", "Organization Stats files [.NLD]")
    'ChooseFile$ (diskPaths$(2) + "*.NLD", "LEADER", ExitCode%)

    If leaderFile$ <> "" Then

        Call LoadLeaderFiles_Natl(leaderFile$, FF$, NRT)
        Call StatOptions_Natl(GetFileBaseName$(leaderFile$), NRT)

    End If

End Sub


'----------------------------------------
'     LoadLeaderFiles_Natl Subroutine
'----------------------------------------
'This subroutine loads the organization
'stats from the file into several arrays
Sub LoadLeaderFiles_Natl (targetFile$, confName$, NRT)

    Shared divLosses(), divTies(), divWins()
    Shared pollRatings!()
    Shared leaderVal1!(), leaderVal2!()
    Shared leagPtsAgnst!(), leagPtsFor!()
    Shared totalTeamVal!()

    Shared pollTeam$(), divTeamNames$()
    Shared leaderName$(), leaderTeam$()
    Shared totalTeamName$()

    Cls

    Locate 9, 6
    Print "PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + GetFileBaseName$(targetFile$) + ".CON" For Input As #1
    Input #1, confName$
    Close #1

    Open targetFile$ For Input As #2

    For X = 1 To 60
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

    Open diskPaths$(2) + GetFileBaseName$(targetFile$) + ".POL" For Input As #3

    Input #3, NRT

    For X = 1 To NRT
        Input #3, pollTeam$(X), pollRatings!(X, 1), pollRatings!(X, 2), pollRatings!(X, 3), pollRatings!(X, 4), pollRatings!(X, 5)
    Next

    Close #3

End Sub

'----------------------------------------
'      StatOptions_Natl Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions_Natl (confName$, NRT)

    Do

        Cls

        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SaveFileDialog$("Save National Org Report",_CWD$ + "\National Org Report","*.txt","Text File")
                    If userFile$ <> "" Then
                        Call NatLeaders_Report(confName$, NRT, 0, userFile$)
                    End If

                Case "P":
                    Call NatLeaders_Report(confName$, NRT, 1, "REPORT")

                Case "S":
                    Call NatlStdgs_Screen(confName$, NRT)
                    Call Leaders_Screen

            End Select

            Color 15, 0
            Cls

            Call box0(6, 17, 10, 63, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW NATIONAL STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or userKey$ = Chr$(27)

End Sub



'----------------------------------------
'     NatlStdgs_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub NatlStdgs_Screen (confName$, NRT)

    Shared pollRatings!()
    Shared pollTeam$()

    Color 15, 0
    Cls

    Color 12

    XCoord = Int((80 - Len(confName$) - 17) / 2)
    Locate 2, XCoord
    Print confName$ + " OVERALL RANKINGS"

    Locate 4, 1
    Color 14
    Print "                    RATING     W    L    T   STRENGTH OF SCHEDULE"
    Color 15

    For P = 1 To NRT

        Print Using "###  "; P;
        Print pollTeam$(P); Tab(22); Using "###.#    ##   ##   ##         ###.#"; pollRatings!(P, 1); pollRatings!(P, 2); pollRatings!(P, 3); pollRatings!(P, 4); pollRatings!(P, 5)

        If P <> 0 And (P Mod 20 = 0) Then

            Color 12
            Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
            QK$ = GetKeyPress$

            Color 15
            Cls

            Color 12

            Locate 2, XCoord
            Print confName$ + " OVERALL RANKINGS"

            Locate 4, 1
            Color 14
            Print Tab(21); "RATING     W    L    T   STRENGTH OF SCHEDULE"
            Color 15

        End If

    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'     Leaders_Screen Subroutine
'----------------------------------------
Sub Leaders_Screen ()

    Shared leaderVal1!(), leaderVal2!()
    Shared totalTeamVal!()

    Shared totalTeamName$()
    Shared leaderName$(), leaderTeam$()

    Cls

    Locate 2, 1
    Color 14
    'Print "RUSHING LEADERS (MIN 10 ATT/G)"
    Print "RUSHING LEADERS"

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

    Locate 2, 41
    Color 14
    'Print "RECEIVING LEADERS (MIN 3 REC/G)"
    Print "RECEIVING LEADERS"
    Locate , 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1

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

    Locate 2, 41
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

    Locate 14, 1
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

    Locate 14, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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

    Locate 2, 41
    Color 14
    Print "SCORING NON-KICKERS"
    Color 15

    For P = 111 To 120
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "####"; leaderVal1!(P)
        End If
    Next

    Locate 14
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

    Locate 14, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "COMPLETION PERCENTAGE"
    'Print "COMPLETION % (MIN 15 COMP/GAME)"
    Color 15

    For P = 121 To 130
        If leaderName$(P) <> "*" Then
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 28
            Print Using "###.#"; leaderVal1!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    'Print "QUARTERBACK RATING  (MIN 15 COMP/GAME)"
    Print "QUARTERBACK RATING"
    Color 15

    For P = 131 To 140
        If leaderName$(P) <> "*" Then
            Locate , 41
            Print leaderName$(P) + ", " + leaderTeam$(P);
            Locate , 68
            Print Using "###.#"; leaderVal1!(P)
        End If
    Next

    Locate 14, 1
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

    Locate 14, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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

    Locate 2, 41
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

    Locate 14, 1
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

    Locate 14, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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

    Locate 2, 41
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

    Locate 14, 1
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

    Locate 14, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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

    Locate 2, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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

    Locate 2, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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

    Locate 2, 41
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "PUNTING        NO     AVG"
    Color 15

    CA = 9

    For P = 1 To 20
        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print totalTeamName$(CA, P);
            Locate , 15
            Print Using "###    ##.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 33
    Color 14
    Print "TURNOVER MARGIN"
    Locate 4, 1
    Print "                     GAINED          LOST           MARGIN/"
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
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$

End Sub


'----------------------------------------
'     NatLeaders_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file
'that is then looped through to send to
'the default printer
Sub NatLeaders_Report (confName$, NRT, printDest, destFile$)

    Shared leaderVal2!(), pollRatings!()
    Shared totalTeamVal!()

    Shared leaderName$(), leaderTeam$()
    Shared pollTeam$()
    Shared totalTeamName$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    XCoord = Int((80 - Len(confName$) - 17) / 2)
    Print #1,
    Print #1, Tab(XCoord); confName$; " OVERALL RANKINGS"
    Print #1, 
    Print #1, "                    RATING     W    L    T   STRENGTH OF SCHEDULE"

    For P = 1 To NRT
        Print #1, Using "###  "; P;
        Print #1, pollTeam$(P); Tab(22); Using "###.#    ##   ##   ##         ###.#"; pollRatings!(P, 1); pollRatings!(P, 2); pollRatings!(P, 3); pollRatings!(P, 4); pollRatings!(P, 5)
    Next

    Print #1, 
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 

    Print #1, "RUSHING LEADERS"; Tab(41); "RECEIVING LEADERS"
    Print #1, "                            AVG  ATT"; Tab(41); "                            AVG  REC"

    For P = 1 To 20

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.# ####"; leaderVal1!(P); leaderVal2!(P);
        End If

		Print #1, Tab(41);
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

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
        End If

		Print #1, Tab(41);
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

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
        End If

		Print #1, Tab(41);
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

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
        End If

		Print #1, Tab(41);
        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; leaderVal1!(P + 10)
        End If

    Next

    Print #1, 
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 

    Print #1, "RUSHING TOUCHDOWNS"; Tab(41); "RECEIVING TOUCHDOWNS"

    For P = 61 To 70

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
        End If

		Print #1, Tab(41);
        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; leaderVal1!(P + 10)
        End If

    Next

    Print #1,
    'Print #1, "COMPLETION % (MIN 15 COMP/GAME)"; Tab(41); "QUARTERBACK RATING (MIN 15 COMP/GAME)"
    Print #1, "COMPLETION PERCENTAGE"; Tab(41); "QUARTERBACK RATING"

    For P = 121 To 130

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#"; leaderVal1!(P);
        End If

		Print #1, Tab(41);
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

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; leaderVal1!(P);
        End If

		Print #1, Tab(41);
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

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ##"; leaderVal1!(P), leaderVal2!(P);
        End If

		Print #1, Tab(41);
        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##.#  ##"; leaderVal1!(P + 10), leaderVal2!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "PUNTING AVERAGE"; Tab(41); "SCORING (KICKERS)"

    For P = 181 To 190

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ###"; leaderVal1!(P), leaderVal2!(P);
        End If

		Print #1, Tab(41);
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

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#  ###"; leaderVal1!(P), leaderVal2!(P);
        End If

		Print #1, Tab(41);
        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###.#  ##"; leaderVal1!(P + 10), leaderVal2!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "INTERCEPTIONS"; Tab(41); "SACKS"

    For P = 221 To 230

        If leaderName$(P) <> "*" Then
            Print #1, leaderName$(P) + ", " + leaderTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##"; leaderVal1!(P);
        End If

		Print #1, Tab(41);
        If leaderName$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, leaderName$(P + 10) + ", " + leaderTeam$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##"; leaderVal1!(P + 10)
        End If

    Next

    Print #1, 
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 

    Print #1, "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"; Tab(41); "TOTAL DEFENSE   G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 1

        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5);
        End If

		Print #1, Tab(41);
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

        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5);
        End If

		Print #1, Tab(41);
        CA = 4

        If totalTeamName$(CA, P) = "*" Or totalTeamName$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, totalTeamName$(CA, P); Tab(56);
            Print #1, Using "## #### ##### ##.# ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3); totalTeamVal!(CA, P, 4); totalTeamVal!(CA, P, 5)
        End If

    Next

    Print #1, 
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 

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

    Print #1, 
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 

    Print #1, "SCORING OFF    G  POINTS    AVG"; Tab(41); "SCORING DEF     G  POINTS    AVG"

    For P = 1 To 20

        CA = 5

        If totalTeamName$(CA, P) <> "*" And totalTeamName$(CA, P) <> "" Then
            Print #1, totalTeamName$(CA, P); Tab(15);
            Print #1, Using "##   ####   ###.#"; totalTeamVal!(CA, P, 1); totalTeamVal!(CA, P, 2); totalTeamVal!(CA, P, 3);
        End If

		Print #1, Tab(41);
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

    Print #1, 
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1, 

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
