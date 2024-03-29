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
Sub LoadLeaderFiles_Pro (league$, leagueFile$)

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TT$()

    Shared seeL!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared seeT!(), compTT!(), seeW!()

    Shared div1$, div2$, div3$

    Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #1
    Input #1, league$, div1$, div2$, div3$
    Close #1

    div1$ = PadRight$(div1$, 8)
    div2$ = PadRight$(div2$, 8)
    div3$ = PadRight$(div3$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + leagueFile$ + ".LDR" For Input As #2

    For X = 1 To 30
        Input #2, TM$(X), seeW!(X), seeL!(X), seeT!(X), PTSF!(X), PTSA!(X)
    Next

    For X = 1 To 240
        Input #2, compZ2$(X), compZ3$(X), LD!(X), LZ!(X)
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Input #2, compTT!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Input #2, TT$(I, I1)
        Next
    Next

    Close #2

    D1 = seeW!(1) - seeL!(1)

    For P = 1 To 10:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            rightStr$ = Right$("     " + Str$(Int(seeW!(P))), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 16)

            rightStr$ = Right$("     " + Str$(Int(seeL!(P))), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 20)

            rightStr$ = Right$("     " + Str$(Int(seeT!(P))), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 24)

            A = 0

            If seeW!(P) + seeL!(P) > 0 Then A = Int((seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                rightStr$ = Str$(A)
                L = Len(rightStr$)
                rightStr$ = Right$(rightStr$, L - 1)

                Do While (Len(rightStr$) < 3)
                    rightStr$ = "0" + rightStr$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(rightStr$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)
            A = PTSF!(P)

            seeP$(P) = seeP$(P) + Right$("     " + Str$(A), 3)
            A = PTSA!(P)

            seeP$(P) = seeP$(P) + Right$("     " + Str$(A), 4)

        End If

    Next P

    D1 = seeW!(11) - seeL!(11)

    For P = 11 To 20:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            A = Int(seeW!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 16)

            A = Int(seeL!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 20)

            A = Int(seeT!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 24)

            A = 0

            If seeW!(P) + seeL!(P) > 0 Then A = Int((seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                rightStr$ = Str$(A)
                L = Len(rightStr$)
                rightStr$ = Right$(rightStr$, L - 1)

                Do While (Len(rightStr$) < 3)
                    rightStr$ = "0" + rightStr$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(rightStr$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)
            A = PTSF!(P)
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = seeP$(P) + rightStr$

            A = PTSA!(P)
            rightStr$ = Right$("     " + Str$(A), 4)
            seeP$(P) = seeP$(P) + rightStr$

            'seeP$(P) = seeP$(P)
        End If

    Next P

    D1 = seeW!(21) - seeL!(21)

    For P = 21 To 30:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            A = Int(seeW!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 16)

            A = Int(seeL!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 20)

            A = Int(seeT!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 24)

            A = 0

            If seeW!(P) + seeL!(P) > 0 Then A = Int((seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                rightStr$ = Str$(A)
                L = Len(rightStr$)
                rightStr$ = Right$(rightStr$, L - 1)

                Do While (Len(rightStr$) < 3)
                    rightStr$ = "0" + rightStr$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(rightStr$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)

            A = PTSF!(P)
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = seeP$(P) + rightStr$

            A = PTSA!(P)
            rightStr$ = Right$("     " + Str$(A), 4)
            seeP$(P) = seeP$(P) + rightStr$

            'seeP$(P) = seeP$(P)
        End If

    Next P

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

        Call MenuBox(8, 8, 61, 5, 15, 1)

        Locate 7, 32

        Do
            Locate 10
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "F";: Color 7: Print "ILE"

            I$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P" Or UCase$(I$) = "S"

        If I$ <> Chr$(27) Then

            Select Case UCase$(I$)

                Case "F":
                    Call ReportFilePrompt_Pro(SF$)
                    Call SavePrintProLeaders(1, SF$, league$)

                Case "P":
                    Call SavePrintProLeaders(0, SF$, league$)

                Case "S":
                    Call LeaderStatsPro_Screen

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

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27)

End Sub


'----------------------------------------
'     ReportFilePrompt_Pro Subroutine
'----------------------------------------
'This subroutine prompts the user for
'the name of the report file to be saved
Sub ReportFilePrompt_Pro (reportFile$)

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Locate 7, 32

        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
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

    Loop Until UCase$(I$) = "N" And reportFile$ <> ""

End Sub


'----------------------------------------
'     LeaderStatsPro_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeaderStatsPro_Screen ()

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TT$()

    Shared seeL!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared seeT!(), compTT!(), seeW!()

    Shared div1$, div2$, div3$

    Color 15, 0
    Cls

    Color 12
    Print div1$; " DIVISION"

    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 1 To 10:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P + 2 - RT)
            Print TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (seeW!(P) + seeT!(P) * .5) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    RT = 0

    Locate 1, 41
    Color 12
    Print div2$ + " DIVISION"
    Color 14

    Locate 2, 41
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 11 To 20:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 8 - RT), 41
            Print TM$(P); Tab(54); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print Tab(71); Using "### ###"; PTSF!(P); PTSA!(P)
        End If
    Next

    RT = 0

    Locate 13, 20

    Color 12
    Print div3$ + " DIVISION"

    Locate 14, 20
    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 21 To 30:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 6 - RT), 20
            Print TM$(P); Tab(33); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print Tab(50); Using "### ###"; PTSF!(P); PTSA!(P)

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
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##.# ####"; LD!(P); LZ!(P)
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
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "##.#  ###"; LD!(P); LZ!(P)
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
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RECEIVING YARDS TOTAL"
    Color 15

    For P = 51 To 60
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "RUSHING ATTEMPTS"
    Color 15

    For P = 81 To 90
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "TOTAL RECEPTIONS"
    Color 15

    For P = 91 To 100
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
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
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "SCORING NON-KICKERS"
    Color 15

    For P = 111 To 120
        If compZ2$(P) <> "*" Then
            Locate , 41: Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "RUSHING TOUCHDOWNS"
    Color 15

    For P = 61 To 70
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "RECEIVING TOUCHDOWNS"
    Color 15

    For P = 71 To 80
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
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
    Print "COMPLETION PERCENTAGE"
    Color 15

    For P = 121 To 130
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "###.#"; LD!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "QUARTERBACK RATING"
    Color 15

    For P = 131 To 140
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "###.#"; LD!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "PASSING YARDAGE"
    Color 15

    For P = 141 To 150
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "PASSING TOUCHDOWNS"
    Color 15

    For P = 151 To 160
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
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
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "PUNT RETURN AVERAGE               #"
    Color 15

    For P = 171 To 180
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "##.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "PUNTING AVERAGE"
    Color 15

    For P = 181 To 190
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##.#  ###"; LD!(P); LZ!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "SCORING (KICKERS)"
    Color 15

    For P = 191 To 200
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "###"; LD!(P)
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
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "###.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "EXTRA POINT %                    EPA"
    Color 15

    For P = 211 To 220
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "###.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "INTERCEPTIONS"
    Color 15

    For P = 221 To 230
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "SACKS"
    Color 15

    For P = 231 To 240
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "##"; LD!(P)
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
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print TT$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "TOTAL DEFENSE  G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 2

    For P = 1 To 20
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Locate , 41
            Print TT$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
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
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print TT$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RUSHING DEF    G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 4

    For P = 1 To 20
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Locate , 41
            Print TT$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
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
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print TT$(CA, P);
            Locate , 15
            Print Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "SCORING DEF    G  POINTS    AVG"
    Color 15

    CA = 6

    For P = 1 To 20
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Locate , 41
            Print TT$(CA, P);
            Locate , 55
            Print Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3)
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
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print TT$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
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
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print TT$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
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
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print TT$(CA, P);: Locate , 15
            Print Using "###    ##.#"; compTT!(CA, P, 1); compTT!(CA, P, 2)
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
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print TT$(CA, P);: Locate , 18
            Print Using "### ###  ###    ### ###  ###       ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5); compTT!(CA, P, 6); compTT!(CA, P, 7)
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
'       SavePrintProLeaders Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file
'that is then looped through to send to
'the default printer
Sub SavePrintProLeaders (destination, reportFile$, league$)

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TT$()

    Shared seeL!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared seeT!(), compTT!(), seeW!()

    Shared div1$, div2$, div3$

    If destination <> 1 Then
        Open "REPORT" For Output As #1
    Else
        Open reportFile$ + ".RPT" For Output As #1
    End If

    Color 0, 15
    Cls

    Print #1, league$ + " OVERALL RECORD"
    Print #1, div1$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 1 To 10

        If seeP$(P) <> "" Then
            Print #1, TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print #1, "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; seeW!(P) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #1,
    Print #1, div2$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 11 To 20

        If seeP$(P) <> "" Then
            Print #1, TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print #1, "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; seeW!(P) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #1,
    Print #1, div3$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 21 To 30

        If seeP$(P) <> "" Then

            Print #1, TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print #1, "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; seeW!(P) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "RUSHING LEADERS (MIN 10 ATT/G)"; Tab(41); "RECEIVING LEADERS (MIN 4 REC/G)"
    Print #1, "                            AVG  ATT"; Tab(41); "                            AVG  REC"

    For P = 1 To 20

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.# ####"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 20) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 20) + ", " + compZ3$(P + 20);
            Print #1, Tab(68);
            Print #1, Using "##.# ####"; LD!(P + 20); LZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "RUSHING YARDS TOTAL"; Tab(41); "RECEIVING YARDS TOTAL"

    For P = 41 To 50

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "RUSHING ATTEMPTS"; Tab(41); "TOTAL RECEPTIONS"

    For P = 81 To 90

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "ALL PURPOSE YARDAGE"; Tab(41); "SCORING NON-KICKERS"

    For P = 101 To 110
        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "RUSHING TOUCHDOWNS"; Tab(41); "RECEIVING TOUCHDOWNS"

    For P = 61 To 70
        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "COMPLETION PERCENTAGE"; Tab(41); "QUARTERBACK RATING"

    For P = 121 To 130

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###.#"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "PASSING YARDAGE"; Tab(41); "PASSING TOUCHDOWNS"

    For P = 141 To 150
        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "KO RETURN AVERAGE                 #"; Tab(41); "PUNT RETURN AVERAGE               #"

    For P = 161 To 170

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ##"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##.#  ##"; LD!(P + 10); LZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "PUNTING AVERAGE"; Tab(41); "SCORING (KICKERS)"

    For P = 181 To 190

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ###"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "FIELD GOAL %                     FGA"; Tab(41); "EXTRA POINT %                   EPA"

    For P = 201 To 210

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#  ###"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###.#  ##"; LD!(P + 10); LZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "INTERCEPTIONS"; Tab(41); "SACKS"

    For P = 221 To 230

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##"; LD!(P + 10)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"; Tab(41); "TOTAL DEFENSE   G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 1

        If TT$(CA, P) = "*" Or TT$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, TT$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5);
            Print #1, Tab(41);
        End If

        CA = 2

        If TT$(CA, P) = "*" Or TT$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, TT$(CA, P); Tab(56);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If

    Next

    Print #1,
    Print #1, "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"; Tab(41); "RUSHING DEF     G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 3

        If TT$(CA, P) = "*" Or TT$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, TT$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5);
            Print #1, Tab(41);
        End If

        CA = 4

        If TT$(CA, P) = "*" Or TT$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, TT$(CA, P); Tab(56);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"

    CA = 7

    For P = 1 To 20
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print #1, TT$(CA, P); Tab(15);
            Print #1, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
        End If
    Next

    Print #1,
    Print #1, "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"

    CA = 8

    For P = 1 To 20
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print #1, TT$(CA, P); Tab(15);
            Print #1, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "SCORING OFF    G  POINTS    AVG"; Tab(41); "SCORING DEF     G  POINTS    AVG"

    For P = 1 To 20

        CA = 5

        If TT$(CA, P) = "*" Or TT$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, TT$(CA, P); Tab(15);
            Print #1, Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3);
            Print #1, Tab(41);
        End If

        CA = 6

        If TT$(CA, P) = "*" Or TT$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, TT$(CA, P); Tab(56);
            Print #1, Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3)
        End If

    Next

    Print #1,
    Print #1, "PUNTING        NO     AVG"

    CA = 9

    For P = 1 To 20
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print #1, TT$(CA, P); Tab(15);
            Print #1, Using "###    ##.#"; compTT!(CA, P, 1); compTT!(CA, P, 2)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
    Print #1, "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "

    CA = 10

    For P = 1 To 20
        If TT$(CA, P) <> "*" And TT$(CA, P) <> "" Then
            Print #1, TT$(CA, P); Tab(18);
            Print #1, Using "### ###  ###    ### ###  ###       ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5); compTT!(CA, P, 6); compTT!(CA, P, 7)
        End If
    Next

    Close #1

    If destination <> 1 Then

        Open "REPORT" For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

    End If

End Sub

