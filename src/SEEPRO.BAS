Sub SEEPRO ()

    _Title "3-in-1 Football - View League Stats"

    Do
        'Display list of files...
        Spec$ = diskPaths$(2) + "*.LDR"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim leagueLeaderFiles$(0 To Count%)
            For X = 1 To Count%
                leagueLeaderFiles$(X) = Space$(12)
            Next
            leagueLeaderFiles$(0) = Spec$
            Call ReadFile(leagueLeaderFiles$())

            Do

                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT LEAGUE TO VIEW": Color 15
                Selection% = 1
                START% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS LEAGUE";

                _MouseShow "DEFAULT"
                MAMenu leagueLeaderFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

					Color 15, 1
                    targetFile$ = leagueLeaderFiles$(Selection%)
                    Locate 2, 33: Print targetFile$
					
					Do
						Locate 3, 33: Print "ANY CHANGE (YN)";
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

					Color 15, 0
					
                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If UCase$(I$) = "N" Then

                targetFile$ = RTrim$(targetFile$)
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                targetFile$ = Left$(WE$, 8)

                Call LoadLeaderFiles_Pro(FF$, targetFile$)
                Call StatOptions_Pro(FF$)

            End If

        End If 'Done checking for file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'       LoadLeaderFiles_Pro Subroutine
'----------------------------------------
'...explanation...
Sub LoadLeaderFiles_Pro (leagueName$, WE$)

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TR$()

    Shared L!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared T!(), TT!(), W!()

    Shared div1$, div2$, div3$

    Open diskPaths$(2) + WE$ + ".TMS" For Input As #2
    Input #2, leagueName$, div1$, div2$, div3$
    Close 2

    div1$ = PadRight$(div1$, 8)
    div2$ = PadRight$(div2$, 8)
    div3$ = PadRight$(div3$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + WE$ + ".LDR" For Input As #2

    For X = 1 To 30
        Input #2, TM$(X), W!(X), L!(X), T!(X), PTSF!(X), PTSA!(X)
    Next

    For X = 1 To 240
        Input #2, compZ2$(X), compZ3$(X), LD!(X), LZ!(X)
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Input #2, TT!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Input #2, TR$(I, I1)
        Next
    Next

    Close 2

    D1 = W!(1) - L!(1)

    For P = 1 To 10:
        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            A = Int(W!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 16)

            A = Int(L!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 20)

            A = Int(T!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 24)

            A = 0

            If W!(P) + L!(P) > 0 Then A = Int((W!(P) + .5 * T!(P)) / (W!(P) + L!(P) + T!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                A$ = Str$(A)
                L = Len(A$)
                A$ = Right$(A$, L - 1)

                Do While (Len(A$) < 3)
                    A$ = "0" + A$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(A$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)
            A = PTSF!(P)

            A$ = Right$("     " + Str$(A), 3)

            seeP$(P) = seeP$(P) + A$
            A = PTSA!(P)

            A$ = Right$("     " + Str$(A), 4)

            seeP$(P) = seeP$(P) + A$
            'seeP$(P) = seeP$(P)
        End If

    Next P

    D1 = W!(11) - L!(11)

    For P = 11 To 20:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            A = Int(W!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 16)

            A = Int(L!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 20)

            A = Int(T!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 24)

            A = 0

            If W!(P) + L!(P) > 0 Then A = Int((W!(P) + .5 * T!(P)) / (W!(P) + L!(P) + T!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                A$ = Str$(A)
                L = Len(A$)
                A$ = Right$(A$, L - 1)

                Do While (Len(A$) < 3)
                    A$ = "0" + A$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(A$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)
            A = PTSF!(P)
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = seeP$(P) + A$

            A = PTSA!(P)
            A$ = Right$("     " + Str$(A), 4)
            seeP$(P) = seeP$(P) + A$

            'seeP$(P) = seeP$(P)
        End If

    Next P

    D1 = W!(21) - L!(21)

    For P = 21 To 30:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            A = Int(W!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 16)

            A = Int(L!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 20)

            A = Int(T!(P))
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + A$, 24)

            A = 0

            If W!(P) + L!(P) > 0 Then A = Int((W!(P) + .5 * T!(P)) / (W!(P) + L!(P) + T!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                A$ = Str$(A)
                L = Len(A$)
                A$ = Right$(A$, L - 1)

                Do While (Len(A$) < 3)
                    A$ = "0" + A$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(A$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)

            A = PTSF!(P)
            A$ = Right$("     " + Str$(A), 3)
            seeP$(P) = seeP$(P) + A$

            A = PTSA!(P)
            A$ = Right$("     " + Str$(A), 4)
            seeP$(P) = seeP$(P) + A$

            'seeP$(P) = seeP$(P)
        End If

    Next P

End Sub

'----------------------------------------
'       StatOptions_Pro Subroutine
'----------------------------------------
'...explanation...
Sub StatOptions_Pro (leagueName$)

    Do

        Cls
        Call TITLE_STATS

        J = 5

        Locate 8, 8: Color 15, 1

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To J
            Locate 8 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 9 + J, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

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
                    Call SavePrintProLeaders(1, SF$, leagueName$)

                Case "P":
                    Call SavePrintProLeaders(0, SF$, leagueName$)

                Case "S":
                    Call LeaderStatsPro_Screen(leagueName$)

            End Select

            Color 15, 0
            Cls

            JJ = 4

            Locate 6, 8
            Color 15, 1

            Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

            For II = 1 To JJ:
                Locate 6 + II, 8
                Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
            Next

            Locate 7 + JJ, 8

            Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW LEAGUE STATS AGAIN (YN)"
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
'...explanation...
Sub ReportFilePrompt_Pro (reportFile$)

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8: Color 15, 1
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
        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES"
        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
        Locate 7, 37: Input reportFile$

        If reportFile$ <> "" Then

            Do
				Locate 13, 12: Print "ANY CHANGE (YN) "
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" And reportFile$ <> ""

End Sub


'----------------------------------------
'     LeaderStatsPro_Screen Subroutine
'----------------------------------------
'...explanation...
Sub LeaderStatsPro_Screen (leagueName$)

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TR$()

    Shared L!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared T!(), TT!(), W!()

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
            Print TM$(P); Tab(14); Using "##  ##  ##"; W!(P); L!(P); T!(P);

            If W!(P) + L!(P) + T!(P) = 0 Then
                Print "  .---";
            Else
                If L!(P) + T!(P) = 0 Then
                    Print " 1.000";
                Else
                    If W!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (W!(P) + T!(P) * .5) / (W!(P) + L!(P) + T!(P));
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
            Print TM$(P); Tab(54); Using "##  ##  ##"; W!(P); L!(P); T!(P);

            If W!(P) + L!(P) + T!(P) = 0 Then
                Print "  .---";
            Else
                If L!(P) + T!(P) = 0 Then
                    Print " 1.000";
                Else
                    If W!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (W!(P) + .5 * T!(P)) / (W!(P) + L!(P) + T!(P));
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
            Print TM$(P); Tab(33); Using "##  ##  ##"; W!(P); L!(P); T!(P);

            If W!(P) + L!(P) + T!(P) = 0 Then
                Print "  .---";
            Else
                If L!(P) + T!(P) = 0 Then
                    Print " 1.000";
                Else
                    If W!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (W!(P) + .5 * T!(P)) / (W!(P) + L!(P) + T!(P));
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
    Print "RECEIVING LEADERS (MIN 4 REC/G)"
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
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "TOTAL DEFENSE  G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 2

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Locate , 41
            Print TR$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5)
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
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RUSHING DEF    G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 4

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Locate , 41
            Print TR$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5)
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
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "##   ####   ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "SCORING DEF    G  POINTS    AVG"
    Color 15

    CA = 6

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Locate , 41
            Print TR$(CA, P);
            Locate , 55
            Print Using "##   ####   ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3)
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
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5) * 100; TT!(CA, P, 6); TT!(CA, P, 7); TT!(CA, P, 8)
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
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5) * 100; TT!(CA, P, 6); TT!(CA, P, 7); TT!(CA, P, 8)
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
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);: Locate , 15
            Print Using "###    ##.#"; TT!(CA, P, 1); TT!(CA, P, 2)
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
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);: Locate , 18
            Print Using "### ###  ###    ### ###  ###       ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5); TT!(CA, P, 6); TT!(CA, P, 7)
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
'...explanation...
Sub SavePrintProLeaders (destination, reportFile$, leagueName$)

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TR$()

    Shared L!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared T!(), TT!(), W!()

    Shared div1$, div2$, div3$

    If destination <> 1 Then
        Open "REPORT" For Output As #2
    Else
        Open reportFile$ + ".RPT" For Output As #2
    End If

	Color 0, 15
    Cls

    Print #2, leagueName$ + " OVERALL RECORD"
    Print #2, div1$; " DIVISION"
    Print #2, "              W   L   T   PCT  PF  PA"

    For P = 1 To 10

        If seeP$(P) <> "" Then
            Print #2, TM$(P); Tab(14); Using "##  ##  ##"; W!(P); L!(P); T!(P);

            If W!(P) + L!(P) + T!(P) = 0 Then
                Print #2, "  .---";
            Else
                If L!(P) + T!(P) = 0 Then
                    Print #2, " 1.000";
                Else
                    If W!(P) = 0 Then
                        Print #2, "  .000";
                    Else
                        Print #2, Using "  .###"; W!(P) / (W!(P) + L!(P) + T!(P));
                    End If
                End If
            End If

            Print #2, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #2,
    Print #2, div2$; " DIVISION"
    Print #2, "              W   L   T   PCT  PF  PA"

    For P = 11 To 20

        If seeP$(P) <> "" Then
            Print #2, TM$(P); Tab(14); Using "##  ##  ##"; W!(P); L!(P); T!(P);

            If W!(P) + L!(P) + T!(P) = 0 Then
                Print #2, "  .---";
            Else
                If L!(P) + T!(P) = 0 Then
                    Print #2, " 1.000";
                Else
                    If W!(P) = 0 Then
                        Print #2, "  .000";
                    Else
                        Print #2, Using "  .###"; W!(P) / (W!(P) + L!(P) + T!(P));
                    End If
                End If
            End If

            Print #2, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #2,
    Print #2, div3$; " DIVISION"
    Print #2, "              W   L   T   PCT  PF  PA"

    For P = 21 To 30

        If seeP$(P) <> "" Then

            Print #2, TM$(P); Tab(14); Using "##  ##  ##"; W!(P); L!(P); T!(P);

            If W!(P) + L!(P) + T!(P) = 0 Then
                Print #2, "  .---";
            Else
                If L!(P) + T!(P) = 0 Then
                    Print #2, " 1.000";
                Else
                    If W!(P) = 0 Then
                        Print #2, "  .000";
                    Else
                        Print #2, Using "  .###"; W!(P) / (W!(P) + L!(P) + T!(P));
                    End If
                End If
            End If

            Print #2, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #2, Chr$(12)
    Print #2, "RUSHING LEADERS (MIN 10 ATT/G)"; Tab(41); "RECEIVING LEADERS (MIN 4 REC/G)"
    Print #2, "                            AVG  ATT"; Tab(41); "                            AVG  REC"

    For P = 1 To 20

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "##.# ####"; LD!(P); LZ!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 20) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 20) + ", " + compZ3$(P + 20);
            Print #2, Tab(68);
            Print #2, Using "##.# ####"; LD!(P + 20); LZ!(P + 20)
        End If

    Next

    Print #2,
    Print #2, "RUSHING YARDS TOTAL"; Tab(41); "RECEIVING YARDS TOTAL"

    For P = 41 To 50

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "####"; LD!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "####"; LD!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "RUSHING ATTEMPTS"; Tab(41); "TOTAL RECEPTIONS"

    For P = 81 To 90

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "####"; LD!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "####"; LD!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "ALL PURPOSE YARDAGE"; Tab(41); "SCORING NON-KICKERS"

    For P = 101 To 110
        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "####"; LD!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "####"; LD!(P + 10)
        End If

    Next

    Print #2, Chr$(12)
    Print #2, "RUSHING TOUCHDOWNS"; Tab(41); "RECEIVING TOUCHDOWNS"

    For P = 61 To 70
        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "####"; LD!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "####"; LD!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "COMPLETION PERCENTAGE"; Tab(41); "QUARTERBACK RATING"

    For P = 121 To 130

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "###.#"; LD!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "###.#"; LD!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "PASSING YARDAGE"; Tab(41); "PASSING TOUCHDOWNS"

    For P = 141 To 150
        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "####"; LD!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "####"; LD!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "KO RETURN AVERAGE                 #"; Tab(41); "PUNT RETURN AVERAGE               #"

    For P = 161 To 170

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "##.#  ##"; LD!(P); LZ!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "##.#  ##"; LD!(P + 10); LZ!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "PUNTING AVERAGE"; Tab(41); "SCORING (KICKERS)"

    For P = 181 To 190

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "##.#  ###"; LD!(P); LZ!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "###"; LD!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "FIELD GOAL %                     FGA"; Tab(41); "EXTRA POINT %                   EPA"

    For P = 201 To 210

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "###.#  ###"; LD!(P); LZ!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "###.#  ##"; LD!(P + 10); LZ!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "INTERCEPTIONS"; Tab(41); "SACKS"

    For P = 221 To 230

        If compZ2$(P) = "*" Then
            Print #2, Tab(41);
        Else
            Print #2, compZ2$(P) + ", " + compZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "##"; LD!(P);
            Print #2, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #2,
        Else
            Print #2, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #2, Tab(68);
            Print #2, Using "##"; LD!(P + 10)
        End If

    Next

    Print #2, Chr$(12)
    Print #2, "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"; Tab(41); "TOTAL DEFENSE   G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 1

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, TR$(CA, P); Tab(15);
            Print #2, Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5);
            Print #2, Tab(41);
        End If

        CA = 2

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #2,
        Else
            Print #2, TR$(CA, P); Tab(56);
            Print #2, Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5)
        End If

    Next

    Print #2,
    Print #2, "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"; Tab(41); "RUSHING DEF     G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 3

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, TR$(CA, P); Tab(15);
            Print #2, Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5);
            Print #2, Tab(41);
        End If

        CA = 4

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #2,
        Else
            Print #2, TR$(CA, P); Tab(56);
            Print #2, Using "## #### ##### ##.# ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5)
        End If

    Next

    Print #2, Chr$(12)
    Print #2, "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"

    CA = 7

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #2, TR$(CA, P); Tab(15);
            Print #2, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5) * 100; TT!(CA, P, 6); TT!(CA, P, 7); TT!(CA, P, 8)
        End If
    Next

    Print #2,
    Print #2, "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"

    CA = 8

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #2, TR$(CA, P); Tab(15);
            Print #2, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5) * 100; TT!(CA, P, 6); TT!(CA, P, 7); TT!(CA, P, 8)
        End If
    Next

    Print #2, Chr$(12)
    Print #2, "SCORING OFF    G  POINTS    AVG"; Tab(41); "SCORING DEF     G  POINTS    AVG"

    For P = 1 To 20

        CA = 5

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, TR$(CA, P); Tab(15);
            Print #2, Using "##   ####   ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3);
            Print #2, Tab(41);
        End If

        CA = 6

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #2,
        Else
            Print #2, TR$(CA, P); Tab(56);
            Print #2, Using "##   ####   ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3)
        End If

    Next

    Print #2,
    Print #2, "PUNTING        NO     AVG"

    CA = 9

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #2, TR$(CA, P); Tab(15);
            Print #2, Using "###    ##.#"; TT!(CA, P, 1); TT!(CA, P, 2)
        End If
    Next

    Print #2, Chr$(12)
    Print #2, "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
    Print #2, "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "

    CA = 10

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #2, TR$(CA, P); Tab(18);
            Print #2, Using "### ###  ###    ### ###  ###       ###.#"; TT!(CA, P, 1); TT!(CA, P, 2); TT!(CA, P, 3); TT!(CA, P, 4); TT!(CA, P, 5); TT!(CA, P, 6); TT!(CA, P, 7)
        End If
    Next

    Close 2

    If destination <> 1 Then

        Open "REPORT" For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close 2

        LPrint Chr$(12)

    End If

End Sub
