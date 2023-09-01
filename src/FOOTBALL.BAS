'----------------------------------------
'           FOOTBALL Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining weather and
'other game time settings.
Sub FOOTBALL ()

    _Title "3-in-1 Football v" + GAME_VERSION$ + " - GRIDIRON!"

    'Open "pbplog" For Output As #7

    Shared scheduleFile$
    Shared tickerStart

    Color 7, 0
    Cls

    Randomize Timer

    '----------------------------------------
    '        INITIALIZE ALL VARIABLES
    '----------------------------------------
    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------
    If Not _FileExists("DEV.BAS") Then

        Open "DEV.BAS" For Output As #1

        For I = 0 To 2
            Print #1, _CWD$ + "\"
        Next

        Close #1

    End If

    Open "DEV.BAS" For Input As #2

    For I = 0 To 2
        Input #2, diskPaths$(I)
    Next

    Close #2

    If Not _FileExists("DEFAULT") Then

        Open "DEFAULT" For Output As #3

        For I = 1 To 8:
            Print #3, 0
        Next

        Close #3

    End If

    Open "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, ruleOptType
    Input #4, gameLoc ' Home Field Advantage
    Input #4, pbpDelay!
    Input #4, R5%
    Input #4, ruleOptPro
    Input #4, ruleOptColl
    Input #4, NG%(14) 'Overtime setting
    Close #4

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #5

        For I = 1 To 32:
            For I1 = 0 To 1:
                For I2 = 1 To 14
                    Input #5, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #5, SX$(I, I1)
            Next
        Next

        For I = 1 To 32
            Input #5, ST%(I)
        Next

        Close #5

        TMT = 1
    Else
        TMT = 0
    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2: 
            Input #6, schedGame(I)
        Next

        Close #6

        'Read in schedule options
        Open diskPaths$(0) + scheduleFile$ As #7 Len = SCHEDULE_SIZE_BYTES

        For X = 0 To 20
            Field #7, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
        Next

        Field #7, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)

        Get #7, schedGame(0) + schedGame(2)

        For X = 0 To 20:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            YN$(X) = Q$(22 + X)
        Next

        Close #7

        autoPlay = 1

    End If


    If autoPlay = 1 Then

        Call AUTOSETUP

    Else

        goalPostAdj = 0

        Do
            Color 15, 0
            Cls
            Color 2

            Print Tab(29); "SELECT OPENING OPTIONS"
            Print
            Locate 3, 2
            Color 14: Print "(1) ";: Color 15: Print playMode$(playerOpt)
            Locate 4, 2
            Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: ";
            Color tmRat_GAME(0, 2), tmRat_GAME(0, 1): Print gameTeams$(0)
            Color , 0
            Locate 5, 2
            Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: ";
            Color tmRat_GAME(1, 2), tmRat_GAME(1, 1): Print gameTeams$(1)
            Color , 0
            Locate 6, 2
            Color 14: Print "(4) ";: Color 15: Print "TYPE OF GAME: "; rulesType$(ruleOptType)
            Locate 7, 2
            Color 14: Print "(5) ";: Color 15: Print "LOCATION: "; location$(gameLoc)
            Locate 8, 2
            Color 14: Print "(6) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; pbpDelay!
            Locate 9, 2
            Color 14: Print "(7) ";: Color 15: Print "START GAME"
            Locate 10, 2
            Color 14: Print "(8) ";: Color 15: Print "COLLEGE RULES: "; ruleYrColl$(ruleOptColl)
            Locate 11, 2
            Color 14: Print "(9) ";: Color 15: Print "PRO RULES: "; ruleYrPro$(ruleOptPro)
            Locate 12, 2
            Color 14: Print "(A) ";: Color 15: Print "SUDDEN DEATH: "; overtime$(NG%(14))
            Locate 13, 2
            Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS"

            Locate 14, 2: Print "NOTES: CLOCK IS STOPPED AFTER A 1ST DOWN IN COLLEGE BEGINNING IN 1968"
            Locate , 2: Print "       HOLDING IS A 15 YARD PENALTY FOR COLLEGE PRE-1981 AND PRO PRE-1974"
            Locate , 2: Print "       1991-PRESENT, COLLEGE HOLDING MARKED FROM POINT OF INFRACTION"
            Locate , 2: Print "       1974-77, PRO HOLDING ON A RUN IS 10 YDS AND 15 ON A PASS"
            Locate , 2: Print "       1978-1994, ALL PRO HOLDING PENALTIES ARE 10 YARDS"
            Locate , 2: Print "       PRE-1974, IN THE PROS, GOAL POSTS WERE ON THE GOAL LINE, THE BALL WAS"
            Locate , 2: Print "       PLACED ON THE 20 AFTER MISSED FIELD GOALS, KICKOFFS WERE FROM THE 40"
            Locate , 2: Print "       1992-PRESENT, COLLEGE RULES ALLOW ADVANCEMENT OF A FUMBLE"
            Locate , 2: Print "       PRE-1984, IN COLLEGE PASS INTERFERENCE AT SPOT OF INFRACTION"
            Locate , 2: Print "       PRO 1994-, KO FROM 35 TO 30, 2-PT CONVERSION, MISSED FG @ POINT OF KICK"

            Do

                Color 7

                I$ = GetKeyPress$

                I = Val(I$)

                If UCase$(I$) = "A" Then I = 10

            Loop Until I >= 0 And I <= 10 Or I$ = Chr$(27)

            'Why is this done at all???
            'We just turn around and give it
            'the value of I$, which is what
            'need to process menu options
            'I = Rnd(1)

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case I

                    Case 0:
                        Open "DEFAULT" For Output As #8
                        Print #8, playerOpt '
                        Print #8, ruleOptType '
                        Print #8, gameLoc ' Home Field advantage
                        Print #8, pbpDelay! '
                        Print #8, R5% '
                        Print #8, ruleOptPro '
                        Print #8, ruleOptColl '
                        Print #8, NG%(14) 'overtime setting
                        Close #8

                    Case 1
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        I = 0
                        Call ChooseGameTeam(NN$(I), NTMS, teamNbr, gameTeams$(I), I)
                        teamIdx_GAME(I) = teamNbr
                        X = 0

                    Case 3:
                        I = 1
                        Call ChooseGameTeam(NN$(I), NTMS, teamNbr, gameTeams$(I), I)
                        teamIdx_GAME(I) = teamNbr
                        X = 0

                    Case 4:
                        ruleOptType = ruleOptType + 1
                        If ruleOptType = 6 Then ruleOptType = 0

                    Case 5:
                        gameLoc = gameLoc + 1
                        If gameLoc = 3 Then gameLoc = 1

                    Case 6:
                        Do
                            Locate 8, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 7
                        '

                    Case 8:
                        ruleOptColl = ruleOptColl + 1
                        If ruleOptColl > 5 Then ruleOptColl = 0

                    Case 9:
                        ruleOptPro = ruleOptPro + 1
                        If ruleOptPro > 3 Then ruleOptPro = 0

                    Case 10
                        NG%(14) = NG%(14) + 1
                        If NG%(14) > 3 Then NG%(14) = 0

                End Select

                If I >= 4 Then
                    goalPostAdj = 0
                    R5% = 0
                    If ruleOptPro = 1 And ruleOptType <> 0 Then goalPostAdj = -10
                    If ruleOptType = 4 Then goalPostAdj = 0
                    If ruleOptColl = 0 And ruleOptType = 0 Then R5% = 1
                End If

            End If

        Loop Until (I = 7 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27) Or UCase$(I$) = "Q"

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

            'TICKER PROMPT
            Cls
            Color 3

            Call TEAMBOX(7, 1)

            Locate 7, 11

            Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
            Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
            Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER"
            Print
            Locate , 11: Input "START TIME: "; startTime$

            L = Len(startTime$)

            If L < 4 Then
                startTime$ = "1:00P"
            Else
                If L = 5 Then HR = Val(Left$(startTime$, 1)): MIN = Val(Mid$(startTime$, 3, 2))
                If L = 6 Then HR = Val(Left$(startTime$, 2)): MIN = Val(Mid$(startTime$, 4, 2))
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

        Else

            'End
            'System
            Exit Sub

        End If

    End If

    For I = 0 To 1:
        YR%(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
    Next I

    goalPostAdj = 0
    R5% = 0

    'Determine attendance
    If gameLoc = 2 Then

        'Neutral stadium and attendance
        gameStadium$ = "NEUTRAL FIELD"
        actualAttendance& = Int(Rnd(1) * 10000) + 1 + 5000

    Else

        If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + NN$(gameLoc)) Then
            'Generic stadium and attendance
            gameStadium$ = "MEMORIAL STADIUM"
            actualAttendance& = Int(Rnd(1) * 5000) + 10000

        Else

            Open diskPaths$(0) + DATA_FILE_NAME$ + "." + NN$(gameLoc) For Random As #9 Len = DATA_SIZE_BYTES

            Field #9, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)

            Get #9, teamIdx_GAME(gameLoc)

            gameStadium$ = RTrim$(Q$(1))
            averageAttendance& = CVL(Q$(3))
            Z$ = Q$(4)

            Close #9

            'Determine attendance randomly
            If Z$ <> "x" Then
                'Generic stadium and attendance
                gameStadium$ = "MEMORIAL STADIUM"
                actualAttendance& = Int(Rnd(1) * 5000) + 10000
            Else
                actualAttendance& = Int(Rnd(1) * 10000) + 1 - 5000 + averageAttendance&
                If actualAttendance& < 0 Then actualAttendance& = Int(Rnd(1) * 500) + 200
            End If

        End If

    End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------
    If ruleOptPro = 1 And ruleOptType <> 0 Then goalPostAdj = -10
    If ruleOptType = 4 Then goalPostAdj = 0
    If ruleOptColl = 0 And ruleOptType = 0 Then R5% = 1

    Color 15, 0

    Select Case playerOpt

        Case 1:
            compTeam = 0
        Case 2:
            playerOpt = 1
            compTeam = 1
        Case 3:
            playerOpt = 2

    End Select

    If ruleOptType <> 0 Then
        leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 2
        leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 2
    End If

    leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 4
    leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 4

    Call NAMEEDIT

    For I3 = 0 To 1

        If autoPlay = 1 Then
            'Auutoamtic determine QB
            Q7%(0) = 0
            Q7%(1) = 0

            Q6%(0) = NG%(5)

            If Q6%(0) = 4 Then
                RQ%(0) = 1
            Else
                qbMobility(0) = NG%(6)
            End If

            Q6%(1) = NG%(8)

            If Q6%(1) = 4 Then
                RQ%(1) = 1
            Else
                qbMobility(1) = NG%(9)
            End If

            For I = 0 To 3
                V4%(I3, I) = -1
            Next I

            If RQ%(I3) <> 1 Then
                For I4 = 0 To 9:
                    For I5 = 0 To 3
                        If rbacks$(I3, I4) = qbacks$(I3, I5) And I5 <> Q6%(I3) Then V4%(I3, I5) = I4
                    Next I5
                Next I4
            End If

        Else

            'Manual prompt for QB
            '-- we could probably update the routine
            '-- to handle autoPlay = 1 by itself
            Call CHANGEQB

        End If

    Next I3

    Call SETWEATHER

    '----------
    'Coin Toss 
    '----------
    W% = Int(Rnd(1) * 2)

    Call SCOUT
    Call HALFSTART

    Q = 1
    gameClock! = 900

    For I = 0 To 1
        For I1 = 0 To 4
            score(I, I1) = 0
        Next I1
    Next I

    Call SETSCREEN

    Call d100(RN)

    If RN <= 50 Then
        U$ = gameTeams$(W%) + " WON THE COIN TOSS"
    Else
        U$ = gameMascots$(W%) + " WON THE COIN TOSS"
    End If

    Call PBP


    '----------------------------------------
    '           START OF GAME
    '----------------------------------------

    '-- Start of a "kickoff" routine

    330 '
    If ruleOptType <> 0 Then
        Call KICKOFF
    Else
        Call COLKICKOFF
    End If

    340 '
    Swap P1, D

    Call MARKER1

    X = 0
    U3 = 0

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    For I = 0 To 2
        If krNumRet(D, I) = 100 Then KN = I: Exit For
    Next

    ' We found a returner for the team
    ' Now let's set them up for the kickoff
    I2 = Int(100 * Rnd(1)) + 1

    For I = 2 To 0 Step -1
        If I2 <= krNumRet(D, I) Then I1 = I
    Next I

    Call d100 (RN)

    Select Case KN

        Case 0
            U$ = kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) IS DEEP TO RECEIVE THE KICK"
            Call PBP

        Case 1
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

        Case 2
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

    End Select

    If KN > 0 And RN <= 50 Then
        U$ = kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) AND " + kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) ARE DEEP"
    Else
        U$ = kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) AND " + kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) ARE DEEP"
    End If

    Call PBP

    If playerOpt = 1 And P1 = compTeam Or playerOpt = 2 Then
        I$ = "0"

        If gameClock! <= 180 And Q = 4 And score(P1, 0) - score(D, 0) < 0 Then I$ = "2"

    Else

        U$ = "0-REGULAR  1-SQUIBB  2-ONSIDE KICK " + gameTeams$(P1)
        Call PBP

        Do
            I$ = GetKeyPress$
        Loop Until Val(I$) >= 0 And Val(I$) <= 2

    End If

    skipReturnCatch% = 0
    skipReturnYds% = 0

    Select Case Val(I$)

        Case 0
            Call KICKSET

            If N <= -9.99 Then

                Call CHANGEPOSS
                Call Touchback
                skipReturnCatch% = 1
                skipReturnYds% = 1

            Else

                X = 0

                If N < .01 Then

                    Y = -(N - 2 * N)
                    YT = 100 - Y

                    Call MARKER
                    Call YARDLINE

                    U$ = "KICK TAKEN BY " + kickRet$(P1, I1) + Str$(Int((N - 2 * N) + .5)) + " YDS DEEP IN THE END ZONE"

                    Call PBP

                    If playerOpt = 1 And P1 = compTeam Or playerOpt = 2 Then

                        I$ = "2"
                        If N < -2 Then I$ = "1"

                    Else

                        U$ = "1-DOWN IT 2-RETURN IT"
                        Call PBP
                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = "1" Or I$ = "2"

                    End If

                    If Val(I$) = 1 Then

                        Call CHANGEPOSS

                        Call Touchback

                        skipReturnYds% = 1
                        skipReturnCatch% = 1

                    Else

                        skipReturnYds% = 1

                    End If

                End If

            End If

            If skipReturnCatch% <> 1 Then

                N = N + (Int(Rnd(1) * 11) - 5) / 10
                Y = N
                YT = 100 - Y

                Call MARKER
                Call YARDLINE

                U$ = "KICK TAKEN BY " + kickRet$(P1, I1) + " AT THE" + Str$(Int(N + .5)) + " YD LINE "
                Call PBP

            End If

            '----------       SIDEBAR      ----------
            'Confirm this is supposed to be 1000
            'That means below, most cases would
            'result in the randomized options
            'This is likely what was intended,
            'because 110 represents yards gained
            'on the return (which ~ TOUCHDOWN)

            'That also means the % chance of
            'a return for TD is fairly small
            'Yet I feel like it still happens
            'often when I sim games.
            'It could also be a result of me
            'simming games where the average
            'return yards were high (ie,
            '"Greatest Teams")

            'All-in-all, a good item to review
            'and be sure the percentages are decent
            '----------------------------------------
            If skipReturnYds% <> 1 Then

                RN = Int(Rnd(1) * 1000) + 1

                Select Case krYdsPerRet(P1, I1)

                    Case Is <= 8
                        Select Case RN
                            Case 1 To 5
                                G = 110
                            Case 6 To 20
                                G = Int((Rnd(1) * 21) + 45) + (krYdsPerRet(P1, I1) - 7) * 3
                            Case 21 To 70
                                G = Int((Rnd(1) * 21) + 25) + (krYdsPerRet(P1, I1) - 7) * 3
                            Case Is >= 71
                                G = Int((Rnd(1) * 10) + 15) + (krYdsPerRet(P1, I1) - 7) * 3
                        End Select

                    Case 9
                        If RN <= 50 Then
                            G = 110
                        Else
                            G = Int((Rnd(1) * 19) + 15)
                        End If

                    Case Is >= 10
                        If RN <= 80 Then
                            G = 110
                        Else
                            G = Int((Rnd(1) * 19) + 15) + (krYdsPerRet(P1, I1) - 10) * 3
                        End If

                End Select

                SY = 100 - Y
                EY = SY - G

                Call MOVEBALL

                X1 = 3
                K2(P1, I1, 14) = K2(P1, I1, 14) + 1

                If G + N > 89.99 Then

                    K2(P1, I1, 15) = K2(P1, I1, 15) + 100 - N
                    K2(P1, I1, 16) = K2(P1, I1, 16) + 1
                    If 100 - N > KR(P1, I1) Then strKR$(P1, I1) = "t": KR(P1, I1) = 100 - N

                    GoTo 3200

                End If

                If G + N <= -.05 Then

                    Call CHANGEPOSS
                    Call Touchback

                Else

                    Y = G + N

                    If G > KR(P1, I1) Then strKR$(P1, I1) = "": KR(P1, I1) = G

                    K2(P1, I1, 15) = K2(P1, I1, 15) + G
                    F% = 3

                    Call FUMBLE

                    If F9 = 1 Then Call FumblePBP: GoTo 650

                    Call AddPossessionTime(4, 7)

                    I = Int(Rnd(1) * 1000) + 1
                    If I >= 960 - tmRat_GAME(P1, 0) Then Call CLIPPING

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                End If

            End If

        Case 1
            'SQUIB KICK
            Call SQUIB
            Call FUMBLE

            If F9 = 1 Then Call FumblePBP: GoTo 650

            Call AddPossessionTime(4, 7)
            Call d100(I)

            If I >= 97 Then
                Call CLIPPING
            End If

            Call CheckGameClock
            If gameClock! = 0 Then GoTo 3360

        Case 2
            'NORMAL KICK
            Call ONSIDE
            D1 = 1
            Y1 = 10

            Call CheckGameClock
            If gameClock! = 0 Then GoTo 3360

    End Select

    '--- End of a "kickoff" routine


    GoTo 700


    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    '----- NOODLE -----
    'If we can get rid of references
    'to labels in the main game then
    'this can be a formal subroutine

    PointsAfterTD:

    If ruleOptType = 4 Then
        'WFL = No PAT
        I$ = "2"

    Else

        U$ = "1-KICK EP 2-GO FOR TWO"

        Call PBP

        If playerOpt = 1 And P1 = compTeam Or playerOpt = 2 Then

            I$ = "1"

            I5 = score(P1, 0) - score(D, 0)

            If ruleOptType = 1 And ruleOptPro = 3 Then
                If Q = 4 And (I5 = -9 And gameClock! < 600 Or I5 = -2 And gameClock! < 120 Or I5 = -5 And gameClock! < 420) Then I$ = "2"
            End If

            'Even after rewriting this is still seems absurd
            If I5 = -2 Or I5 = 1 Or I5 = 5 Or I5 = 12 Then
                I$ = "2"
            Else
                If I5 = -1 Or I5 = -5 Or I5 = -8 Or I5 = -13 Or I5 = -4 Or I5 = -10 Then
                    I$ = "2"
                Else
                    'Is this logic accurate?
                    'Or just an artifact of how things
                    'were written before???
                    'If Q = 4 And gameClock! > 240 Or Q < 4 Then 3702
                End If
            End If

        Else
            I$ = GetKeyPress$
        End If

    End If

    '3702
    If ruleOptType = 1 Then

        If ruleOptPro <> 3 Then

            I$ = "1"

            If Val(I$) = 2 Then

                G = 0
                Y = 97
                E = 1

                If ruleOptType = 1 And ruleOptPro = 3 Then Y = 98

                GoTo 700

            End If

        End If

    Else

        I$ = "1"

    End If

    K1(X2, 4) = K1(X2, 4) + 10

    Call d100(RN1)

    'Was previously I2 not J
    For J = 1 To 0 Step -1:
        If RN1 <= kickerPATPct(P1, J) Then I1 = J
    Next J

    Q7%(P1) = I1

    Call d100(RN1)

    K2(P1, 6 + Q7%(P1) * 4, 17) = K2(P1, 6 + Q7%(P1) * 4, 17) + 1

    U$ = kicker$(P1, Q7%(P1)) + " IS ON TO TRY THE EXTRA POINT"

    Call PBP

    If RN1 <= kickerNumPAT(P1, Q7%(P1)) Then

        K2(P1, 5 + Q7%(P1) * 4, 17) = K2(P1, 5 + Q7%(P1) * 4, 17) + 1

        U$ = "THE KICK IS UP..."
        Call PBP
        Call MADEPAT

        score(P1, 0) = score(P1, 0) + 1
        score(P1, Q) = score(P1, Q) + 1
        K1(X2, 6) = Q7%(P1)
        K1(X2, 4) = K1(X2, 4) + 1

        Call DisplayField

    Else

        Call d100(RN2)

        If RN2 <= 9 Then

            U$ = "KICK IS BLOCKED!!"
            Call PBP

            If ruleOptType <> 1 Then
                GoTo 6605
            End If

        Else

            Call MISSPAT

        End If

    End If

    Return 'For PointsAfterTD "sub"


    '----- NOODLE -----

    650 '

    Call d100(I6)

    K(P1, 9) = K(P1, 9) + 1

    If I6 <= 50 Then

        Call PBPINT

        U$ = U$ + gameTeams$(D) + " RECOVERS!!"
        Call PBP

        D1 = 1
        Y1 = 10
        K(P1, 10) = K(P1, 10) + 1
        Y = 100 - Y
        Call CHANGEPOSS

        '659
        If ruleOptType <> 0 Or ruleOptColl >= 5 Then

            I = Int(Rnd(1) * 3) + 1

            If I > 2 Then

                G1 = Int(Rnd(1) * 48) + 1: X1 = 10

                If G1 >= 46 Then
                    G1 = 100
                Else
                    G1 = Int(Rnd(1) * 18)
                End If

                G1 = G1 + (Int(Rnd(1) * 11) - 5) / 10

                U$ = "RETURNED FOR "

                returnYards = G1 + Y

                If returnYards >= 99.99 Then
                    U$ = U$ + Str$(Int((100 - Y) * 10 + .5) / 10) + " YDS "
                    Call PBP
                    GoTo 3200
                Else
                    'returnYards < 99.99
                    U$ = U$ + Str$(Int((G1) * 10 + .5) / 10) + " YDS"
                    Call PBP
                    If returnYards <= .01 Then
                        GoTo 3250
                    End If

                    Y = Y + G1

                End If

            End If

        End If

        If Y >= 90 Then Y1 = 100 - Y
    Else
        'I6 > 50 Then
        U$ = gameTeams$(P1) + " RECOVERS!!"
        Call PBP

        I = 50
        O = 1

        GoTo 3303

    End If

    O = 5

    GoSub 3900

    'College Overtime
    If NG%(14) = 3 And Q > 4 Then 3364

    700 '
    Call MARKER1

    Call DISPWEATHER

    O = 0
    F9 = 0
    G = 0
    W5 = 0

    If U3 = 9 Then U3 = 0: GoTo 340

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    Do

        Call SCOREBOARD

        Call DEFMENU

        'Call pbplog(1351)
        'Offense Play Selection
        validPlay% = 1
        autoPlaySelect% = 0

        Do

            If playerOpt = 1 And P1 = compTeam Or playerOpt = 2 Then

                Call OFFMENU

                RP = P2

                If P2 > 8 Then RP = P2 + 6

                autoPlaySelect% = 1

            Else

                Do
                    3118 '
                    Call FRAMEOPTION(10, P1)
                    Print "SELECT OFFENSE "; gameTeams$(P1)

                    For P2 = 0 To 4:
                        Locate 12 + P2, 14
                        Print P2; "-"; PS$(P2); Tab(41); P2 + 5; "-"; PS$(P2 + 5)
                    Next P2

                    Locate , 15
                    Print "G -GADGET PLAY"

                    Do

                        I$ = GetKeyPress$

                        If I$ = "8" Then I$ = "T"

                        If UCase$(I$) = "Q" Then
                            'End
                            'System
                            Exit Sub
                        End If

                        If UCase$(I$) = "G" Then

                            Call FRAMEOPTION(8, P1)
                            Print "SELECT GADGET PLAY "; gameTeams$(P1)

                            For G2 = 1 To 3:
                                Locate 11 + G2, 14
                                Print G2; "- "; G$(G2)
                            Next G2

                            Do
                                I$ = GetKeyPress$

                                If I$ <> Chr$(27) Then
                                    G2 = Val(I$)
                                End If

                            Loop Until G2 >= 1 And G2 <= 3 Or I$ = Chr$(27)

                            Screen , , 0, 0

                            If I$ = Chr$(27) Then GoTo 3118

                            Select Case G2

                                Case 1
                                    ' *** FLEA FLICKER ***
                                    Call FLEA

                                    If RN <= S% Then 
                                        R1 = 1
                                        W5 = 1
                                        G = (Int(Rnd(1) * 6) + 10)

                                        'Sacked
                                        GoTo 3541

                                    Else

                                        Call d100(RN)

                                        If RN <= intChance Then 

                                            G = CP
                                            FF% = 2
                                            GoTo 3552
                                        
                                        Else

                                            Call d100(RN)
                                            RV$ = wdRec$(P1, I3): I3 = I3 + 10

                                            If RN <= C% Then 
                                                FF% = 2
                                                GoTo 3586
                                            Else
                                                G = 0
                                                GoTo 3535
                                            End If

                                        End If

                                    End If

                                Case 2
                                    ' *** REVERSE PLAY ***
                                    RV% = 1

                                    GoSub 3128

                                    Call QBLINE

                                    If KJ = 1 Then

                                        KJ = 0
                                        R1 = 1
                                        I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0)
                                        Call REVGAIN
                                        GoTo 3416

                                    Else

                                        3181 '
                                        Call d100(I2)

                                        'Check contribution % and
                                        'assign rusher index to I1
                                        For I = 9 To 0 Step -1:
                                            If I2 <= rbRushAtt(P1, I) Then I1 = I
                                        Next I

                                        R1 = 1

                                        If I1 = (V4%(P1, 0) Or I1 = V4%(P1, 1) Or I1 = V4%(P1, 2) Or I1 = V4%(P1, 3)) Then

                                            If rbNumRec(P1, I1) > 0 Then 
                                                GoTo 3403
                                            Else

                                                For I5 = 0 To 9
                                                    If rbacks$(P1, I5) = qbacks$(P1, Q6%(P1)) Then 
                                                        I1 = I5
                                                        GoTo 3403
                                                    End If
                                                Next

                                                If RV% = 1 Then 
                                                    GoTo 3181
                                                Else
                                                    GoTo 3401
                                                End If

                                            End If

                                        Else

                                            I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + WX%(4)

                                            If RV% = 1 Then
                                                Call REVGAIN
                                                GoTo 3416
                                            Else
                                                GoTo 3403
                                            End If

                                        End If

                                    End If

                                Case 3
                                    ' *** HB PASS ***
                                    HB% = 1

                                    Call FRAMEOPTION(11, P1)
                                    Print "SELECT PASSER":
                                    For KK = 0 To 4
                                        Locate 12 + KK, 14
                                        Print KK; "-"; rbacks$(P1, KK); Tab(41); KK + 5; "-"; rbacks$(P1, KK + 5)
                                    Next KK

                                    Do
                                        I$ = GetKeyPress$
                                        I1 = Val(I$)
                                    Loop Until I1 >= 0 And I1 <= 9

                                    Screen , , 0, 0

                                    C% = 5

                                    If (D3 = 1 Or D3 = 3) And HB%(P9) = 0 Then C% = 30

                                    intChance = 20
                                    Call d100(RN)

                                    G = Int(Rnd(1) * 71) + 30
                                    CP = Int(Rnd(1) * 11) + 30

                                    HB%(P9) = 1

                                    U$ = qbacks$(P1, Q6%(P1)) + " TAKES THE SNAP AND HANDS OFF TO " + rbacks$(P1, I1)
                                    Call PBP

                                    U$ = rbacks$(P1, I1) + " HAS THE BALL AND WANTS TO THROW!!!"
                                    Call PBP

                                    Call d100(RN)
                                    I3 = Int(Rnd(1) * 3)

                                    U$ = "HE HAS " + wdRec$(P1, I3) + " OPEN DOWNFIELD"
                                    Call PBP

                                    If D3 = 10 Or D3 = 11 Or D3 = 8 And RN <= 35 Then 
                                        G = (Int(Rnd(1) * 4) + 4)
                                        'Sacked
                                        GoTo 3541
                                    End If

                                    Call d100(RN)
                                    If RN <= intChance Then 
                                        G = CP
                                        FF% = 1
                                        HB% = 0
                                        GoTo 3552
                                    End If

                                    Call d100(RN)
                                    RV$ = wdRec$(P1, I3)
                                    I3 = I3 + 10

                                    If RN <= C% Then FF% = 1: GoTo 3586
                                    G = 0

                                    GoTo 3535

                            End Select

                        End If

                        If UCase$(I$) = "T" Then 850

                        XX = Val(I$)

                    Loop Until XX >= 0 And XX <= 9

                    Screen , , 0, 0

                    Select Case XX

                        Case 0

                            Call RUNMENU

                            Do
                                Locate 20, 14: Print "             SELECT PLAY ";: Input P2
                            Loop Until P2 >= 0 And P2 <= 15

                            If P2 = 0 Then Screen , , 0, 0: GoTo 3118
                            RP = P2
                            If P2 = 9 Or P2 = 10 Then P2 = 4
                            If P2 = 11 Then P2 = 7
                            If P2 = 12 Then P2 = 3
                            If P2 = 13 Then P2 = 6
                            If P2 = 14 Then P2 = 7
                            If P2 = 15 Then P2 = 2

                        Case 1
                            Call PASSMENU
                            Do
                                Locate 19, 14: Print "             SELECT PLAY ";: Input P2
                            Loop Until P2 >= 0 And P2 <= 14
                            Screen , , 0, 0

                            If P2 = 0 Then GoTo 3118
                            RP = P2 + 15
                            If P2 = 11 Or P2 = 12 Then P2 = 3
                            If P2 = 13 Or P2 = 14 Then P2 = 4
                            P2 = P2 - 1
                            P2 = P2 + 9

                        Case Is >= 2
                            P2 = XX + 17

                    End Select

                Loop Until P2 <> 19 Or Y < 70

                If P2 = 26 Then P2 = 24

                If XX = 8 Then
                    U$ = gameTeams$(P1) + " IS LETTING THE PLAY CLOCK WIND DOWN"
                    Call PBP

                    U$ = "PENALTY - " + gameTeams$(P1) + " - DELAY OF GAME"
                    Call PBP

                    O = 8

                    I2 = 0
                    P = 5
                    GoTo 3326
                End If
                If XX = 7 Then P2 = 9: R1 = 1: GoTo 3403

                If (Q = 3 Or Q = 1) And P2 = 16 Or gameClock! > 120 And P2 = 16 Then
                    U$ = "CAN ONLY USE LAST 2 MIN. OF EACH HALF"
                    Call PBP
                    validPlay% = 0
                End If

            End If

        Loop Until validPlay% = 1

        'Call pbplog(1606)

        If autoPlaySelect% = 0 Then

            If P2 = 8 Then
                Screen , , 0, 0
            End If

            If P2 >= 1 And P2 <= 8 Then

                3128 '
                ' !!! Beginning of a sub !!!
                ' but where does it end???
                KJ = 1
                Call FRAMEOPTION(11, P1)

                Print "SELECT BALL CARRIER":

                For KK = 0 To 4
                    Locate 12 + KK, 14
                    Print KK; "-"; rbacks$(P1, KK);
                    If K(P1, 1) >= 20 And K2(P1, KK, 0) / K(P1, 1) > PC%(P1, KK) / 100 + .1 Then Print " ###" Else Print
                    If rbRushAtt(P1, KK) = 100 Then GoTo CSELECT
                Next

                For KK = 0 To 4
                    Locate 12 + KK, 41
                    Print KK + 5; "-"; rbacks$(P1, KK + 5);
                    If K(P1, 1) >= 20 And K2(P1, KK + 5, 0) / K(P1, 1) > PC%(P1, KK + 5) / 100 + .1 Then Print " ###" Else Print
                    If rbRushAtt(P1, KK + 5) = 100 Then GoTo CSELECT
                Next KK

                CSELECT:
                Locate 17, 28: Print "C-COMPUTER SELECT"

                Do
                    I$ = GetKeyPress$

                    If UCase$(I$) = "C" Then
                        KJ = 0
                    Else
                        I1 = Val(I$)
                    End If

                Loop Until I1 >= 0 And I1 <= 9 Or KJ = 0
                'End If

                Screen , , 0, 0

            End If

        End If

        'Call pbplog(1658)

        If playerOpt = 1 And P1 = compTeam And (P2 = 12 Or P2 = 15) And Y >= 85 Or playerOpt = 2 And (P2 = 12 Or P2 = 15) And Y >= 85 Then P2 = 11

        If timeouts(P1) > 0 And Y > 80 And Int(Rnd(1) * 25) + 1 = 16 Then
            U$ = "THERE'S SOME CONFUSION AT THE LINE OF SCRIMMAGE"
            Call PBP

            U$ = qbacks$(P1, Q6%(P1)) + " HAS TO CALL TIME OUT TO AVOID DELAY OF GAME"
            Call PBP

            timeouts(P1) = timeouts(P1) - 1
            QBN% = 0

        End If

        'Call pbplog(1675)

    Loop While (timeouts(P1) > 0 And Y > 80 And Int(Rnd(1) * 25) + 1 = 16)

    'Call pbplog(1680)

    Call SCOREBOARD

    Call MARKER1

    If playerOpt = 1 And D = compTeam Then Call COMPCHEAT

    If QBN% = 1 Then QBN% = 0: GoTo 3403

    I = Int(Rnd(1) * 1000) + 1
    'I = Int(Rnd(1) * 100) + 1

    If I <= 36 And E <> 1 And P2 < 21 Then

        U$ = "*** PENALTY - "
        I = Int(Rnd(1) * 36) + 1

        Select Case I

            Case 1 To 2 + tmRat_GAME(P1, 0)
                U$ = U$ + "OFFENSE... OFFSIDES 5 YARDS ***"
                Call PBP
                P = -5

            Case 2 + tmRat_GAME(P1, 0) To 20 + tmRat_GAME(P1, 0)
                U$ = U$ + "ILLEGAL PROCEDURE OFFENSE... 5 YARDS ***"
                Call PBP
                P = -5

            Case 20 + tmRat_GAME(P1, 0) To 27 + tmRat_GAME(P1, 0)
                U$ = U$ + "OFFENSE... DELAY OF GAME 5 YARDS ***"
                Call PBP
                P = -5

            Case 27 + tmRat_GAME(P1, 0) To 36 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                U$ = U$ + "DEFENSE... ENCROACHMENT 5 YARDS ***"
                Call PBP
                P = 5

        End Select

        O = 8

        If P > 0 Then
            GoTo 3330
        Else
            P = P * -1
            GoTo 3326
        End If

    End If

    Select Case P2

        Case 1 To 8

            ' ****  RUNNING PLAYS  ****

            Call QBLINE

            If KJ = 1 Then

                KJ = 0
                R1 = 1
                I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + WX%(4)

                3403 '
                'rbRushAvg(P1, I1) is rushing average

                I2 = rbRushAvg(P1, I1)
                RY = I2

                'We essentially restore our value of I2 here
                'RY will be negative for several "OTHERS" entries
                'This is stored as a larger value than the actual
                'average, and therefore almost never becomes a
                'positive integer

                I2 = I2 + leagRat_GAME(D, 0) + WX%(4)

                If K(P1, 1) >= 20 And K2(P1, I1, 0) / K(P1, 1) > PC%(P1, I1) / 100 + .1 Then I2 = Int(I2 * .4)

                RN = Int(Rnd(1) * 200) + 1

                If RN = 97 And Y <= 98.899 And Y >= 1.101 Then

                    U$ = qbacks$(P1, Q6%(P1)) + " FUMBLES THE SNAP!"
                    Call PBP

                    G = Int((Rnd(1) * 20) - 10) / 10

                    X = 4
                    X1 = 9
                    I1 = Q6%(P1)

                    If Y + G < 99.94999 Then
                        If G - Y1 >= -.3 And G - Y1 <= .3 Then Call MEASUREMENT
                    End If

                    U$ = "HE GAINS" + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    If E = 1 Then Call GoForTwo

                    'There was a RETURN in the original code
                    'But it was always sent to with GOTO
                    'So where was it returning from?

                    F9 = 1

                    Call COMPILER

                    Y = Y + G
                    Y1 = Y1 - G
                    D1 = D1 + 1
                    If D1 = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                    GoTo 650

                End If

                If P2 = 8 Then

                    Call QBSNEAK

                    If E = 1 Then Call GoForTwo

                    If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then 3200
                    If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250

                    Call FUMBLE

                    GoTo 3309

                End If

                If P2 = 9 Then
                    U$ = qbacks$(P1, Q6%(P1)) + " TAKES THE SNAP AND KNEELS DOWN FOR "
                    Call PBP
                    G = -1.0
                    GoTo 3546
                End If

                Call d100(RN0)

                If P2 = 5 And RN0 = 49 Then

                    U$ = qbacks$(P1, Q6%(P1)) + " MAKES A BAD PITCH ON THE OPTION!!"
                    Call PBP
                    
                    G = Int((Rnd(1) * 7) + 6) * -1
                    X = 4
                    X1 = 9
                    I1 = Q6%(P1)
                    U$ = "HE LOSES" + Str$(Int((G * -1) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    If Int((G + Y) * 10 + .5) / 10 < .05 Then 

                        F9 = 0
                        GoTo 3250

                    Else

                        If E = 1 Then Call GoForTwo

                        F9 = 1

                        Call COMPILER

                        Y = Y + G
                        Y1 = Y1 - G
                        D1 = D1 + 1
                        If D1 = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        GoTo 650

                    End If

                End If

                Call HANDOFF

                If D2 = 11 Then

                    Call GOALLINE

                Else

                    I2 = I2 + D3%(P2, D2)

                    S6 = 0

                    If K(P1, 1) + K(P1, 3) <> 0 Then
                        If (K(P1, 1) / (K(P1, 1) + K(P1, 3))) > (leagRat_GAME(P1, 7) / 100) And K3(P1, 0) - (leagRat_GAME(D, 0) * .3) > 4.5 And (leagRat_GAME(D, 0) * .3) > 4.5 And leagRat_GAME(P1, 7) < 50 Then
                            S6 = Int(((K3(P1, 0) - 3.6) / .3) + .5) * -1
                        End If
                    End If

                    I2 = I2 + S6

                    If P1 = gameLoc Then I2 = I2 + 1
                    If P1 <> gameLoc And gameLoc <> 2 Then I2 = I2 - 1

                    If I2 < 1 Then I2 = 1

                    'I2 = 1 to 38
                    If I2 <= 38 Then

                        If (P2 <= 2 And D2 = 5 Or P2 <= 2 And D2 = 6 Or P2 >= 6 And D2 = 5 Or P2 >= 6 And D2 = 6) Or (P2 > 2 And P2 < 5 And D2 = 7 Or P2 > 2 And P2 < 5 And D2 = 8) Then

                            ' *** STUNTS ***
                            Call d100(I4)

                            Select Case I2

                                Case Is <= 15
                                    Select Case I4
                                        Case 1 To 15: G = Int(Rnd(1) * 3) - 2
                                        Case 16 To 60: G = Int(Rnd(1) * 2) + 1
                                        Case 61 To 100: G = Int(Rnd(1) * (I2 * 2.13)) + 1
                                    End Select

                                Case 16, 17
                                    Select Case I4
                                        Case 1 To 10: G = Int(Rnd(1) * 3) - 2
                                        Case 11 To 50: G = Int(Rnd(1) * 2) + 1
                                        Case 51 To 100: G = Int(Rnd(1) * (I2 * 1.73)) + 1
                                    End Select

                                Case 18, 19
                                    Select Case I4
                                        Case 1 To 10: G = Int(Rnd(1) * 3) - 2
                                        Case 11 To 40: G = Int(Rnd(1) * 2) + 1
                                        Case 41 To 100: G = Int(Rnd(1) * (I2 * 1.5)) + 1
                                    End Select

                                Case 20, 21
                                    Select Case I4
                                        Case 1 To 5: G = Int(Rnd(1) * 3) - 2
                                        Case 6 To 30: G = Int(Rnd(1) * 2) + 1
                                        Case 31 To 100: G = Int(Rnd(1) * (I2 * 1.3)) + 1
                                    End Select

                                Case Is >= 22
                                    Select Case I4
                                        Case 1 To 5: G = Int(Rnd(1) * 3) - 2
                                        Case 6 To 20: G = Int(Rnd(1) * 2) + 1
                                        Case 21 To 100: G = Int(Rnd(1) * (I2 * 1.2)) + 1
                                    End Select

                            End Select

                            GoTo 3416

                        End If

                        Call d100(I4)

                        If (P2 <= 2 And I4 <= 50 And D3 = 7) Or (P2 >= 6 And I4 <= 50 And D3 = 7) Or (P2 <= 2 And D3 = 9) Or (P2 >= 6 And D3 = 9) Then
                            Call BLITZ2
                        End If

                        If P2 >= 3 And P2 <= 5 Then
                            If D3 = 8 And I4 <= 50 Or D3 = 10 Then
                                Call BLITZ2
                            End If
                        End If

                        If D3 = 11 Then

                            Call BLITZ2

                        Else

                            If I2 > 17 Then

                                G = Int(Rnd(1) * 48) + 1

                                Select Case G

                                    Case 1 To 44
                                        G = (Int(Rnd(1) * 11) - 1) - (((18 - I2) * 2) / 10)

                                    Case 45
                                        G = Int(Rnd(1) * 2 * I2) + 1

                                    Case 46 To 47
                                        G = Int(Rnd(1) * I2) + 1

                                    Case 48
                                        G = Int(Rnd(1) * 5 * I2) + 1

                                End Select

                            Else

                                Call d100(I4)

                                If I4 > 92 Then
                                    ' *** BREAKAWAY ***
                                    Call d100(I5)
                                    If I5 <= 50 Then
                                        G = Int(Rnd(1) * BY%(I2, 0)) + BY%(I2, 1)
                                    Else
                                        G = Int(Rnd(1) * BY%(I2, 3)) + BY%(I2, 4)
                                    End If

                                Else

                                    I5 = 0

                                    If P2 = 4 Or P2 = 5 Then I5 = 1

                                    For I6 = 0 To 14:
                                        If I4 <= RM%(I5, I6) Then G = I6 - 5: Exit For
                                    Next I6

                                    Call d100(I4)
                                    If I4 <= RN%(0, I2) Then G = G + RN%(1, I2)

                                End If

                            End If

                        End If 'Done checking div3$

                    Else
                        'I2 = 39 +
                        G = Int(Rnd(1) * 48) + 1

                        Select Case G

                            Case 1 To 44
                                G = (Int(Rnd(1) * 11) - 1) - (((18 - I2) * 2) / 10)

                            Case 45
                                G = Int(Rnd(1) * 2 * I2) + 1

                            Case 46 To 47
                                G = Int(Rnd(1) * I2) + 1

                            Case 48
                                G = Int(Rnd(1) * 5 * I2) + 1

                        End Select

                    End If

                End If

                3416 '
                Call YARDAGE

                If G <= 0 Then Call STUFFED

                If G > 10 And Y + G < 99.94999 Then Call LGPBP

                If Y + G >= 99.94999 Then Call RUNTDPBP

                SY = 100 - Y
                EY = SY - G
                Call MOVEBALL

                If Y + G < 99.94999 Then

                    F% = 2

                    Call FUMBLE

                    If F9 <> 1 Then
                        If G - Y1 >= -.3 And G - Y1 <= .3 Then Call MEASUREMENT
                    End If

                End If

                If G < 0 Then

                    U$ = rbacks$(P1, I1) + " LOST"
                    Print
                    X = 1
                    X1 = 1
                    U$ = U$ + Str$(Int((G * -1) * 10 + .5) / 10)

                Else

                    Call RUNPBP

                    X = 1
                    X1 = 1

                    If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then
                        U$ = U$ + Str$(Int((100 - Y) * 10 + .5) / 10)
                    Else
                        U$ = U$ + Str$(Int((G) * 10 + .5) / 10)
                    End If

                End If

                U$ = U$ + " YARDS"
                Call PBP

                If RV% > 0 Then RV% = 0

                If E = 1 Then F9 = 0: Call GoForTwo

                If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then F9 = 0: GoTo 3200
                If Int((G + Y) * 10 + .5) / 10 < .05 Then F9 = 0: GoTo 3250

                GoTo 3309

            Else

                3401 '
                Call d100(I2)

                For I = 9 To 0 Step -1:
                    If I2 <= rbRushAtt(P1, I) Then I1 = I
                Next I

                R1 = 1

                'If I1 = (V4%(P1, 0) Or I1 = V4%(P1, 1) Or I1 = V4%(P1, 2) Or I1 = V4%(P1, 3)) And RV% = 1 Then 8337
                If I1 = (V4%(P1, 0) Or I1 = V4%(P1, 1) Or I1 = V4%(P1, 2) Or I1 = V4%(P1, 3)) Then

                    If rbNumRec(P1, I1) > 0 Then 
                        GoTo 3403
                    Else

                        For I5 = 0 To 9
                            If rbacks$(P1, I5) = qbacks$(P1, Q6%(P1)) Then I1 = I5: GoTo 3403
                        Next

                        If RV% = 1 Then 
                            GoTo 3181
                        Else
                            GoTo 3401
                        End If

                    End If

                Else

                    I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + WX%(4)

                    If RV% = 1 Then
                        Call REVGAIN
                        GoTo 3416
                    Else
                        GoTo 3403
                    End If

                End If

            End If 'Done checking KJ

        Case 9 To 18

            Call PASSBALL

            'SACKED
            If I < S1%(3, P2 - 8, D3) + leagRat_GAME(P1, 5) + leagRat_GAME(D, 6) + 1 Then

                R1 = 1
                W5 = 1

                ' *** SACKED ***
                If P2 = 12 Or P2 = 15 Then G = (Int(Rnd(1) * 9) + 5)
                If P2 = 10 Or P2 = 9 Or P2 = 13 Then G = (Int(Rnd(1) * 5) + 3)
                If P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18 Then G = (Int(Rnd(1) * 7) + 4)

                3541 '
                Call YARDAGE
                Call SACK

                'Yardage for sack is negative
                G = G * -1

                If defSacks(1 - P1) <> 0 Then

                    M5 = 0
                    RN = Int(Rnd(1) * DDS(1 - P1)) + 1

                    For Z1 = 0 To 14

                        M5 = M5 + sacks(1 - P1, Z1)

                        If RN <= M5 Then
                            U$ = U$ + " BY " + defSacks$(1 - P1, Z1)
                            Exit For
                        End If

                    Next

                End If

                '3699
                Call PBP

                X = 4
                R1 = 1

                If E = 1 Then Call GoForTwo

                If Int((G + Y) * 10 + .5) / 10 < .05 Then 
                    GoTo 3250
                Else
                    F% = 1
                    Call FUMBLE
                End If

                GoTo 3309

            End If

            'FORCED TO RUN ON BOOTLEG/ROLLOUT/KEEPER
            rollout% = 0

            'When would mobility ever be 0???
            If qbMobility(P1) > 0 Then
                Call d100(I)
                If I <= qbMobility(P1) Then GoTo 3545
            End If

            If qbMobility(P1) > 0 Then GoTo 3532

            If (P2 = 9 Or P2 = 10 Or P2 = 13) And I <= 5 Then rollout% = 1
            If (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17) And I <= 10 Then rollout% = 1
            If (P2 = 12 Or P2 = 15) And I <= 15 Then rollout% = 1
            If P2 = 18 And I <= 35 Then rollout% = 1

            If rollout% = 1 Then
                3545 '
                If qbMobility(P1) > 0 Then
                    'Yardage gain = 0-12, based on mobility
                    'R1 = 4/3 of rating, rounded up
                    'Gain is from 0 to R1
                    R1 = (Int((qbMobility(P1) * .67) + .5)) * 2
                    G = Int(Rnd(1) * R1)
                Else
                    'Mobility = 0
                    'Yardage gain = 1-12
                    G = Int(Rnd(1) * 12) + 1
                End If

                Call YARDAGE

                'QB Keeper
                If P2 = 17 Or P2 = 18 Then

                    'QB RUN
                    I6 = 0

                    For I4 = 0 To 9
                        If rbacks$(P1, I4) = qbacks$(P1, Q6%(P1)) Then
                            I6 = rbRushAvg(P1, I4)
                            Exit For
                        End If
                    Next I4

                    I7 = Int(Rnd(1) * 10) + 1

                    If I6 = 0 Then
                        If I7 <= 9 Then
                            G = Int(Rnd(1) * 5)
                        Else
                            G = Int(Rnd(1) * 16)
                        End If
                    Else
                        If I7 <= 9 Then
                            G = Int(Rnd(1) * 7)
                        Else
                            G = Int(Rnd(1) * (12 + ((I6 - 13) * 3) * 2))
                        End If
                    End If

                    Call YARDAGE

                    SY = 100 - Y
                    EY = SY - G
                    Call MOVEBALL

                    If P2 = 17 Then
                        U$ = qbacks$(P1, I1) + " KEEPS IT ON THE ROLLOUT AND RUNS FOR"
                    Else
                        U$ = qbacks$(P1, I1) + " KEEPS IT ON THE BOOTLEG AND RUNS FOR"
                    End If

                Else
                    U$ = qbacks$(P1, I1) + " IS CHASED FROM THE POCKET"
                    Call PBP
                    U$ = "HE IS FORCED TO RUN FOR"
                End If

                3546 '
                X = 4
                X1 = 9
                R1 = 1

                If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then
                    U$ = U$ + Str$(Int(100 - Y))
                Else
                    U$ = U$ + Str$(Int((G) * 10 + .5) / 10)
                End If

                U$ = U$ + " YARDS"

                Call PBP

                If E = 1 Then Call GoForTwo

                If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then K(P1, 0) = K(P1, 0) + 1: GoTo 3200

                If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250

                F% = 2

                Call FUMBLE

                GoTo 3309
            Else

                3532 '
                'Pass Completion %
                Call d100(I)

                'INTERCEPTED
                If I > (100 - qbPctInt(P1, I1) - S1%(0, P2 - 8, D3) + leagRat_GAME(D, 2)) Then 3552

                Call ARMADJUST

                If I <= S2 + S3 + PQ + PW Then
                    'Pass completed

                    If I3 > 9 Then
                        I4 = wrYdsPerC(P1, I3 - 10)
                        RY = I4
                    Else
                        I4 = rbYdsPerC(P1, I3)
                        RY = I4
                    End If

                    If WX%(4) = -3 Then I4 = I4 - 1
                    If WX%(4) = -6 Then I4 = I4 - 2

                    If I4 < 11 Then

                        Call BLITZ

                    Else

                        I4 = I4 + S1%(2, P2 - 8, D3)

                        If ydsPerComp(D) / 100 > .5 Then I4 = I4 - (1 - ydsPerComp(D) / 100) * 10
                        If ydsPerComp(D) / 100 < .5 Then I4 = I4 + ydsPerComp(D) / 100 * 10

                        Call OVERUSE

                        If P2 = 9 Or P2 = 10 Or P2 = 13 Then I4 = I4 * ((((S2 + S6) * K3(P1, 3)) / (S2 + S6 + 18)) / K3(P1, 3))
                        If P2 = 12 Or P2 = 15 Then I4 = I4 * ((((S2 + S6) * K3(P1, 3)) / (S2 + S6 - 18)) / K3(P1, 3))

                        I = Int(Rnd(1) * 25) + 1

                        If P2 <> 12 And P2 <> 15 And I = 25 Or (P2 = 12 Or P2 = 15) And I = 25 And ruleOptType = 0 Or (P2 = 12 Or P2 = 15) And ruleOptType <> 0 And I >= 24 Then
                            G = Int(Rnd(1) * I4 * 12) + 1 + (I4 * 6)
                            If I = 25 And Int(Rnd(1) * 32) = 11 And score(P1, 0) > 13 And XD(D) > .6 Then G = 102
                        Else
                            If I <= 19 Then
                                G = Int(Rnd(1) * 13) + 1 + ((I4 * 2.3) - 7.2)
                            Else
                                G = (Int(Rnd(1) * (5 + (I4 * 2))) + 1) + (3 * (I4 - 1))
                            End If
                        End If

                    End If

                    3586 ' 
                    'ADDED AFTER REPLAYS OF 5-24-93
                    If Int(Rnd(1) * 3) + 1 = 1 And ruleOptType <> 0 Then G = G - 1

                    Call YARDAGE

                    X = 3
                    X1 = 2

                    If G + Y > 99.9499 Then G = 109 - Y

                    Call PCPBP

                    If FF% <> 0 Then FF% = 0

                    If Y + G < 99.9499 Then

                        F% = 2
                        Call FUMBLE

                        If F9 <> 1 Then
                            If G - Y1 >= -.3 And G - Y1 <= .3 Then Call MEASUREMENT
                        End If

                    End If

                    If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then

                        U$ = "THAT CATCH BY "
                        Call WHICHRECEIVER
                        U$ = U$ + " GAINED" + Str$(Int(100 - Y)) + " YARDS"

                        G = 100 - Y
                        If G > WR(P1, I3) Then strWR$(P1, I3) = "t": WR(P1, I3) = G
                        If G > QB(P1, Q6%(P1)) Then strQB$(P1, Q6%(P1)) = "t": QB(P1, Q6%(P1)) = G

                        Call PBP

                    Else

                        If G < 0 Then
                            U$ = "THAT CATCH BY "
                            Call WHICHRECEIVER
                            U$ = U$ + " LOST" + Str$(Int((G * -1) * 10 + .5) / 10) + " YARDS"

                            If G > WR(P1, I3) Then strWR$(P1, I3) = "": WR(P1, I3) = G
                            If G > QB(P1, Q6%(P1)) Then strQB$(P1, Q6%(P1)) = "": QB(P1, Q6%(P1)) = G

                        Else

                            If Int((G + Y) * 10 + .5) / 10 < 99.9499 Then U$ = "THAT CATCH BY "
                            Call WHICHRECEIVER
                            U$ = U$ + " GAINED" + Str$(Int((G) * 10 + .5) / 10) + " YARDS"

                            If G > WR(P1, I3) Then strWR$(P1, I3) = "": WR(P1, I3) = G
                            If G > QB(P1, Q6%(P1)) Then strQB$(P1, Q6%(P1)) = "": QB(P1, Q6%(P1)) = G
                        End If

                    End If

                    Call PBP

                    If E = 1 Then F9 = 0: Call GoForTwo

                    If Int((G + Y) * 10 + .5) / 10 < .05 Then F9 = 0: GoTo 3250
                    If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then F9 = 0: K(P1, 0) = K(P1, 0) + 1: GoTo 3200

                    If P2 = 16 Then

                        I = Int(Rnd(1) * 10) + 1

                        If I <= 7 Then

                            O = 5

                            RN = Int(Rnd(1) * 5) + 1
                            Select Case RN
                                Case 1: U$ = "AND HE GETS OUT OF BOUNDS"
                                Case 2: U$ = "AND HE'S KNOCKED OUT OF BOUNDS"
                                Case 3: U$ = "AND HE STEPS OUT OF BOUNDS"
                                Case 4: U$ = "AND HE'S PUSHED OUT OF BOUNDS"
                                Case 5: U$ = "AND HE'S SHOVED OUT OF BOUNDS"
                            End Select

                            Call PBP
                            F9 = 0

                        End If

                    End If

                    GoTo 3309

                Else

                    'Incomplete pass
                    GoTo 3535

                End If 'Done checking for pass complete

            End If 'Done checking if rollout


        Case 19

            Call d100(I3)

            For J = 2 To 0 Step -1
                If I3 <= prNumRet(D, J) Then I1 = J
            Next J

            PR = I1

            U$ = pret$(D, PR) + " (" + Str$(Int(prYdsPerRet(D, PR) * 3)) + " AVG) IS DEEP TO RECEIVE"
            Call PBP

            If playerOpt = 1 And P1 = compTeam Or playerOpt = 2 Then
                I1 = 0
            Else

                Call FRAMEOPTION(9, P1)
                Print "PUNTING OPTIONS "; gameTeams$(P1)
                Locate 12, 14
                Locate , 14: Print "0 - PUNT NORMAL"
                Locate , 14: Print "1 - PUNT AWAY FROM THE RETURN MAN"
                Locate , 14: Print "2 - FAKE PUNT (RUN)"
                Locate , 14: Print "3 - FAKE PUNT (PASS)"
                Locate , 14: Print "4 - TAKE INTENTIONAL SAFETY (INSIDE OWN 20)"
                Locate , 14: Print "5 - QUICK KICK (1ST-3RD DOWN)"

                Do
                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        Screen , , 0, 0
                        GoTo 3118
                    Else
                        I1 = Val(I$)
                    End If

                Loop Until I1 >= 0 And I1 <= 5 Or Not (Y > 20 And I1 = 4) Or Not (D1 = 4 And I1 = 5) Or Not (D1 < 4 And I1 < 5)

                Screen , , 0, 0

                If I1 = 5 Then

                    Call d100(I)
                    X = 0
                    G = puntYdsPerP(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * puntYdsPerP(P1, 0)))) + 11

                    If WX%(0) = P1 Then
                        G = G - Int(Rnd(1) * 7) + 3 - WX%(1)
                    Else
                        G = G + Int(Rnd(1) * 7) - 3 + WX%(1)
                    End If

                    Call d100(RN)

                    If RN <= 50 Then
                        U$ = gameTeams$(P1) + " IS QUICK KICKING!!"
                    Else
                        U$ = gameMascots$(P1) + " ARE QUICK KICKING!!"
                    End If
                    Call PBP

                    If (D3 > 6 And I >= 60 And I <= 72) Or (D3 <= 6 And I >= 50 And I <= 56) Then 3840

                    U$ = "THE KICK IS UP AND ON ITS WAY"
                    Call PBP

                    G = G + 2

                    Call YARDAGE

                    U$ = "KICK BY " + gameTeams$(P1) + " TRAVELS" + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    K(P1, 7) = K(P1, 7) + 1

                    If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then

                        K(P1, 8) = K(P1, 8) + 100 - Y

                        If 100 - Y > K(P1, 26) Then K(P1, 26) = 100 - Y

                        gameClock! = gameClock! - 5
                        timePoss(P1) = timePoss(P1) + 5
                        K(P1, 27) = K(P1, 27) + 1

                        Call Touchback

                        GoTo 700
                    End If

                    K(P1, 8) = K(P1, 8) + G

                    If G + Y > 80 Then K(P1, 29) = K(P1, 29) + 1
                    If G > K(P1, 26) Then K(P1, 26) = G

                    Y = Y + G
                    YT = Y
                    Call YARDLINE
                    Y = 100 - Y
                    Call AddPossessionTime(4, 7)

                    RN = Int(Rnd(1) * 48) + 1

                    If RN <= 11 Or I1 = 1 Then
                        U$ = "THE BALL GOES OUT OF BOUNDS AT THE" + Str$(Int(YL)) + " YARD LINE"
                        Call PBP
                    Else
                        If RN <= 20 Or YL <= 15 Then
                            U$ = "THE BALL IS DOWNED AT THE" + Str$(Int(YL)) + " YARD LINE"
                            Call PBP
                        Else
                            If D1 = 4 Then
                                U$ = "A FAIR CATCH IS CALLED AT THE" + Str$(Int(YL)) + " YARD LINE"
                                Call PBP
                            End If
                        End If
                    End If

                    Call CHANGEPOSS

                    D1 = 1
                    Y1 = 10

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                    Call DELAY

                    GoTo 700

                End If

            End If

            X = 0
            G = puntYdsPerP(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * puntYdsPerP(P1, 0)))) + 3

            If 100 - Y <= ((puntYdsPerP(P1, 0) * 2) + 28) Then G = G + 100 - Y - (31 + puntYdsPerP(P1, 0) * 2)

            If WX%(0) = P1 Then
                G = G - Int(Rnd(1) * 7) + 3 - WX%(1)
            Else
                G = G + Int(Rnd(1) * 7) - 3 + WX%(1)
            End If

            Call YARDAGE

            Call d100(RN)

            If playerOpt = 1 And D = compTeam Or playerOpt = 2 Then
                If RN <= 10 Then
                    choice = 1
                Else
                    choice = 0
                End If

            Else

                Call FRAMEOPTION(9, D)
                Print "THE PUNTING TEAM FOR "; gameTeams$(P1); " IS ON THE FIELD"
                Locate 12, 14
                Locate , 14: Print "0 - SET UP PUNT RETURN"
                Locate , 14: Print "1 - GO FOR THE BLOCK"
                Locate , 14: Print "2 - CALL PUNT-SAFE"
                Print
                Locate , 14: Print "SELECT OPTION "; gameTeams$(D); " COACH"

                Do
                    I$ = GetKeyPress$
                    choice = Val(I$)
                Loop Until choice >= 0 And choice <= 2

                Screen , , 0, 0

            End If

            'PUNTSNAP
            Select Case I1

                Case 2, 3

                    ' *** FAKE PUNT - RUN/PASS ***
                    If choice = 2 Then
                        U$ = gameTeams$(D) + " IS IN PUNT SAFE SET...FAKE PUNT? (Y/N)"
                        Call PBP

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    End If

                    If UCase$(I$) = "Y" Then

                        If I1 = 3 Then
                            ' *** FAKE W/PASS ***
                            G = 0
                            U$ = "IT'S A FAKE!!! " + punter$(P1, 0) + " IS LOOKING TO PASS"
                            Call PBP

                            Call d100(RN)

                            If choice = 0 Then

                                Select Case RN
                                    Case 1 To 36:
                                        G = Int(Rnd(1) * 27)

                                    Case 37 To 90:
                                        U$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP:
                                        O = 4
                                        GoTo 3309

                                    Case 91 To 100:
                                        U$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876
                                End Select

                            End If

                            If choice = 1 Then

                                Select Case RN
                                    Case 1 To 26:
                                        G = Int(Rnd(1) * 20)

                                    Case 27 To 80:
                                        U$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4
                                        GoTo 3309

                                    Case 81 To 90:
                                        U$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                    Case 91 To 100: G = 100 - Y
                                End Select

                            End If

                            If choice = 2 Then

                                Select Case RN
                                    Case 1 To 10:
                                        G = Int(Rnd(1) * 7) + 6

                                    Case 11 To 80:
                                        U$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4: GoTo 3309
                                    Case 81 To 100:
                                        U$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876
                                End Select

                            End If

                            U$ = "PASS IS COMPLETE FOR"

                            If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then
                                U$ = U$ + Str$(Int(100 - Y)) + " YARDS"
                                Call PBP
                            Else
                                U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
                                Call PBP
                            End If

                            If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
                            If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then 3200

                            F% = 2
                            Call FUMBLE

                        Else

                            Call FAKEYARDS(choice)

                            If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then 3200
                            If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250

                            F% = 2

                            Call FUMBLE

                        End If

                        GoTo 3309

                    End If 'Done processing fake punt

                Case 4
                    Call TAKESAFETY
                    GoTo 3251

            End Select

            PSNAP:
            U$ = punter$(P1, 0) + " WAITS FOR THE SNAP"
            Call PBP

            I5 = Int(Rnd(1) * 300) + 1

            If I5 = 12 Then

                U$ = "THERE'S A BAD SNAP FROM CENTER!!"
                Call PBP

                G = (Int(Rnd(1) * 14) + 12) * -1
                Call AddPossessionTime(4, 7)

                K(P1, 10) = K(P1, 10) + 1
                K(P1, 9) = K(P1, 9) + 1

                If Y + G < -9.999 Then score(D, Q) = score(D, Q) + 2: score(D, 0) = score(D, 0) + 2: GoTo 3251

                If Y + G <= .001 And Y + G >= -9.999 Then
                
                    Call d100(RN0)

                    If RN0 <= 50 Then
                        score(D, Q) = score(D, Q) + 2
                        score(D, 0) = score(D, 0) + 2
                        GoTo 3251
                    Else
                        X1 = 10
                        Call CHANGEPOSS
                        GoTo 3200
                    End If

                End If

                Call CHANGEPOSS
                D1 = 1
                Y1 = 10
                Y = 100 - Y

                If Y1 >= 90 Then Y1 = 100 - Y

                Call d100(RN)

                If RN <= 50 Then
                    U$ = gameTeams$(P1) + " TAKES OVER ON THE"
                Else
                    U$ = gameMascots$(P1) + " TAKE OVER ON THE"
                End If

                If Y > 50 Then
                    U$ = U$ + Str$(Int(100 - Y)) + " YARD LINE"
                Else
                    U$ = U$ + Str$(Int(Y)) + " YARD LINE"
                End If

                O = 5

                GoSub 3900

            Else

                Call d100(I5)

                If choice = 1 Then U$ = "THERE'S A HEAVY RUSH!!!": Call PBP

                If choice = 1 And I5 >= 72 And I5 <= 76 Then 3840

                If choice = 1 And I5 >= 13 And I5 <= 17 Then

                    U$ = "THERE'S A FLAG ON THE PLAY!!"

                    Call PBP

                    U$ = "ROUGHING THE KICKER !!!...AUTOMATIC FIRST DOWN !!!"

                    Call PBP

                    K(D, 11) = K(D, 11) + 1
                    K(D, 12) = K(D, 12) + 15
                    Y = Y + 15
                    D1 = 1
                    Y1 = 10
                    K(P1, 0) = K(P1, 0) + 1
                    O = 4

                    GoSub 3900

                    Call DELAY

                    GoTo 700
                End If

                U$ = punter$(P1, 0) + " TAKES THE SNAP"
                Call PBP

                Call d100(RN)

                If I1 = 1 Then

                    If RN <= 10 Then
                        G = G - (Int(Rnd(1) * 11) + 25)
                        U$ = "HE SHANKS IT!"
                    Else
                        G = G - (Int(Rnd(1) * 9) + 4)
                        U$ = "HE KICKS IT TOWARDS THE CORNER"
                    End If

                Else

                    If G < 40 Then
                        U$ = "HE GETS THE PUNT AWAY"
                    Else
                        Call PUNTPBP
                    End If

                End If

                Call PBP

                U$ = "PUNT BY " + punter$(P1, 0) + " TRAVELS" + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
                Call PBP

                K(P1, 7) = K(P1, 7) + 1

                If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then

                    K(P1, 8) = K(P1, 8) + 100 - Y
                    gameClock! = gameClock! - 5
                    timePoss(P1) = timePoss(P1) + 5
                    K(P1, 27) = K(P1, 27) + 1

                    Call Touchback

                Else

                    K(P1, 8) = K(P1, 8) + G

                    Call d100(I)

                    If G + Y > 80 Then K(P1, 29) = K(P1, 29) + 1
                    If G > K(P1, 26) Then K(P1, 26) = G

                    Call d100(RN0)
                    If G + Y >= 90 And I >= 5 Or choice = 2 Or I > 60 Or I1 = 1 And RN0 <= 80 Then

                        Y = Y + G
                        YT = Y

                        Call YARDLINE

                        Y = 100 - Y

                        Call AddPossessionTime(4, 7)

                        RN = Int(Rnd(1) * 48) + 1

                        If RN <= 11 Or I1 = 1 Then
                            U$ = "THE BALL GOES OUT OF BOUNDS AT THE" + Str$(Int(YL)) + " YARD LINE"
                        Else
                            If RN <= 20 Or YL <= 15 Then
                                U$ = "THE BALL IS DOWNED AT THE" + Str$(Int(YL)) + " YARD LINE"
                            Else
                                If D1 = 4 Then
                                    U$ = "A FAIR CATCH IS CALLED AT THE" + Str$(Int(YL)) + " YARD LINE"
                                End If
                            End If
                        End If

                        Call PBP

                        Call CHANGEPOSS

                        D1 = 1
                        Y1 = 10

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Call DELAY

                    Else

                        Call PUNTYARDS

                        If G + Y >= 99.99 Then
                            K2(P1, PR + 3, 14) = K2(P1, PR + 3, 14) + 1
                            K2(P1, PR + 3, 15) = K2(P1, PR + 3, 15) + 100 - Y
                            K2(P1, PR + 3, 16) = K2(P1, PR + 3, 16) + 1
                            BG = 100 - Y
                            If BG > PR(P1, PR) Then strPR$(P1, PR) = "t": PR(P1, PR) = BG
                            I1 = PR
                            GoTo 3200
                        End If

                        K2(P1, PR + 3, 14) = K2(P1, PR + 3, 14) + 1
                        K2(P1, PR + 3, 15) = K2(P1, PR + 3, 15) + G

                        If G > PR(P1, PR) Then strPR$(P1, PR) = "": PR(P1, PR) = G

                        If G + Y <= .01 Then 3250

                        D1 = 1
                        Y = Y + G
                        Y1 = 10

                        If Y >= 89.99 Then Y1 = 100 - Y

                        I = Int(Rnd(1) * 1000) + 1

                        If I >= 960 - tmRat_GAME(P1, 0) Then
                            U$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
                            Call PBP

                            If Y < 20 Then Y = Y / 2: K(P1, 12) = K(P1, 12) + Y / 2

                            K(P1, 11) = K(P1, 11) + 1

                            If Y >= 20 Then Y = Y - 10: K(P1, 12) = K(P1, 12) + 10

                        Else

                            F% = 4

                            Call FUMBLE

                            If F9 = 1 Then Call FumblePBP: GoTo 650
                        End If

                        hangtime = Int(G / 10)
                        Call AddPossessionTime(8 + hangtime, 13 + hangtime)
                        Call DELAY

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                    End If

                End If

            End If

            GoTo 700


        Case 20

            Call d100(I)

            'Was previously I2 not J
            For J = 1 To 0 Step -1
                If I <= kickerFGPct(P1, J) Then I1 = J
            Next J

            Q7%(P1) = I1

            'FG range is considered up to 62 yards
            '--- this should be changed, but not
            '--- without considering how the
            '--- distance impacts accuracy
            If Y < 52 + goalPostAdj Then
                U$ = "NOT IN FIELD GOAL RANGE"
                Call PBP
                GoTo 700
            Else

                ' *** FIELD GOAL DECISION ***
                If playerOpt = 1 And P1 = compTeam Or playerOpt = 2 Then
                    I1 = 0
                Else
                    Call FRAMEOPTION(6, P1)
                    Print "FIELD GOAL OPTIONS "; gameTeams$(P1)
                    Locate 12, 14
                    Locate , 14: Print "0 - ATTEMPT FIELD GOAL"
                    Locate , 14: Print "1 - FAKE FIELD GOAL (RUN)"
                    Locate , 14: Print "2 - FAKE FIELD GOAL (PASS)"

                    Do
                        I$ = GetKeyPress$
                        I1 = Val(I$)
                    Loop Until I1 >= 0 And I1 <= 2 Or I$ = Chr$(27)

                    Screen , , 0, 0

                End If

                If I$ = Chr$(27) Then GoTo 3118

                Call d100(RN)

                If playerOpt = 1 And D = compTeam Or playerOpt = 2 Then
                    choice = 1
                Else
                    Call FRAMEOPTION(9, D)

                    Print "THE FIELD GOAL UNIT FOR "; gameTeams$(P1); " IS ON THE FIELD"
                    Locate 12, 14
                    Locate , 14: Print "1 - GO FOR THE BLOCK"
                    Locate , 14: Print "2 - CALL FIELD GOAL-SAFE"
                    Print
                    Locate , 14: Print "SELECT OPTION "; gameTeams$(D); " COACH"

                    Do
                        I$ = GetKeyPress$
                        choice = Val(I$)
                    Loop Until choice >= 1 And choice <= 2

                End If

                Screen , , 0, 0

                If I1 = 2 Or I1 = 1 Then

                    ' *** FAKE PUNT - RUN/PASS ***
                    If choice = 2 Then
                        U$ = gameTeams$(D) + " IS IN FIELD GOAL SAFE SET...FAKE FIELD GOAL? (Y/N)"
                        Call PBP
                    Else
                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "N" Then GoTo 3602

                    End If

                    If I1 = 2 Then

                        ' *** FAKE W/PASS ***
                        U$ = "IT'S A FAKE!!! THE HOLDER IS LOOKING TO PASS"
                        Call PBP
                        G = 0
                        Call d100(RN)

                        passIncomplete% = 0
                        passIntercepted% = 0

                        Select Case I2

                            Case 1
                                Select Case RN
                                    Case 1 To 20:
                                        G = Int(Rnd(1) * 20)
                                    Case 21 To 80:
                                        U$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4
                                        passIncomplete% = 1
                                    Case 81 To 90:
                                        U$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted% = 1
                                    Case 91 To 100:
                                        G = 100 - Y
                                End Select

                            Case 2
                                Select Case RN
                                    Case 1 To 10:
                                        G = Int(Rnd(1) * 7) + 6
                                    Case 11 To 80:
                                        U$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4
                                        passIncomplete% = 1
                                        GoTo 3309
                                    Case 81 To 100:
                                        U$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted% = 1
                                End Select

                        End Select

                        If passIntercepted% = 1 Then

                            X = 0
                            K(P1, 3) = K(P1, 3) + 1
                            K(P1, 5) = K(P1, 5) + 1
                            K2(P1, I1, 9) = K2(P1, I1, 9) + 1
                            K2(P1, I1, 11) = K2(P1, I1, 11) + 1

                            Call AddPossessionTime(5, 8)

                            G = Int(Rnd(1) * 10) + 1

                            Call YARDAGE

                            If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then
                                U$ = U$ + " IN THE END ZONE"
                                Call PBP
                                Call Touchback
                            Else
                                If G + Y > 49.99 Then U$ = U$ + " AT THE " + Str$(Int(100 - G - Y)) + " YARD LINE": Call PBP
                                If G + Y < 49.99 Then U$ = U$ + " AT THE " + Str$(Int(G + Y)) + " YARD LINE": Call PBP

                                Call CHANGEPOSS
                                D1 = 1
                                Y = 100 - G - Y
                                YT = 100 - Y

                                Call YARDLINE

                                G = Int(Rnd(1) * 25) + 1

                                If G >= 24 Then G = Int(Rnd(1) * 70) + 31

                                Call YARDAGE

                                X1 = 5
                                SY = 100 - Y
                                EY = SY - G

                                Call MOVEBALL

                                U$ = "RETURNED FOR "

                                If G + Y > 99.99 Then
                                    U$ = U$ + Str$(Int(100 - Y)) + " YARDS"
                                    Call PBP
                                    GoTo 3200
                                Else
                                    U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
                                    Call PBP
                                    O = 5
                                    Y = Y + G
                                    Y1 = 10
                                    If Y >= 90 Then Y1 = 100 - Y

                                    GoSub 3900

                                    Call DELAY
                                End If

                            End If

                            GoTo 700

                        Else

                            If passIncomplete% <> 1 Then

                                U$ = "PASS IS COMPLETE FOR"

                                If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then
                                    U$ = U$ + Str$(Int(100 - Y)) + " YARDS"
                                Else
                                    U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
                                End If

                                Call PBP

                                If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
                                If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then 3200

                                F% = 2

                                Call FUMBLE

                            End If

                            GoTo 3309

                        End If

                    End If

                    Call FAKERUN(choice)

                    If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then 3200
                    If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250

                    F% = 2

                    Call FUMBLE

                    GoTo 3309

                End If

                GoTo 3602

            End If

        Case 21
            Call SCOUTREPT
            GoTo 700

        Case 22
            I3 = P1
            Call CHANGEQB

            GoTo 700

        Case 23
            Call BOXOPTIONS(0)
            Call TICKER

        Case 24
            I3 = P1
            Call CHOOSEKICKER

            GoTo 700

    End Select

    '----------

    3535 ' PASS NOT COMPLETE
    'Incomplete / Interference Penalty
    X = 2
    I = Int(Rnd(1) * 1000) + 1

    If I <= 50 + tmRat_GAME(D, 0) And P2 <> 13 And E = 0 Then

        U$ = "DEFENSIVE PASS INTERFERENCE"
        If P2 = 9 Or P2 = 10 Or P2 = 13 Then G = Int(Rnd(1) * 10) + 1
        If P2 = 12 Or P2 = 15 Then G = Int(Rnd(1) * 20) + 21
        If P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18 Then G = Int(Rnd(1) * 10) + 11

        If ruleOptType = 0 And ruleOptColl >= 3 And G > 15 Then G = 15

        K(D, 11) = K(D, 11) + 1

        Call YARDAGE

        If G + Y >= 99.99 Then

            If ruleOptType <> 0 Then
                K(D, 12) = K(D, 12) + 100 - Y - 1
                U$ = U$ + " IN THE END ZONE"
                Call PBP

                Y = 99
                Y1 = 1
            Else
                K(D, 12) = K(D, 12) + 100 - Y - 2

                U$ = U$ + " IN THE END ZONE"
                Call PBP

                Y = 98
                Y1 = 2
            End If

        Else

            K(D, 12) = K(D, 12) + G
            U$ = U$ + " AT THE "

            If G + Y < 50 Then
                U$ = U$ + Str$(Int((G + Y) * 10 + .5) / 10)
            Else
                U$ = U$ + Str$(Int((100 - Y - G) * 10 + .5) / 10)
            End If

            U$ = U$ + " YARD LINE"
            Call PBP

            Y = Y + G
            Y1 = 10

            If Y >= 90 Then Y1 = 100 - Y

        End If

        D1 = 1
        K(P1, 0) = K(P1, 0) + 1
        O = 4

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

        GoTo 700

    Else

        Call INCOMPLETE
        If E = 1 Then Call GoForTwo
        O = 4

    End If

    GoTo 3309


    '----- NOODLE -----

    3200 '
    Randomize Timer

    Call d100(RN)

    'So essentially we have a 50/50 chance
    'of seeing the team name or the mascot name
    'IE, TOUCHDOWN! 89 SAN FRANCISCO
    '    TOUCHDOWN! 49ERS
    If RN <= 50 Then
        U$ = "T O U C H D O W N !!! " + gameTeams$(P1)
    Else
        U$ = "T O U C H D O W N !!! " + gameMascots$(P1)
    End If

    Call PBP
    Call SCOREUPD
    Call COMPILER

    If ruleOptType <> 4 Then
        'Not WFL
        'Touchdowns worth 6
        score(P1, Q) = score(P1, Q) + 6
        score(P1, 0) = score(P1, 0) + 6
    Else
        'WFL
        'Touchdowns worth 7
        score(P1, Q) = score(P1, Q) + 7
        score(P1, 0) = score(P1, 0) + 7
    End If

    Call DisplayField

    Y = 40
    Y1 = 10
    D1 = 1
    Call SCOREBOARD

    'Pro Overtime
    If ruleOptType <> 0 And Q > 4 Then 3364

    GoSub PointsAfterTD

    Y = 35
    YT = 65
    Y1 = 10

    Call CHANGEPOSS

    O = 4
    U3 = 9

    GoSub 3900

    U3 = 0

    Call SCOREBOARD

    If ruleOptType <> 0 And goalPostAdj < 0 Then Y = 40: YT = 60
    If ruleOptType = 1 And ruleOptPro = 3 Or ruleOptType = 4 Then Y = 30: YT = 70
    If ruleOptType = 0 And ruleOptColl = 0 Then Y = 40: YT = 60

    Call SCOREBOARD

    If R1 > 0 And R1 < 3 And 100 - Y > 10 And ruleOptType <> 0 Then K(D, 0) = K(D, 0) + 1

    'Overtime
    If Q > 4 Then 3364

    GoTo 340

    '----- NOODLE -----

    3250 ' *** SAFETY ***
    score(D, Q) = score(D, Q) + 2
    score(D, 0) = score(D, 0) + 2
    K(D, 19) = K(D, 19) + 1

    Call COMPILER

    3251 '
    Y = 20
    Y1 = 10
    D1 = 1
    O = 5
    X1 = 7

    Call SCOREUPD
    Call CHANGEPOSS

    I6 = 99
    U3 = 9

    GoSub 3900

    U3 = 0
    U$ = "S A F E T Y  !!!"

    Call PBP

    Call DisplayField

    'Overtime
    If Q > 4 Then 3364

    GoTo 340

    '----- NOODLE -----

    3303 '
    If Int(Y1 * 10 + .5) / 10 < .05 Then

        3335 '
        D1 = 1
        Y1 = 10

        If 100 - Y < 9.95 Then Y1 = 100 - Y
        If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

        K(P1, 0) = K(P1, 0) + 1

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

        GoTo 700

    End If

    If D1 = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1
    If D1 = 4 Then thirdDownFail(P1) = thirdDownFail(P1) + 1
    If D1 > 4 Then 3339

    I = Int(Rnd(1) * 1000) + 1

    If I <= 6 + tmRat_GAME(D, 0) Then

        U$ = "PENALTY AFTER WHISTLE - "
        I = Int(Rnd(1) * 10) + 1

        Select Case I
            Case 1
                U$ = U$ + "OFF PERSONAL FOUL - 15 YARDS"
                Call PBP

                K(P1, 11) = K(P1, 11) + 1

                If Y < 30 Then
                    Y1 = Y1 + Y / 2
                    Y = Y - Y / 2
                    K(P1, 12) = K(P1, 12) + Y / 2
                Else
                    Y1 = Y1 + 15
                    Y = Y - 15
                    K(P1, 12) = K(P1, 12) + 15
                End If

            Case 2 To 6
                U$ = U$ + "DEF PERSONAL FOUL - 15 YARDS"
                Call PBP

                K(D, 11) = K(D, 11) + 1

                If 100 - Y < 30 Then
                    Y1 = Y1 - (100 - Y) / 2
                    Y = Y + (100 - Y) / 2
                    K(D, 12) = K(D, 12) + (100 - Y) / 2
                Else
                    Y1 = Y1 - 15
                    Y = Y + 15
                    K(D, 12) = K(D, 12) + 15
                End If

                If gameClock! <= 0 Then gameClock! = 1

            Case 7 To 9
                U$ = U$ + "DEF INCIDENTAL FACEMASK - 5 YARDS"
                Call PBP

                K(D, 11) = K(D, 11) + 1

                If 100 - Y < 10 Then
                    Y1 = Y1 - (100 - Y) / 2
                    Y = Y + (100 - Y) / 2
                    K(D, 12) = K(D, 12) + (100 - Y) / 2
                Else
                    Y1 = Y1 - 5
                    Y = Y + 5
                    K(D, 12) = K(D, 12) + 5
                End If

                If gameClock! <= 0 Then gameClock! = 1

            Case 10
                U$ = U$ + "DEF FLAGRANT FACEMASK - 15 YARDS 1ST DOWN!"
                Call PBP

                K(D, 11) = K(D, 11) + 1

                If 100 - Y < 30 Then
                    Y1 = Y1 - (100 - Y) / 2
                    Y = Y + (100 - Y) / 2
                    K(D, 12) = K(D, 12) + (100 - Y) / 2
                Else
                    Y1 = Y1 - 15
                    Y = Y + 15
                    K(D, 12) = K(D, 12) + 15
                End If

                If gameClock! <= 0 Then gameClock! = 1

        End Select

    Else

        GoSub 3900

        GoTo 700

    End If

    '----- NOODLE -----

    3309 '
    U$ = "*** PENALTY - "

    I4 = Int(Rnd(1) * 1000) + 1

    Select Case R1

        Case 1

            Select Case I4
                Case 1 To 7 + tmRat_GAME(P1, 0)
                    U$ = U$ + "OFFENSE... ILLEGAL MOTION 5 YARDS ***"
                    P = -5

                Case 7 + tmRat_GAME(P1, 0) To 25 + tmRat_GAME(P1, 0)
                    If (ruleOptType <> 0 And ruleOptPro = 1 Or ruleOptType = 0 And ruleOptColl < 3) Then
                        U$ = U$ + "OFF... HOLDING 15 YARDS ***"
                        P = -15
                    Else
                        U$ = U$ + "OFF... HOLDING 10 YARDS ***"
                        P = -10
                    End If

                    HL% = 1

                Case 25 + tmRat_GAME(P1, 0) To 42 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                    U$ = U$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    P = 5

                Case Else
                    Call COMPILER

                    Y = Y + G
                    Y1 = Y1 - G
                    D1 = D1 + 1

                    If F9 = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

        Case 2
            Select Case I4

                Case 1 To 17 + tmRat_GAME(P1, 0)
                    If (ruleOptType <> 0 And ruleOptPro <> 0 And ruleOptPro <> 3 Or ruleOptType = 0 And ruleOptColl < 3) Then
                        U$ = U$ + "OFF... HOLDING 15 YARDS ***"
                        P = -15
                        HL% = 1
                    Else
                        U$ = U$ + "OFF... HOLDING 10 YARDS ***"
                        P = -10
                        HL% = 1
                    End If

                Case 17 + tmRat_GAME(P1, 0) To 22 + tmRat_GAME(P1, 0)
                    U$ = U$ + "OFF... ILLEGAL MOTION 5 YARDS ***"
                    P = -5

                Case 22 + tmRat_GAME(P1, 0) To 27 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                    U$ = U$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    P = 5

                Case 27 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0) To 42 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                    U$ = U$ + "DEF... HOLDING 5 YARDS - 1ST DOWN! ***"
                    IH = 1
                    P = 5

                Case Else
                    Call COMPILER

                    Y = Y + G
                    Y1 = Y1 - G
                    D1 = D1 + 1

                    If F9 = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

    End Select

    If (ruleOptType <> 0 Or ruleOptColl >= 4) Or (ruleOptType = 0 And HL% = 1) Then
        If R1 = 2 Then P = P - Int(Rnd(1) * 8) + 1
        If R1 = 1 Then P = P + Int(Rnd(1) * 5) + 1
    End If

    HL% = 0

    U$ = U$ + " 1-ACCEPT 2-DECLINE "
    Call PBP

    If playerOpt <> 0 Then

        I$ = "2"

        If playerOpt = 1 Then

            If (P > 0 And D = compTeam) Or (P < 0 And P1 = compTeam) Then I$ = GetKeyPress$

            If Not (P < 0 And P1 <> compTeam And D1 > 3 And G < Y1) Then

                If P > 0 And D <> compTeam Then

                    If IH = 1 And Y1 - G > 0 Then
                        I$ = "1"
                    Else
                        If G > 5 Or P < 0 And P1 <> compTeam And D1 = 3 And Y1 - G > 3 Or P < 0 And P1 <> compTeam And G < 0 Then
                            '3319
                            'Not sure why this is designed as is I can only imagine 
                            'it was helpful to have many statements going to 3319
                            'because it wasn't written from if/then logic to start with
                        End If

                    End If

                End If

            End If

        End If

    End If

    3319 '
    If Val(I$) = 1 Then

        O = 5

    Else

        U$ = "PENALTY DECLINED"

        Call PBP
        Call COMPILER

        IH = 0
        Y = Y + G
        Y1 = Y1 - G
        D1 = D1 + 1

        If D1 = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1
        If O < 4 Or O > 5 Then O = 1

        If Int((Y1) * 10 + .5) / 10 < .05 Then
            D1 = 1
            Y1 = 10

            If 100 - Y < 9.95 Then Y1 = 100 - Y
            If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

            K(P1, 0) = K(P1, 0) + 1

            GoSub 3900

            If gameClock! <= 0 Then gameClock! = 1

        Else

            If D1 > 4 Then 3339

            GoSub 3900

        End If

        GoTo 700

    End If

    3325 If ruleOptType = 0 And ruleOptColl = 0 Then O = 1
    U$ = "PENALTY ACCEPTED"
    Call PBP

    If P > 0 Then 3330

    P = P * -1

    'OFF PEN

    3326 If Y < P * 2 Then
        Y1 = Y1 + Y / 2
        Y = Y - Y / 2
        K(P1, 12) = K(P1, 12) + Y / 2
    Else
        Y1 = Y1 + P
        Y = Y - P
        K(P1, 12) = K(P1, 12) + P
    End If

    K(P1, 11) = K(P1, 11) + 1

    If Int(Y1 * 10 + .5) / 10 < .05 Then 3335

    GoSub 3900

    GoTo 700

    '----- NOODLE -----

    'DEF PEN

    3330 '
    If 100 - Y < P * 2 Then
        Y1 = Y1 - (100 - Y) / 2
        Y = Y + (100 - Y) / 2
        K(D, 12) = K(D, 12) + (100 - Y) / 2
    Else
        Y1 = Y1 - P
        Y = Y + P
        K(D, 12) = K(D, 12) + P
    End If

    K(D, 11) = K(D, 11) + 1

    If Y1 < .01 Or IH = 1 Then
        IH = 0
    Else

        If gameClock! <= 0 Then gameClock! = 1

        GoSub 3900

        'skipSomething
        GoTo 700

    End If

    'If skipSomething

    '3335
    D1 = 1
    Y1 = 10

    If 100 - Y < 9.95 Then Y1 = 100 - Y
    If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

    K(P1, 0) = K(P1, 0) + 1

    GoSub 3900

    If gameClock! <= 0 Then gameClock! = 1

    'End If

    GoTo 700

    '----- NOODLE -----

    'College Overtime
    3339 If NG%(14) = 3 And Q > 4 Then 3364

    D1 = 1
    Y1 = 10
    Y = 100 - Y

    If Y > 90 Then Y1 = 100 - Y

    Call CHANGEPOSS

    O = 5

    GoSub 3900

    GoTo 700

    '----- NOODLE -----

    '3349
    O = 6

    If Int(Y1 * 10 + .5) / 10 < .05 Then

        D1 = 1
        Y1 = 10

        If 100 - Y < 9.95 Then Y1 = 100 - Y
        If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

        K(P1, 0) = K(P1, 0) + 1

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

    Else

        GoSub 3900

    End If

    GoTo 700


    '--------------------
    ' End Of Quarter
    '--------------------

    3360 '
    gameClock! = 0
    WX%(0) = 1 - WX%(0)
    gameClock! = 900

    Q = Q + 1

    If Q = 3 Or Q >= 5 Then
        'If end of half
        U3 = 0
    End If

    endGame = 0

    'Q - 1 = previous quarter
    'We've already incremented it

    Select Case Q - 1

        Case 1, 3
            'Q = 2 / 4
            'End of 1st / 3rd Quarter
            Call SCOREBOARD
            U$ = "END OF QUARTER"
            Call PBP

            GoTo 700

        Case 2
            'Q = 2 / 4
            'Half Time
            Call SCOREBOARD
            U$ = "HALF TIME"
            Call PBP

            W% = 1 - W%

            Call HALFSTART
            Call GetKeyWhenNoDelay
            halfTime = 1

            Do
                Color 15, 0
                Call BOXOPTIONS(0)

                'Reset option for next call
                If X <> 0 Then X = 0

                Color 15, 0
                Cls

                Call TEAMBOX(1, 1)

                If Not (autoPlay = 1 And playerOpt = 2) Then
                    Do
                        U$ = "DISPLAY STATS AGAIN? (Y/N)"
                        Locate 7, (80 - Len(U$)) / 2
                        Print U$
                        Color 15, 0
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                End If

            Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerOpt = 2)

            GoTo 850

        Case Else
            'End of 4th Quarter / Overtime

            '--- start analysis here
            3364 '
            Call SCOREBOARD

            'Check if we're configured for overtime
            If NG%(14) = 0 Or (score(0, 0) <> score(1, 0) And NG%(14) <> 3) Or (score(0, 0) <> score(1, 0) And NG%(14) = 3 And (OT(1) = 1 And OT(0) = 1 Or OT(1) = 0 And OT(0) = 0)) Then

                'Nope!
                'Leave Q at 4
                If OT <> 1 Then
                    Q = Q - 1
                End If

                U$ = "END OF GAME"
                Call PBP
                Call GetKeyWhenNoDelay

                endGame = 1

            End If

            '--- should this be outside the SELECT

            'Check if game is over
            If endGame <> 1 Then

                'Game not over
                Call SCOREBOARD
                Call MARKER1

                'Determine if we should display stats
                Select Case NG%(14)

                    Case 1
                        'Show stats before overtime
                        I$ = "Y"

                        'Sudden Death 1 Quarter
                        'play overtime as 1 quarter with no overtime
                        NG%(14) = 0

                    Case 2
                        'Show stats before overtime
                        I$ = "Y"

                        'Sudden Death to Win
                        'keep playing quarters

                    Case 3
                        'Not showing stats

                        'Sudden Death
                        'College Rules
                        '1st and 10 from the 25
                        Y = 75
                        YT = 25
                        Y1 = 10
                        D1 = 1

                        '1st Overtime
                        OT = 1

                        'Track overtime possesions for each team
                        If OT(0) = 1 And OT(1) = 1 Then Q = Q + 1

                        If OT(0) = 1 And OT(1) = 1 Or OT(0) = 0 And OT(1) = 0 Then
                            OT(0) = 0
                            OT(1) = 0
                            P1 = Int(Rnd(1) * 2)
                            D = 1 - P1
                            Call CHANGEPOSS
                            OT(P1) = 1

                        ElseIf OT(0) = 1 Then
                            OT(1) = 1
                            P1 = 1

                            D = 0
                            If score(P1, 0) > score(D, 0) Then 3364

                        ElseIf OT(1) = 1 Then
                            OT(0) = 1
                            P1 = 0
                            D = 1
                            If score(P1, 0) > score(D, 0) Then 3364
                        End If

                        GoTo 700

                End Select

                'We're going into overtime
                If UCase$(I$) = "Y" Then

                    If Q = 5 Then

                        P1 = Int(Rnd(1) * 2)

                        Call CHANGEPOSS

                        '1 timeout for each team
                        timeouts(0) = timeouts(0) + 1
                        timeouts(1) = timeouts(1) + 1

                        '1st and 10
                        D1 = 1
                        Y1 = 10

                        'Starting yard line for possession
                        '-- College Rules ??
                        Y = 40

                        '-- Pro Rules
                        If ruleOptType <> 0 Then Y = 35

                        If P1 = 0 Then D = 1
                        If P1 = 1 Then D = 0

                        OT = 1

                        GoTo 340

                    Else

                        timeouts(0) = timeouts(0) + 1
                        timeouts(1) = timeouts(1) + 1
                        OT = 1

                        GoTo 700

                    End If

                End If

                'Since Q increases at end of quarter,
                'we need to decrease
                Q = Q - 1

                U$ = "END OF GAME"
                Call PBP
                Call GetKeyWhenNoDelay
            End If

            '8540
            Call TICKERUPDATE

            '-- When would this NOT happen???
            If Q <> 3 Then
                endGame = 1
                Call EndOfGame
                Exit Sub
            Else
                GoTo 850
            End If

    End Select

    '----- NOODLE -----

    3552 '

    If defInts(1 - P1) > 0 Then
        Call QBTHROW
        Call FINDINT
        U$ = "PASS IS INTERCEPTED BY " + X$
    Else
        U$ = "PASS IS INTERCEPTED"
    End If

    X = 0
    SI(1 - P1, Z1, 0) = SI(1 - P1, Z1, 0) + 1
    K(P1, 3) = K(P1, 3) + 1
    K(P1, 5) = K(P1, 5) + 1

    If FF% <> 1 Then K2(P1, I1, 9) = K2(P1, I1, 9) + 1: K2(P1, I1, 11) = K2(P1, I1, 11) + 1

    If E = 1 Then
        U$ = U$ + " IN THE END ZONE"
        Call PBP

        GoTo 6605

    End If

    Call AddPossessionTime(5, 8)

    If FF% <> 0 Then
        FF% = 0
    Else
        If P2 = 12 Or P2 = 15 Then G = Int(Rnd(1) * 20) + 21
        If P2 <> 12 And P2 <> 15 Then G = Int(Rnd(1) * 10) + 1
        If P2 = 13 Then G = G - 5
        If P2 = 11 Or P2 = 14 Or P2 = 17 Or P2 = 16 Or P2 = 18 Then G = G + 10
    End If

    Call YARDAGE

    If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then
        U$ = U$ + " IN THE END ZONE"
        Call PBP
        'College overtime
        If NG%(14) = 3 And Q > 4 Then 3364
        Call Touchback
        GoTo 700
    End If

    If G + Y > 49.99 Then U$ = U$ + " AT THE " + Str$(Int(100 - G - Y)) + " YARD LINE": Call PBP
    If G + Y < 49.99 Then U$ = U$ + " AT THE " + Str$(Int(G + Y)) + " YARD LINE": Call PBP

    Call CHANGEPOSS

    D1 = 1
    Y = 100 - G - Y
    YT = 100 - Y

    Call YARDLINE

    G = Int(Rnd(1) * 25) + 1

    If G >= 24 Then
        G = Int(Rnd(1) * 70) + 31
    End If

    Call YARDAGE

    X1 = 5
    SY = 100 - Y
    EY = SY - G

    Call MOVEBALL

    U$ = "RETURNED FOR "

    If G + Y > 99.99 Then
        U$ = U$ + Str$(Int(100 - Y)) + " YARDS"
        SI(P1, Z1, 1) = SI(P1, Z1, 1) + 100 - Y
        SI(P1, Z1, 2) = SI(P1, Z1, 2) + 1
        If 100 - Y > IR(P1, Z1) Then 
            strIR$(P1, Z1) = "t"
            IR(P1, Z1) = 100 - Y
        End If
        Call PBP
        GoTo 3200
    End If

    U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
    Call PBP

    O = 5
    Y = Y + G
    Y1 = 10

    SI(P1, Z1, 1) = SI(P1, Z1, 1) + G

    If G > IR(P1, Z1) Then 
        strIR$(P1, Z1) = ""
        IR(P1, Z1) = G
    End If

    If Y >= 90 Then Y1 = 100 - Y

    'College overtime
    If NG%(14) = 3 And Q > 4 Then 3364

    F% = 5

    Call FUMBLE

    If F9 = 1 Then Call FumblePBP: GoTo 650

    I = Int(Rnd(1) * 1000) + 1

    If I > 950 - tmRat_GAME(P1, 0) Then

        U$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
        Call PBP

        If Y < 20 Then Y = Y / 2: K(P1, 12) = K(P1, 12) + Y / 2

        K(P1, 11) = K(P1, 11) + 1

        If Y >= 20 Then Y = Y - 10: K(P1, 12) = K(P1, 12) + 10

    End If

    GoSub 3900

    GoTo 700


    '----- NOODLE -----

    3900 '

    If O <> 8 Then

        'Pro Rules
        If ruleOptType <> 0 Then

            If gameClock! > 120.01 And Q <> 1 And Q <> 3 And Not (gameClock! > 159.99 And O = 0) And Not (gameClock! > 147 And O = 1) And Not (gameClock! > 142 And O = 2) And Not (gameClock! > 137 And O = 3) And Not (gameClock! > 126 And O = 4) And Not (gameClock! > 128 And O = 5) And Not (gameClock! > 130 And O = 6) And Not (gameClock! > 132 And O = 7) Then

                U$ = "* * * T W O   M I N U T E   W A R N I N G * * *"
                Call PBP

                If gameClock! <= 126.01 And gameClock! >= 120.99 Then gameClock! = gameClock! - 6: timePoss(P1) = timePoss(P1) + 6

                If gameClock! > 125.99 Then timePoss(P1) = timePoss(P1) + (gameClock! - 120): gameClock! = 120

            End If

        End If

        If (gameClock! <= 120 And Q = 2) Or (gameClock! <= 120 And Q >= 4) Then

            If O < 4 Then

                Call CheckGameClock
                If gameClock! = 0 Then GoTo 3360
                Call TWOMIN

                Select Case I1

                    Case 1:
                        U$ = "HUDDLE"
                        Call PBP

                    Case 2:
                        If timeouts(P1) = 0 Then
                            'If no timeouts
                            U$ = "HUDDLE"
                            Call PBP
                        Else
                            U$ = "TIME OUT " + gameTeams$(P1) + " - THEY HAVE" + Str$(timeouts(P1) - 1) + " REMAINING"
                            Call PBP
                            timeouts(P1) = timeouts(P1) - 1
                            O = 9

                            Call AddPossessionTime(7, 10)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Return 'aka Exit Sub

                        End If

                    Case 3

                        If timeouts(D) = 0 Then
                            'If no timeouts
                            U$ = "HUDDLE"
                            Call PBP
                        Else
                            U$ = "TIME OUT " + gameTeams$(D) + " - THEY HAVE" + Str$(timeouts(D) - 1) + " REMAINING"
                            Call PBP
                            timeouts(D) = timeouts(D) - 1
                            O = 9

                            Call AddPossessionTime(7, 10)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Return 'aka Exit Sub

                        End If

                    Case 4
                        If O = 0 Or O = 1 Then
                            U$ = "HURRY UP"
                            Call PBP
                            O = O + 2
                        End If

                    Case 5
                        U$ = qbacks$(P1, Q6%(P1)) + " IS TRYING TO GET THE TEAM LINED UP"
                        Call PBP

                        O = 3
                        U$ = "HE STEPS BACK AND THROWS IT OUT OF BOUNDS"
                        Call PBP

                        K(P1, 3) = K(P1, 3) + 1
                        K2(P1, Q6%(P1), 9) = K2(P1, Q6%(P1), 9) + 1

                        D1 = D1 + 1
                        If D1 = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        Call AddPossessionTime(7, 10)
                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Return 'aka Exit Sub

                End Select

            End If

        End If

        If O = 0 Then Call AddPossessionTime(33, 38)

        If O = 0 And ruleOptType <> 0 And gameClock! > 40 Then gameClock! = gameClock! - 4: timePoss(P1) = timePoss(P1) + 4
        If O = 0 And ruleOptType <> 0 And gameClock! > 43 And Val(Left$(gameTeams$(0), 2)) >= 90 And Val(Left$(gameTeams$(1), 2)) >= 90 Then gameClock! = gameClock! - 3: timePoss(P1) = timePoss(P1) + 3
        If O = 0 And ruleOptType = 0 And gameClock! > 43 And Val(Left$(gameTeams$(0), 2)) = 56 And Val(Left$(gameTeams$(1), 2)) = 56 Then gameClock! = gameClock! + 3: timePoss(P1) = timePoss(P1) - 3
        If O = 0 And ruleOptType = 0 And gameClock! > 43 And Val(Left$(gameTeams$(0), 2)) = 57 And Val(Left$(gameTeams$(1), 2)) = 57 Then gameClock! = gameClock! + 3: timePoss(P1) = timePoss(P1) - 3
        If O = 0 And ruleOptType <> 0 And gameClock! > 43 And Val(Left$(gameTeams$(0), 2)) >= 93 And Val(Left$(gameTeams$(1), 2)) >= 93 Then gameClock! = gameClock! + 3: timePoss(P1) = timePoss(P1) - 3

        If O >= 1 And O <= 3 Then timeElapsed! = 27 - (O * 5) + (Int(Rnd(1) * 5) + 1): gameClock! = gameClock! - timeElapsed!: timePoss(P1) = timePoss(P1) + timeElapsed!

        If O >= 4 Then timeElapsed! = (2 * O) - 5 + (Int(Rnd(1) * 3) + 1): gameClock! = gameClock! - timeElapsed!: timePoss(P1) = timePoss(P1) + timeElapsed!

        Call CheckGameClock
        If gameClock! = 0 Then GoTo 3360

    End If

    Return


    '----- NOODLE -----

    3602 'FIELD GOAL

    If Y >= 97 + goalPostAdj Then
        F2 = kickerNumPAT(P1, Q7%(P1))
    Else
        Call FGDIST
    End If

    U$ = kicker$(P1, Q7%(P1)) + " TROTS ON TO TRY A FIELD GOAL"
    Call PBP

    U$ = "THIS WILL BE A" + Str$(Int(117 + goalPostAdj - Y + .5)) + " YARD ATTEMPT"
    Call PBP

    U$ = "HE WAITS FOR THE SNAP"
    Call PBP

    I5 = Int(Rnd(1) * 300) + 1

    If I5 = 12 Then

        U$ = "THERE'S A BAD SNAP FROM CENTER!!"
        Call PBP
        G = (Int(Rnd(1) * 9) + 6) * -1

        ' *** BAD SNAP FROM CENTER ***

        Call AddPossessionTime(4, 7)

        K(P1, 10) = K(P1, 10) + 1
        K(P1, 9) = K(P1, 9) + 1

        If Y + G < -9.999 Then score(D, Q) = score(D, Q) + 2: score(D, 0) = score(D, 0) + 2: GoTo 3251

        If Y + G <= .001 And Y + G >= -9.999 Then

            Call d100(RN0)

            If RN0 <= 50 Then
                score(D, Q) = score(D, Q) + 2
                score(D, 0) = score(D, 0) + 2
                GoTo 3251
            Else
                X1 = 10
                Call CHANGEPOSS
                GoTo 3200
            End If

            Call CHANGEPOSS
            D1 = 1
            Y1 = 10
            Y = 100 - Y

            If Y1 >= 90 Then Y1 = 100 - Y

            Call d100(RN)

            If RN <= 50 Then
                U$ = gameTeams$(P1) + " TAKES OVER ON THE"
            Else
                U$ = gameMascots$(P1) + " TAKE OVER ON THE"
            End If

            If Y > 50 Then
                U$ = U$ + Str$(Int(100 - Y)) + " YARD LINE"
            Else
                U$ = U$ + Str$(Int(Y)) + " YARD LINE"
            End If

            O = 5

            GoSub 3900

            GoTo 700

        End If

    Else

        Call d100(I)

        If (I >= 97 And Y >= 68 + goalPostAdj And I2 <> 2) Or (I >= 92 And Y < 68 + goalPostAdj And I2 <> 2) Then

            K2(P1, 4 + Q7%(P1) * 4, 17) = K2(P1, 4 + Q7%(P1) * 4, 17) + 1
            U$ = "KICK IS BLOCKED!!!"
            Call PBP

            Call AddPossessionTime(4, 7)

            G = Int(Rnd(1) * 22) * -1
            Call YARDAGE
            Y = 100 - Y - G

            If Y >= 109.949 Then 3250

            If Y >= 100 Then

                Call CHANGEPOSS

                Call d100(RN)
                If RN <= 50 Then
                    U$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
                Else
                    U$ = gameMascots$(P1) + " RECOVER IT IN THE END ZONE!!"
                End If

                Call PBP

                X1 = 6

            Else

                Call d100(RN)

                If RN <= 94 Then
                    Call CHANGEPOSS

                    Y1 = 10

                    If Y >= 90 Then Y1 = 100 - Y

                    D1 = 1

                    Call DELAY

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                    GoTo 700

                Else

                    Call CHANGEPOSS

                    G = 100 - Y
                    SY = 100 - Y
                    EY = SY - G

                    Call MOVEBALL

                    Call d100(RN)
                    If RN <= 50 Then
                        U$ = gameTeams$(P1) + " TAKES IT INTO THE END ZONE!!"
                    Else
                        U$ = gameMascots$(P1) + " TAKE IT INTO THE END ZONE!!"
                    End If

                    Call PBP

                    X1 = 6

                End If

            End If

            GoTo 3200

        End If

        K2(P1, 4 + Q7%(P1) * 4, 17) = K2(P1, 4 + Q7%(P1) * 4, 17) + 1

        U$ = "THE KICK IS UP AND ON ITS WAY"
        Call PBP

        If WX%(0) <> P1 Then
            F2 = F2 - Int(Rnd(1) * 7) + 3 - WX%(1)
        Else
            F2 = F2 + Int(Rnd(1) * 7) - 3 + WX%(1)
        End If

        Call FGATT

        If I <= F2 Then

            K2(P1, 3 + Q7%(P1) * 4, 17) = K2(P1, 3 + Q7%(P1) * 4, 17) + 1
            X1 = 8
            I1 = Q7%(P1)

            Call SCOREUPD

            D1 = 1
            Y1 = 10
            score(P1, 0) = score(P1, 0) + 3
            score(P1, Q) = score(P1, Q) + 3
            Call FGMADE

            Call AddPossessionTime(4, 7)
            Call MADEFG
            Call DisplayField

            Y = 35

            Call CHANGEPOSS
            Call SCOREBOARD

            If ruleOptType <> 0 And goalPostAdj < 0 Then Y = 40
            If ruleOptType = 1 And ruleOptPro = 3 Or ruleOptType = 4 Then Y = 30
            If ruleOptColl = 0 And ruleOptType = 0 Then Y = 40

            Call SCOREBOARD
            Call CheckGameClock
            If gameClock! = 0 Then U3 = 9: GoTo 3360

            If Q > 4 Then 3364

            GoTo 340

        End If

        Call MISSFG
        D1 = 1
        Y1 = 10

        Call CHANGEPOSS

        If Y > 40 And Y <= 79.99 And ruleOptPro = 3 And ruleOptType = 1 Then
            Y = Y - 7
            Y = 100 - Y
        Else
            If Y > 79.99 Or goalPostAdj < 0 Then Y = 20
            If Y > 40 And Y <= 79.99 And goalPostAdj = 0 Then Y = 100 - Y
        End If

        Call AddPossessionTime(4, 7)
        Call CheckGameClock
        If gameClock! = 0 Then GoTo 3360

        'College overtime
        If Q > 4 And NG%(14) = 3 Then 3364

    End If

    GoTo 700


    '----- NOODLE -----


    3840 '
    K(P1, 28) = K(P1, 28) + 1
    U$ = "KICK IS BLOCKED!!!"

    Call PBP

    Call AddPossessionTime(4, 7)

    G = Int(Rnd(1) * 30) * -1

    Call YARDAGE
    Y = 100 - Y - G

    If Y >= 109.949 Then 3250

    If Y >= 100 Then
        Call CHANGEPOSS

        U$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
        Call PBP

        X1 = 6
        GoTo 3200
    End If

    Call d100(RN)

    If RN > 60 Then

        Call CHANGEPOSS
        G = 100 - Y
        SY = 100 - Y
        EY = SY - G

        Call MOVEBALL

        U$ = gameTeams$(P1) + " TAKES THE BALL INTO THE END ZONE!!"
        Call PBP

        X1 = 6

        GoTo 3200

    Else

        Call CHANGEPOSS: Y1 = 10

        If Y >= 90 Then Y1 = 100 - Y

        D1 = 1

        Call DELAY

        If gameClock! = 0 Then GoTo 3360

    End If

    GoTo 700


    '----- NOODLE -----


    'Pass Intercepted

    3876 '
    X = 0
    K(P1, 3) = K(P1, 3) + 1
    K(P1, 5) = K(P1, 5) + 1
    K2(P1, I1, 9) = K2(P1, I1, 9) + 1
    K2(P1, I1, 11) = K2(P1, I1, 11) + 1

    Call AddPossessionTime(5, 8)

    G = Int(Rnd(1) * 10) + 1
    Call YARDAGE

    If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then
        U$ = U$ + " IN THE END ZONE"
        Call PBP
        Call Touchback
    Else

        If G + Y > 49.99 Then U$ = U$ + " AT THE " + Str$(Int(100 - G - Y)) + " YARD LINE": Call PBP
        If G + Y < 49.99 Then U$ = U$ + " AT THE " + Str$(Int(G + Y)) + " YARD LINE": Call PBP

        Call CHANGEPOSS

        D1 = 1
        Y = 100 - G - Y
        YT = 100 - Y

        Call YARDLINE

        G = Int(Rnd(1) * 25) + 1

        If G >= 24 Then
            G = Int(Rnd(1) * 70) + 31
        End If

        Call YARDAGE

        X1 = 5
        SY = 100 - Y
        EY = SY - G

        Call MOVEBALL

        U$ = "RETURNED FOR "

        If G + Y > 99.99 Then

            U$ = U$ + Str$(Int(100 - Y)) + " YARDS"
            Call PBP
            GoTo 3200

        Else

            U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
            Call PBP

            O = 5
            Y = Y + G
            Y1 = 10

            If Y >= 90 Then Y1 = 100 - Y

        End If

        GoSub 3900

        Call DELAY

    End If

    GoTo 700

    '----- NOODLE -----

    'start of a subroutine??

    6605 '
    Call CHANGEPOSS

    Y = 1
    YT = 100 - Y

    Call YARDLINE

    Call d100(RN)
    If RN <= 18 Then
        G = 110
    Else
        G = Int(Rnd(1) * 21) + 50
    End If

    Call YARDAGE

    X1 = 12
    SY = 100 - Y
    EY = SY - G
    Call MOVEBALL

    U$ = "RETURNED FOR "

    If G + Y > 99.99 Then

        U$ = U$ + Str$(Int(100 - Y)) + " YARDS"
        Call PBP

        score(P1, Q) = score(P1, Q) + 2
        score(P1, 0) = score(P1, 0) + 2

        U$ = "GOOD FOR 2 POINTS!!"
        Call PBP

        Call DisplayField
        Call SCOREUPD

    Else

        U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
        Call PBP

    End If

    E = 0
    Call CHANGEPOSS

    '--- end of a subroutine

    GoTo 700

    '----- NOODLE -----

    850 '
    'This could be a small sub
    'if other places don't need
    'to go to label 330
    Call TICKER

    If halfTime = 1 Then

        halfTime = 0

        'PAH

        If autoPlay <> 1 Then
            For I3 = 0 To 1: Call CHANGEQB: Next I3
        End If

        Randomize Timer

        Call SCOREBOARD

        GoTo 330

    Else

        If endGame = 1 Then
            Call EndOfGame
            Exit Sub
        Else
            GoTo 700
        End If

    End If

End Sub
