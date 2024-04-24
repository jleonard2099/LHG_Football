'----------------------------------------
'           FOOTBALL Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining weather and
'other game time settings.
Sub FOOTBALL ()

    Shared actualAttendance&, avgAttendance&

    _Title "3-in-1 Football v" + GAME_VERSION$ + " - GRIDIRON!"

    Shared scheduleFile$
    Shared tickerStart

    Shared location$(), overtime$()
    Shared playMode$()
    Shared rulesType$(), ruleYrPro$(), ruleYrColl$()


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
    Input #4, firstDownClockOpt
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

        tickerGames = 1

    Else

        tickerGames = 0

    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2:
            Input #6, schedGame(I)
        Next

        Close #6

        currGame = schedGame(0) + schedGame(2)

        'Build in an artificial delay for every 50 games
        'This might let things in memory / CPU clear out?
        'If (currGame Mod 50) = 0 Then
        '_Delay 2
        'End If

        'Open "pbplog" + Str$(currGame) For Append As #11
        Open "runlog" For Append As #11

        'Read in schedule options
        Open diskPaths$(3) + scheduleFile$ As #7 Len = SCHEDULE_SIZE_BYTES

        For X = 0 To 20
            Field #7, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
        Next

        Field #7, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)

        Get #7, schedGame(0) + schedGame(2)

        For X = 0 To 20:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            diskIDs$(X) = Q$(22 + X)
        Next

        Close #7

        autoPlay = 1

    Else

        'Open "pbplog" For Append As #11

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

            Locate 2, 2: Color 14
            Print "(1) ";
            Color 15: Print playMode$(playerOpt)
            Locate 3, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 2), teamRat_GAME(0, 1): Print gameTeams$(0)
            Locate 4, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 2), teamRat_GAME(1, 1): Print gameTeams$(1)
            Locate 5, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "GAME LOCATION: "; location$(gameLoc)
            Locate 6, 2: Color 14
            Print "(5) ";
            Color 15: Print "TYPE OF GAME: "; rulesType$(ruleOptType)
            Locate 7, 2: Color 14
            Print "(6) ";
            Color 15: Print "PRO RULES: "; ruleYrPro$(ruleOptPro)
            Locate 8, 2: Color 14
            Print "(7) ";
            Color 15: Print "COLLEGE RULES: "; ruleYrColl$(ruleOptColl)
            Locate 9, 2: Color 14
            Print "(8) ";
            Color 15: Print "OVERTIME: "; overtime$(NG%(14))
            Locate 10, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 11, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; pbpDelay!

            'Locate 14, 2: Print "NOTES: CLOCK IS STOPPED AFTER A 1ST DOWN IN COLLEGE BEGINNING IN 1968"
            'Locate , 2: Print "       HOLDING IS A 15 YARD PENALTY FOR COLLEGE PRE-1981 AND PRO PRE-1974"
            'Locate , 2: Print "       1991-PRESENT, COLLEGE HOLDING MARKED FROM POINT OF INFRACTION"
            'Locate , 2: Print "       1974-77, PRO HOLDING ON A RUN IS 10 YDS AND 15 ON A PASS"
            'Locate , 2: Print "       1978-1994, ALL PRO HOLDING PENALTIES ARE 10 YARDS"
            'Locate , 2: Print "       PRE-1974, IN THE PROS, GOAL POSTS WERE ON THE GOAL LINE, THE BALL WAS"
            'Locate , 2: Print "       PLACED ON THE 20 AFTER MISSED FIELD GOALS, KICKOFFS WERE FROM THE 40"
            'Locate , 2: Print "       1992-PRESENT, COLLEGE RULES ALLOW ADVANCEMENT OF A FUMBLE"
            'Locate , 2: Print "       PRE-1984, IN COLLEGE PASS INTERFERENCE AT SPOT OF INFRACTION"
            'Locate , 2: Print "       PRO 1994-, KO FROM 35 TO 30, 2-PT CONVERSION, MISSED FG @ POINT OF KICK"

            Locate 14, 2: Color 14
            Print "NOTE:: "
            Color 15

            Locate , 2: Print "*NCAA*  1968+, CLOCK STOPPED AFTER A 1ST DOWN"
            Locate , 3: Print "        PRE-1984, PASS INTERFERENCE SPOTTED AT INFRACTION"
            Locate , 3: Print "        1992+, ADVANCING FUMBLE ALLOWED"
            Locate , 3: Print "        HOLDING: PRE-1984, 15 YDS; 1984+ 10 YDS @ PT OF INFRACTION"
            Locate , 2: Print "*PRO*   PRE-1974, GOAL POSTS ON GOAL, BALL ON 20 AFTER MISS FG"
            Locate , 3: Print "        1994+, 2-PT CONVERSION, MISSED FG @ POINT OF KICK"
            Locate , 3: Print "        2017+, 10 MINUTE OT, 2 TOs, EACH TEAM POSSESS + FIRST SCORE WINS"
            Locate , 3: Print "        HOLDING: PRE-1974, 15 YDS; 1974-77, 10 YDS RUN / 15 YDS PASS;"
            Locate , 3: Print "                  1978+, 10 YDS ON ALL";
            Locate , 3: Print "        KO: PRE-1974, 40 YL; 1974-1993, 35 YL; 1994-2010, 30 YL; 2011+, 35 YL"

            Do

                Color 7

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    iVal = Val(I$)
                    If UCase$(I$) = "A" Then iVal = 10
                End If

            Loop Until iVal >= 0 And iVal <= 10 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case iVal

                    Case 0:
                        Open "DEFAULT" For Output As #8
                        Print #8, playerOpt '
                        Print #8, ruleOptType '
                        Print #8, gameLoc ' Home Field advantage
                        Print #8, pbpDelay! '
                        Print #8, firstDownClockOpt '
                        Print #8, ruleOptPro '
                        Print #8, ruleOptColl '
                        Print #8, NG%(14) 'overtime setting
                        Close #8

                    Case 1
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(diskIDs$(0), NTMS, teamNbr, gameTeams$(0), 0)
                        teamIdx_GAME(0) = teamNbr

                    Case 3:
                        Call ChooseGameTeam(diskIDs$(1), NTMS, teamNbr, gameTeams$(1), 1)
                        teamIdx_GAME(1) = teamNbr

                    Case 4:
                        gameLoc = gameLoc + 1
                        If gameLoc = 3 Then gameLoc = 1

                    Case 5:
                        ruleOptType = ruleOptType + 1
                        If ruleOptType = 6 Then ruleOptType = 0

                    Case 6:
                        ruleOptPro = ruleOptPro + 1
                        If ruleOptPro > 9 Then ruleOptPro = 0

                    Case 7
                        ruleOptColl = ruleOptColl + 1
                        If ruleOptColl > 11 Then ruleOptColl = 0

                    Case 8:
                        NG%(14) = NG%(14) + 1
                        If NG%(14) > 3 Then NG%(14) = 0

                    Case 9:
                        '

                    Case 10
                        Do
                            Locate 12, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                End Select

                If iVal >= 4 Then

                    goalPostAdj = 0
                    firstDownClockOpt = 0

                    If ruleOptType > 0 And ruleOptPro <= 1 Then
                        'Pro, Pre-1974
                        goalPostAdj = -10
                    End If

                    If ruleOptType = 4 Then
                        'WFL
                        goalPostAdj = 0
                    End If

                    If ruleOptType = 0 And ruleOptColl = 0 Then
                        'College, Pre-1868
                        firstDownClockOpt = 1
                    End If

                End If

            End If

        Loop Until (iVal = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27) Or UCase$(I$) = "Q"

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

            'System
            Exit Sub

        End If

    End If

    For I = 0 To 1:
        teamYears(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
    Next I

    'Determine attendance
    ' ** In the future, we could break this up
    ' ** by averages over each decade, based on
    ' ** available data (specifics not often recorded)

    If gameLoc = 2 Then

        'Neutral stadium and attendance
        gameStadium$ = "NEUTRAL FIELD"

        'These averages should be pretty accurate for
        'at least years 1980 +
        If ruleOptType = 0 Then
            'Random # 5,000 - 35,000 for College
            actualAttendance& = Int(Rnd(1) * 30000) + 5000
        Else
            'Random # 50,000 - 80,000 for Pro
            actualAttendance& = Int(Rnd(1) * 30000) + 50000
        End If

    Else

        If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + diskIDs$(gameLoc)) Then
            'Generic stadium and attendance
            gameStadium$ = "MEMORIAL STADIUM"

            'Random # 45,000 - 55,000
            actualAttendance& = Int(Rnd(1) * 10000) + 45000

        Else

            Open diskPaths$(0) + DATA_FILE_NAME$ + "." + diskIDs$(gameLoc) For Random As #9 Len = DATA_SIZE_BYTES

            Field #9, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)

            Get #9, teamIdx_GAME(gameLoc)

            gameStadium$ = RTrim$(Q$(1))
            avgAttendance& = CVL(Q$(3))
            Z$ = Q$(4)

            Close #9

            'Determine attendance randomly
            If Z$ <> "x" Then
                'Generic stadium and attendance
                gameStadium$ = "MEMORIAL STADIUM"

                'Random # 45,000 - 55,000
                actualAttendance& = Int(Rnd(1) * 10000) + 45000
            Else
                'Use random # +/- 5,000 of team's average attendance
                '-- real #'s reflect a wider range throughout the year,
                '-- however many teams have their avg. attendance close
                '-- to max capacity. Since we don't have max capacity
                '-- available for now, we're less likely to go over if
                '-- we don't go higher on our range.
                actualAttendance& = avgAttendance& + Int(Rnd(1) * 10000) - 5000

                'If original team's average attendance was less than 10,000,
                'we could end up with a negative number. So let's use an
                'arbitrary one instead.
                'Random # from 500 - 700
                ' ** I commented this out as it seems absurdly low.
                ' ** Was it meant to entertain the idea of high school football???
                'If actualAttendance& < 0 Then actualAttendance& = Int(Rnd(1) * 500) + 200
            End If

        End If

    End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case playerOpt

        Case 1:
            playerMode = 1
            compTeam = 0

        Case 2:
            playerMode = 1
            compTeam = 1

        Case 3:
            playerMode = 2

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    goalPostAdj = 0
    firstDownClockOpt = 0

    If teamRat_GAME(0, 2) = teamRat_GAME(1, 2) And teamRat_GAME(0, 1) = teamRat_GAME(1, 1) Then
        Call ColorChange
    End If

    If ruleOptType > 0 And ruleOptPro <= 1 Then
        'Pro, Pre-1974
        goalPostAdj = -10
    End If

    If ruleOptType = 4 Then
        'WFL
        goalPostAdj = 0
    End If

    If ruleOptType = 0 And ruleOptColl = 0 Then
        'College, Pre-1868
        firstDownClockOpt = 1
    End If

    Color 15, 0

    If ruleOptType > 0 Then
        leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 2
        leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 2
    End If

    leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 4
    leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 4

    Call NAMEEDIT

    For currTeam = 0 To 1

        If autoPlay = 1 Then
            'Autoamtic determine QB
            Erase activeKicker

            qbNumber(0) = NG%(5)

            If qbNumber(0) = 4 Then
                useRandomQB(0) = 1
                qbNumber(0) = 0
            Else
                qbMobility(0) = NG%(6)
            End If

            qbNumber(1) = NG%(8)

            If qbNumber(1) = 4 Then
                useRandomQB(1) = 1
                qbNumber(1) = 0
            Else
                qbMobility(1) = NG%(9)
            End If

            For I = 0 To 3
                qbRushIdx(currTeam, I) = -1
            Next I

            If useRandomQB(currTeam) = 0 Then

                'Store the index (from running backs)
                'of each QB that has rushing contributions
                'We leave out the current QB because this
                'is only useded to check that the QB doesn't
                'handoff to another QB
                For currBack = 0 To 9:
                    For currQB = 0 To 3
                        If rbacks$(currTeam, currBack) = qbacks$(currTeam, currQB) And currQB <> qbNumber(currTeam) Then
                            player$ = rbacks$(currTeam, currBack)
                            qbRushIdx(currTeam, currQB) = currBack
                        End If
                    Next currQB
                Next currBack

            Else

                'We need to assign a mobility rating
                'based on the QB contribution.
                'We'll start with the starter.
                qbContrib = GetQBRushPctGame%(qbacks$(currTeam, qbNumber(currTeam)), currTeam)
                qbMobility(currTeam) = GetQBMobility%(qbContrib)

            End If

        Else

            'Manual prompt for QB
            '-- we could probably update the routine
            '-- to handle autoPlay = 1 by itself
            Call CHOOSEQB(currTeam)

        End If

    Next currTeam

    Call SETWEATHER

    '----------
    'Coin Toss
    '----------
    'Print #11, gameTeams$(0); " vs "; gameTeams$(1)

    winTeam = Int(Rnd(1) * 2)

    Call SCOUT
    Call HALFSTART

    quarter = 1
    gameClock! = 900

    For I = 0 To 1
        For I1 = 0 To 4
            score(I, I1) = 0
        Next I1
    Next I

    Call SETSCREEN

    Call Rolld100(RN0)

    If RN0 <= 50 Then
        pbpString$ = gameTeams$(winTeam) + " WON THE COIN TOSS"
    Else
        pbpString$ = gameMascots$(winTeam) + " WON THE COIN TOSS"
    End If

    Call PBP


    '----------------------------------------
    '           START OF GAME
    '----------------------------------------

    '-- Start of a "kickoff" routine

    330 '
    If ruleOptType > 0 And ruleOptPro < 8 Then
        'Pro rules before 2008
        Call KOChoices_NoDefer
    Else
        Call KOChoices_Defer
    End If

    340 '
    Swap P1, D

    Call MARKER1

    X = 0
    U3 = 0

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    'Determine 1st returner
    For I = 0 To 2
        If krNumRet(D, I) = 100 Then
            KN = I
            Exit For
        End If
    Next

    'Find 2nd returner by contribution
    krChance = Int(100 * Rnd(1)) + 1

    For I = 2 To 0 Step -1
        If krChance <= krNumRet(D, I) Then
            I1 = I
        End If
    Next I

    Call Rolld100(RN0)

    Select Case KN

        Case 0
            pbpString$ = kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) IS DEEP TO RECEIVE THE KICK"
            Call PBP

        Case 1
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

        Case 2
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

    End Select

    If KN > 0 And RN0 <= 50 Then
        pbpString$ = kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) AND " + kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) ARE DEEP"
    Else
        pbpString$ = kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) AND " + kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) ARE DEEP"
    End If

    Call PBP

    If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

        kickChoice = 0
        If gameClock! <= 180 And quarter = 4 And score(P1, 0) - score(D, 0) < 0 Then
            kickChoice = 2
        End If

    Else

        pbpString$ = "0-REGULAR  1-SQUIBB  2-ONSIDE KICK " + gameTeams$(P1)
        Call PBP

        Do
            I$ = GetKeyPress$
            kickChoice = Val(I$)
        Loop Until kickChoice >= 0 And kickChoice <= 2

    End If

    kickNotReturned = 0

    Select Case kickChoice

        Case 0
            Call KickoffDistance
            'kickYL represents yard line where kick lands

            'Swap defense and offense
            Call CHANGEPOSS

            fairCatchZone = 0

            Select Case ruleOptType

                Case 0
                    'College
                    If ruleOptColl <= 10 Then
                        fairCatchLine = 20
                    Else
                        fairCatchLine = 25
                    End If

                Case Is >= 1
                    'Pro
                    If ruleOptPro <= 7 Then
                        fairCatchLine = 20
                    Else
                        fairCatchLine = 25
                    End If

            End Select

            If kickYL <= fairCatchLine Then fairCatchZone = 1

            If ((ruleOptType > 0 And ruleOptPro >= 9) Or (ruleOptType = 0 And ruleOptColl >= 11)) And fairCatchZone = 1 Then
                'Newer rules - fair Catch Allowed

                Call Rolld100(RN0)

                If (ruleOptType = 0 And RN0 <= 12) Or (ruleOptTyp1 = 1 And RN0 <= 6) Then
                    'Based on available date, fair catch was called:
                    ' NCAA - about 12% of the time since 2018
                    ' NFL - about 6% of the time in 2023

                    fairCatch = 1

                End If

            End If

            If fairCatch = 1 Then

                ydsToScore = 100 - kickYL
                Call YARDLINE

                RN0 = Int(Rnd(1) * 2) + 1

                If RN0 = 1 Then
                    pbpString$ = kickRet$(P1, I1) + " SIGNALS FOR FAIR CATCH AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                Else
                    pbpString$ = kickRet$(P1, I1) + " CALLS FOR A FAIR CATCH ON THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                End If

                Call PBP

                'Touchback will auto-change possession
                Call CHANGEPOSS
                Call Touchback
                kickNotReturned = 1

            Else


                If kickYL <= -9.99 Then

                    'Kicked out of endzone
                    'Automatic touchback
                    'Touchback will auto-change possession
                    Call CHANGEPOSS
                    Call Touchback
                    'Print #11, "TOUCHBACK - Kickoff, Automatic"
                    kickNotReturned = 1

                Else

                    X = 0


                    If kickYL < .01 Then

                        'Kick taken in end zone
                        ydLine = -(kickYL - 2 * kickYL)
                        ydsToScore = 100 - ydLine

                        Call MARKER
                        Call YARDLINE

                        pbpString$ = "KICK TAKEN BY " + kickRet$(P1, I1) + Str$(Int((kickYL - 2 * kickYL) + .5)) + " YDS DEEP IN THE END ZONE"

                        Call PBP

                        'Computer is receiving?
                        'Return it within first 2 yards of endzone
                        If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                            If kickYL < -2 Then
                                returnChoice = 1
                            Else
                                returnChoice = 2
                            End If

                        Else

                            pbpString$ = "1-DOWN IT 2-RETURN IT"
                            Call PBP

                            Do
                                I$ = GetKeyPress$
                                returnChoice = Val(I$)
                            Loop Until returnChoice = 1 Or returnChoice = 2

                        End If

                        If returnChoice = 1 Then

                            'Received in end zone,
                            'kneel for touchback
                            'Touchback will auto-change possession
                            Call CHANGEPOSS
                            Call Touchback
                            'Print #11, "TOUCHBACK, Kickoff, Downed"
                            kickNotReturned = 1

                        Else

                            kickNotReturned = 0

                        End If

                    Else

                        'Kick outside end zone - adjust position
                        kickYL = kickYL + (Int(Rnd(1) * 11) - 5) / 10

                        ydLine = kickYL
                        ydsToScore = 100 - ydLine

                        Call MARKER
                        Call YARDLINE

                        pbpString$ = "KICK TAKEN BY " + kickRet$(P1, I1) + " AT THE" + Str$(Int(kickYL + .5)) + " YD LINE "
                        Call PBP

                    End If

                End If

            End If 'Done checking if fair catch


            'Determine return yards based on returner's average
            If kickNotReturned = 0 Then

                RN0 = Int(Rnd(1) * 1000) + 1

                Select Case krYdsPerRet(P1, I1)

                    Case Is <= 8

                        Select Case RN0

                            Case 1 To 5
                                'Kick return for TD - 0.5% chance
                                ydsGained = 110

                            Case 6 To 20 '1.5% chance
                                '45 to 65 yds + (-21 to to 3 yds); based on KR's avg - 7
                                ydsGained = Int((Rnd(1) * 21) + 45) + (krYdsPerRet(P1, I1) - 7) * 3

                            Case 21 To 70 '5% chance
                                '25 to 45 ys + (-21 to to 3 yds); based on KR's avg - 7
                                ydsGained = Int((Rnd(1) * 21) + 25) + (krYdsPerRet(P1, I1) - 7) * 3

                            Case Is >= 71 '93% chance
                                '15 to 25 yds  + (-21 to to 3 yds); based on KR's avg - 7
                                ydsGained = Int((Rnd(1) * 10) + 15) + (krYdsPerRet(P1, I1) - 7) * 3

                        End Select

                    Case 9

                        'If RN0 <= 50 Then
                        If RN0 <= 12 Then
                            'Kick return for TD - 1.2% chance
                            ydsGained = 110
                        Else
                            '15 to 33 yds
                            ydsGained = Int((Rnd(1) * 19) + 15)
                        End If

                    Case Is >= 10

                        'If RN0 <= 80 Then
                        If RN0 <= 20 Then
                            'Kick return for TD - 2% chance
                            ydsGained = 110
                        Else
                            '15 to 33 yds + (3 yds / int value of return average > 10)
                            'So a returner with 12 yard average would be 21 to 39 yards
                            ydsGained = Int((Rnd(1) * 19) + 15) + (krYdsPerRet(P1, I1) - 10) * 3
                        End If

                End Select

                startYds = 100 - ydLine
                endYds = startYds - ydsGained

                Call MOVEBALL

                X1 = 3
                gameStatsPlayer(P1, I1, 14) = gameStatsPlayer(P1, I1, 14) + 1

                If ydsGained + kickYL > 89.99 Then

                    gameStatsPlayer(P1, I1, 15) = gameStatsPlayer(P1, I1, 15) + 100 - kickYL
                    gameStatsPlayer(P1, I1, 16) = gameStatsPlayer(P1, I1, 16) + 1

                    If 100 - kickYL > kickReturners(P1, I1) Then
                        gameKR$(P1, I1) = "t"
                        kickReturners(P1, I1) = 100 - kickYL
                    End If

                    'Touchdown!
                    GoTo 3200

                End If

                If ydsGained + kickYL <= -.05 Then
                    'Returner didn't make it out of the endzone
                    Call CHANGEPOSS
                    Call Touchback
                    'Print #11, "TOUCHBACK, Kickoff, Nowhere"

                Else

                    ydLine = ydsGained + kickYL

                    If ydsGained > kickReturners(P1, I1) Then
                        gameKR$(P1, I1) = ""
                        kickReturners(P1, I1) = ydsGained
                    End If

                    gameStatsPlayer(P1, I1, 15) = gameStatsPlayer(P1, I1, 15) + ydsGained

                    F% = 3

                    Call FUMBLE

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    Call AddPossessionTime(4, 7)

                    RN1 = Int(Rnd(1) * 1000) + 1
                    If RN1 >= 960 - teamRat_GAME(P1, 0) Then Call CLIPPING

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                End If

            End If

        Case 1
            'SQUIB KICK
            Call SQUIB
            Call FUMBLE

            If ballFumbled = 1 Then Call FumblePBP: GoTo 650

            Call AddPossessionTime(4, 7)
            Call Rolld100(RN0)

            '3% chance of clipping on squib kick
            If RN0 >= 97 Then
                Call CLIPPING
            End If

            Call CheckGameClock
            If gameClock! = 0 Then GoTo 3360

        Case 2
            'Onside Kick
            Call ONSIDE

            currDown = 1
            ydsToFirst = 10

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
        convChoice = 2
    Else

        If ruleOptType = 1 And ruleOptPro < 4 Then
            'NFL prior to 1994 (2 PT conversion not supported)
            convChoice = 1

        Else

            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                pbpString$ = "1-KICK EP 2-GO FOR TWO"
                Call PBP

                ' *** Computer on Offense, or PC vs. PC ***

                'By default, go for one
                convChoice = 1

                'Determine score deficit
                I5 = score(P1, 0) - score(D, 0)

                ' ***** When to "Go for TWO" *****

                If ruleOptType = 1 And ruleOptPro >= 4 Then
                    'NFL, Post 1994 (when 2 pt conversion was implemented)
                    ' 4th quarter, down by 2 and 2 minutes left
                    ' 4th quarter, down by 9 and 5 minutes left
                    ' 4th quarter, down by 6 and 7 minutes left
                    If quarter = 4 And (I5 = -9 And gameClock! < 600 Or I5 = -2 And gameClock! < 120 Or I5 = -5 And gameClock! < 420) Then convChoice = 2

                Else
                    'College / AFL / WLAF / USFL
                    ' 3rd / 4th quarter, down by 18
                    ' 4th quarter, down by 16
                    If quarter >= 3 And I5 = -18 Or quarter = 4 And I5 = -16 Then
                        convChoice = 2
                    Else
                        '1) down by 2
                        '2) ahead by 5 | 12
                        If I5 = -2 Or I5 = 1 Or I5 = 5 Or I5 = 12 Then
                            convChoice = 2
                        Else
                            'Anytime outside the final 2-min warning
                            If Not (quarter = 4 And gameClock! > 240 Or quarter < 4) Then
                                'down by 1 | 4 | 5 | 8 | 10 | 13
                                If I5 = -1 Or I5 = -5 Or I5 = -8 Or I5 = -13 Or I5 = -4 Or I5 = -10 Then convChoice = 2
                            End If

                        End If

                    End If

                End If

            Else
                ' *** Human player on Offense ***
                pbpString$ = "1-KICK EP 2-GO FOR TWO"

                Do
                    Call PBP
                    I$ = GetKeyPress$
                    convChoice = Val(I$)
                Loop Until convChoice = 1 Or convChoice = 2

            End If

        End If

    End If

    '3702
    If convChoice = 2 Then

        'Since the "down" is not reset after the score,
        'we should treat the 2-point conversion as a
        '"1st down" for AI / play selection purposes
        'D = 1

        ydsGained = 0
        endZone = 1

        If ruleOptType = 1 And ruleOptPro >= 4 Then
            '1994 and beyond
            'NFL rules
            '2 point conversion from the 2
            ydLine = 98
        End If

        If ruleOptType = 0 Then
            'College rules
            '2 point conversion from the 3
            ydLine = 97
        End If

        'Choose a play for 2-point conversion
        GoTo 700

    Else

        ' ******************************
        ' **** POINT AFTER ATTEMPT *****
        ' ******************************
        'Adjust distance for goal posts
        K1(nbrScores, 4) = K1(nbrScores, 4) + 10

        'Determine kicker
        Call Rolld100(RN1)

        For J = 1 To 0 Step -1:
            'Auto-determine target kicker, I1
            'If RN1 <= kickerPATPct(P1, J) Then I1 = J
            If RN1 <= kickerPATPct(P1, J) Then activeKicker(P1) = J

        Next J

        'Determine success
        Call Rolld100(RN1)

        gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) + 1

        pbpString$ = kicker$(P1, activeKicker(P1)) + " IS ON TO TRY THE EXTRA POINT"
        Call PBP

        If RN1 <= kickerIdx(P1, activeKicker(P1)) Then

            gameStatsPlayer(P1, 5 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 5 + activeKicker(P1) * 4, 17) + 1

            pbpString$ = "THE KICK IS UP..."
            Call PBP
            Call MADEPAT

            'Increase score
            score(P1, 0) = score(P1, 0) + 1
            score(P1, quarter) = score(P1, quarter) + 1

            'Track the attempt for the kicker
            K1(nbrScores, 6) = activeKicker(P1)
            K1(nbrScores, 4) = K1(nbrScores, 4) + 1

            Call DisplayField

        Else

            Call Rolld100(RN2)

            '-- 9% chance of blocked PAT
            If RN2 <= 9 Then

                pbpString$ = "KICK IS BLOCKED!!"
                Call PBP

                If ruleOptType <> 1 Then
                    'Not NFL
                    GoTo 6605
                End If

            Else

                Call MISSPAT

            End If

        End If

    End If

    Return 'For PointsAfterTD "sub"





    '----- NOODLE -----

    650 '

    Call Rolld100(RN0)

    gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1

    If RN0 <= 50 Then

        Call PBPINT

        pbpString$ = pbpString$ + gameTeams$(D) + " RECOVERS!!"
        Call PBP

        currDown = 1
        ydsToFirst = 10
        gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1
        ydLine = 100 - ydLine
        Call CHANGEPOSS

        '659

        If ruleOptType > 0 Or ruleOptColl >= 7 Then
            'Pro Rules or College after 1991

            RN1 = Int(Rnd(1) * 3) + 1

            If RN1 > 2 Then

                G1 = Int(Rnd(1) * 48) + 1
                X1 = 10

                If G1 >= 46 Then
                    G1 = 100
                Else
                    G1 = Int(Rnd(1) * 18)
                End If

                G1 = G1 + (Int(Rnd(1) * 11) - 5) / 10

                pbpString$ = "RETURNED FOR "

                returnYards = G1 + ydLine

                If returnYards >= 99.99 Then
                    pbpString$ = pbpString$ + Str$(Int((100 - ydLine) * 10 + .5) / 10) + " YDS "
                    Call PBP

                    'Touchdown!
                    GoTo 3200

                Else

                    'returnYards < 99.99
                    pbpString$ = pbpString$ + Str$(Int((G1) * 10 + .5) / 10) + " YDS"
                    Call PBP

                    If returnYards <= .01 Then
                        'Safety!
                        GoTo 3250
                    End If

                    ydLine = ydLine + G1

                End If

            End If

        End If

        If ydLine >= 90 Then ydsToFirst = 100 - ydLine
    Else

        pbpString$ = gameTeams$(P1) + " RECOVERS!!"
        Call PBP

        I = 50
        playSegment = 1

        GoTo 3303

    End If

    playSegment = 5

    GoSub 3900

    'College Overtime
    If NG%(14) = 3 And quarter > 4 Then
        GoTo 3364
    End If

    700 '
    Call MARKER1

    Call DISPWEATHER

    playSegment = 0
    ballFumbled = 0
    ydsGained = 0
    W5 = 0

    If U3 = 9 Then
        U3 = 0
        GoTo 340
    End If

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    Do

        Call SCOREBOARD
        Call DEFMENU

        'Offense Play Selection
        '-- Default to user-select
        autoPlaySelect = 0

        Do

            validPlay = 1

            'If computer is on Offense
            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                userSelectBC = 0
                autoPlaySelect = 1

                'This chooses the call but not the intended player
                Call ComputerOffense
                chosenPlay = playCall

                'If a run play was not chosen, shift chosenPlay because
                'It should be a unique value over 14
                If playCall > 8 Then chosenPlay = playCall + 6

            Else

                userSelectBC = 1
                autoPlaySelect = 0

                Do
                    3118 '
                    'Determine play call

                    Call FRAMEOPTION(10, P1)
                    Print "SELECT OFFENSE "; gameTeams$(P1)

                    For playCall = 0 To 4:
                        Locate 11 + playCall, 14
                        Print playCall; "- "; playSelect$(playCall); Tab(41); playCall + 5; "- "; playSelect$(playCall + 5)
                    Next playCall

                    Locate , 15
                    Print "G - GADGET PLAY"; Tab(42); "T - CALL TIMEOUT"

                    Do

                        I$ = GetKeyPress$

                        If I$ = "8" Then
                            I$ = "K"
                        End If

                        If UCase$(I$) = "Q" Then
                            'System
                            Exit Sub
                        End If

                        ' *** Select Gadget Play ***
                        If UCase$(I$) = "G" Then

                            Call FRAMEOPTION(8, P1)
                            Print "SELECT GADGET PLAY "; gameTeams$(P1)

                            For G2 = 1 To 3:
                                Locate 11 + G2, 14
                                Print G2; "- "; gadget$(G2)
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
                                    Call FLEA(ffSuccess)

                                    If ffSuccess <= ffPctSuccess Then

                                        playType = 1
                                        W5 = 1
                                        ydsGained = (Int(Rnd(1) * 6) + 10)

                                        'Sacked
                                        GoTo 3541

                                    Else

                                        Call Rolld100(RN0)

                                        If RN0 <= intChance Then

                                            ydsGained = CP
                                            FF% = 2
                                            'Intercepted!
                                            GoTo 3552

                                        Else

                                            Call Rolld100(RN0)
                                            targetWRName$ = wdRec$(P1, I3)
                                            I3 = I3 + 10

                                            If RN0 <= catchPctSuccess Then
                                                FF% = 2
                                                GoTo 3586
                                            Else
                                                ydsGained = 0
                                                GoTo 3535
                                            End If

                                        End If

                                    End If

                                Case 2
                                    ' *** REVERSE, REVERSE! ***
                                    isReverse = 1

                                    GoSub 3128

                                    Call QBLINE

                                    If userSelectBC = 1 Then

                                        userSelectBC = 0
                                        playType = 1
                                        I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0)

                                        Call REVGAIN
                                        GoTo 3416

                                    Else

                                        3181 '
                                        'Random ball carrier
                                        Call Rolld100(I2)

                                        For I = 9 To 0 Step -1:
                                            If I2 <= rbrushContrib(P1, I) Then
                                                I1 = I
                                            End If
                                        Next I

                                        playType = 1

                                        '8335
                                        'If the ball carrier chosen is a benched QB
                                        If I1 = (qbRushIdx(P1, 0) Or I1 = qbRushIdx(P1, 1) Or I1 = qbRushIdx(P1, 2) Or I1 = qbRushIdx(P1, 3)) Then

                                            'Check if they have any receptions
                                            If rbNumRec(P1, I1) > 0 Then
                                                'If so, use them as a runner
                                                GoTo 3403

                                            Else
                                                'Check if active QB is a runner
                                                For currBack = 0 To 9

                                                    If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                                                        I1 = currBack
                                                        'Regular gain
                                                        GoTo 3403
                                                    End If

                                                Next

                                                If isReverse = 1 Then
                                                    'QB can't reverse to himself
                                                    'Choose another carrier
                                                    GoTo 3181
                                                Else
                                                    'Regular rush gain (can't do a reverse)
                                                    GoTo 3401
                                                End If

                                            End If

                                        Else

                                            '8336
                                            'Adjust yardage based on league rating and weather
                                            I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                                            If isReverse = 1 Then
                                                'Reverse play?
                                                Call REVGAIN
                                                GoTo 3416
                                            Else
                                                'Regular gain
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

                                    catchPctSuccess = 5

                                    If (coverage = 1 Or coverage = 3) And HB%(P9) = 0 Then catchPctSuccess = 30

                                    'Arbitrary 20% chance of interception
                                    intChance = 20
                                    ydsGained = Int(Rnd(1) * 71) + 30
                                    CP = Int(Rnd(1) * 11) + 30

                                    HB%(P9) = 1

                                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP AND HANDS OFF TO " + rbacks$(P1, I1)
                                    Call PBP

                                    pbpString$ = rbacks$(P1, I1) + " HAS THE BALL AND WANTS TO THROW!!!"
                                    Call PBP

                                    I3 = Int(Rnd(1) * 3)
                                    pbpString$ = "HE HAS " + wdRec$(P1, I3) + " OPEN DOWNFIELD"
                                    Call PBP

                                    Call Rolld100(RN0)
                                    If coverage = 10 Or coverage = 11 Or coverage = 8 And RN0 <= 35 Then
                                        ydsGained = (Int(Rnd(1) * 4) + 4)
                                        'Sacked
                                        GoTo 3541
                                    Else

                                        Call Rolld100(RN0)

                                        If RN0 <= intChance Then
                                            'Interception
                                            ydsGained = CP
                                            FF% = 1
                                            HB% = 0
                                            'Intercepted!
                                            GoTo 3552
                                        Else
                                            Call Rolld100(RN1)
                                            targetWRName$ = wdRec$(P1, I3)
                                            I3 = I3 + 10

                                            If RN1 <= catchPctSuccess Then
                                                FF% = 1
                                                GoTo 3586
                                            Else
                                                ydsGained = 0
                                                GoTo 3535
                                            End If
                                        End If
                                    End If

                            End Select

                        End If

                        If UCase$(I$) = "K" Then
                            GoTo 850
                        End If

                        If UCase$(I$) = "T" Then
                            Call TimeoutCalled(P1, periodOver)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360
                            'If periodOver = 1 Then GoTo 3360
                        End If

                        XX = Val(I$)

                    Loop Until XX >= 0 And XX <= 9

                    Screen , , 0, 0

                    ' *** Play Type Selection
                    Select Case XX

                        Case 0

                            Call RUNMENU

                            Do
                                Locate 20, 14
                                Print "             SELECT PLAY ";
                                Input playCall
                            Loop Until playCall >= 0 And playCall <= 15

                            If playCall = 0 Then
                                Screen , , 0, 0
                                GoTo 3118
                            End If

                            chosenPlay = playCall

                            'Here we are categorizing plays that are
                            'simply alternate names for the same thing

                            'Power Sweep / Quick Pitch = Toss Sweep
                            If playCall = 9 Or playCall = 10 Then playCall = 4

                            'Quick Trap = Trap / Misdirection
                            If playCall = 11 Then playCall = 7

                            'Isolation = Off Tackle
                            If playCall = 12 Then playCall = 3

                            'Sprint Draw = Draw
                            If playCall = 13 Then playCall = 6

                            'Over the Top = Trap / Misdirection
                            If playCall = 14 Then playCall = 7

                            'Counter = Dive
                            If playCall = 15 Then playCall = 2

                        Case 1

                            Call PASSMENU

                            Do
                                Locate 19, 14
                                Print "             SELECT PLAY ";
                                Input playCall
                            Loop Until playCall >= 0 And playCall <= 14

                            Screen , , 0, 0

                            If playCall = 0 Then GoTo 3118

                            'we have 15 run plays, so chosenPlay needs
                            'a unique value up to 29
                            chosenPlay = playCall + 15

                            'Here we are categorizing plays that are
                            'simply alternate names for the same thing

                            'Cross / Curl = Medium Pass
                            If playCall = 11 Or playCall = 12 Then playCall = 3

                            'Deep Post / Deep Corner = Long Pass
                            If playCall = 13 Or playCall = 14 Then playCall = 4

                            'playCall needs a unique value as well
                            'that represents a chosen play,
                            'not just a number on the menu
                            'So here we account for the
                            'presence of 8 run play types
                            playCall = playCall + 8

                        Case Is >= 2
                            'playCall needs a unique value; values of XX from 3+
                            'will translate to 20+
                            playCall = XX + 17

                    End Select

                Loop Until playCall <> 19 Or ydLine < 70

                If playCall = 26 Then playCall = 24

                'playCall = 25, from above
                If XX = 8 Then

                    pbpString$ = gameTeams$(P1) + " IS LETTING THE PLAY CLOCK WIND DOWN"
                    Call PBP

                    pbpString$ = "PENALTY - " + gameTeams$(P1) + " - DELAY OF GAME"
                    Call PBP

                    playSegment = 8
                    penaltyYds = 5

                    'Reset ball carrier
                    I2 = 0

                    GoTo 3326

                End If

                If XX = 7 Then
                    playCall = 9
                    playType = 1
                    GoTo 3403
                End If

                If (quarter = 3 Or quarter = 1) And playCall = 16 Or gameClock! > 120 And playCall = 16 Then
                    pbpString$ = "CAN ONLY USE LAST 2 MIN. OF EACH HALF"
                    Call PBP
                    validPlay = 0
                End If

            End If

        Loop Until validPlay = 1

        'Determine intended ball carrier
        If autoPlaySelect = 0 Then

            If playCall = 8 Then
                Screen , , 0, 0
            End If

            If playCall >= 1 And playCall <= 8 Then

                3128 '

                Call FRAMEOPTION(11, P1)

                Print "SELECT BALL CARRIER":

                For KK = 0 To 4

                    Locate 12 + KK, 14
                    Print KK; "-"; rbacks$(P1, KK);

                    If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, KK, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, KK) / 100 + .1 Then
                        Print " ###"
                    Else
                        Print
                    End If

                    If rbrushContrib(P1, KK) = 100 Then GoTo CSELECT

                Next

                For KK = 0 To 4

                    Locate 12 + KK, 41
                    Print KK + 5; "-"; rbacks$(P1, KK + 5);

                    If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, KK + 5, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, KK + 5) / 100 + .1 Then
                        Print " ###"
                    Else
                        Print
                    End If

                    If rbrushContrib(P1, KK + 5) = 100 Then GoTo CSELECT

                Next KK

                CSELECT:
                Locate 17, 28: Print "C-COMPUTER SELECT"

                Do
                    I$ = GetKeyPress$

                    If UCase$(I$) = "C" Then
                        userSelectBC = 0
                    Else
                        userSelectBC = 1
                        I1 = Val(I$)
                    End If

                Loop Until I1 >= 0 And I1 <= 9 Or userSelectBC = 0
                'End If

                Screen , , 0, 0

            End If

        End If

        ResolvePlay:
        'If Computer is playing offense and a long Pass was chosen inside the Opp 15 --> Change to Medium Pass
        If ((playerMode = 1 And P1 = compTeam Or playerMode = 2) And (playCall = 12 Or playCall = 15) And ydLine >= 85) Then
            playCall = 11
        End If

        'If there are timeouts and we're in the red zone, there's a 4% chance of confusion
        If timeouts(P1) > 0 And ydLine > 80 And Int(Rnd(1) * 25) + 1 = 16 Then

            pbpString$ = "THERE'S SOME CONFUSION AT THE LINE OF SCRIMMAGE"
            Call PBP

            pbpString$ = qbacks$(P1, qbNumber(P1)) + " HAS TO CALL TIME OUT TO AVOID DELAY OF GAME"
            Call PBP

            timeouts(P1) = timeouts(P1) - 1
            qbTakeKnee = 0

        End If

    Loop While (timeouts(P1) > 0 And ydLine > 80 And Int(Rnd(1) * 25) + 1 = 16)

    Call SCOREBOARD
    Call MARKER1

    If playerMode = 1 And D = compTeam Then
        Call COMPCHEAT
    End If

    If qbTakeKnee = 1 Then
        qbTakeKnee = 0
        GoTo 3403
    End If

    RN1 = Int(Rnd(1) * 1000) + 1

    If RN1 <= 36 And endZone <> 1 And playCall < 21 Then

        pbpString$ = "*** PENALTY - "
        RN2 = Int(Rnd(1) * 36) + 1

        Select Case RN2

            Case 1 To 2 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFFENSE... OFFSIDES 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 2 + teamRat_GAME(P1, 0) To 20 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "ILLEGAL PROCEDURE OFFENSE... 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 20 + teamRat_GAME(P1, 0) To 27 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFFENSE... DELAY OF GAME 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 27 + teamRat_GAME(P1, 0) To 36 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                pbpString$ = pbpString$ + "DEFENSE... ENCROACHMENT 5 YARDS ***"
                Call PBP
                penaltyYds = 5

        End Select

        playSegment = 8

        If penaltyYds > 0 Then
            GoTo 3330
        Else
            penaltyYds = penaltyYds * -1
            GoTo 3326
        End If

    End If


    'At this point the ball carrier has been chosen
    'Now we have to resolve the play outcome and yardage
    Select Case playCall

        Case 1 To 8

            ' ****  RUNNING PLAYS  ****

            Call QBLINE

            'If user picked the ball carrier, work out yardage / fumbles
            If userSelectBC = 1 Then

                userSelectBC = 0
                playType = 1

                3403 '
                'Average rush yards
                rushYds = rbRushAvg(P1, I1)

                'Adjust yardage based on DEF league ratings and weather
                I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, I1, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, I1) / 100 + .1 Then
                    'If team has more than 20 attempts and current runner's contribution > his average?
                    I2 = Int(I2 * .4)
                End If

                RN0 = Int(Rnd(1) * 200) + 1

                '0.5 % chance of fumble outside the end zones
                If RN0 = 97 And ydLine <= 98.899 And ydLine >= 1.101 Then

                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " FUMBLES THE SNAP!"
                    Call PBP

                    ydsGained = Int((Rnd(1) * 20) - 10) / 10

                    X = 4
                    X1 = 9
                    I1 = qbNumber(P1)

                    If ydLine + ydsGained < 99.94999 Then
                        'If we're within a foot of the first down
                        If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then Call MEASUREMENT
                    End If

                    pbpString$ = "HE GAINS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    If endZone = 1 Then Call TryForTwoResult

                    'There was a RETURN in the original code
                    'But it was always sent to with GOTO
                    'So where was it returning from?

                    ballFumbled = 1

                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1
                    If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                    GoTo 650

                End If

                'QB Sneak
                If playCall = 8 Then

                    'Print #11, qbacks$(P1, qbNumber(P1)) + " is going for the SNEAK!"
                    Call QBSNEAK

                    If endZone = 1 Then Call TryForTwoResult

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                        'Safety!
                        GoTo 3250
                    End If

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                        'Touchdown!
                        GoTo 3200
                    End If

                    Call FUMBLE

                    GoTo 3309

                End If

                If playCall = 9 Then

                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP AND KNEELS DOWN FOR "
                    Call PBP
                    ydsGained = -1.0

                    GoTo 3546

                End If

                Call Rolld100(RN0)

                'Option play = 1% cance of fumble?
                If playCall = 5 And RN0 = 49 Then

                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " MAKES A BAD PITCH ON THE OPTION!!"
                    Call PBP

                    'Loss of 6 - 12 yds
                    ydsGained = Int((Rnd(1) * 7) + 6) * -1
                    X = 4
                    X1 = 9
                    I1 = qbNumber(P1)

                    pbpString$ = "HE LOSES" + Str$(Int((ydsGained * -1) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then

                        ballFumbled = 0

                        'Safety!
                        GoTo 3250

                    Else

                        If endZone = 1 Then Call TryForTwoResult

                        ballFumbled = 1

                        Call COMPILER

                        ydLine = ydLine + ydsGained
                        ydsToFirst = ydsToFirst - ydsGained
                        currDown = currDown + 1
                        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        GoTo 650

                    End If

                End If

                Call HANDOFF

                If dLine = 11 Then

                    Call GOALLINE

                Else

                    I2 = I2 + defYdAdj(playCall, dLine)

                    S6 = 0

                    If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then
                        If (gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3))) > (leagRat_GAME(P1, 7) / 100) And K3(P1, 0) - (leagRat_GAME(D, 0) * .3) > 4.5 And (leagRat_GAME(D, 0) * .3) > 4.5 And leagRat_GAME(P1, 7) < 50 Then
                            S6 = Int(((K3(P1, 0) - 3.6) / .3) + .5) * -1
                        End If
                    End If

                    I2 = I2 + S6

                    If P1 = gameLoc Then
                        'Home field advantage
                        I2 = I2 + 1
                    Else
                        'Away game (not home, not Neutral)
                        If gameLoc <> 2 Then I2 = I2 - 1
                    End If

                    'Select Case

                    'I2 = 0
                    If I2 < 1 Then I2 = 1

                    'I2 = 1 to 38
                    If I2 <= 38 Then

                        If (playCall <= 2 And dLine = 5 Or playCall <= 2 And dLine = 6 Or playCall >= 6 And dLine = 5 Or playCall >= 6 And dLine = 6) Or (playCall > 2 And playCall < 5 And dLine = 7 Or playCall > 2 And playCall < 5 And dLine = 8) Then

                            ' *** STUNTS ***
                            Call Rolld100(RN4)

                            Select Case I2

                                Case Is <= 15
                                    Select Case RN4
                                        Case 1 To 15: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 16 To 60: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 61 To 100: ydsGained = Int(Rnd(1) * (I2 * 2.13)) + 1
                                    End Select

                                Case 16, 17
                                    Select Case RN4
                                        Case 1 To 10: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 11 To 50: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 51 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.73)) + 1
                                    End Select

                                Case 18, 19
                                    Select Case RN4
                                        Case 1 To 10: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 11 To 40: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 41 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.5)) + 1
                                    End Select

                                Case 20, 21
                                    Select Case RN4
                                        Case 1 To 5: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 6 To 30: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 31 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.3)) + 1
                                    End Select

                                Case Is >= 22
                                    Select Case RN4
                                        Case 1 To 5: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 6 To 20: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 21 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.2)) + 1
                                    End Select

                            End Select

                            GoTo 3416

                        End If

                        Call Rolld100(RN4)

                        If (playCall <= 2 And RN4 <= 50 And coverage = 7) Or (playCall >= 6 And RN4 <= 50 And coverage = 7) Or (playCall <= 2 And coverage = 9) Or (playCall >= 6 And coverage = 9) Then
                            Call BLITZ2
                        End If

                        If playCall >= 3 And playCall <= 5 Then
                            If coverage = 8 And RN4 <= 50 Or coverage = 10 Then
                                Call BLITZ2
                            End If
                        End If

                        If coverage = 11 Then

                            Call BLITZ2

                        Else

                            If I2 > 17 Then

                                ydsGained = Int(Rnd(1) * 48) + 1

                                Select Case ydsGained

                                    Case 1 To 44
                                        ydsGained = (Int(Rnd(1) * 11) - 1) - (((18 - I2) * 2) / 10)

                                    Case 45
                                        ydsGained = Int(Rnd(1) * 2 * I2) + 1

                                    Case 46 To 47
                                        ydsGained = Int(Rnd(1) * I2) + 1

                                    Case 48
                                        ydsGained = Int(Rnd(1) * 5 * I2) + 1

                                End Select

                            Else

                                Call Rolld100(RN4)

                                If RN4 > 92 Then
                                    ' *** BREAKAWAY ***
                                    Call Rolld100(RN5)
                                    If RN5 <= 50 Then
                                        ydsGained = Int(Rnd(1) * brkawayYds(I2, 0)) + brkawayYds(I2, 1)
                                    Else
                                        ydsGained = Int(Rnd(1) * brkawayYds(I2, 3)) + brkawayYds(I2, 4)
                                    End If

                                Else

                                    I5 = 0

                                    If playCall = 4 Or playCall = 5 Then
                                        I5 = 1
                                    End If

                                    For I6 = 0 To 14:
                                        If RN4 <= RM%(I5, I6) Then
                                            ydsGained = I6 - 5
                                            Exit For
                                        End If
                                    Next I6

                                    Call Rolld100(RN4)
                                    If RN4 <= runYdAdj(0, I2) Then
                                        ydsGained = ydsGained + runYdAdj(1, I2)
                                    End If

                                End If

                            End If

                        End If 'Done checking div3$

                    Else
                        'I2 = 39 +
                        ydsGained = Int(Rnd(1) * 48) + 1

                        Select Case ydsGained

                            Case 1 To 44
                                ydsGained = (Int(Rnd(1) * 11) - 1) - (((18 - I2) * 2) / 10)

                            Case 45
                                ydsGained = Int(Rnd(1) * 2 * I2) + 1

                            Case 46 To 47
                                ydsGained = Int(Rnd(1) * I2) + 1

                            Case 48
                                ydsGained = Int(Rnd(1) * 5 * I2) + 1

                        End Select

                    End If

                    'End Select

                End If 'Done checking / adjusting for position


                3416 '
                'Determine play yardage
                Call YARDAGE

                'Call PBP based on special scenarios
                '"Stuffed", Long Play or Run for TD
                If ydsGained <= 0 Then
                    Call STUFFED
                End If

                If ydsGained > 10 And ydLine + ydsGained < 99.94999 Then
                    Call LGPBP
                End If

                If ydLine + ydsGained >= 99.94999 Then
                    Call RUNTDPBP
                End If

                startYds = 100 - ydLine
                endYds = startYds - ydsGained

                Call MOVEBALL

                If ydLine + ydsGained < 99.94999 Then

                    F% = 2

                    Call FUMBLE

                    If ballFumbled <> 1 Then
                        'If we're within a foot of the first down
                        If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then
                            Call MEASUREMENT
                        End If
                    End If

                End If

                'Print / call PBP for actual yardage
                If ydsGained < 0 Then

                    pbpString$ = rbacks$(P1, I1) + " LOST"
                    'Print
                    X = 1
                    X1 = 1
                    pbpString$ = pbpString$ + Str$(Int((ydsGained * -1) * 10 + .5) / 10)

                Else

                    Call RUNPBP

                    X = 1
                    X1 = 1

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                        pbpString$ = pbpString$ + Str$(Int((100 - ydLine) * 10 + .5) / 10)
                    Else
                        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
                    End If

                End If


                pbpString$ = pbpString$ + " YARDS"
                Call PBP

                If isReverse = 1 Then isReverse = 0

                If endZone = 1 Then
                    ballFumbled = 0
                    Call TryForTwoResult
                End If

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                    ballFumbled = 0
                    'Safety!
                    GoTo 3250
                End If

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                    ballFumbled = 0
                    'Touchdown!
                    GoTo 3200
                End If

                GoTo 3309

            Else
                'userSelectBC = 0

                3401 '
                playType = 1

                'Pick random ball carrier based on contribution %
                Call Rolld100(I2)

                For I = 9 To 0 Step -1:
                    If I2 <= rbrushContrib(P1, I) Then
                        I1 = I
                    End If
                Next I

                CheckQBRunner:
                'If the QB was chosen, we need to go with a pass play.
                'The game handles all QB rushes as scrambles / etc.
                'Without this in place, rush attempts for a team run
                'way high and pass attempts run too low
                If rbacks$(P1, I1) = qbacks$(P1, qbNumber(P1)) Then
                    'Basically we're picking a pass play
                    'Chances here are based on calculations of
                    'how often the AI would call these types
                    'of passes during the regular game.
                    'Currently, very little context is going into this.

                    Call Rolld100(RN0)

                    Select Case RN0

                        Case 0 To 12
                            playCall = 10

                        Case 13 To 25
                            playCall = 13

                        Case 26 To 85
                            playCall = 11

                        Case 86 To 100
                            playCall = 12

                    End Select

                    GoTo ResolvePlay

                End If

                'See if chosen runner is a benched QB
                If I1 = (qbRushIdx(P1, 0) Or I1 = qbRushIdx(P1, 1) Or I1 = qbRushIdx(P1, 2) Or I1 = qbRushIdx(P1, 3)) Then

                    'Check if runner has receptions
                    If rbNumRec(P1, I1) > 0 Then

                        'Normal rush
                        GoTo 3403

                    Else

                        'Assing active QB instead
                        For currBack = 0 To 9
                            If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                                I1 = currBack
                                'GoTo 3403
                                GoTo CheckQBRunner
                            End If
                        Next

                        If isReverse = 1 Then
                            'Reverse play?
                            GoTo 3181
                        Else
                            'Look for new ball carrier
                            GoTo 3401
                        End If

                    End If

                Else

                    'Adjust rush yds based on league rating and weather
                    I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                    If isReverse = 1 Then
                        'Reverse play?
                        Call REVGAIN
                        GoTo 3416
                    Else
                        'Regular rush yardage gain
                        GoTo 3403
                    End If

                End If

            End If 'Done checking userSelectBC

        Case 9 To 18

            Call PASSBALL(compChance)

            'SACKED
            If compChance < passCovAdj(3, playCall - 8, coverage) + leagRat_GAME(P1, 5) + leagRat_GAME(D, 6) + 1 Then

                playType = 1
                W5 = 1

                ' *** SACKED ***
                If playCall = 12 Or playCall = 15 Then
                    ydsGained = (Int(Rnd(1) * 9) + 5)
                End If

                If playCall = 10 Or playCall = 9 Or playCall = 13 Then
                    ydsGained = (Int(Rnd(1) * 5) + 3)
                End If

                If playCall = 11 Or playCall = 14 Or playCall = 16 Or playCall = 17 Or playCall = 18 Then
                    ydsGained = (Int(Rnd(1) * 7) + 4)
                End If

                3541 '
                Call YARDAGE
                Call SACK

                'Yardage for sack is negative
                ydsGained = ydsGained * -1

                If defSacks(1 - P1) <> 0 Then

                    M5 = 0
                    RN0 = Int(Rnd(1) * teamSacks(1 - P1)) + 1

                    For Z1 = 0 To 14

                        M5 = M5 + playerSacks(1 - P1, Z1)

                        If RN0 <= M5 Then
                            pbpString$ = pbpString$ + " BY " + defSacks$(1 - P1, Z1)
                            Exit For
                        End If

                    Next

                End If

                '3699
                Call PBP

                X = 4
                playType = 1

                If endZone = 1 Then Call TryForTwoResult

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then

                    'Safety!
                    GoTo 3250

                Else

                    F% = 1

                    Call FUMBLE

                End If

                GoTo 3309

            End If

            'FORCED TO SCRAMBLE / RUN
            willScramble = 0
            qbContrib = GetQBRushPct%(qbacks$(P1, qbNumber(P1)))

            If qbMobility(P1) = 9 Then

                Select Case qbContrib

                    Case 27 To 33
                        mobileBonus = 0

                    Case 34 To 39
                        mobileBonus = 6

                    Case 40 To 49
                        mobileBonus = 5

                    Case 50 To 99
                        mobileBonus = 4

                End Select

            Else

                mobileBonus = 0

            End If

            Call Rolld100(RN0)

            'Short Pass
            If (playCall = 9 Or playCall = 10 Or playCall = 13) Then

                Select Case qbMobility(P1)

                    Case 1 To 6
                        scrambleChance = Int(qbMobility(P1))

                    Case 7
                        scrambleChance = 13

                    Case 8
                        scrambleChance = 19

                    Case 9
                        '21% - 27%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 21

                End Select

            End If

            'Medium Pass / Sideline Pass / Rollout
            If (playCall = 11 Or playCall = 14 Or playCall = 16 Or playCall = 17) Then

                Select Case qbMobility(P1)

                    Case 1 To 2
                        '2%, 4%
                        scrambleChance = qbMobility(P1) + 1

                    Case 3 To 4
                        scrambleChance = 5 'was 4

                    Case 5 To 6
                        '7%, 8%
                        scrambleChance = qbMobility(P1) + 2

                    Case 7 To 8
                        '14%, 16%
                        scrambleChance = qbMobility(P1) * 2

                    Case 9
                        '27% - 33%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 27

                End Select

            End If

            'Long Pass
            If (playCall = 12 Or playCall = 15) Then

                Select Case qbMobility(P1)

                    Case 1 To 6
                        '4%, 5%, 6%, 7%, 8%, 9%
                        scrambleChance = Int(qbMobility(P1)) + 3

                    Case 7
                        scrambleChance = 12

                    Case 8
                        '18%
                        scrambleChance = Int(qbMobility(P1) * 2) + 3

                    Case 9
                        '35% - 41%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 35

                End Select


            End If 'was 4

            'Fake Kick
            If playCall = 18 Then

                Select Case qbMobility(P1)

                    Case 1 To 4
                        '4%, 5%, 6%, 7%
                        scrambleChance = Int(qbMobility(P1)) + 3

                    Case 5
                        scrambleChance = 9

                    Case 6 To 7
                        scrambleChance = 14

                    Case 8
                        scrambleChance = 23

                    Case 9
                        '43% - 49%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 43

                End Select

            End If

            'Increase chances for College, since stats
            'are recorded as reduced by 15%
            If ruleOptType = 0 Then
                scrambleChance = scrambleChance + 15
            End If

            'Determine chance of QB scramble
            If RN0 <= scrambleChance Then
                willScramble = 1
            End If

            If willScramble = 1 Then

                'QB RUN
                I6 = 0

                For currBack = 0 To 9
                    If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                        'Def Rush Adj doesn't apply
                        I6 = rbRushAvg(P1, currBack)
                        Exit For
                    End If
                Next currBack

                'About 10% of all football plays are outside the average gain
                specialGain = Int(Rnd(1) * 10) + 1

                If I6 = 0 Or qbMobility(P1) < 2 Then
                    'QB is not a rusher, or mobility is low
                    'Essentially, we're treating these as the expected behavior.
                    '(Low mobility QB's will often have low or negative averages)
                    '(This will translate to larger negatives below)
                    If specialGain < 10 Then
                        'average gains are 1-5 yds
                        qbRushMax = 5
                    Else
                        'longer gain
                        qbRushMax = 16
                    End If
                Else
                    'QB is a rusher
                    If specialGain < 10 Then
                        'Average gains previously maxed at 7
                        'Here we use a fraction of the rushing Average
                        'This is not the real world average, but a
                        'representation using integers.
                        '   4.8 = 16 (1.6 * 3 = 4.8)
                        '   4.2 = 14 (1.4 * 3 = 4.2)
                        '   3.9 = 13 (1.3 * 3 = 3.9)
                        '   2.7 = 9 (0.9 * 3 = 2.7)
                        'So our maximums become:
                        '   Avg 4.8 = 11
                        '   Avg 4.2 = 10
                        '   Avg 3.9 = 9
                        '   ...
                        '   Avg 2.7 = 6
                        'This limits the that even some major
                        'scrambles yield over 10 yards, but that
                        'would be the rarity.
                        qbRushMax = (I6 * 3) / 4
                    Else
                        'Here our maximums become:
                        '   Avg 4.8 = 18
                        '   Avg 3.9 = 12
                        '   Avg 2.7 = 4
                        qbRushMax = 12 + ((I6 - 13) * 2)
                    End If

                End If

                ydsGained = Int(Rnd(1) * qbRushMax)
                Call YARDAGE

                startYds = 100 - ydLine
                endYds = startYds - ydsGained

                Call MOVEBALL

                RN0 = Int(Rnd(1) * 2) + 1

                If playCall = 17 Or playCall = 18 Then

                    If RN0 = 1 Then
                        pbpString$ = qbacks$(P1, I1) + " ROLLS OUT TO HIS RIGHT"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " ROLLS OUT TO HIS LEFT"
                    End If
                    Call PBP

                    If playCall = 17 Then
                        pbpString$ = qbacks$(P1, I1) + " TUCKS IT IN AND TAKES OFF!"
                        Call PBP
                        pbpString$ = "HE PICKS UP ABOUT"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " KEEPS THE BALL AND AND RUNS FOR"
                    End If

                Else

                    If RN0 = 1 Then
                        pbpString$ = qbacks$(P1, I1) + " IS CHASED FROM THE POCKET"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " CAN'T FIND AN OPEN TARGET"
                        Call PBP
                        pbpString$ = "HE SPRINTS AHEAD FOR "
                    End If

                End If

                '-- Make sure if rush was for negative yardage, that
                ' it's recorded / tracked right

                3546 '
                X = 4
                X1 = 9
                playType = 1

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
                Else
                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
                End If

                pbpString$ = pbpString$ + " YARDS"
                Call PBP

                If endZone = 1 Then Call TryForTwoResult

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                    'Safety!
                    GoTo 3250
                End If

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                    gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1
                    'Touchdown!
                    GoTo 3200
                End If

                F% = 2

                Call FUMBLE

                GoTo 3309

            Else

                'Determine pass outcome
                Call Rolld100(passChance)

                'Compare value to expected interception %
                '               QB's Int % - Pass type adjustment + league DEF Int %
                intChance = qbPctInt(P1, I1) - passCovAdj(0, playCall - 8, coverage) + leagRat_GAME(D, 2)

                '                If I > (100 - qbPctInt(P1, I1) - passCovAdj(0, playCall - 8, coverage) + leagRat_GAME(D, 2)) Then
                If passChance > (100 - intChance) Then

                    'Intercepted!
                    GoTo 3552

                Else

                    'Determine % chance of completion
                    Call AdjustCompPct

                    If passChance <= expCompPct + passTypeCmpAdj + ballPosCmpAdj + windCmpAdj Then
                        'Pass completed

                        'Determine yards based on player's average
                        If I3 > 9 Then
                            I4 = wrYdsPerC(P1, I3 - 10)
                            rushYds = I4
                        Else
                            I4 = rbYdsPerC(P1, I3)
                            rushYds = I4
                        End If

                        'Adjust yards for weather
                        If climate(4) = -3 Then I4 = I4 - 1
                        If climate(4) = -6 Then I4 = I4 - 2

                        If I4 < 11 Then
                            'Determine impact of blitz
                            Call BLITZ

                        Else

                            I4 = I4 + passCovAdj(2, playCall - 8, coverage)

                            If ydsPerComp(D) / 100 > .5 Then I4 = I4 - (1 - ydsPerComp(D) / 100) * 10
                            If ydsPerComp(D) / 100 < .5 Then I4 = I4 + ydsPerComp(D) / 100 * 10

                            'Check if the play is overused
                            Call OVERUSE

                            If playCall = 9 Or playCall = 10 Or playCall = 13 Then
                                I4 = I4 * ((((expCompPct + S6) * K3(P1, 3)) / (expCompPct + S6 + 18)) / K3(P1, 3))
                            End If

                            If playCall = 12 Or playCall = 15 Then
                                I4 = I4 * ((((expCompPct + S6) * K3(P1, 3)) / (expCompPct + S6 - 18)) / K3(P1, 3))
                            End If

                            'At this point I'm lost but playCall is the pass play type
                            RN0 = Int(Rnd(1) * 25) + 1

                            If playCall <> 12 And playCall <> 15 And RN0 = 25 Or (playCall = 12 Or playCall = 15) And RN0 = 25 And ruleOptType = 0 Or (playCall = 12 Or playCall = 15) And ruleOptType <> 0 And I >= 24 Then
                                'Longer gains
                                If RN0 = 25 And Int(Rnd(1) * 32) = 11 And score(P1, 0) > 13 And XD(D) > .6 Then
                                    ydsGained = 102
                                Else
                                    ydsGained = Int(Rnd(1) * I4 * 12) + 1 + (I4 * 6)
                                End If
                            Else
                                If RN0 <= 19 Then
                                    ydsGained = Int(Rnd(1) * 13) + 1 + ((I4 * 2.3) - 7.2)
                                Else
                                    ydsGained = (Int(Rnd(1) * (5 + (I4 * 2))) + 1) + (3 * (I4 - 1))
                                End If
                            End If

                        End If

                        3586 '
                        'ADDED AFTER REPLAYS OF 5-24-93 <-- ???
                        'Random # 1 - 3 = 1 And Pro Game = adjust by 1
                        If Int(Rnd(1) * 3) + 1 = 1 And ruleOptType <> 0 Then
                            ydsGained = ydsGained - 1
                        End If

                        Call YARDAGE

                        X = 3
                        X1 = 2

                        If ydsGained + ydLine > 99.9499 Then ydsGained = 109 - ydLine

                        'Play-by-Play for Completion %
                        Call PCPBP

                        If FF% <> 0 Then FF% = 0

                        If ydLine + ydsGained < 99.9499 Then

                            F% = 2
                            Call FUMBLE

                            If ballFumbled <> 1 Then
                                'If we're within a foot of the first down
                                If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then Call MEASUREMENT
                            End If

                        End If

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then

                            pbpString$ = "THAT CATCH BY "
                            Call WHICHRECEIVER
                            pbpString$ = pbpString$ + " GAINED" + Str$(Int(100 - ydLine)) + " YARDS"

                            ydsGained = 100 - ydLine

                            If ydsGained > WR(P1, I3) Then
                                gameWR$(P1, I3) = "t"
                                WR(P1, I3) = ydsGained
                            End If

                            If ydsGained > quarterbacks(P1, qbNumber(P1)) Then
                                gameQB$(P1, qbNumber(P1)) = "t"
                                quarterbacks(P1, qbNumber(P1)) = ydsGained
                            End If

                            Call PBP

                        Else

                            If ydsGained < 0 Then

                                pbpString$ = "THAT CATCH BY "
                                Call WHICHRECEIVER
                                pbpString$ = pbpString$ + " LOST" + Str$(Int((ydsGained * -1) * 10 + .5) / 10) + " YARDS"

                                If ydsGained > WR(P1, I3) Then
                                    gameWR$(P1, I3) = ""
                                    WR(P1, I3) = ydsGained
                                End If

                                If ydsGained > quarterbacks(P1, qbNumber(P1)) Then
                                    gameQB$(P1, qbNumber(P1)) = ""
                                    quarterbacks(P1, qbNumber(P1)) = ydsGained
                                End If

                            Else

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 < 99.9499 Then pbpString$ = "THAT CATCH BY "
                                Call WHICHRECEIVER
                                pbpString$ = pbpString$ + " GAINED" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"

                                If ydsGained > WR(P1, I3) Then
                                    gameWR$(P1, I3) = ""
                                    WR(P1, I3) = ydsGained
                                End If

                                If ydsGained > quarterbacks(P1, qbNumber(P1)) Then
                                    gameQB$(P1, qbNumber(P1)) = ""
                                    quarterbacks(P1, qbNumber(P1)) = ydsGained
                                End If

                            End If

                        End If

                        Call PBP

                        If endZone = 1 Then ballFumbled = 0: Call TryForTwoResult

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then

                            ballFumbled = 0

                            'Safety!
                            GoTo 3250

                        End If

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then

                            ballFumbled = 0
                            gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

                            'Touchdown!
                            GoTo 3200

                        End If

                        If playCall = 16 Then

                            obChance = Int(Rnd(1) * 10) + 1

                            If obChance <= 7 Then

                                playSegment = 5

                                RN0 = Int(Rnd(1) * 5) + 1
                                Select Case RN0
                                    Case 1: pbpString$ = "AND HE GETS OUT OF BOUNDS"
                                    Case 2: pbpString$ = "AND HE'S KNOCKED OUT OF BOUNDS"
                                    Case 3: pbpString$ = "AND HE STEPS OUT OF BOUNDS"
                                    Case 4: pbpString$ = "AND HE'S PUSHED OUT OF BOUNDS"
                                    Case 5: pbpString$ = "AND HE'S SHOVED OUT OF BOUNDS"
                                End Select

                                Call PBP
                                ballFumbled = 0

                            End If

                        End If

                        GoTo 3309

                    Else

                        'Incomplete pass
                        GoTo 3535

                    End If 'Done checking for pass complete

                End If

            End If 'Done checking if rollout


        Case 19

            Call Rolld100(prChance)

            For J = 2 To 0 Step -1
                If prChance <= prNumRet(D, J) Then I1 = J
            Next J

            puntRetNbr = I1

            pbpString$ = pret$(D, puntRetNbr) + " (" + Str$(Int(prYdsPerRet(D, puntRetNbr) * 3)) + " AVG) IS DEEP TO RECEIVE"
            Call PBP

            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then
                puntChoice = 0
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
                        puntChoice = Val(I$)
                    End If

                Loop Until puntChoice >= 0 And puntChoice <= 5 Or Not (ydLine > 20 And puntChoice = 4) Or Not (currDown = 4 And puntChoice = 5) Or Not (currDown < 4 And puntChoice < 5)

                Screen , , 0, 0

                If puntChoice = 5 Then

                    Call Rolld100(kickChance)
                    X = 0
                    ydsGained = ydsPerPunt(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * ydsPerPunt(P1, 0)))) + 11

                    If climate(0) = P1 Then
                        ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - climate(1)
                    Else
                        ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + climate(1)
                    End If

                    Call Rolld100(RN0)

                    If RN0 <= 50 Then
                        pbpString$ = gameTeams$(P1) + " IS QUICK KICKING!!"
                    Else
                        pbpString$ = gameMascots$(P1) + " ARE QUICK KICKING!!"
                    End If

                    Call PBP

                    If (coverage > 6 And kickChance >= 60 And kickChance <= 72) Or (coverage <= 6 And kickChance >= 50 And kickChance <= 56) Then
                        'Blocked punt
                        GoTo 3840
                    End If

                    pbpString$ = "THE KICK IS UP AND ON ITS WAY"
                    Call PBP

                    ydsGained = ydsGained + 2

                    Call YARDAGE
                    'Gets us a value for ydLineTeam

                    pbpString$ = "KICK BY " + gameTeams$(P1) + " TRAVELS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    gameStatsTeam(P1, 7) = gameStatsTeam(P1, 7) + 1

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                        'Punt travels to end zone

                        'Track punt yards
                        gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + 100 - ydLine

                        If 100 - ydLine > gameStatsTeam(P1, 26) Then
                            'Longest punt?
                            gameStatsTeam(P1, 26) = 100 - ydLine
                        End If

                        gameClock! = gameClock! - 5
                        timePoss(P1) = timePoss(P1) + 5

                        'Track touchback for punter
                        gameStatsTeam(P1, 27) = gameStatsTeam(P1, 27) + 1

                        Call Touchback

                    Else

                        'Why do these punts not get returned??
                        'Simply be design / "quick kick" ???

                        'Track punt yards
                        gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + ydsGained

                        fairCatchZone = 0

                        Select Case ruleOptType

                            Case 0
                                'College
                                If ruleOptColl <= 10 Then
                                    fairCatchLine = 80
                                Else
                                    fairCatchLine = 75
                                End If

                            Case Is >= 1
                                'Pro
                                If ruleOptPro <= 8 Then
                                    fairCatchLine = 80
                                Else
                                    fairCatchLine = 75
                                End If

                        End Select

                        If ydsGained + ydLine > fairCatchLine Then
                            'Inside the 20
                            fairCatchZone = 1
                            gameStatsTeam(P1, 29) = gameStatsTeam(P1, 29) + 1
                        End If

                        If ydsGained > gameStatsTeam(P1, 26) Then
                            'Longest punt?
                            gameStatsTeam(P1, 26) = ydsGained
                        End If

                        Call PuntNotReturned(fairCatchZone)

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Call DELAY
    
                    End If

                    GoTo 700

                End If

            End If

            X = 0
            ydsGained = ydsPerPunt(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * ydsPerPunt(P1, 0)))) + 3

            If 100 - ydLine <= ((ydsPerPunt(P1, 0) * 2) + 28) Then ydsGained = ydsGained + 100 - ydLine - (31 + ydsPerPunt(P1, 0) * 2)

            If climate(0) = P1 Then
                ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - climate(1)
            Else
                ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + climate(1)
            End If

            Call YARDAGE

            Call Rolld100(RN0)

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then

                If RN0 <= 10 Then
                    puntDefChoice = 1
                Else
                    puntDefChoice = 0
                End If

            Else

                Call FRAMEOPTION(9, D)
                Print "THE PUNTING TEAM FOR "; gameTeams$(P1); " IS ON THE FIELD"
                Locate 11, 14
                Locate , 14: Print "0 - SET UP PUNT RETURN"
                Locate , 14: Print "1 - GO FOR THE BLOCK"
                Locate , 14: Print "2 - CALL PUNT-SAFE"
                Print
                Locate , 14: Print "SELECT OPTION "; gameTeams$(D)

                Do
                    I$ = GetKeyPress$
                    puntDefChoice = Val(I$)
                Loop Until puntDefChoice >= 0 And puntDefChoice <= 2

                Screen , , 0, 0

            End If

            'PUNTSNAP
            Select Case puntChoice

                Case 2, 3

                    ' *** FAKE PUNT - RUN/PASS ***
                    If puntDefChoice = 2 Then
                        pbpString$ = gameTeams$(D) + " IS IN PUNT SAFE SET...FAKE PUNT? (Y/N)"
                        Call PBP

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    End If

                    If UCase$(I$) = "Y" Then

                        If puntChoice = 3 Then

                            ' *** FAKE W/PASS ***
                            ydsGained = 0
                            pbpString$ = "IT'S A FAKE!!! " + punter$(P1, 0) + " IS LOOKING TO PASS"
                            Call PBP

                            Call Rolld100(RN0)

                            If puntDefChoice = 0 Then

                                Select Case RN0

                                    'Fake punts convert about 50% of the time
                                    Case 1 To 50:
                                        ydsGained = Int(Rnd(1) * 27)

                                    Case 37 To 90:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        GoTo 3309

                                    Case 91 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                End Select

                            End If

                            If puntDefChoice = 1 Then

                                Select Case RN0

                                    Case 1 To 26:
                                        ydsGained = Int(Rnd(1) * 20)

                                    Case 27 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        GoTo 3309

                                    Case 81 To 90:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                    Case 91 To 100:
                                        ydsGained = 100 - ydLine

                                End Select

                            End If

                            If puntDefChoice = 2 Then

                                Select Case RN0

                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 7) + 6

                                    Case 11 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        GoTo 3309

                                    Case 81 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                End Select

                            End If

                            pbpString$ = "PASS IS COMPLETE FOR"

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                Call PBP
                            Else
                                pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                Call PBP
                            End If

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                                'Safety!
                                GoTo 3250
                            End If

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                'Touchdown!
                                GoTo 3200
                            End If

                            F% = 2
                            Call FUMBLE

                        Else

                            Call KickFakeYards(puntDefChoice)

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                                'Safety!
                                GoTo 3250
                            End If

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                'Touchdown!
                                GoTo 3200
                            End If

                            F% = 2
                            Call FUMBLE

                        End If

                        GoTo 3309

                    End If 'Done processing fake punt

                Case 4
                    Call TAKESAFETY
                    'Offensive Safety
                    GoTo 3251

            End Select

            PSNAP:
            'Normal punt
            pbpString$ = punter$(P1, 0) + " WAITS FOR THE SNAP"
            Call PBP

            RN5 = Int(Rnd(1) * 300) + 1

            If RN5 = 12 Then
                '~0.3% chance

                pbpString$ = "THERE'S A BAD SNAP FROM CENTER!!"
                Call PBP

                ydsGained = (Int(Rnd(1) * 14) + 12) * -1
                Call AddPossessionTime(4, 7)

                gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1
                gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1

                If ydLine + ydsGained < -9.999 Then

                    'Offensive Safety
                    score(D, quarter) = score(D, quarter) + 2
                    score(D, 0) = score(D, 0) + 2
                    GoTo 3251

                End If

                If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

                    Call Rolld100(RN0)

                    If RN0 <= 50 Then

                        'Offensive Safety
                        score(D, quarter) = score(D, quarter) + 2
                        score(D, 0) = score(D, 0) + 2
                        GoTo 3251

                    Else

                        X1 = 10
                        Call CHANGEPOSS

                        'Touchdown!
                        GoTo 3200

                    End If

                End If

                Call CHANGEPOSS
                currDown = 1
                ydsToFirst = 10
                ydLine = 100 - ydLine

                If ydsToFirst >= 90 Then ydsToFirst = 100 - ydLine

                Call Rolld100(RN0)

                If RN0 <= 50 Then
                    pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
                Else
                    pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
                End If

                If ydLine > 50 Then
                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
                Else
                    pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
                End If

                playSegment = 5

                GoSub 3900

            Else

                RN6 = Int(Rnd(1) * 200) + 1

                If puntDefChoice = 1 Then pbpString$ = "THERE'S A HEAVY RUSH!!!": Call PBP

                If puntDefChoice = 1 And RN6 >= 72 And RN6 <= 73 Then
                    'Blocked punt - 0.5% chance
                    GoTo 3840
                End If

                If puntDefChoice = 1 And RN6 >= 13 And RN6 <= 14 Then
                    'Roughing the Kicker - 1% chance
                    pbpString$ = "THERE'S A FLAG ON THE PLAY!!"

                    Call PBP

                    pbpString$ = "ROUGHING THE KICKER !!!...AUTOMATIC FIRST DOWN !!!"

                    Call PBP

                    gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 15
                    gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

                    ydLine = ydLine + 15

                    currDown = 1
                    ydsToFirst = 10
                    playSegment = 4

                    GoSub 3900

                    Call DELAY

                    GoTo 700
                End If

                pbpString$ = punter$(P1, 0) + " TAKES THE SNAP"
                Call PBP

                Call Rolld100(RN0)

                If puntChoice = 1 Then
                    'Punt away from returner

                    If RN0 <= 10 Then
                        ydsGained = ydsGained - (Int(Rnd(1) * 11) + 25)
                        pbpString$ = "HE SHANKS IT!"
                    Else
                        ydsGained = ydsGained - (Int(Rnd(1) * 9) + 4)
                        pbpString$ = "HE KICKS IT TOWARDS THE CORNER"
                    End If

                Else

                    If ydsGained < 40 Then
                        pbpString$ = "HE GETS THE PUNT AWAY"
                    Else
                        Call PUNTPBP
                    End If

                End If

                Call PBP

                pbpString$ = "PUNT BY " + punter$(P1, 0) + " TRAVELS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                Call PBP

                gameStatsTeam(P1, 7) = gameStatsTeam(P1, 7) + 1

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then

                    'Track punt yards
                    gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + 100 - ydLine

                    'Update clock;
                    '-- Punts always take 5 seconds???
                    gameClock! = gameClock! - 5
                    timePoss(P1) = timePoss(P1) + 5

                    'Track touchback
                    gameStatsTeam(P1, 27) = gameStatsTeam(P1, 27) + 1

                    Call Touchback

                Else

                    'Track punt yards
                    gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + ydsGained

                    Call Rolld100(puntChance)

                    If ydsGained + ydLine > 80 Then
                        'Kicks inside the 20
                        gameStatsTeam(P1, 29) = gameStatsTeam(P1, 29) + 1
                    End If

                    If ydsGained > gameStatsTeam(P1, 26) Then
                        'Longest punt?
                        gameStatsTeam(P1, 26) = ydsGained
                    End If

                    Call Rolld100(RN0)

                    'See if punt can be returned
                    If ydsGained + ydLine >= 90 And puntChance >= 5 Or puntDefChoice = 2 Or puntChance > 60 Or puntChoice = 1 And RN0 <= 80 Then
                        ' - Punt travels inside the 10, 95% of the time
                        ' - Punt Safe formation
                        ' - 40% chance overall
                        ' - Normal Punt, 80% Chance
                        '---> Overall this should end up 56% of the time
                        fairCatchZone = 0

                        Select Case ruleOptType

                            Case 0
                                'College
                                If ruleOptColl <= 10 Then
                                    fairCatchLine = 80
                                Else
                                    fairCatchLine = 75
                                End If

                            Case Is >= 1
                                'Pro
                                If ruleOptPro <= 8 Then
                                    fairCatchLine = 80
                                Else
                                    fairCatchLine = 75
                                End If

                        End Select

                        Call PuntNotReturned(fairCatchZone)

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Call DELAY

                    Else

                        Call PuntReturnYards

                        If ydsGained + ydLine >= 99.99 Then

                            'Track Punt Return
                            gameStatsPlayer(P1, puntRetNbr + 3, 14) = gameStatsPlayer(P1, puntRetNbr + 3, 14) + 1

                            'Track Return Yards
                            gameStatsPlayer(P1, puntRetNbr + 3, 15) = gameStatsPlayer(P1, puntRetNbr + 3, 15) + 100 - ydLine

                            'Track Return TD
                            gameStatsPlayer(P1, puntRetNbr + 3, 16) = gameStatsPlayer(P1, puntRetNbr + 3, 16) + 1

                            returnGain = 100 - ydLine

                            If returnGain > puntReturners(P1, puntRetNbr) Then
                                gamePR$(P1, puntRetNbr) = "t"
                                puntReturners(P1, puntRetNbr) = returnGain
                            End If

                            I1 = puntRetNbr

                            'Touchdown!
                            GoTo 3200

                        End If

                        gameStatsPlayer(P1, puntRetNbr + 3, 14) = gameStatsPlayer(P1, puntRetNbr + 3, 14) + 1
                        gameStatsPlayer(P1, puntRetNbr + 3, 15) = gameStatsPlayer(P1, puntRetNbr + 3, 15) + ydsGained

                        If ydsGained > puntReturners(P1, puntRetNbr) Then
                            gamePR$(P1, puntRetNbr) = ""
                            puntReturners(P1, puntRetNbr) = ydsGained
                        End If

                        If ydsGained + ydLine <= .01 Then
                            'Safety!
                            GoTo 3250
                        End If

                        currDown = 1
                        ydLine = ydLine + ydsGained
                        ydsToFirst = 10

                        If ydLine >= 89.99 Then ydsToFirst = 100 - ydLine

                        RN1 = Int(Rnd(1) * 1000) + 1

                        If RN1 >= 960 - teamRat_GAME(P1, 0) Then

                            pbpString$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
                            Call PBP

                            If ydLine < 20 Then
                                ydLine = ydLine / 2
                                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
                            End If

                            gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

                            If ydLine >= 20 Then
                                ydLine = ydLine - 10
                                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + 10
                            End If

                        Else

                            F% = 4

                            Call FUMBLE

                            If ballFumbled = 1 Then Call FumblePBP: GoTo 650
                        End If

                        hangtime = Int(ydsGained / 10)
                        Call AddPossessionTime(8 + hangtime, 13 + hangtime)
                        Call DELAY

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                    End If

                End If

            End If

            GoTo 700


        Case 20

            Call Rolld100(fgChance)

            'Was previously I2 not J
            For J = 1 To 0 Step -1
                If fgChance <= kickerFGPct(P1, J) Then
                    I1 = J
                End If
            Next J

            activeKicker(P1) = I1

            'FG range is considered up to 67 yards
            '-- 50 yard line = 67 yard attempt
            If ydLine < 50 + goalPostAdj Then

                pbpString$ = "NOT IN FIELD GOAL RANGE"
                Call PBP
                GoTo 700

            Else

                ' *** FIELD GOAL DECISION ***
                If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

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

                Call Rolld100(RN0)

                If playerMode = 1 And D = compTeam Or playerMode = 2 Then

                    choice = 1

                Else

                    Call FRAMEOPTION(9, D)

                    Print "THE FIELD GOAL UNIT FOR "; gameTeams$(P1); " IS ON THE FIELD"
                    Locate 12, 14
                    Locate , 14: Print "1 - GO FOR THE BLOCK"
                    Locate , 14: Print "2 - CALL FIELD GOAL-SAFE"
                    Print
                    Locate , 14: Print "SELECT OPTION "; gameTeams$(D)

                    Do
                        I$ = GetKeyPress$
                        choice = Val(I$)
                    Loop Until choice >= 1 And choice <= 2

                End If

                Screen , , 0, 0

                'Fake Field Goal Was Chosen
                If I1 = 2 Or I1 = 1 Then

                    ' *** FAKE PUNT - RUN/PASS ***
                    If choice = 2 Then

                        pbpString$ = gameTeams$(D) + " IS IN FIELD GOAL SAFE SET...FAKE FIELD GOAL? (Y/N)"
                        Call PBP

                    Else

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "N" Then GoTo 3602

                    End If

                    If I1 = 2 Then

                        ' *** FAKE W/PASS ***
                        pbpString$ = "IT'S A FAKE!!! THE HOLDER IS LOOKING TO PASS"
                        Call PBP
                        ydsGained = 0
                        Call Rolld100(RN0)

                        passIncomplete = 0
                        passIntercepted = 0

                        Select Case I2

                            Case 1

                                Select Case RN0

                                    Case 1 To 20:
                                        ydsGained = Int(Rnd(1) * 20)

                                    Case 21 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        passIncomplete = 1

                                    Case 81 To 90:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted = 1

                                    Case 91 To 100:
                                        ydsGained = 100 - ydLine

                                End Select

                            Case 2

                                Select Case RN0

                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 7) + 6

                                    Case 11 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        passIncomplete = 1
                                        GoTo 3309

                                    Case 81 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted = 1

                                End Select

                        End Select

                        If passIntercepted = 1 Then

                            X = 0
                            'Track Pass Att, QB Int
                            gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                            gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1
                            gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
                            gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1

                            Call AddPossessionTime(5, 8)

                            ydsGained = Int(Rnd(1) * 10) + 1

                            Call YARDAGE

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                'Interception in the end zone
                                pbpString$ = pbpString$ + " IN THE END ZONE"
                                Call PBP
                                Call Touchback

                            Else

                                If ydsGained + ydLine > 49.99 Then
                                    pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE"
                                End If
                                If ydsGained + ydLine < 49.99 Then
                                    pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE"
                                End If

                                Call PBP
                                Call CHANGEPOSS

                                currDown = 1
                                ydLine = 100 - ydsGained - ydLine
                                ydsToScore = 100 - ydLine

                                Call YARDLINE

                                ydsGained = Int(Rnd(1) * 25) + 1

                                If ydsGained >= 24 Then ydsGained = Int(Rnd(1) * 70) + 31

                                Call YARDAGE

                                X1 = 5
                                startYds = 100 - ydLine
                                endYds = startYds - ydsGained

                                Call MOVEBALL

                                pbpString$ = "RETURNED FOR "

                                If ydsGained + ydLine > 99.99 Then
                                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                    Call PBP

                                    'Touchdown!
                                    GoTo 3200

                                Else
                                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                    Call PBP

                                    playSegment = 5
                                    ydLine = ydLine + ydsGained
                                    ydsToFirst = 10
                                    If ydLine >= 90 Then ydsToFirst = 100 - ydLine

                                    GoSub 3900

                                    Call DELAY

                                End If

                            End If

                            GoTo 700

                        Else

                            If passIncomplete <> 1 Then

                                pbpString$ = "PASS IS COMPLETE FOR"

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                Else
                                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                End If

                                Call PBP

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                                    'Safety!
                                    GoTo 3250
                                End If

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                    'Touchdown!
                                    GoTo 3200
                                End If

                                F% = 2

                                Call FUMBLE

                            End If

                            GoTo 3309

                        End If

                    End If

                    Call FAKERUN(choice)

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                        'Safety!
                        GoTo 3250
                    End If

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                        'Touchdown!
                        GoTo 3200
                    End If

                    F% = 2

                    Call FUMBLE

                    GoTo 3309

                Else

                    'Real Field Goal Attempt
                    GoTo 3602

                End If


            End If

        Case 21
            Call SCOUTREPT
            GoTo 700

        Case 22
            Call CHOOSEQB(P1)
            GoTo 700

        Case 23
            Call BOXOPTIONS(0)
            GoTo 850

        Case 24
            Call CHOOSEKICKER(P1)
            GoTo 700

    End Select

    '----------

    3535 ' PASS NOT COMPLETE
    'Incomplete / Interference Penalty
    X = 2
    RN1 = Int(Rnd(1) * 1000) + 1

    If RN1 <= 50 + teamRat_GAME(D, 0) And playCall <> 13 And endZone = 0 Then

        pbpString$ = "DEFENSIVE PASS INTERFERENCE"

        If playCall = 9 Or playCall = 10 Or playCall = 13 Then
            ydsGained = Int(Rnd(1) * 10) + 1
        End If

        If playCall = 12 Or playCall = 15 Then
            ydsGained = Int(Rnd(1) * 20) + 21
        End If

        If playCall = 11 Or playCall = 14 Or playCall = 16 Or playCall = 17 Or playCall = 18 Then
            ydsGained = Int(Rnd(1) * 10) + 11
        End If

        If ruleOptType = 0 And ruleOptColl >= 3 And ydsGained > 15 Then
            ydsGained = 15
        End If

        gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

        Call YARDAGE

        If ydsGained + ydLine >= 99.99 Then

            If ruleOptType <> 0 Then
                gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 100 - ydLine - 1
                pbpString$ = pbpString$ + " IN THE END ZONE"
                Call PBP

                ydLine = 99
                ydsToFirst = 1
            Else
                gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 100 - ydLine - 2

                pbpString$ = pbpString$ + " IN THE END ZONE"
                Call PBP

                ydLine = 98
                ydsToFirst = 2
            End If

        Else

            gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + ydsGained
            pbpString$ = pbpString$ + " AT THE "

            If ydsGained + ydLine < 50 Then
                pbpString$ = pbpString$ + Str$(Int((ydsGained + ydLine) * 10 + .5) / 10)
            Else
                pbpString$ = pbpString$ + Str$(Int((100 - ydLine - ydsGained) * 10 + .5) / 10)
            End If

            pbpString$ = pbpString$ + " YARD LINE"
            Call PBP

            ydLine = ydLine + ydsGained
            ydsToFirst = 10

            If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        End If

        'Automatic first down
        currDown = 1

        gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1
        playSegment = 4

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

        GoTo 700

    Else

        Call INCOMPLETE
        If endZone = 1 Then Call TryForTwoResult
        playSegment = 4

    End If

    GoTo 3309


    '----- NOODLE -----

    3200 '
    Randomize Timer

    Call Rolld100(RN0)

    'So essentially we have a 50/50 chance
    'of seeing the team name or the mascot name
    'IE, TOUCHDOWN! 89 SAN FRANCISCO
    '    TOUCHDOWN! 49ERS
    If RN0 <= 50 Then
        pbpString$ = "T O U C H D O W N !!! " + gameTeams$(P1)
    Else
        pbpString$ = "T O U C H D O W N !!! " + gameMascots$(P1)
    End If

    Call PBP
    Call SCOREUPD
    Call COMPILER

    If ruleOptType <> 4 Then
        'Not WFL; Touchdowns worth 6
        score(P1, quarter) = score(P1, quarter) + 6
        score(P1, 0) = score(P1, 0) + 6
    Else
        'WFL; Touchdowns worth 7
        score(P1, quarter) = score(P1, quarter) + 7
        score(P1, 0) = score(P1, 0) + 7
    End If

    Call DisplayField

    Call KickoffLine
    ydsToFirst = 10
    currDown = 1

    Call SCOREBOARD

    'Pro Overtime
    If ruleOptType > 0 And quarter > 4 Then
        GoTo 3364
    End If

    GoSub PointsAfterTD

    Call CHANGEPOSS

    playSegment = 4
    U3 = 9

    ydsToFirst = 10

    GoSub 3900

    U3 = 0

    Call SCOREBOARD

    'Lineup the kickoff
    Call KickoffLine

    Call SCOREBOARD

    If playType > 0 And playType < 3 And 100 - ydLine > 10 And ruleOptType <> 0 Then
        gameStatsTeam(D, 0) = gameStatsTeam(D, 0) + 1
    End If

    'Overtime
    If quarter > 4 Then 3364

    GoTo 340


    '----- NOODLE -----


    3250 '
    ' *** SAFETY ***
    score(D, quarter) = score(D, quarter) + 2
    score(D, 0) = score(D, 0) + 2
    gameStatsTeam(D, 19) = gameStatsTeam(D, 19) + 1

    Call COMPILER

    3251 '
    ydLine = 20
    ydsToFirst = 10
    currDown = 1
    playSegment = 5
    X1 = 7

    Call SCOREUPD
    Call CHANGEPOSS

    I6 = 99
    U3 = 9

    GoSub 3900

    U3 = 0
    pbpString$ = "S A F E T Y  !!!"

    Call PBP

    Call DisplayField

    'Overtime
    If quarter > 4 Then 3364

    GoTo 340

    '----- NOODLE -----

    3303 '
    If Int(ydsToFirst * 10 + .5) / 10 < .05 Then
        GoTo 3335
    End If

    If currDown = 3 Then
        thirdDownAtt(P1) = thirdDownAtt(P1) + 1
    End If

    If currDown = 4 Then
        thirdDownFail(P1) = thirdDownFail(P1) + 1
    End If

    If currDown > 4 Then
        GoTo 3339
    End If

    'Restore this when done
    RN0 = Int(Rnd(1) * 1000) + 1

    If RN0 <= 6 + teamRat_GAME(D, 0) Then

        pbpString$ = "PENALTY AFTER WHISTLE - "

        RN1 = Int(Rnd(1) * 10) + 1

        If (ruleOptType > 0 And optRulePro = 0) Or (ruleOptType = 0 And optRuleColl = 0) Then
            'No facemask penalties before 1962!
            'It was simply illegal from 1955-1961 but not penalized
            If (RN1 >= 7 And RN1 <= 10) Then RN1 = 6
        End If

        If (ruleOptType > 0 And ruleOptPro >= 5) Or (ruleOptType = 0 And ruleOptColl >= 9) Then
            'Starting 2008, all facemasks are flagrant
            If (RN1 >= 7 And RN1 <= 9) Then RN1 = 10
        End If

        Select Case RN1

            Case 1

                pbpString$ = pbpString$ + "OFF - PERSONAL FOUL - "
                gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

                If ydLine < 30 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL"
                    ydsToFirst = ydsToFirst + ydLine / 2
                    ydLine = ydLine - ydLine / 2
                    gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
                Else
                    pbpString$ = pbpString$ + "15 YARDS"
                    ydsToFirst = ydsToFirst + 15
                    ydLine = ydLine - 15
                    gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + 15
                End If

                autoFD = 1

                Call PBP

            Case 2 To 6

                pbpString$ = pbpString$ + "DEF - PERSONAL FOUL - "
                gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

                If 100 - ydLine < 30 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL"
                    ydsToFirst = ydsToFirst - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
                Else
                    pbpString$ = pbpString$ + "15 YARDS"
                    ydsToFirst = ydsToFirst - 15
                    ydLine = ydLine + 15
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 15
                End If

                autoFD = 1

                Call PBP

                'Can't end on defensive penalty
                If gameClock! <= 0 Then gameClock! = 1

            Case 7 To 9
                pbpString$ = pbpString$ + "DEF - INCIDENTAL FACEMASK - "
                gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

                If 100 - ydLine < 10 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL"
                    ydsToFirst = ydsToFirst - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
                Else
                    pbpString$ = pbpString$ + "5 YARDS"
                    ydsToFirst = ydsToFirst - 5
                    ydLine = ydLine + 5
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 5
                End If

                Call PBP

                'Can't end on defensive penalty
                If gameClock! <= 0 Then gameClock! = 1

            Case 10
                pbpString$ = pbpString$ + "DEF - FACEMASK - "
                gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

                If 100 - ydLine < 30 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL + 1ST DOWN!"
                    ydsToFirst = ydsToFirst - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
                Else
                    pbpString$ = pbpString$ + "15 YARDS + 1ST DOWN!"
                    ydsToFirst = ydsToFirst - 15
                    ydLine = ydLine + 15
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 15
                End If

                Call PBP

                autoFD = 1

                'Can't end on defensive penalty
                If gameClock! <= 0 Then gameClock! = 1

        End Select

    Else

        GoSub 3900

        GoTo 700

    End If



    '----- NOODLE -----

    3309 '

    pbpString$ = "*** PENALTY - "

    RN4 = Int(Rnd(1) * 1000) + 1

    Select Case playType

        Case 1
            'Run Play ???
            Select Case RN4

                Case 1 To 7 + teamRat_GAME(P1, 0)
                    pbpString$ = pbpString$ + "OFF... ILLEGAL MOTION 5 YARDS ***"
                    penaltyYds = -5

                Case 7 + teamRat_GAME(P1, 0) To 25 + teamRat_GAME(P1, 0)
                    'Pro Rules before 1974 | College Rules before 1984
                    If (ruleOptType > 0 And ruleOptPro <= 1) Or (ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF... HOLDING 15 YARDS ***"
                        penaltyYds = -15
                    Else
                        pbpString$ = pbpString$ + "OFF... HOLDING 10 YARDS ***"
                        penaltyYds = -10
                    End If

                    holding = 1

                Case 25 + teamRat_GAME(P1, 0) To 42 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    penaltyYds = 5

                Case Else
                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

        Case 2
            'Pass Play?

            Select Case RN4

                Case 1 To 17 + teamRat_GAME(P1, 0)

                    'Pro Rules before 1978 | College Rules before 1984
                    If (ruleOptType > 0 And ruleOptPro < 3) Or (ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF... HOLDING 15 YARDS ***"
                        penaltyYds = -15
                        holding = 1
                    Else
                        pbpString$ = pbpString$ + "OFF... HOLDING 10 YARDS ***"
                        penaltyYds = -10
                        holding = 1
                    End If

                Case 17 + teamRat_GAME(P1, 0) To 22 + teamRat_GAME(P1, 0)
                    pbpString$ = pbpString$ + "OFF... ILLEGAL MOTION 5 YARDS ***"
                    penaltyYds = -5

                Case 22 + teamRat_GAME(P1, 0) To 27 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    penaltyYds = 5

                Case 27 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0) To 42 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... HOLDING 5 YARDS - 1ST DOWN! ***"
                    autoFD = 1
                    penaltyYds = 5

                Case Else
                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

    End Select

    'College 1991+ Holding penalty
    'Randomly determine the "infraction point"
    'for enforcing the penalty
    If ruleOptType = 0 And ruleOptColl >= 6 And holding = 1 Then
        If playType = 1 Then
            'Run Play - past scrimmage
            penaltyYds = penaltyYds + Int(Rnd(1) * 5) + 1
        End If
        If playType = 2 Then
            'Pass Play - behind scrimmage
            penaltyYds = penaltyYds - Int(Rnd(1) * 8) + 1
        End If
    End If

    pbpString$ = pbpString$ + " 1-ACCEPT 2-DECLINE "
    Call PBP

    'Reset for next penalty
    holding = 0

    'Determine how to handle PENALTIES
    'At this point, both penalty yardage and
    'yards gained from the play have been determined
    If playerMode = 0 Then

        I$ = GetKeyPress$

    Else

        'Computer Player - default to DECLINE
        penaltyChoice = 2

        'Computer Player
        If penaltyYds < 0 Then

            'On the Offense
            If playerMode = 1 And P1 = compTeam Then
                'Computer Offense | Human Defense
                Do
                    I$ = GetKeyPress$
                    penaltyChoice = Val(I$)
                Loop Until penaltyChoice = 1 Or penaltyChoice = 2
            Else
                '4th Down and didn't get a 1st  --OR--
                ' 3rd Down and we'll have <= 3 yds for 1st --OR--
                ' we didn't net any yards on the play
                If playerMode = 1 And ((currDown > 3 And ydsGained < ydsToFirst) Or (currDown = 3 And ydsToFirst - ydsGained > 3 Or ydsGained < 0)) Then
                    I$ = GetKeyPress$
                Else
                    penaltyChoice = 1
                End If
            End If

        Else

            'Penalty Yds > 0 = On the Defense
            If playerMode = 1 And D = compTeam Then
                'Human Offense | Computer Defense
                Do
                    I$ = GetKeyPress$
                    penaltyChoice = Val(I$)
                Loop Until penaltyChoice = 1 Or penaltyChoice = 2
            Else
                'If we didn't get the first down but would be granted it,
                'or we gained <= 5 yards [penalties give us 5+ yards]
                If autoFD = 1 And ydsToFirst - ydsGained > 0 Or (ydsGained <= 5) Then
                    penaltyChoice = 1
                End If
            End If

        End If

    End If


    If penaltyChoice = 1 Then

        'Accept

        playSegment = 5

        If ruleOptType = 0 And ruleOptColl = 0 Then
            'College Pre-1968
            playSegment = 1
        End If

        pbpString$ = "PENALTY ACCEPTED"
        Call PBP

        If penaltyYds > 0 Then 3330

        penaltyYds = penaltyYds * -1

        'OFF PEN
        3326 '
        If ydLine < penaltyYds * 2 Then
            ydsToFirst = ydsToFirst + ydLine / 2
            ydLine = ydLine - ydLine / 2
            gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
        Else
            ydsToFirst = ydsToFirst + penaltyYds
            ydLine = ydLine - penaltyYds
            gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + penaltyYds
        End If

        gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

        If Int(ydsToFirst * 10 + .5) / 10 < .05 Then 3335

    Else

        pbpString$ = "PENALTY DECLINED"

        Call PBP
        Call COMPILER

        autoFD = 0
        ydLine = ydLine + ydsGained
        ydsToFirst = ydsToFirst - ydsGained
        currDown = currDown + 1

        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1
        If playSegment < 4 Or playSegment > 5 Then playSegment = 1

        If Int((ydsToFirst) * 10 + .5) / 10 < .05 Then
            currDown = 1
            ydsToFirst = 10

            If 100 - ydLine < 9.95 Then ydsToFirst = 100 - ydLine
            If playSegment < 1 And ruleOptType = 0 And firstDownClockOpt = 0 Then playSegment = 1

            gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

            If gameClock! <= 0 Then gameClock! = 1

        Else

            If currDown > 4 Then 3339

        End If

    End If

    GoSub 3900

    GoTo 700


    '----- NOODLE -----

    'DEFENSIVE PENALTY

    3330 '

    If 100 - ydLine < penaltyYds * 2 Then
        ydsToFirst = ydsToFirst - (100 - ydLine) / 2
        ydLine = ydLine + (100 - ydLine) / 2
        gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
    Else
        ydsToFirst = ydsToFirst - penaltyYds
        ydLine = ydLine + penaltyYds
        gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + penaltyYds
    End If

    gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

    If ydsToFirst < .01 Or autoFD = 1 Then

        autoFD = 0

        3335 '
        currDown = 1
        ydsToFirst = 10

        If 100 - ydLine < 9.95 Then
            ydsToFirst = 100 - ydLine
        End If

        If playSegment < 1 And ruleOptType = 0 And firstDownClockOpt = 0 Then
            playSegment = 1
        End If

        gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

    End If

    GoSub 3900

    If gameClock! <= 0 Then gameClock! = 1

    GoTo 700



    '----- NOODLE -----

    'College Overtime
    3339 '
    If NG%(14) = 3 And quarter > 4 Then
        GoTo 3364
    End If

    currDown = 1
    ydsToFirst = 10
    ydLine = 100 - ydLine

    If ydLine > 90 Then ydsToFirst = 100 - ydLine

    Call CHANGEPOSS

    playSegment = 5

    GoSub 3900

    GoTo 700

    '----- NOODLE -----

    '3349
    playSegment = 6

    If Int(ydsToFirst * 10 + .5) / 10 < .05 Then

        currDown = 1
        ydsToFirst = 10

        If 100 - ydLine < 9.95 Then ydsToFirst = 100 - ydLine
        If playSegment < 1 And ruleOptType = 0 And firstDownClockOpt = 0 Then playSegment = 1

        gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

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
    climate(0) = 1 - climate(0)
    gameClock! = 900

    quarter = quarter + 1

    If quarter = 3 Or quarter >= 5 Then
        'If end of half
        U3 = 0
    End If

    gameOver = 0

    'quarter - 1 = previous quarter
    'We've already incremented it

    Select Case quarter - 1

        Case 1, 3
            'quarter = 2 / 4
            'End of 1st / 3rd Quarter
            Call SCOREBOARD
            pbpString$ = "END OF QUARTER"
            Call PBP

            GoTo 700

        Case 2
            'quarter = 2 / 4
            'Half Time
            Call SCOREBOARD
            pbpString$ = "HALF TIME"
            Call PBP

            winTeam = 1 - winTeam

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

                If Not (autoPlay = 1 And playerMode = 2) Then

                    Do
                        'Using Home Team's color for the box
                        Call TEAMBOX(1, 1)
                        pbpString$ = "DISPLAY STATS AGAIN? (Y/N)"
                        Locate 7, (80 - Len(pbpString$)) / 2
                        Print pbpString$
                        I$ = GetKeyPress$
                        Color 15, 0
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                End If

            Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerMode = 2)

            GoTo 850

        Case Else
            'End of 4th Quarter / Overtime

            '--- start analysis here
            3364 '
            Call SCOREBOARD

            'Check if we're configured for overtime
            If NG%(14) = 0 Or (score(0, 0) <> score(1, 0) And NG%(14) <> 3) Or (score(0, 0) <> score(1, 0) And NG%(14) = 3 And (nbrPossOT(1) = 1 And nbrPossOT(0) = 1 Or nbrPossOT(1) = 0 And nbrPossOT(0) = 0)) Then

                'Nope!
                'Leave quarter at 4
                If isOT = 0 Then
                    quarter = quarter - 1
                End If

                pbpString$ = "END OF GAME"
                Call PBP
                Call GetKeyWhenNoDelay

                gameOver = 1

            End If

            '--- should this be outside the SELECT

            'Check if game is over
            If gameOver <> 1 Then

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
                        ydLine = 75
                        ydsToScore = 25
                        ydsToFirst = 10
                        currDown = 1

                        '1st Overtime
                        isOT = 1

                        'Track overtime possesions for each team
                        If nbrPossOT(0) = 1 And nbrPossOT(1) = 1 Then quarter = quarter + 1

                        If nbrPossOT(0) = 1 And nbrPossOT(1) = 1 Or nbrPossOT(0) = 0 And nbrPossOT(1) = 0 Then

                            Erase nbrPossOT
                            P1 = Int(Rnd(1) * 2)
                            D = 1 - P1
                            Call CHANGEPOSS
                            nbrPossOT(P1) = 1

                        ElseIf nbrPossOT(0) = 1 Then

                            nbrPossOT(1) = 1
                            P1 = 1
                            D = 0
                            If score(P1, 0) > score(D, 0) Then 3364

                        ElseIf nbrPossOT(1) = 1 Then

                            nbrPossOT(0) = 1
                            P1 = 0
                            D = 1
                            If score(P1, 0) > score(D, 0) Then 3364

                        End If

                        GoTo 700

                End Select

                'We're going into overtime
                If UCase$(I$) = "Y" Then

                    If quarter = 5 Then

                        P1 = Int(Rnd(1) * 2)

                        Call CHANGEPOSS

                        '1 timeout for each team
                        timeouts(0) = timeouts(0) + 1
                        timeouts(1) = timeouts(1) + 1

                        '1st and 10
                        currDown = 1
                        ydsToFirst = 10

                        'Starting yard line for possession
                        Call KickoffLine

                        If P1 = 0 Then D = 1
                        If P1 = 1 Then D = 0

                        isOT = 1

                        GoTo 340

                    Else

                        timeouts(0) = timeouts(0) + 1
                        timeouts(1) = timeouts(1) + 1
                        isOT = 1

                        GoTo 700

                    End If

                End If

                'Since this increases at end of quarter,
                'we need to decrease
                quarter = quarter - 1

                pbpString$ = "END OF GAME"
                Call PBP
                Call GetKeyWhenNoDelay
            End If

            '8540
            Call TICKERUPDATE

            '-- When would this NOT happen???
            If quarter <> 3 Then
                gameOver = 1
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
        pbpString$ = "PASS IS INTERCEPTED BY " + defTeam$
    Else
        pbpString$ = "PASS IS INTERCEPTED"
    End If

    X = 0

    'Record INT's for Defense
    gameInts(1 - P1, Z1, 0) = gameInts(1 - P1, Z1, 0) + 1

    'Track INT's for team
    gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
    gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1

    If FF% <> 1 Then
        gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
        gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1
    End If

    If endZone = 1 Then
        'Intercepted in the end zone
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP

        GoTo 6605

    End If

    Call AddPossessionTime(5, 8)

    If FF% <> 0 Then
        FF% = 0
    Else
        If playCall = 12 Or playCall = 15 Then ydsGained = Int(Rnd(1) * 20) + 21
        If playCall <> 12 And playCall <> 15 Then ydsGained = Int(Rnd(1) * 10) + 1
        If playCall = 13 Then ydsGained = ydsGained - 5
        If playCall = 11 Or playCall = 14 Or playCall = 17 Or playCall = 16 Or playCall = 18 Then ydsGained = ydsGained + 10
    End If

    Call YARDAGE

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
        'Intercepted doesn't leave the endzone
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP

        'College overtime
        If NG%(14) = 3 And quarter > 4 Then
            GoTo 3364
        End If

        Call Touchback
        GoTo 700

    End If

    If ydsGained + ydLine > 49.99 Then
        pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE"
    Else
        pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE"
    End If

    Call PBP

    Call CHANGEPOSS

    currDown = 1
    ydLine = 100 - ydsGained - ydLine
    ydsToScore = 100 - ydLine

    Call YARDLINE

    ydsGained = Int(Rnd(1) * 25) + 1

    If ydsGained >= 24 Then
        ydsGained = Int(Rnd(1) * 70) + 31
    End If

    Call YARDAGE

    X1 = 5
    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

    pbpString$ = "RETURNED FOR "

    If ydsGained + ydLine > 99.99 Then

        pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"

        'Record INT TDs, Yds
        gameInts(P1, Z1, 2) = gameInts(P1, Z1, 2) + 1
        gameInts(P1, Z1, 1) = gameInts(P1, Z1, 1) + 100 - ydLine

        If 100 - ydLine > IR(P1, Z1) Then
            gameIR$(P1, Z1) = "t"
            IR(P1, Z1) = 100 - ydLine
        End If

        Call PBP

        'Touchdown!
        GoTo 3200

    Else

        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
        Call PBP

        playSegment = 5
        ydsToFirst = 10
        ydLine = ydLine + ydsGained

        'Record INT Yds
        gameInts(P1, Z1, 1) = gameInts(P1, Z1, 1) + ydsGained

        If ydsGained > IR(P1, Z1) Then
            gameIR$(P1, Z1) = ""
            IR(P1, Z1) = ydsGained
        End If

        If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        'College overtime
        If NG%(14) = 3 And quarter > 4 Then
            GoTo 3364
        End If

        F% = 5

        Call FUMBLE

        If ballFumbled = 1 Then Call FumblePBP: GoTo 650

        RN1 = Int(Rnd(1) * 1000) + 1

        If RN1 > 950 - teamRat_GAME(P1, 0) Then

            pbpString$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
            Call PBP

            'Record penalty
            gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

            'Record yds
            If ydLine < 20 Then
                '1/2 the distance to the goal
                ydLine = ydLine / 2
                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
            Else
                '10 yds
                ydLine = ydLine - 10
                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + 10
            End If

        End If

        GoSub 3900

        GoTo 700

    End If


    '----- NOODLE -----

    3900 '
    'Adjust possession time, check for special scenarios
    '(ie, hurry up, 2-minute warning, etc.)

    '--> Convert to a subroutine
    '--> takes a parameter regarding gameClock
    '--> then returns that so we can issue the following afterwards
    '       If gameClock! = 0 Then GoTo 3360

    If playSegment <> 8 Then

        'Pro Rules
        If ruleOptType <> 0 Then

            If gameClock! > 120.01 And quarter <> 1 And quarter <> 3 And Not (gameClock! > 159.99 And playSegment = 0) And Not (gameClock! > 147 And playSegment = 1) And Not (gameClock! > 142 And playSegment = 2) And Not (gameClock! > 137 And playSegment = 3) And Not (gameClock! > 126 And playSegment = 4) And Not (gameClock! > 128 And playSegment = 5) And Not (gameClock! > 130 And playSegment = 6) And Not (gameClock! > 132 And playSegment = 7) Then

                pbpString$ = "* * * T W O   M I N U T E   W A R N I N G * * *"
                Call PBP

                If gameClock! <= 126.01 And gameClock! >= 120.99 Then
                    gameClock! = gameClock! - 6
                    timePoss(P1) = timePoss(P1) + 6
                End If

                If gameClock! > 125.99 Then
                    timePoss(P1) = timePoss(P1) + (gameClock! - 120)
                    gameClock! = 120
                End If

            End If

        End If

        If (gameClock! <= 120 And quarter = 2) Or (gameClock! <= 120 And quarter >= 4) Then

            If playSegment < 4 Then

                Call CheckGameClock
                If gameClock! = 0 Then GoTo 3360
                Call TwoMinMenu(I1)

                Select Case I1

                    Case 1:
                        pbpString$ = "HUDDLE"
                        Call PBP

                    Case 2:
                        If timeouts(P1) = 0 Then
                            'If no timeouts
                            pbpString$ = "HUDDLE"
                            Call PBP
                        Else
                            pbpString$ = "TIME OUT " + gameTeams$(P1) + " - THEY HAVE" + Str$(timeouts(P1) - 1) + " REMAINING"
                            Call PBP
                            timeouts(P1) = timeouts(P1) - 1
                            playSegment = 9

                            Call AddPossessionTime(7, 10)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Return 'aka Exit Sub

                        End If

                    Case 3

                        If timeouts(D) = 0 Then
                            'If no timeouts
                            pbpString$ = "HUDDLE"
                            Call PBP
                        Else
                            pbpString$ = "TIME OUT " + gameTeams$(D) + " - THEY HAVE" + Str$(timeouts(D) - 1) + " REMAINING"
                            Call PBP
                            timeouts(D) = timeouts(D) - 1
                            playSegment = 9

                            Call AddPossessionTime(7, 10)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Return 'aka Exit Sub

                        End If

                    Case 4
                        If playSegment = 0 Or playSegment = 1 Then
                            pbpString$ = "HURRY UP"
                            Call PBP
                            playSegment = playSegment + 2
                        End If

                    Case 5
                        pbpString$ = qbacks$(P1, qbNumber(P1)) + " IS TRYING TO GET THE TEAM LINED UP"
                        Call PBP

                        playSegment = 3
                        pbpString$ = "HE STEPS BACK AND THROWS IT OUT OF BOUNDS"
                        Call PBP

                        'Increase pass attempts
                        gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                        gameStatsPlayer(P1, qbNumber(P1), 9) = gameStatsPlayer(P1, qbNumber(P1), 9) + 1

                        currDown = currDown + 1
                        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        Call AddPossessionTime(7, 10)
                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Return 'aka Exit Sub

                End Select

            End If

        End If

        If playSegment = 0 Then

            Call AddPossessionTime(33, 38)

            If ruleOptType <> 0 Then
                'Pro Rules
                If gameClock! > 40 Then
                    gameClock! = gameClock! - 4
                    timePoss(P1) = timePoss(P1) + 4
                End If

                If gameClock! > 43 And teamYears(0) >= 1990 And teamYears(1) >= 1990 Then
                    gameClock! = gameClock! - 3
                    timePoss(P1) = timePoss(P1) + 3
                End If

                If gameClock! > 43 And teamYears(0) >= 1993 And teamYears(1) >= 1993 Then
                    gameClock! = gameClock! + 3
                    timePoss(P1) = timePoss(P1) - 3
                End If

            Else
                'College Rules
                If gameClock! > 43 Then

                    If teamYears(0) = 1956 And teamYears(1) = 1956 Then
                        gameClock! = gameClock! + 3
                        timePoss(P1) = timePoss(P1) - 3
                    End If

                    If teamYears(0) = 1957 And teamYears(1) = 1957 Then
                        gameClock! = gameClock! + 3
                        timePoss(P1) = timePoss(P1) - 3
                    End If

                End If

            End If

        Else
            'playSegment > 0
            If playSegment >= 1 And playSegment <= 3 Then
                timeElapsed! = 27 - (playSegment * 5) + (Int(Rnd(1) * 5) + 1)
            Else
                'playSegment >= 4
                timeElapsed! = (2 * playSegment) - 5 + (Int(Rnd(1) * 3) + 1)
            End If

            gameClock! = gameClock! - timeElapsed!
            timePoss(P1) = timePoss(P1) + timeElapsed!

        End If

        Call CheckGameClock
        If gameClock! = 0 Then GoTo 3360

    End If

    Return


    '----- NOODLE -----

    3602 'FIELD GOAL

    If ydLine >= 97 + goalPostAdj Then
        fgSuccessChance = kickerIdx(P1, activeKicker(P1))
    Else
        Call FGDIST
    End If

    pbpString$ = kicker$(P1, activeKicker(P1)) + " TROTS ON TO TRY A FIELD GOAL"
    Call PBP

    pbpString$ = "THIS WILL BE A" + Str$(Int(117 + goalPostAdj - ydLine + .5)) + " YARD ATTEMPT"
    Call PBP

    pbpString$ = "HE WAITS FOR THE SNAP"
    Call PBP

    '.33% chance of bad snap
    I5 = Int(Rnd(1) * 300) + 1

    If I5 = 12 Then

        pbpString$ = "THERE'S A BAD SNAP FROM CENTER!!"
        Call PBP
        ydsGained = (Int(Rnd(1) * 9) + 6) * -1

        ' *** BAD SNAP FROM CENTER ***

        Call AddPossessionTime(4, 7)

        gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1
        gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1

        If ydLine + ydsGained < -9.999 Then
            score(D, quarter) = score(D, quarter) + 2
            score(D, 0) = score(D, 0) + 2
            GoTo 3251
        End If

        If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

            Call Rolld100(RN0)

            If RN0 <= 50 Then

                score(D, quarter) = score(D, quarter) + 2
                score(D, 0) = score(D, 0) + 2

                GoTo 3251

            Else

                X1 = 10
                Call CHANGEPOSS

                'Touchdown!
                GoTo 3200

            End If

            Call CHANGEPOSS
            currDown = 1
            ydsToFirst = 10
            ydLine = 100 - ydLine

            If ydsToFirst >= 90 Then ydsToFirst = 100 - ydLine

            Call Rolld100(RN0)

            If RN0 <= 50 Then
                pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
            Else
                pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
            End If

            If ydLine > 50 Then
                pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
            Else
                pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
            End If

            playSegment = 5

            GoSub 3900

            GoTo 700

        End If

    Else

        Call Rolld100(blockChance)

        If (blockChance >= 97 And ydLine >= 68 + goalPostAdj And I2 <> 2) Or (blockChance >= 92 And ydLine < 68 + goalPostAdj And I2 <> 2) Then

            gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) + 1
            pbpString$ = "KICK IS BLOCKED!!!"
            Call PBP

            Call AddPossessionTime(4, 7)

            ydsGained = Int(Rnd(1) * 22) * -1
            Call YARDAGE
            ydLine = 100 - ydLine - ydsGained

            If ydLine >= 109.949 Then
                'Safety!
                GoTo 3250
            End If

            If ydLine >= 100 Then

                Call CHANGEPOSS

                Call Rolld100(RN0)
                If RN0 <= 50 Then
                    pbpString$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
                Else
                    pbpString$ = gameMascots$(P1) + " RECOVER IT IN THE END ZONE!!"
                End If

                Call PBP

                X1 = 6

            Else

                Call Rolld100(RN0)

                If RN0 <= 94 Then

                    Call CHANGEPOSS

                    ydsToFirst = 10

                    If ydLine >= 90 Then ydsToFirst = 100 - ydLine

                    currDown = 1

                    Call DELAY

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                    GoTo 700

                Else

                    Call CHANGEPOSS

                    ydsGained = 100 - ydLine
                    startYds = 100 - ydLine
                    endYds = startYds - ydsGained

                    Call MOVEBALL

                    Call Rolld100(RN0)
                    If RN0 <= 50 Then
                        pbpString$ = gameTeams$(P1) + " TAKES IT INTO THE END ZONE!!"
                    Else
                        pbpString$ = gameMascots$(P1) + " TAKE IT INTO THE END ZONE!!"
                    End If

                    Call PBP

                    X1 = 6

                End If

            End If

            'Touchdown!
            GoTo 3200

        Else

            'Increase FG attempt
            gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) + 1

            pbpString$ = "THE KICK IS UP AND ON ITS WAY"
            Call PBP

            If climate(0) <> P1 Then
                fgSuccessChance = fgSuccessChance - Int(Rnd(1) * 7) + 3 - climate(1)
            Else
                fgSuccessChance = fgSuccessChance + Int(Rnd(1) * 7) - 3 + climate(1)
            End If

            Call FGATT

            'Where does "I" come from ???
            If fgChance <= fgSuccessChance Then

                'Increase FGM
                gameStatsPlayer(P1, 3 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 3 + activeKicker(P1) * 4, 17) + 1
                X1 = 8
                I1 = activeKicker(P1)

                Call SCOREUPD

                currDown = 1
                ydsToFirst = 10
                score(P1, 0) = score(P1, 0) + 3
                score(P1, quarter) = score(P1, quarter) + 3

                Call FGMADE
                Call AddPossessionTime(4, 7)
                Call ITSGOOD
                Call DisplayField
                Call CHANGEPOSS

                Call SCOREBOARD

                Call KickoffLine

                Call SCOREBOARD

                Call CheckGameClock
                If gameClock! = 0 Then
                    U3 = 9
                    GoTo 3360
                End If

                If quarter > 4 Then 3364

                GoTo 340

            Else

                Call MISSEDFG
                currDown = 1
                ydsToFirst = 10

                Call CHANGEPOSS

                If ydLine > 40 And ydLine <= 79.99 And ruleOptType = 1 And ruleOptPro >= 4 Then
                    'NFL, after 1993
                    ydLine = ydLine - 7
                    ydLine = 100 - ydLine
                Else
                    If ydLine > 79.99 Or goalPostAdj < 0 Then ydLine = 20
                    If ydLine > 40 And ydLine <= 79.99 And goalPostAdj = 0 Then ydLine = 100 - ydLine
                End If

                Call AddPossessionTime(4, 7)
                Call CheckGameClock
                If gameClock! = 0 Then GoTo 3360

                'College overtime
                If quarter > 4 And NG%(14) = 3 Then 3364

            End If 'Done checking FG success

        End If ' Done handling successful FG

    End If ' Done checking for bad snap

    GoTo 700


    '----- NOODLE -----


    3840 '
    gameStatsTeam(P1, 28) = gameStatsTeam(P1, 28) + 1
    pbpString$ = "KICK IS BLOCKED!!!"

    Call PBP

    Call AddPossessionTime(4, 7)

    ydsGained = Int(Rnd(1) * 30) * -1

    Call YARDAGE
    ydLine = 100 - ydLine - ydsGained

    If ydLine >= 109.949 Then
        'Safety!
        GoTo 3250
    End If

    If ydLine >= 100 Then
        Call CHANGEPOSS

        pbpString$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
        Call PBP

        X1 = 6

        'Touchdown!
        GoTo 3200

    End If

    Call Rolld100(RN0)

    If RN0 > 60 Then

        Call CHANGEPOSS
        ydsGained = 100 - ydLine
        startYds = 100 - ydLine
        endYds = startYds - ydsGained

        Call MOVEBALL

        pbpString$ = gameTeams$(P1) + " TAKES THE BALL INTO THE END ZONE!!"
        Call PBP

        X1 = 6

        'Touchdown!
        GoTo 3200

    Else

        Call CHANGEPOSS: ydsToFirst = 10

        If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        currDown = 1

        Call DELAY

        If gameClock! = 0 Then GoTo 3360

    End If

    GoTo 700


    '----- NOODLE -----


    'Pass Intercepted

    3876 '
    X = 0

    'Track attempt and interception
    gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
    gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1
    gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
    gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1

    Call AddPossessionTime(5, 8)

    ydsGained = Int(Rnd(1) * 10) + 1
    Call YARDAGE

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then

        'Interception in the end zone
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP
        Call Touchback

    Else

        If ydsGained + ydLine > 49.99 Then
            pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE"
        End If
        If ydsGained + ydLine < 49.99 Then
            pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE"
        End If

        Call PBP

        Call CHANGEPOSS

        currDown = 1
        ydLine = 100 - ydsGained - ydLine
        ydsToScore = 100 - ydLine

        Call YARDLINE

        ydsGained = Int(Rnd(1) * 25) + 1

        If ydsGained >= 24 Then
            ydsGained = Int(Rnd(1) * 70) + 31
        End If

        Call YARDAGE

        X1 = 5
        startYds = 100 - ydLine
        endYds = startYds - ydsGained

        Call MOVEBALL

        pbpString$ = "RETURNED FOR "

        If ydsGained + ydLine > 99.99 Then

            pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
            Call PBP

            'Touchdown!
            GoTo 3200

        Else

            pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
            Call PBP

            playSegment = 5
            ydLine = ydLine + ydsGained
            ydsToFirst = 10

            If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        End If

        GoSub 3900

        Call DELAY

    End If

    GoTo 700

    '----- NOODLE -----

    'start of a subroutine??

    6605 '
    Call CHANGEPOSS

    ydLine = 1
    ydsToScore = 100 - ydLine

    Call YARDLINE

    Call Rolld100(RN0)
    If RN0 <= 18 Then
        ydsGained = 110
    Else
        ydsGained = Int(Rnd(1) * 21) + 50
    End If

    Call YARDAGE

    X1 = 12
    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

    pbpString$ = "RETURNED FOR "

    If ydsGained + ydLine > 99.99 Then

        pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
        Call PBP

        score(P1, quarter) = score(P1, quarter) + 2
        score(P1, 0) = score(P1, 0) + 2

        pbpString$ = "GOOD FOR 2 POINTS!!"
        Call PBP

        Call DisplayField
        Call SCOREUPD

    Else

        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
        Call PBP

    End If

    endZone = 0
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
            For I3 = 0 To 1:
                Call CHOOSEQB(I3)
            Next I3
        End If

        Randomize Timer

        Call SCOREBOARD

        GoTo 330

    Else

        If gameOver = 1 Then
            Call EndOfGame
            Exit Sub
        Else
            GoTo 700
        End If

    End If

End Sub
