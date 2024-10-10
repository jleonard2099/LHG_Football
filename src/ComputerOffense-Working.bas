Sub ComputerOffense ()

    fgAttLine = (117 - fgLimit% + goalPostAdj)

    If endZone = 1 Then

        'We should only be here if going for 2
        Call Rolld100(RN0)

        Select Case RN0

            'Rollout Pass
            Case 1 To 25:
                playCall = 17

                ' Medium Pass
            Case 26 To 50:
                playCall = 11

                ' Short Pass
            Case 51 To 70:
                playCall = 10

                ' Option
            Case 71 To 80:
                playCall = 5

                ' Sweep
            Case 81 To 90:
                playCall = 4

                ' Off Tackle
            Case 91 To 100:
                playCall = 3

        End Select

    Else

        '4th Qtr, Less than 35 secs to go, No TOs (Def), Offense leading
        If quarter = 4 And gameClock! <= 35 * (4 - currDown) And timeouts(D) = 0 And score(P1, 0) > score(D, 0) Then

            playCall = 9
            playType = 1
            qbTakeKnee = 1

        Else

            'If not inside 2 minute warning and past Own 40 Yd Line
            If Not (quarter = 2 And gameClock! <= 120 And ydLine > 40 Or quarter >= 4 And gameClock! <= 120) Then

                If ruleOptType <> 0 Then
                    leagueRushPct = leagRat_GAME(P1, 7)
                Else
                    'Do we need to adjust rush play call frequency?
                    'This is predicated on the idea that QB sneaks
                    'are recorded differently in NCAA. If QB rushes,
                    'in my game, get redirected to passes, then teams
                    'should be passing more often???
                    leagueRushPct = leagRat_GAME(P1, 7) ' - 4
                End If

                '4th Down, 4th Qtr+, Opponent's side of field, Down by 11 or more (Off)
                If currDown = 4 And quarter >= 4 And ydLine >= 50 And score(D, 0) - score(P1, 0) > 10 Then
                    GoTo 8833
                End If

                '4th Down, 3rd Qtr+, Inside opponent's 45, Down by 15 or more (Off)
                If currDown = 4 And quarter >= 3 And ydLine >= 55 And score(D, 0) - score(P1, 0) > 14 Then
                    GoTo 8833
                End If

                '4th Down, 1 Yd To Go, 3 Pt. Lead, Opponent's 32 to 38, Score Not Tied
                If currDown = 4 And ydsToFirst <= 1 And score(P1, 0) - score(D, 0) <= 3 And score(P1, 0) - score(D, 0) > 0 And ydLine >= 62 + goalPostAdj And ydLine <= 68 + goalPostAdj Then
                    GoTo 8827
                End If

                '4th Qtr+
                '---------------
                '4th Down, Opponent's 32 to 38, Score = 3 Pt. Margin, Modern Era (FGs uncommon before 1960)
                If quarter >= 4 And currDown = 4 And score(P1, 0) - score(D, 0) <= 3 And score(P1, 0) - score(D, 0) >= -3 And ydLine >= 62 + goalPostAdj And ydLine < 68 + goalPostAdj And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                    'Field Goal
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, Opponent's 32 to 38, Score <= 3 Pt. Lead, Modern Era (FGs uncommon before 1960)
                If quarter >= 4 And currDown = 4 And ydLine >= 62 + goalPostAdj And ydLine < 68 + goalPostAdj And score(D, 0) - score(P1, 0) <= 3 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                    'Field Goal
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, <= 2 Yds to Go, Opponent's 25 - 35, Down by 11 or more (Off)
                If quarter >= 4 And ydLine >= 65 + goalPostAdj And ydLine < 75 + goalPostAdj And ydsToFirst <= 2 And score(D, 0) - score(P1, 0) > 10 Then
                    GoTo 8827
                End If

                '4th Down, <= 2 Yds to Go, Opponent's 25 - 35, Down by 10 or less (Off), 4 Minutes Remaining, Modern Era (FGs uncommon before 1960)
                If quarter >= 4 And ydLine >= 65 + goalPostAdj And ydLine < 75 + goalPostAdj And score(D, 0) - score(P1, 0) <= 10 And score(D, 0) - score(P1, 0) > 0 And currDown = 4 And gameClock! < 240 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                    'Field Goal
                    playCall = 20
                    GoTo 8800
                End If

                '4th Qtr Only
                '---------------
                '4th Down, More Than 2 Minutes, Down by 4 to 10 (Off), Inside Red Zone
                If currDown = 4 And quarter = 4 And gameClock! > 240 And score(D, 0) - score(P1, 0) >= 4 And score(D, 0) - score(P1, 0) <= 10 And ydLine > 80 + goalPostAdj Then
                    'Field Goal
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, Down by 4 or More, Inside Red Zone
                If currDown = 4 And quarter = 4 And score(D, 0) - score(P1, 0) >= 4 And ydLine >= 80 + goalPostAdj Then
                    GoTo 8833
                End If


                'Any Qtr
                '---------------
                '4th Down, <= 2 Yds to Go, Inside Opponent's 40
                If currDown = 4 And ydLine >= 60 And ydsToFirst <= 2 Then
                    GoTo 8827
                End If

                '4th Down, >= 3 Yds To Go, Inside Opponent's 32, Modern Era (FGs uncommon before 1960)
                If currDown = 4 And ydLine >= 68 + goalPostAdj And ydsToFirst > 2 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                    'Field Goal
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, Outside Opponent's 32
                If currDown = 4 And ydLine < 68 + goalPostAdj Then
                    'Punt
                    playCall = 19
                    GoTo 8800
                End If

                Call Rolld100(RN0)

                'Inside Team's Own 8 Yd Line
                If ydLine <= 8 Then
                    GoTo 8827
                End If

                '2nd Down, <=12 Yds To Go
                If leagueRushPct > 75 And currDown = 2 And ydsToFirst <= 12 Then
                    GoTo 8827
                End If

                '3rd Down, <= 5 Yds to Go
                If leagueRushPct > 75 And currDown = 3 And ydsToFirst <= 5 Then
                    GoTo 8827
                End If

                '1st or 2nd Down, Any Time
                If currDown = 1 And leagueRushPct >= 70 Or currDown <= 2 And leagueRushPct >= 80 Then
                    GoTo 8827
                End If

                '1st Down, >= 11 Yds To Go
                If currDown = 1 And ydsToFirst > 10 Then
                    GoTo 8833
                End If

                '2nd Down, >= 11 Yds To Go
                If currDown = 2 And ydsToFirst > 10 Then
                    GoTo 8833
                End If

                '2nd Down, <= 4 Yds to Go
                If currDown = 2 And ydsToFirst < 5 Then
                    GoTo 8827
                End If

                '3nd Down, >= 4 Yds to Go
                If currDown = 3 And ydsToFirst >= 4 Then
                    GoTo 8833
                End If

                '3rd Down, 1 Yds to Go
                If currDown = 3 And ydsToFirst < 2 Then
                    GoTo 8827
                End If

                '4th Down, >= 4 Yds to Go
                If currDown = 4 And ydsToFirst >= 4 Then
                    GoTo 8833
                End If

                '4th Down, 1 Yds To Go
                If currDown = 4 And ydsToFirst < 2 Then
                    GoTo 8827
                End If

                '4th Qtr
                '---------------
                'Less than 8 Minutes in Game
                'Either team is ahead by 9 or more pts
                If quarter = 4 And gameClock! <= 480 And score(D, 0) - score(P1, 0) >= 9 And RN0 >= leagueRushPct - leagRat_GAME(P1, 7) / 2 Then
                    GoTo 8833
                End If
                If quarter = 4 And gameClock! <= 480 And score(P1, 0) - score(D, 0) >= 9 And RN0 >= leagueRushPct + leagRat_GAME(P1, 7) / 2 Then
                    GoTo 8833
                End If

                If RN0 > leagRat_GAME(P1, 7) Then
                    GoTo 8833
                End If


                '----- DECISION BRANCH 1 -----
                8827 '
                If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then
                    'leagueRushPct = Run / Pass %
                    If gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3)) > leagueRushPct / 100 And leagueRushPct < 50 Then
                        'PASS!
                        GoTo 8833
                    End If
                End If

                Call Rolld100(RN0)

                Select Case qbMobility(P1)

                    Case 1 To 2
                        '16%, 22%
                        sneakChance = (qbMobility(P1) * 6) + 10

                    Case 3 To 4
                        '36%, 43
                        sneakChance = (qbMobility(P1) * 7) + 15

                    Case 5 To 6
                        '50%, 58%
                        sneakChance = (qbMobility(P1) * 8) + 10

                    Case 7 To 8
                        '66%, 74%
                        sneakChance = (qbMobility(P1) * 8) + 10

                    Case 9
                        '86%
                        sneakChance = (qbMobility(P1) * 9) + 5

                End Select

                If ydsToFirst <= 1.32 Then

                    'QB Sneak
                    'Print #11, "Evaluating sneak"
                    If RN0 <= sneakChance Then playCall = 8

                Else

                    Call Rolld100(RN1)

                    Select Case RN1

                        'Long Pass
                        Case 1 To 2:
                            If leagueRushPct < 75 Then
                                playCall = 15
                            End If

                            'Med Pass
                        Case 3 To 5:
                            If leagueRushPct < 75 Then
                                playCall = 14
                            End If

                            'Option
                        Case 6 To 10:
                            playCall = 5

                            'Sweep
                        Case 11 To 20:
                            playCall = 4

                            'Trap
                        Case 21 To 30:
                            playCall = 7

                            'Off Tackle
                        Case 31 To 50:
                            playCall = 3

                            'Dive
                        Case 51 To 70:
                            playCall = 2

                            'Blast
                        Case 71 To 100:
                            playCall = 1

                    End Select

                End If

                If qbacks$(P1, qbNumber(P1)) = "J.HURTS" And (gameTeams$(P1) = "22 PHILADELPHIA" Or gameTeams$(P1) = "23 PHILADELPHIA") And ydsToFirst <= 2 Then
                    playCall = 8
                End If

                GoTo 8800



                '----- DECISION BRANCH 2 -----
                8833 '
                Call Rolld100(RN0)

                Select Case RN0

                    ' Draw
                    Case 1 To 5:
                        playCall = 6

                        ' Rollout
                    Case 6 To 30:
                        playCall = 17

                        ' 3-Step Drop
                    Case 31 To 36:
                        playCall = 9

                        ' Screen
                    Case 37 To 44:
                        playCall = 13

                        ' Long Pass
                    Case 45 To 63:
                        playCall = 12

                        ' Medium Pass
                    Case 64 To 89:
                        playCall = 11

                        ' Short Pass
                    Case 90 To 100:
                        playCall = 10

                End Select

                '3rd / 4th Down, >= 5 Yds To Go
                '----------------------------------------
                '-- If Draw was selected, change to Long Pass
                If currDown >= 3 And ydsToFirst >= 5 And playCall = 6 Then
                    playCall = 12
                End If

                '-- If Short Pass was selected, change to Medium Pass
                If currDown >= 3 And ydsToFirst >= 5 And playCall = 10 Then
                    playCall = 11
                End If

                '-- If 3-Step Drop or screen was selected, change to Rollout
                If currDown >= 3 And ydsToFirst >= 5 And (playCall = 13 Or playCall = 9) Then
                    playCall = 17
                End If

                'Inside Opponent's 10 or < 7 Yds To Go
                '----------------------------------------
                '-- If Long Pass was selected, change to Medium Pass
                If ydLine > 90 And (playCall = 12 Or playCall = 15) Or ydsToFirst < 7 And (playCall = 12 Or playCall = 15) Then
                    playCall = 11
                End If

                GoTo 8800

            Else

                '2 Min Warning
                '--------------------
                'Down by >= 4 (Off)
                If score(D, 0) - score(P1, 0) > 3 Then
                    If quarter >= 4 Then 8880
                    If quarter = 2 Then 8885
                End If

                'Score Tied or Down
                If score(D, 0) - score(P1, 0) >= 0 Then 8885

                '2nd Qtr, Past Own 40 Yd Line
                If ydLine > 40 And quarter = 2 Then 8885

                If d100% <= 60 Then 
                    playCall = 3
                Else
                    playCall = 4
                End If

                'In FG range, Less than 22 secs to go, 2nd Qtr | 4th Qtr Tie | Overtime
                If ydLine >= fgAttLine And gameClock! <= 22 And (quarter = 2 Or quarter > 4 Or (quarter = 4 And score(P1, 0) - score(D, 0) = 0)) Then
                    'Field Goal
                    playCall = 20
                End If

                'In FG range, 4th Down, >= 1 Yard to go, Modern Era (FGs uncommon before 1960)
                If ydLine >= fgAttLine And currDown = 4 And ydsToFirst >= 2 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                    'Field Goal
                    playCall = 20
                End If

                'Outside FG range, 4th Down
                If ydLine <= fgAttLine And currDown = 4 Then
                    'Punt
                    playCall = 19
                End If

            End If

            '----- DECISION MADE -----
            8880 '
            '2nd Qtr, 4th Down
            If quarter = 2 And currDown = 4 Then

                'Outside FG range
                If ydLine < fgAttLine Then
                    'Punt
                    playCall = 19
                Else
                    'In FG range, Modern Era (FGs uncommon before 1960)
                    If (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                        'Field Goal
                        playCall = 20
                    End If
                End If

                GoTo 8800

            End If

            'Any Qtr
            '---------------
            'Less than 22 secs to go, No TOs (Off)
            If gameClock! <= 22 And timeouts(P1) = 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            'Less than 12 secs to go, TOs Remaining (Off)
            If gameClock! <= 12 And timeouts(P1) > 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            GoTo 8892

            '-----

            8885 '
            'Outside FG range, 4th Down, 2nd Qtr --OR-- 4th Qtr + Score Tied
            If ydLine < fgAttLine And ((quarter = 2 And currDown = 4) Or (quarter >= 4 And score(P1, 0) = score(D, 0) And currDown = 4)) Then
                'Punt
                playCall = 19
                GoTo 8800
            End If

            '2nd Qtr Only
            '4th Down, In FG range, Modern Era (FGs uncommon before 1960)
            If ydLine >= fgAttLine And quarter = 2 And currDown = 4 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                'Field GOal
                playCall = 20
                GoTo 8800
            End If

            'Any Qtr
            '---------------
            'Outside FG range, less than 37 secs to go, No TOs (Off)
            If ydLine < fgAttLine And gameClock! <= 37 And timeouts(P1) = 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            'Outside FG range, less than 22 secs to go, No TOs (Off)
            If ydLine < fgAttLine And gameClock! <= 12 And timeouts(P1) > 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            'In FG range, less than 22 secs to go, No TOs (Off)
            If ydLine < fgAttLine And gameClock! <= 22 And timeouts(P1) = 0 Then
                'Field Goal
                playCall = 20
                GoTo 8800
            End If

            'In FG range, less than 12 secs to go, TOs Remaining (Off)
            If ydLine < fgAttLine And gameClock! <= 12 And timeouts(P1) > 0 Then
                'Field Goal
                playCall = 20
                GoTo 8800
            End If

            '4th Down, In FG range, Modern Era (FGs uncommon before 1960)
            If currDown = 4 And ydLine >= fgAttLine And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                'Field Goal
                playCall = 20
                GoTo 8800
            End If

            8892 '
            'Sideline pass
            playCall = 16

            '50% random chance of Short/Med/Long Pass
            If d100% < 50 Then
                playCall = Int(Rnd(1) * 3) + 10
            End If

            'Inside Opponent's 10 = Medium Pass
            If ydLine >= 90 Then
                playCall = 11
            End If

            'In the Red Zone, Down by < 3  (Off) --OR-- Inside Opponent's 10, Down by 3
            'Random choice of Blast / Dive / Off Tackle / Sweep
            If ydLine > 80 And score(D, 0) - score(P1, 0) < 3 Or ydLine > 90 And score(D, 0) - score(P1, 0) = 3 Then
                playCall = Int(Rnd(1) * 4) + 1
            End If

        End If

        'Final Checks
        '-----
        8800 '
        'Overtime and 4th Down - Change to Long Pass
        If quarter > 4 And NG%(14) = 3 And currDown = 4 Then

            'Punt chosen but score tied
            If playCall = 19 And score(D, 0) = score(P1, 0) Then
                playCall = 12
            End If

            'Punt chosen, Ahead by 3 or less (Off)
            '(ie, if we punt and give the team the ball,
            'they could win by a FG; let's go for it
            If playCall = 19 And score(D, 0) - score(P1, 0) <= 3 Then
                playCall = 12
            End If

            'Punt or FG chosen, Down by 4 or more (Off)
            '(ie, we have to try and get a 1st down)
            If (playCall = 19 Or playCall = 20) And score(D, 0) - score(P1, 0) > 3 Then
                playCall = 12
            End If

        End If

    End If

End Sub
