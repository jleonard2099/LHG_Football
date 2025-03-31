'----------------------------------------
'            TRADE Subroutine
'----------------------------------------
'This subroutine relies on several other
'routines to proceed through the general
'steps for trading players.
'First, 2 teams are chosen and their Stats
'are loaded.
'Then, the player choices are made and the
'corresponding team files are updated.
Sub TRADE ()

    _Title "3-in-1 Football - Trade Players"

    Dim yearNumbers$(1)

    Shared teamIndexes()
        
    Shared tmInfo_TRADE$()

    tradeIdx = 0
    Call ChooseTradeTeam (yearNumbers$(tradeIdx), numberTeams, teamNbr, tmInfo_TRADE$(tradeIdx), tradeIdx)    

    If teamNbr> 0 Then

        teamIndexes(tradeIdx) = teamNbr
        
        Call LoadStatsForTrade(tradeIdx)

        tradeIdx = 1
        Call ChooseTradeTeam (yearNumbers$(tradeIdx), numberTeams, teamNbr, tmInfo_TRADE$(tradeIdx), tradeIdx)
        teamIndexes(tradeIdx) = teamNbr
        Call LoadStatsForTrade(tradeIdx)

        'Teams / Stats are loaded
        Call ContinueTrade

    End If

End Sub


'----------------------------------------
'      LoadStatsForTrade Subroutine
'----------------------------------------
'This subroutine prompts the user in reg-
'ards to the trading of stats associated
'with the players. If chosen to trade them,
'the stats are loaded from the team stats.
Sub LoadStatsForTrade (tradeIdx)

    Shared intNam_TRADE$(), sackNam_TRADE$()
    Shared puntNam_TRADE$(), statNam_TRADE$()

    Shared tradeFG$(), tradeKR$()
    Shared recLongRecTxt$(), recLongFGTxt$(), recLongIntTxt$(), recLongKRTxt$(), recLongPassTxt$(), recLongRushTxt$()
    Shared recLongPRTxt$(), tradePR$()
    Shared tradeQB$(), tradeRB$(), tradeWR$()

    Shared tradeFGA!(), tradeFGM()
    Shared tradeLRec!(), tradeLcompFGPct!(), tradeLInt!(), tradeLKR!(), tradeLPass!(), tradeLRush!()
    Shared tradeLPR!()
    Shared tradeTeamStat!(), tradeOppStat!(), tradePlayerStat!()

    Shared tradeGI(), tradeGS()

    Shared haveStats()

    Color 15, 0
    Call TITLE

    haveStats(tradeIdx) = 0

    Call box0 (6, 8, 8, 70, 2, 15, 1)

    Do
        Locate 3
        U$ = "DOES THIS TEAM HAVE SAVED STATS? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        Color 15, 0
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then

        haveStats(tradeIdx) = 1

    Else

        statFile$ = nameStatFile$(tmInfo_TRADE$(tradeIdx))
        
        If _FileExists(diskPaths$(1) + statFile$) Then

            Open diskPaths$(1) + statFile$ For Input As #1

            Input #1, statNam_TRADE$(tradeIdx)

            For I = 0 To 9: 
                Input #1, tradeRB$(tradeIdx, I)
            Next
            
            For I = 0 To 5: 
                Input #1, tradeWR$(tradeIdx, I)
            Next
            
            For I = 0 To 3: 
                Input #1, tradeQB$(tradeIdx, I)
            Next
            
            For I = 0 To 2: 
                Input #1, tradeKR$(tradeIdx, I)
            Next
            
            For I = 0 To 2: 
                Input #1, tradePR$(tradeIdx, I)
            Next

            Input #1, puntNam_TRADE$(tradeIdx)

            For I = 0 To 1: 
                Input #1, tradeFG$(tradeIdx, I)
            Next

            For X = 0 To 13
                For Y = 0 To 17:
                    Input #1, tradePlayerStat!(tradeIdx, X, Y)
                Next
            Next

            For X = 0 To 38: 
                Input #1, tradeTeamStat!(tradeIdx, X)
            Next
            
            For X = 0 To 38: 
                Input #1, tradeOppStat!(tradeIdx, X)
            Next
            
            For X = 0 To 9: 
                Input #1, intNam_TRADE$(tradeIdx, X)
                For J1 = 0 To 2: 
                    Input #1, tradeGI(tradeIdx, X, J1)
                Next
            Next
            
            For X = 0 To 14: 
                Input #1, sackNam_TRADE$(tradeIdx, X)
                For J1 = 0 To 1: 
                    Input #1, tradeGS(tradeIdx, X, J1)
                Next
            Next
            
            For I = 0 To 9: 
                Input #1, tradeLRush!(tradeIdx, I)
                For J1 = 0 To 1: 
                    Input #1, recLongRushTxt$(tradeIdx, I, J1)
                Next
            Next
            
            For I = 0 To 15: 
                Input #1, tradeLRec!(tradeIdx, I)
                For J1 = 0 To 1: 
                    Input #1, recLongRecTxt$(tradeIdx, I, J1)
                Next
            Next
            
            For I = 0 To 3: 
                Input #1, tradeLPass!(tradeIdx, I)
                For J1 = 0 To 1: 
                    Input #1, recLongPassTxt$(tradeIdx, I, J1)
                Next
            Next
            
            For I = 0 To 2: 
                Input #1, tradeLKR!(tradeIdx, I)
                For J1 = 0 To 1: 
                    Input #1, recLongKRTxt$(tradeIdx, I, J1)
                Next
            Next
            
            For I = 0 To 2: 
                Input #1, tradeLPR!(tradeIdx, I)
                For J1 = 0 To 1: 
                    Input #1, recLongPRTxt$(tradeIdx, I, J1)
                Next
            Next
            
            For I = 0 To 1: 
                Input #1, tradeLcompFGPct!(tradeIdx, I)
                Input #1, recLongFGTxt$(tradeIdx, I)
                For I1 = 0 To 4: 
                    Input #1, tradeFGM(tradeIdx, I, I1)
                    Input #1, tradeFGA!(tradeIdx, I, I1)
                Next
            Next
            
            For I = 0 To 9: 
                Input #1, tradeLInt!(tradeIdx, I)
                Input #1, recLongIntTxt$(tradeIdx, I, 0)
                Input #1, recLongIntTxt$(tradeIdx, I, 1)
            Next

            Close #1

        Else

            haveStats(tradeIdx) = 1
            MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        End If

    End If

End Sub


'----------------------------------------
'       ContinueTrade Subroutine
'----------------------------------------
'This subroutine actually presents the user
'a menu for choosing players to trade. It
'relies on separate routines to summarize
'the trade and then save the changes.
Sub ContinueTrade ()

    Do

        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Locate 5
        Locate , 5: Color 3: Print "TRADE ";
        Color 15: Print "R";: Color 3: Print "UNNING BACKS"
        Locate , 5: Color 3: Print "TRADE ";
        Color 15: Print "W";: Color 3: Print "IDE RECEIVERS"
        Locate , 5: Color 3: Print "TRADE ";
        Color 15: Print "Q";: Color 3: Print "UARTERBACKS"
        Locate , 5: Color 3: Print "TRADE ";
        Color 15: Print "K";: Color 3: Print "ICKOFF RETURN"
        Locate , 5: Color 3: Print "TRADE ";
        Color 15: Print "P";: Color 3: Print "UNT RETURN"
        Locate , 5: Color 3: Print "TRADE P";
        Color 15: Print "U";: Color 3: Print "NTERS"
        Locate , 5: Color 3: Print "TRADE PLACE K";
        Color 15: Print "I";: Color 3: Print "CKERS"
        Locate , 5
        Color 15: Print "E";: Color 3: Print "ND AND SAVE TRADES"

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "I":
                Call PromptPlayerStats(W1)
                Call TradeKickers(W1)

            Case "K":
                Call PromptPlayerStats(W1)
                Call TradeKRs(W1)

            Case "P":
                Call PromptPlayerStats(W1)
                Call TradePRs(W1)

            Case "Q":
                Call PromptPlayerStats(W1)
                Call TradeQBs(W1)

            Case "R":
                Call PromptPlayerStats(W1)
                Call TradeRBs(W1)

            Case "U":
                Call PromptPlayerStats(W1)
                Call TradePunters(W1)

            Case "W":
                Call PromptPlayerStats(W1)
                Call TradeWRs(W1)

        End Select

    Loop Until UCase$(I$) = "E"

    For J = 0 To 1

        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next
        Locate 3

        Call DoTheTrade(J)

    Next J

    Call SaveTrades

End Sub


'----------------------------------------
'           TradeRBs Subroutine
'----------------------------------------
'This subroutine presents a summary of
'team RBs, lets the user choose them for
'trade, and then swaps the players.
Sub TradeRBs (W1)

    Shared haveStats()

    Shared rbAtt_TRADE(), rbAvg_TRADE(), rbRec_TRADE(), rbRecAvg_TRADE()
    Shared teamRat_TRADE()

    Shared tradeLRec!(), tradeLRush!(), tradePlayerStat!()

    Shared rbNam_TRADE$(), tmInfo_TRADE$()
    Shared recLongRecTxt$(), recLongRushTxt$(), tradeRB$()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0)
    Color 15, 0

    Locate 4, 20
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1)
    Color 15, 0

    For I = 0 To 9:
        Locate I + 5, 1
        Print I; " "; rbNam_TRADE$(0, I)
        Locate I + 5, 20
        Print I; " "; rbNam_TRADE$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0);
            Color 15, 0: Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 9

        Print rbNam_TRADE$(0, I)

        Do
            Locate 19, 1
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1);
            Color 15, 0: Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I >= 0 And I <= 9

        Print rbNam_TRADE$(1, I1)

        Do
            Locate LAST_ROW - 2, 1: Print "ANY CHANGE? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print rbNam_TRADE$(0, I);
    Color 15, 0

    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print rbNam_TRADE$(1, I1)
    Color 15, 0

    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       TC    ###                                 TC    ###"; tradePlayerStat!(0, I, 0); tradePlayerStat!(1, I1, 0)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 1); tradePlayerStat!(1, I1, 1)
    Locate , 5: Print Using " ##.# AVG   "; rbAvg_TRADE(0, I) * .3;

    If tradePlayerStat!(0, I, 0) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 1) / tradePlayerStat!(0, I, 0); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; rbAvg_TRADE(1, I1) * .3;

    If tradePlayerStat!(1, I1, 0) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 1) / tradePlayerStat!(1, I1, 0) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 2); tradePlayerStat!(1, I1, 2)
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradePlayerStat!(0, I, 3); tradePlayerStat!(1, I1, 3)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 4); tradePlayerStat!(1, I1, 4)
    Locate , 5: Print Using " ##.# AVG   "; rbRecAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 3) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 4) / tradePlayerStat!(0, I, 3); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; rbRecAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 3) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 4) / tradePlayerStat!(1, I1, 3) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 5); tradePlayerStat!(1, I1, 5)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    Swap rbNam_TRADE$(0, I), rbNam_TRADE$(1, I1)
    Swap rbAtt_TRADE(0, I), rbAtt_TRADE(1, I1)
    Swap tradeLRush!(0, I), tradeLRush!(1, I1)
    Swap recLongRushTxt$(0, I, 0), recLongRushTxt$(1, I1, 0)
    Swap recLongRushTxt$(0, I, 1), recLongRushTxt$(1, I1, 1)
    Swap tradeLRec!(0, I), tradeLRec!(1, I1)
    Swap recLongRecTxt$(0, I, 0), recLongRecTxt$(1, I1, 0)
    Swap recLongRecTxt$(0, I, 1), recLongRecTxt$(1, I1, 1)
    Swap rbAvg_TRADE(0, I), rbAvg_TRADE(1, I1)
    Swap rbRec_TRADE(0, I), rbRec_TRADE(1, I1)
    Swap rbRecAvg_TRADE(0, I), rbRecAvg_TRADE(1, I1)
    Swap tradeRB$(0, I), tradeRB$(1, I1)

    If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then
        For I2 = 0 To 5: 
            tradePlayerStat!(0, I, I2) = 0
            tradePlayerStat!(1, I1, I2) = 0
        Next I2
    Else
        For I2 = 0 To 5: 
            Swap tradePlayerStat!(0, I, I2), tradePlayerStat!(1, I1, I2)
        Next
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print rbNam_TRADE$(0, I);
    Color 15, 0

    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print rbNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       TC    ###                                 TC    ###"; tradePlayerStat!(0, I, 0); tradePlayerStat!(1, I1, 0)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 1); tradePlayerStat!(1, I1, 1)
    Locate , 5: Print Using " ##.# AVG   "; rbAvg_TRADE(0, I) * .3;

    If tradePlayerStat!(0, I, 0) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 1) / tradePlayerStat!(0, I, 0); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; rbAvg_TRADE(1, I1) * .3;

    If tradePlayerStat!(1, I1, 0) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 1) / tradePlayerStat!(1, I1, 0) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 2); tradePlayerStat!(1, I1, 2)
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradePlayerStat!(0, I, 3); tradePlayerStat!(1, I1, 3)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 4); tradePlayerStat!(1, I1, 4)
    Locate , 5: Print Using " ##.# AVG   "; rbRecAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 3) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 4) / tradePlayerStat!(0, I, 3); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; rbRecAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 3) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 4) / tradePlayerStat!(1, I1, 3) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 5); tradePlayerStat!(1, I1, 5)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'           TradeWRs Subroutine
'----------------------------------------
'This subroutine presents a summary of
'team WRs, lets the user choose them for
'trade, and then swaps the players.
Sub TradeWRs (W1)

    Shared haveStats()
    Shared teamRat_TRADE()
    Shared wrRec_TRADE(), wrAvg_TRADE()

    Shared tradeLRec!(), tradePlayerStat!()

    Shared tmInfo_TRADE$(), wrNam_TRADE$()
    Shared recLongRecTxt$(), tradeWR$()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0)
    Color 15, 0

    Locate 4, 20
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1)
    Color 15, 0

    For I = 0 To 5:
        Locate I + 5, 1: Print I; " "; wrNam_TRADE$(0, I)
        Locate I + 5, 20: Print I; " "; wrNam_TRADE$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 5

        Print wrNam_TRADE$(0, I)

        Do
            Locate 19, 1
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 5

        Print wrNam_TRADE$(1, I1)

        Do
            Locate LAST_ROW - 2, 1: Print "ANY CHANGE? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print wrNam_TRADE$(0, I);
    Color 15, 0

    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print wrNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradePlayerStat!(0, I, 6); tradePlayerStat!(1, I1, 6)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 7); tradePlayerStat!(1, I1, 7)
    Locate , 5: Print Using " ##.# AVG   "; wrAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 6) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 7) / tradePlayerStat!(0, I, 6); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; wrAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 6) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 7) / tradePlayerStat!(1, I1, 6) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 8); tradePlayerStat!(1, I1, 8)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    Swap wrNam_TRADE$(0, I), wrNam_TRADE$(1, I1)
    Swap tradeLRec!(0, I + 10), tradeLRec!(1, I1 + 10)
    Swap recLongRecTxt$(0, I + 10, 0), recLongRecTxt$(1, I1 + 10, 0)
    Swap recLongRecTxt$(0, I + 10, 1), recLongRecTxt$(1, I1 + 10, 1)
    Swap wrRec_TRADE(0, I), wrRec_TRADE(1, I1)
    Swap wrAvg_TRADE(0, I), wrAvg_TRADE(1, I1)
    Swap tradeWR$(0, I), tradeWR$(1, I1)

    If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then
        For I2 = 6 To 8: 
            tradePlayerStat!(0, I, I2) = 0
            tradePlayerStat!(1, I1, I2) = 0
        Next I2
    Else
        For I2 = 6 To 8: 
            Swap tradePlayerStat!(0, I, I2), tradePlayerStat!(1, I1, I2)
        Next
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print wrNam_TRADE$(0, I);
    Color 15, 0

    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print wrNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradePlayerStat!(0, I, 6); tradePlayerStat!(1, I1, 6)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 7); tradePlayerStat!(1, I1, 7)
    Locate , 5: Print Using " ##.# AVG   "; wrAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 6) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 7) / tradePlayerStat!(0, I, 6); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; wrAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 6) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 7) / tradePlayerStat!(1, I1, 6) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 8); tradePlayerStat!(1, I1, 8)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'           TradeQBs Subroutine
'----------------------------------------
'This subroutine presents a summary of
'team QBs, lets the user choose them for
'trade, and then swaps the players.
Sub TradeQBs (W1)

    Shared haveStats()
    Shared qbPA_TRADE(), qbPCPct_TRADE(), qbIntPct_TRADE()
    Shared teamRat_TRADE(), qbArmRat_TRADE()

    Shared tradeLPass!(), tradePlayerStat!()

    Shared qbNam_TRADE$(), tmInfo_TRADE$()
    Shared recLongPassTxt$(), tradeQB$()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0)
    Color 15, 0

    Locate 4, 20
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1)

    Color 15, 0

    For I = 0 To 3:
        Locate I + 5, 1
        Print I; " "; qbNam_TRADE$(0, I)
        Locate I + 5, 20
        Print I; " "; qbNam_TRADE$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 3

        Print qbNam_TRADE$(0, I)

        Do
            Locate 19, 1
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 3

        Print qbNam_TRADE$(1, I1)

        Do
            Locate LAST_ROW - 2, 1: Print "ANY CHANGE? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7: Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print qbNam_TRADE$(0, I);
    Color 15, 0
    Locate , 49: Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print qbNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       PC    ###                                 PC    ###"; tradePlayerStat!(0, I, 10); tradePlayerStat!(1, I1, 10)
    Locate , 5: Print Using "       PA   ####                                 PA   ####"; tradePlayerStat!(0, I, 9); tradePlayerStat!(1, I1, 9)
    Locate , 5: Print Using "       PI     ##                                 PI     ##"; tradePlayerStat!(0, I, 11); tradePlayerStat!(1, I1, 11)
    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 13); tradePlayerStat!(1, I1, 13)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 12); tradePlayerStat!(1, I1, 12)
    Locate , 5: Print Using " ##.# CMP%  "; qbPCPct_TRADE(0, I);

    If tradePlayerStat!(0, I, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 10) / tradePlayerStat!(0, I, 9) * 100; 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# CMP%  "; qbPCPct_TRADE(1, I1);

    If tradePlayerStat!(1, I1, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 10) / tradePlayerStat!(1, I1, 9) * 100 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using " ##.# INT%  "; qbIntPct_TRADE(0, I);

    If tradePlayerStat!(0, I, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 11) / tradePlayerStat!(0, I, 9) * 100; 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# INT%  "; qbIntPct_TRADE(1, I1);

    If tradePlayerStat!(1, I1, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 11) / tradePlayerStat!(1, I1, 9) * 100 
    Else 
        Print " -.-"
    End If

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    Swap qbNam_TRADE$(0, I), qbNam_TRADE$(1, I1)
    Swap tradeLPass!(0, I), tradeLPass!(1, I1)
    Swap recLongPassTxt$(0, I, 0), recLongPassTxt$(1, I1, 0)
    Swap recLongPassTxt$(0, I, 1), recLongPassTxt$(1, I1, 1)
    Swap qbPA_TRADE(0, I), qbPA_TRADE(1, I1)
    Swap qbPCPct_TRADE(0, I), qbPCPct_TRADE(1, I1)
    Swap qbIntPct_TRADE(0, I), qbIntPct_TRADE(1, I1)
    Swap qbArmRat_TRADE(0, I), qbArmRat_TRADE(1, I1)
    Swap tradeQB$(0, I), tradeQB$(1, I1)

    If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then
        For I2 = 9 To 13: 
            tradePlayerStat!(0, I, I2) = 0
            tradePlayerStat!(1, I1, I2) = 0
        Next I2
        For I2 = 0 To 2: 
            tradePlayerStat!(0, I + 10, I2) = 0
            tradePlayerStat!(1, I1 + 10, I2) = 0
        Next
    Else
        For I2 = 9 To 13: 
            Swap tradePlayerStat!(0, I, I2), tradePlayerStat!(1, I1, I2)
        Next I2
        For I2 = 0 To 2: 
            Swap tradePlayerStat!(0, I + 10, I2), tradePlayerStat!(1, I1 + 10, I2)
        Next I2
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print qbNam_TRADE$(0, I);
    Color 15, 0
    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print qbNam_TRADE$(1, I1)
    Color 15, 0
    Print
    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       PC    ###                                 PC    ###"; tradePlayerStat!(0, I, 10); tradePlayerStat!(1, I1, 10)
    Locate , 5: Print Using "       PA   ####                                 PA   ####"; tradePlayerStat!(0, I, 9); tradePlayerStat!(1, I1, 9)
    Locate , 5: Print Using "       PI     ##                                 PI     ##"; tradePlayerStat!(0, I, 11); tradePlayerStat!(1, I1, 11)
    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 13); tradePlayerStat!(1, I1, 13)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 12); tradePlayerStat!(1, I1, 12)
    Locate , 5: Print Using " ##.# CMP%  "; qbPCPct_TRADE(0, I);

    If tradePlayerStat!(0, I, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 10) / tradePlayerStat!(0, I, 9) * 100; 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# CMP%  "; qbPCPct_TRADE(1, I1);

    If tradePlayerStat!(1, I1, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 10) / tradePlayerStat!(1, I1, 9) * 100 
    Else 
        Print " -.-"
    End If

    Locate , 5
    Print Using " ##.# INT%  "; qbIntPct_TRADE(0, I);

    If tradePlayerStat!(0, I, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 11) / tradePlayerStat!(0, I, 9) * 100; 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# INT%  "; qbIntPct_TRADE(1, I1);

    If tradePlayerStat!(1, I1, 9) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 11) / tradePlayerStat!(1, I1, 9) * 100 
    Else 
        Print " -.-"
    End If

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub

'----------------------------------------
'           TradeKRs Subroutine
'----------------------------------------
'This subroutine presents a summary of
'team KRs, lets the user choose them for
'trade, and then swaps the players.
Sub TradeKRs (W1)

    Shared haveStats()

    Shared krNumRet_TRADE(), krRetAvg_TRADE(), teamRat_TRADE()

    Shared tradeLKR!(), tradePlayerStat!()

    Shared krNam_TRADE$(), tmInfo_TRADE$()
    Shared tradeKR$(), recLongKRTxt$()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0)
    Color 15, 0

    Locate 4, 20
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1)
    Color 15, 0

    For I = 0 To 2:
        Locate I + 5, 1
        Print I; " "; krNam_TRADE$(0, I)
        Locate I + 5, 20
        Print I; " "; krNam_TRADE$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 2

        Print krNam_TRADE$(0, I)

        Do
            Locate 19, 1
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 2

        Print krNam_TRADE$(1, I1)

        Do
            Locate LAST_ROW - 2, 1: Print "ANY CHANGE? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print krNam_TRADE$(0, I);
    Color 15, 0
    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print krNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradePlayerStat!(0, I, 14); tradePlayerStat!(1, I1, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 15); tradePlayerStat!(1, I1, 15)
    Locate , 5: Print Using " ##.# AVG   "; krRetAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 14) > 0 Then 
        Print Using "##.#"; tradePlayerStat!(0, I, 15) / tradePlayerStat!(0, I, 14); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; krRetAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 14) > 0 Then 
        Print Using "##.#"; tradePlayerStat!(1, I1, 15) / tradePlayerStat!(1, I1, 14) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 16); tradePlayerStat!(1, I1, 16)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    Swap krNam_TRADE$(0, I), krNam_TRADE$(1, I1)
    Swap krNumRet_TRADE(0, I), krNumRet_TRADE(1, I1)
    Swap tradeLKR!(0, I), tradeLKR!(1, I1)
    Swap recLongKRTxt$(0, I, 0), recLongKRTxt$(1, I1, 0)
    Swap recLongKRTxt$(0, I, 1), recLongKRTxt$(1, I1, 1)
    Swap krRetAvg_TRADE(0, I), krRetAvg_TRADE(1, I1)
    Swap tradeKR$(0, I), tradeKR$(1, I1)

    If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then
        For I2 = 14 To 16: 
            tradePlayerStat!(0, I, I2) = 0
            tradePlayerStat!(1, I1, I2) = 0
        Next I2
    Else
        For I2 = 14 To 16: 
            Swap tradePlayerStat!(0, I, I2), tradePlayerStat!(1, I1, I2)
        Next I2
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print krNam_TRADE$(0, I);
    Color 15, 0
    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print krNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradePlayerStat!(0, I, 14); tradePlayerStat!(1, I1, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I, 15); tradePlayerStat!(1, I1, 15)
    Locate , 5: Print Using " ##.# AVG   "; krRetAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 14) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I, 15) / tradePlayerStat!(0, I, 14); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; krRetAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 14) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1, 15) / tradePlayerStat!(1, I1, 14) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I, 16); tradePlayerStat!(1, I1, 16)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'           TradePRs Subroutine
'----------------------------------------
'This subroutine presents a summary of
'team PRs, lets the user choose them for
'trade, and then swaps the players.
Sub TradePRs (W1)

    Shared haveStats()

    Shared prRet_TRADE(), prRetAvg_TRADE()
    Shared teamRat_TRADE()

    Shared tradeLPR!(), tradePlayerStat!()

    Shared prNam_TRADE$(), tmInfo_TRADE$()
    Shared recLongPRTxt$(), tradePR$()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0)
    Color 15, 0

    Locate 4, 20
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1)
    Color 15, 0

    For I = 0 To 2:
        Locate I + 5, 1
        Print I; " "; prNam_TRADE$(0, I)
        Locate I + 5, 20
        Print I; " "; prNam_TRADE$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 2

        Print prNam_TRADE$(0, I)

        Do
            Locate 19, 1
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 2

        Print prNam_TRADE$(1, I1)

        Do
            Locate LAST_ROW - 2, 1: Print "ANY CHANGE? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print prNam_TRADE$(0, I);
    Color 15, 0
    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print prNam_TRADE$(1, I1)
    Color 15, 0
    Print
    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradePlayerStat!(0, I + 3, 14); tradePlayerStat!(1, I1 + 3, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I + 3, 15); tradePlayerStat!(1, I1 + 3, 15)
    Locate , 5: Print Using " ##.# AVG   "; prRetAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 14) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I + 3, 15) / tradePlayerStat!(0, I + 3, 14); 
    Else 
        Print " -.-";
    End If

    Print Using "                           ##.# AVG   "; prRetAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 14) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1 + 3, 15) / tradePlayerStat!(1, I1 + 3, 14) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I + 3, 16); tradePlayerStat!(1, I1 + 3, 16)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    Swap prNam_TRADE$(0, I), prNam_TRADE$(1, I1)
    Swap tradeLPR!(0, I), tradeLPR!(1, I1)
    Swap recLongPRTxt$(0, I, 0), recLongPRTxt$(1, I1, 0)
    Swap recLongPRTxt$(0, I, 1), recLongPRTxt$(1, I1, 1)
    Swap prRet_TRADE(0, I), prRet_TRADE(1, I1)
    Swap prRetAvg_TRADE(0, I), prRetAvg_TRADE(1, I1)
    Swap tradePR$(0, I), tradePR$(1, I1)

    If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then
        For I2 = 14 To 16: 
            tradePlayerStat!(0, I + 3, I2) = 0
            tradePlayerStat!(1, I1 + 3, I2) = 0
        Next I2
    Else
        For I2 = 14 To 16: 
            Swap tradePlayerStat!(0, I + 3, I2), tradePlayerStat!(1, I1 + 3, I2)
        Next I2
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print prNam_TRADE$(0, I);
    Color 15, 0
    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print prNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradePlayerStat!(0, I + 3, 14); tradePlayerStat!(1, I1 + 3, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradePlayerStat!(0, I + 3, 15); tradePlayerStat!(1, I1 + 3, 15)
    Locate , 5: Print Using " ##.# AVG   "; prRetAvg_TRADE(0, I) * 3;

    If tradePlayerStat!(0, I, 14) > 0 Then
		Print Using "##.#"; tradePlayerStat!(0, I + 3, 15) / tradePlayerStat!(0, I + 3, 14); 
    Else 
        Print " -.-";
    End If
    Print Using "                           ##.# AVG   "; prRetAvg_TRADE(1, I1) * 3;

    If tradePlayerStat!(1, I1, 14) > 0 Then
		Print Using "##.#"; tradePlayerStat!(1, I1 + 3, 15) / tradePlayerStat!(1, I1 + 3, 14) 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradePlayerStat!(0, I + 3, 16); tradePlayerStat!(1, I1 + 3, 16)

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       TradePunters Subroutine
'----------------------------------------
'This subroutine presents a summary of
'team punters, lets the user choose them 
'for trade, and then swaps the players.
Sub TradePunters (W1)

    Shared puntAvg_TRADE(), teamRat_TRADE()
    Shared haveStats()

    Shared tradeTeamStat!()

    Shared puntNam_TRADE$(), tmInfo_TRADE$()
    Shared pntNam_TRADE$()

    Do
        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Locate 4, 1
        Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0)
        Color 15, 0

        Locate 4, 20
        Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1)
        Color 15, 0

        Print
        Print pntNam_TRADE$(0, 0);
        Locate , 20
        Print " "; pntNam_TRADE$(1, 0)

        Do
            Locate 22, 1: Print "ANY CHANGE? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        If UCase$(I$) = "N" Then

            Call TITLE

            For XY = 2 To 23:
                Locate XY, 1: Print Tab(79);
            Next

            Locate 3
            Locate , 7
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print pntNam_TRADE$(0, 0);
            Color 15, 0

            Locate , 49
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print pntNam_TRADE$(1, 0)
            Color 15, 0

            Print
            Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
            Print
            Locate , 5: Print Using "      PNT     ##                                RET     ##"; tradeTeamStat!(0, 7); tradeTeamStat!(1, 7)
            Locate , 5: Print Using " ##.# AVG   "; puntAvg_TRADE(0, 0) * 2 + 28;

            If tradeTeamStat!(0, 7) > 0 Then 
                Print Using "##.#"; tradeTeamStat!(0, 8) / tradeTeamStat!(0, 7); 
            Else 
                Print " -.-";
            End If

            Print Using "                           ##.# AVG   "; puntAvg_TRADE(1, 0) * 2 + 28;

            If tradeTeamStat!(1, 7) > 0 Then 
                Print Using "##.#"; tradeTeamStat!(1, 8) / tradeTeamStat!(1, 7) 
            Else 
                Print " -.-"
            End If

            Color 15
            Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
            J$ = GetKeyPress$

            Swap pntNam_TRADE$(0, 0), pntNam_TRADE$(1, 0)
            Swap puntAvg_TRADE(0, 0), puntAvg_TRADE(1, 0)
            Swap puntNam_TRADE$(0), puntNam_TRADE$(1)

            If haveStats(0) <> 1 And haveStats(1) <> 1 And W1 <> 0 Then
                Swap tradeTeamStat!(0, 7), tradeTeamStat!(1, 7)
                Swap tradeTeamStat!(0, 8), tradeTeamStat!(1, 8)
            End If

            Call TITLE

            For XY = 2 To 23:
                Locate XY, 1: Print Tab(79);
            Next

            Locate 3
            Locate , 7
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print pntNam_TRADE$(0, 0);
            Color 15, 0

            Locate , 49
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print pntNam_TRADE$(1, 0)
            Color 15, 0
            Print

            Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
            Print
            Locate , 5: Print Using "      PNT     ##                                RET     ##"; tradeTeamStat!(0, 7); tradeTeamStat!(1, 7)
            Locate , 5: Print Using " ##.# AVG   "; puntAvg_TRADE(0, 0) * 2 + 28;

            If tradeTeamStat!(0, 7) > 0 Then
                Print Using "##.#"; tradeTeamStat!(0, 8) / tradeTeamStat!(0, 7); 
            Else 
                Print " -.-";
            End If

            Print Using "                           ##.# AVG   "; puntAvg_TRADE(1, 0) * 2 + 28;

            If tradeTeamStat!(1, 7) > 0 Then
                Print Using "##.#"; tradeTeamStat!(1, 8) / tradeTeamStat!(1, 7) 
            Else 
                Print " -.-"
            End If

        End If

    Loop Until UCase$(I$) = "N"

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
    J$ = GetKeyPress$

End Sub

'----------------------------------------
'       TradeKickers Subroutine
'----------------------------------------
'This subroutine presents a summary of
'team kickers, lets the user choose them 
'for trade, and then swaps the players.
Sub TradeKickers (W1)

    Shared haveStats()
    Shared FGA_TRADE(), FGPct_TRADE(), PAT_TRADE(), PATPct_TRADE()
    Shared teamRat_TRADE()

    Shared tradeFGA!(), tradeFGM()
    Shared tradeLcompFGPct!(), tradePlayerStat!()

    Shared pkNam_TRADE$(), tmInfo_TRADE$()
    Shared tradeFG$()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0)
    Color 15, 0

    Locate 4, 20
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1)
    Color 15, 0

    For I = 0 To 1:
        Locate I + 5, 1
        Print I; " "; pkNam_TRADE$(0, I)
        Locate I + 5, 20
        Print I; " "; pkNam_TRADE$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print tmInfo_TRADE$(0);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 1

        Print pkNam_TRADE$(0, I)

        Do
            Locate 19, 1
            Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print tmInfo_TRADE$(1);
            Color 15, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I >= 0 And I <= 1

        Print pkNam_TRADE$(1, I1)

        Do
            Locate 22, 1: Print "ANY CHANGE? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print pkNam_TRADE$(0, I);
    Color 15, 0

    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print pkNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      FGA  ##                                   FGA  ##"; tradePlayerStat!(0, 4 * I + 4, 17); tradePlayerStat!(1, 4 * I1 + 4, 17)
    Locate , 5: Print Using "      FGM  ##                                   FGM  ##"; tradePlayerStat!(0, 4 * I + 3, 17); tradePlayerStat!(1, 4 * I1 + 3, 17)
    Locate , 5: Print Using " ###  FG% "; FGA_TRADE(0, I) * 5 + 25;

    If tradePlayerStat!(0, 4 * I + 4, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(0, I * 4 + 3, 17) / tradePlayerStat!(0, I * 4 + 4, 17) * 100; 
    Else 
        Print " -.-";
    End If

    Print Using "                             ###  FG% "; FGA_TRADE(1, I1) * 5 + 25;

    If tradePlayerStat!(1, 4 * I1 + 4, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(1, I1 * 4 + 3, 17) / tradePlayerStat!(1, I1 * 4 + 4, 17) * 100 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      EPA  ##                                   EPA  ##"; tradePlayerStat!(0, 4 * I + 6, 17); tradePlayerStat!(1, 4 * I1 + 6, 17)
    Locate , 5: Print Using "      EPM  ##                                   EPM  ##"; tradePlayerStat!(0, 4 * I + 5, 17); tradePlayerStat!(1, 4 * I1 + 5, 17)
    Locate , 5: Print Using " ###  EP% "; PAT_TRADE(0, I);

    If tradePlayerStat!(0, 4 * I + 6, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(0, 4 * I + 5, 17) / tradePlayerStat!(0, 4 * I + 6, 17) * 100; 
    Else 
        Print " -.-";
    End If

    Print Using "                             ###  EP% "; PAT_TRADE(1, I1);

    If tradePlayerStat!(1, 4 * I1 + 6, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(1, 4 * I1 + 5, 17) / tradePlayerStat!(1, 4 * I1 + 6, 17) * 100 
    Else 
        Print " -.-"
    End If

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
    I$ = GetKeyPress$

    Swap pkNam_TRADE$(0, I), pkNam_TRADE$(1, I1)
    Swap FGA_TRADE(0, I), FGA_TRADE(1, I1)
    Swap tradeLcompFGPct!(0, I), tradeLcompFGPct!(1, I1)

    For I2 = 0 To 4
        Swap tradeFGM(0, I, I2), tradeFGM(1, I1, I2)
        Swap tradeFGA!(0, I, I2), tradeFGA!(1, I1, I2)
    Next I2

    Swap FGPct_TRADE(0, I), FGPct_TRADE(1, I1)
    Swap PAT_TRADE(0, I), PAT_TRADE(1, I1)
    Swap PATPct_TRADE(0, I), PATPct_TRADE(1, I1)
    Swap tradeFG$(J, I), tradeFG$(J, I1)

    If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then
        tradePlayerStat!(0, 4 * I + 3, 17) = 0: tradePlayerStat!(1, 4 * I1 + 3, 17) = 0
        tradePlayerStat!(0, 4 * I + 4, 17) = 0: tradePlayerStat!(1, 4 * I1 + 4, 17) = 0
        tradePlayerStat!(0, 4 * I + 5, 17) = 0: tradePlayerStat!(1, 4 * I1 + 5, 17) = 0
        tradePlayerStat!(0, 4 * I + 6, 17) = 0: tradePlayerStat!(1, 4 * I1 + 6, 17) = 0
    Else
        Swap tradePlayerStat!(0, 4 * I + 3, 17), tradePlayerStat!(1, 4 * I1 + 3, 17)
        Swap tradePlayerStat!(0, 4 * I + 4, 17), tradePlayerStat!(1, 4 * I1 + 4, 17)
        Swap tradePlayerStat!(0, 4 * I + 5, 17), tradePlayerStat!(1, 4 * I1 + 5, 17)
        Swap tradePlayerStat!(0, 4 * I + 6, 17), tradePlayerStat!(1, 4 * I1 + 6, 17)
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color teamRat_TRADE(0, 2), teamRat_TRADE(0, 1): Print pkNam_TRADE$(0, I);
    Color 15, 0

    Locate , 49
    Color teamRat_TRADE(1, 2), teamRat_TRADE(1, 1): Print pkNam_TRADE$(1, I1)
    Color 15, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      FGA  ##                                   FGA  ##"; tradePlayerStat!(0, 4 * I + 4, 17); tradePlayerStat!(1, 4 * I1 + 4, 17)
    Locate , 5: Print Using "      FGM  ##                                   FGM  ##"; tradePlayerStat!(0, 4 * I + 3, 17); tradePlayerStat!(1, 4 * I1 + 3, 17)
    Locate , 5: Print Using " ###  FG% "; FGA_TRADE(0, I) * 5 + 25;

    If tradePlayerStat!(0, 4 * I + 4, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(0, I * 4 + 3, 17) / tradePlayerStat!(0, I * 4 + 4, 17) * 100; 
    Else 
        Print " -.-";
    End If
    Print Using "                             ###  FG% "; FGA_TRADE(1, I1) * 5 + 25;

    If tradePlayerStat!(1, 4 * I1 + 4, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(1, I1 * 4 + 3, 17) / tradePlayerStat!(1, I1 * 4 + 4, 17) * 100 
    Else 
        Print " -.-"
    End If

    Locate , 5: Print Using "      EPA  ##                                   EPA  ##"; tradePlayerStat!(0, 4 * I + 6, 17); tradePlayerStat!(1, 4 * I1 + 6, 17)
    Locate , 5: Print Using "      EPM  ##                                   EPM  ##"; tradePlayerStat!(0, 4 * I + 5, 17); tradePlayerStat!(1, 4 * I1 + 5, 17)
    Locate , 5: Print Using " ###  EP% "; PAT_TRADE(0, I);

    If tradePlayerStat!(0, 4 * I + 6, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(0, 4 * I + 5, 17) / tradePlayerStat!(0, 4 * I + 6, 17) * 100; 
    Else 
        Print " -.-";
    End If

    Print Using "                             ###  EP% "; PAT_TRADE(1, I1);

    If tradePlayerStat!(1, 4 * I1 + 6, 17) > 0 Then
		Print Using "### "; tradePlayerStat!(1, 4 * I1 + 5, 17) / tradePlayerStat!(1, 4 * I1 + 6, 17) * 100 
    Else 
        Print " -.-"
    End If

    Color 15
    Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'     PromptPlayerStats Subroutine
'----------------------------------------
'This subroutine simply prmopts the user
'for a choice of whether or not they
'should trade stats with the players.
Sub PromptPlayerStats (W1)

    Do

        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Locate 5, 5
        Color 15: Print "T";
        Color 3: Print "RADE PLAYERS' REPLAY STATS"
        Locate , 5
        Color 15: Print "Z";
        Color 3: Print "ERO PLAYERS' STATS"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "T" Or UCase$(I$) = "Z"

    If UCase$(I$) = "T" Then W1 = 1
    If UCase$(I$) = "Z" Then W1 = 0

End Sub


'----------------------------------------
'       DoTheTrade Subroutine
'----------------------------------------
'This subroutine checks each position type
'to see if trades were done, then swaps the
'actual team data for the trades before
'relying on a routine to save all of the 
'modified teams
Sub DoTheTrade (tradeIdx)

    Shared intNam_TRADE$(), sackNam_TRADE$()
    Shared krNam_TRADE$(), pkNam_TRADE$(), prNam_TRADE$(), pntNam_TRADE$(), rbNam_TRADE$(), wrNam_TRADE$(), qbNam_TRADE$()
    Shared tmInfo_TRADE$(), nicks_TRADE$()
    Shared tradeYN$()

    Shared teamIndexes(), ydCmpAdj_TRADE!()

    Shared rbAtt_TRADE(), rbAvg_TRADE(), rbRec_TRADE(), rbRecAvg_TRADE(), wrRec_TRADE()
    Shared wrAvg_TRADE(), qbPA_TRADE(), qbPCPct_TRADE(), qbIntPct_TRADE(), krNumRet_TRADE()
    Shared krRetAvg_TRADE(), prRet_TRADE(), prRetAvg_TRADE(), puntAvg_TRADE(), FGA_TRADE()
    Shared FGPct_TRADE(), leagRat_TRADE(), PAT_TRADE(), PATPct_TRADE()
    Shared numInt_TRADE(), numSack_TRADE(), teamRat_TRADE(), qbArmRat_TRADE()
    Shared ydsPerComp_TRADE()

    ydsPerComp_TRADE(tradeIdx) = ydCmpAdj_TRADE!(tradeIdx)
    S0 = 0
    S1 = 0
    S2 = 0
    S3 = 0

    For I = 0 To 9: 
        S0 = S0 + rbAtt_TRADE(tradeIdx, I)
        S2 = S2 + rbRec_TRADE(tradeIdx, I)
    Next

    S4 = 100 / S0

    For I = 0 To 9:
        If rbAtt_TRADE(tradeIdx, I) <> 0 Then
			rbAtt_TRADE(tradeIdx, I) = CInt(S4 * rbAtt_TRADE(tradeIdx, I)) + S1
			S1 = rbAtt_TRADE(tradeIdx, I)
        End If
        If rbAtt_TRADE(tradeIdx, I) > 100 Then
			rbAtt_TRADE(tradeIdx, I) = 100
        End If
    Next I

    For I = 0 To 5: 
        S2 = S2 + wrRec_TRADE(tradeIdx, I)
    Next

    S5 = 100 / S2

    For I = 0 To 9:
        If rbRec_TRADE(tradeIdx, I) <> 0 Then
			rbRec_TRADE(tradeIdx, I) = CInt(S5 * rbRec_TRADE(tradeIdx, I)) + S3
			S3 = rbRec_TRADE(tradeIdx, I)
        End If
        If rbRec_TRADE(tradeIdx, I) > 100 Then
			rbRec_TRADE(tradeIdx, I) = 100
        End If
    Next I

    For I = 0 To 5:
        If wrRec_TRADE(tradeIdx, I) <> 0 Then
			wrRec_TRADE(tradeIdx, I) = CInt(S5 * wrRec_TRADE(tradeIdx, I)) + S3
			S3 = wrRec_TRADE(tradeIdx, I)
        End If
        If wrRec_TRADE(tradeIdx, I) > 100 Then
			wrRec_TRADE(tradeIdx, I) = 100
        End If
    Next I

    S6 = 0
    S7 = 0

    For I = 0 To 3: 
        S6 = S6 + qbPA_TRADE(tradeIdx, I)
    Next

    S8 = 100 / S6

    For I = 0 To 3:
        If qbPA_TRADE(tradeIdx, I) <> 0 Then
			qbPA_TRADE(tradeIdx, I) = CInt(S8 * qbPA_TRADE(tradeIdx, I)) + S7
			S7 = qbPA_TRADE(tradeIdx, I)
        End If
        If qbPA_TRADE(tradeIdx, I) > 100 Then
			qbPA_TRADE(tradeIdx, I) = 100
        End If
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 2: 
        G0 = G0 + krNumRet_TRADE(tradeIdx, I)
        G3 = G3 + prRet_TRADE(tradeIdx, I)
    Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 2:
        If krNumRet_TRADE(tradeIdx, I) <> 0 Then
			krNumRet_TRADE(tradeIdx, I) = CInt(G2 * krNumRet_TRADE(tradeIdx, I)) + G1
			G1 = krNumRet_TRADE(tradeIdx, I)
        End If
        If krNumRet_TRADE(tradeIdx, I) > 100 Then
			krNumRet_TRADE(tradeIdx, I) = 100
        End If
    Next

    For I = 0 To 2:
        If prRet_TRADE(tradeIdx, I) <> 0 Then
			prRet_TRADE(tradeIdx, I) = CInt(G5 * prRet_TRADE(tradeIdx, I)) + G4
			G4 = prRet_TRADE(tradeIdx, I)
        End If
        If prRet_TRADE(tradeIdx, I) > 100 Then
			prRet_TRADE(tradeIdx, I) = 100
        End If
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 1: 
        G0 = G0 + FGPct_TRADE(tradeIdx, I)
        G3 = G3 + PATPct_TRADE(tradeIdx, I)
    Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 1:
        If FGPct_TRADE(tradeIdx, I) <> 0 Then
			FGPct_TRADE(tradeIdx, I) = CInt(G2 * FGPct_TRADE(tradeIdx, I)) + G1
			G1 = FGPct_TRADE(tradeIdx, I)
        End If
        If FGPct_TRADE(tradeIdx, I) > 100 Then
			FGPct_TRADE(tradeIdx, I) = 100
        End If
    Next

    For I = 0 To 1:
        If PATPct_TRADE(tradeIdx, I) <> 0 Then
			PATPct_TRADE(tradeIdx, I) = CInt(G5 * PATPct_TRADE(tradeIdx, I)) + G4
			G4 = PATPct_TRADE(tradeIdx, I)
        End If
        If PATPct_TRADE(tradeIdx, I) > 100 Then
			PATPct_TRADE(tradeIdx, I) = 100
        End If
    Next

    For I = 9 To 0 Step -1:
        If rbAtt_TRADE(tradeIdx, I) <> 0 Then
			rbAtt_TRADE(tradeIdx, I) = 100
			Exit For
        End If
    Next

    For I = 5 To 0 Step -1:
        If wrRec_TRADE(tradeIdx, I) <> 0 Then
			wrRec_TRADE(tradeIdx, I) = 100
			Exit For
        End If
    Next

    For I = 3 To 0 Step -1:
        If qbPA_TRADE(tradeIdx, I) <> 0 Then
			qbPA_TRADE(tradeIdx, I) = 100
			Exit For
        End If
    Next

    For I = 2 To 0 Step -1:
        If krNumRet_TRADE(tradeIdx, I) <> 0 Then
			krNumRet_TRADE(tradeIdx, I) = 100
			Exit For
        End If
    Next

    For I = 2 To 0 Step -1:
        If prRet_TRADE(tradeIdx, I) <> 0 Then
			prRet_TRADE(tradeIdx, I) = 100
			Exit For
        End If
    Next

    For I = 1 To 0 Step -1:
        If FGPct_TRADE(tradeIdx, I) <> 0 Then
			FGPct_TRADE(tradeIdx, I) = 100
			Exit For
        End If
    Next

    For I = 1 To 0 Step -1:
        If PATPct_TRADE(tradeIdx, I) <> 0 Then
			PATPct_TRADE(tradeIdx, I) = 100
			Exit For
        End If
    Next

    teamYear$ = RTrim$(tradeYN$(tradeIdx))

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

    LSet Q$(1) = tmInfo_TRADE$(tradeIdx)
    LSet Q$(2) = nicks_TRADE$(tradeIdx)

    For I = 0 To 7: 
        LSet Q$(I + 3) = MKI$(leagRat_TRADE(tradeIdx, I))
    Next I

    For I = 0 To 4: 
        LSet Q$(I + 11) = MKI$(teamRat_TRADE(tradeIdx, I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = rbNam_TRADE$(tradeIdx, I)
        LSet Q$(I * 5 + 17) = MKI$(rbAtt_TRADE(tradeIdx, I))
        LSet Q$(I * 5 + 18) = MKI$(rbAvg_TRADE(tradeIdx, I))
        LSet Q$(I * 5 + 19) = MKI$(rbRec_TRADE(tradeIdx, I))
        LSet Q$(I * 5 + 20) = MKI$(rbRecAvg_TRADE(tradeIdx, I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = wrNam_TRADE$(tradeIdx, I)
        LSet Q$(I * 3 + 67) = MKI$(wrRec_TRADE(tradeIdx, I))
        LSet Q$(I * 3 + 68) = MKI$(wrAvg_TRADE(tradeIdx, I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = qbNam_TRADE$(tradeIdx, I)
        LSet Q$(I * 5 + 85) = MKI$(qbPA_TRADE(tradeIdx, I))
        LSet Q$(I * 5 + 86) = MKI$(qbPCPct_TRADE(tradeIdx, I))
        LSet Q$(I * 5 + 87) = MKI$(qbIntPct_TRADE(tradeIdx, I))
        LSet Q$(I * 5 + 88) = MKI$(qbArmRat_TRADE(tradeIdx, I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = krNam_TRADE$(tradeIdx, I)
        LSet Q$(I * 3 + 105) = MKI$(krNumRet_TRADE(tradeIdx, I))
        LSet Q$(I * 3 + 106) = MKI$(krRetAvg_TRADE(tradeIdx, I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = prNam_TRADE$(tradeIdx, I)
        LSet Q$(I * 3 + 114) = MKI$(prRet_TRADE(tradeIdx, I))
        LSet Q$(I * 3 + 115) = MKI$(prRetAvg_TRADE(tradeIdx, I))
    Next I

    LSet Q$(122) = pntNam_TRADE$(tradeIdx, 0)
    LSet Q$(123) = MKI$(puntAvg_TRADE(tradeIdx, 0))
    LSet Q$(124) = pkNam_TRADE$(tradeIdx, 0)
    LSet Q$(125) = MKI$(FGA_TRADE(tradeIdx, 0))
    LSet Q$(126) = MKI$(FGPct_TRADE(tradeIdx, 0))
    LSet Q$(127) = MKI$(PAT_TRADE(tradeIdx, 0))
    LSet Q$(128) = MKI$(PATPct_TRADE(tradeIdx, 0))
    LSet Q$(129) = pkNam_TRADE$(tradeIdx, 1)
    LSet Q$(130) = MKI$(FGA_TRADE(tradeIdx, 1))
    LSet Q$(131) = MKI$(FGPct_TRADE(tradeIdx, 1))
    LSet Q$(132) = MKI$(PAT_TRADE(tradeIdx, 1))
    LSet Q$(133) = MKI$(PATPct_TRADE(tradeIdx, 1))
    LSet Q$(134) = MKI$(ydsPerComp_TRADE(tradeIdx))

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = sackNam_TRADE$(tradeIdx, I)
        LSet Q$(I * 2 + 136) = MKI$(numSack_TRADE(tradeIdx, I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = intNam_TRADE$(tradeIdx, I)
        LSet Q$(I * 2 + 166) = MKI$(numInt_TRADE(tradeIdx, I))
    Next I

    Put #1, teamIndexes(tradeIdx)

    Close #1

    Erase qbPA_TRADE, qbPCPct_TRADE, qbIntPct_TRADE, qbArmRat_TRADE
    Erase krRetAvg_TRADE, krNumRet_TRADE, prRetAvg_TRADE, prRet_TRADE
    Erase FGA_TRADE, FGPct_TRADE, PAT_TRADE, PATPct_TRADE, puntAvg_TRADE
    Erase rbAtt_TRADE, rbAvg_TRADE, rbRec_TRADE, rbRecAvg_TRADE
    Erase wrAvg_TRADE, wrRec_TRADE
    Erase krNam_TRADE$, pkNam_TRADE$, prNam_TRADE$, pntNam_TRADE$, rbNam_TRADE$, wrNam_TRADE$, qbNam_TRADE$


End Sub


'----------------------------------------
'       SaveTrades Subroutine
'----------------------------------------
'This subroutine saves the modified team
'data from the trade to the team files.
Sub SaveTrades ()

    Shared haveStats()

    Shared intNam_TRADE$(), sackNam_TRADE$()
    Shared puntNam_TRADE$(), statNam_TRADE$()

    Shared tmInfo_TRADE$(), nicks_TRADE$()

    Shared tradeFG$(), tradeKR$()
    Shared recLongRecTxt$(), recLongFGTxt$(), recLongIntTxt$(), recLongKRTxt$(), recLongPassTxt$(), recLongRushTxt$()
    Shared recLongPRTxt$(), tradePR$()
    Shared tradeQB$(), tradeRB$(), tradeWR$()

    Shared tradeFGA!(), tradeFGM()
    Shared tradeLRec!(), tradeLcompFGPct!(), tradeLInt!(), tradeLKR!(), tradeLPass!(), tradeLRush!()
    Shared tradeLPR!()
    Shared tradeTeamStat!(), tradeOppStat!(), tradePlayerStat!()

    Shared teamRat_TRADE()
    Shared tradeGI(), tradeGS()

    For saveIdx = 0 To 1

        If haveStats(saveIdx) <> 1 Then

            Call TITLE

            For XY = 2 To 23:
                Locate XY, 1: Print Tab(79);
            Next

            Locate 12, 1

            Print "SAVING ";: Color teamRat_TRADE(saveIdx, 2), teamRat_TRADE(saveIdx, 1)

            Print tmInfo_TRADE$(saveIdx);

            Color 15, 0
            Print " STATS"
            
            Z = 0

            statFile$ = nameStatFile$(tmInfo_TRADE$(tradeIdx))
            Open diskPaths$(1) + statFile$ For Output As #1

            Print #1, statNam_TRADE$(saveIdx)

            For I = 0 To 9: 
                Print #1, tradeRB$(saveIdx, I)
            Next
            For I = 0 To 5: 
                Print #1, tradeWR$(saveIdx, I)
            Next
            For I = 0 To 3: 
                Print #1, tradeQB$(saveIdx, I)
            Next
            For I = 0 To 2: 
                Print #1, tradeKR$(saveIdx, I)
            Next
            For I = 0 To 2: 
                Print #1, tradePR$(saveIdx, I)
            Next

            Print #1, puntNam_TRADE$(saveIdx)

            For I = 0 To 1: 
                Print #1, tradeFG$(saveIdx, I)
            Next

            For X = 0 To 13
                For Y = 0 To 17:
                    Print #1, tradePlayerStat!(saveIdx, X, Y)
                Next
            Next

            For X = 0 To 38: 
                Print #1, tradeTeamStat!(saveIdx, X)
            Next

            For X = 0 To 38: 
                Print #1, tradeOppStat!(saveIdx, X)
            Next

            For X = 0 To 9: 
                Print #1, intNam_TRADE$(saveIdx, X)
                For J1 = 0 To 2: 
                    Print #1, tradeGI(saveIdx, X, J1)
                Next
            Next

            For X = 0 To 14: 
                Print #1, sackNam_TRADE$(saveIdx, X)
                For J1 = 0 To 1: 
                    Print #1, tradeGS(saveIdx, X, J1)
                Next
            Next

            For I = 0 To 9: 
                Print #1, tradeLRush!(saveIdx, I)
                For J1 = 0 To 1: 
                    Print #1, recLongRushTxt$(saveIdx, I, J1)
                Next
            Next

            For I = 0 To 15: 
                Print #1, tradeLRec!(saveIdx, I)
                For J1 = 0 To 1: 
                    Print #1, recLongRecTxt$(saveIdx, I, J1)
                Next
            Next

            For I = 0 To 3: 
                Print #1, tradeLPass!(saveIdx, I)
                For J1 = 0 To 1: 
                    Print #1, recLongPassTxt$(saveIdx, I, J1)
                Next
            Next

            For I = 0 To 2: 
                Print #1, tradeLKR!(saveIdx, I)
                For J1 = 0 To 1: 
                    Print #1, recLongKRTxt$(saveIdx, I, J1)
                Next
            Next

            For I = 0 To 2: 
                Print #1, tradeLPR!(saveIdx, I)
                For J1 = 0 To 1: 
                    Print #1, recLongPRTxt$(saveIdx, I, J1)
                Next
            Next

            For I = 0 To 1: 
                Print #1, tradeLcompFGPct!(saveIdx, I)
                Print #1, recLongFGTxt$(saveIdx, I)
                For I1 = 0 To 4: 
                    Print #1, tradeFGM(saveIdx, I, I1)
                    Print #1, tradeFGA!(saveIdx, I, I1)
                Next
            Next

            For I = 0 To 9: 
                Print #1, tradeLInt!(saveIdx, I)
                Print #1, recLongIntTxt$(saveIdx, I, 0)
                Print #1, recLongIntTxt$(saveIdx, I, 1)
            Next

            Close #1

        End If

    Next saveIdx

End Sub


'----------------------------------------
'       ChooseTradeTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for 
'choosing the team to trade players from
Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)

    Shared teamRat_TRADE()

    Shared previousYear$

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox (6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12
        If tradeIdx = 0 Then
            Print "ENTER DISK ID FOR TEAM 1: "
        Else
            Print "ENTER DISK ID FOR TEAM 2: "
        End If

        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES COULD BE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                        BoxWidth = 24
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectTradeTeam (numberTeams, yearNumber$, chosenIdx, ExitCode%, tradeIdx)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do
                        Locate 2, 33
                        Color teamRat_TRADE(tradeIdx, 2), teamRat_TRADE(tradeIdx, 1): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectTradeTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for 
'selecting the team to trade players from
Sub SelectTradeTeam (numberTeams, yearNumber$, selectedIdx, ExitCode%, tradeIdx)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = LAST_ROW - 6
        Columns% = 4
        Spacing% = 6
        Row% = 5
        Column% = 1

        Color , 4
        Cls

        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate LAST_ROW, 1
        Color 14
        Print Tab(3); "PG DN";
        Print Tab(18); "ENTER";
        Color 15: Print "-SELECTS TEAM";
        Color 14
        Print Tab(39); "ESC";
        Color 15: Print "-SELECT NEW ID";
        Color 14
        Print Tab(72); "PG UP";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadTradeTeam(yearNumber$, selectedIdx, tradeIdx, validData)
            
        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA.... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'        ReadTradeTeam Subroutine
'----------------------------------------
'This subroutine reads team data into 
'arrays for the selected team
Sub ReadTradeTeam (teamYear$, teamIdx, tradeIdx, dataOK)

    'C2 has to do with RB Rec 
    'C3 has to do with RB Att
    '-- contribution calculation??
    Dim C2!(15)

    Shared ydCmpAdj_TRADE!()

    Shared FGA_TRADE(), FGPct_TRADE(), leagRat_TRADE()
    Shared krNumRet_TRADE(), krRetAvg_TRADE()
    Shared numInt_TRADE(), numSack_TRADE()
    Shared PAT_TRADE(), PATPct_TRADE()
    Shared prRet_TRADE(), prRetAvg_TRADE(), puntAvg_TRADE()
    Shared qbArmRat_TRADE()
    Shared qbPA_TRADE(), qbPCPct_TRADE(), qbIntPct_TRADE()
    Shared rbAtt_TRADE(), rbAvg_TRADE(), rbRec_TRADE(), rbRecAvg_TRADE()
    Shared teamRat_TRADE(), ydsPerComp_TRADE()
    Shared wrAvg_TRADE(), wrRec_TRADE()

    Shared intNam_TRADE$(), krNam_TRADE$()
    Shared pkNam_TRADE$(), pntNam_TRADE$(), prNam_TRADE$()
    Shared qbNam_TRADE$(), rbNam_TRADE$()
    Shared sackNam_TRADE$(), wrNam_TRADE$()
    
    Shared tmInfo_TRADE$(), nicks_TRADE$()

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

    Get #1, teamIdx

    tmInfo_TRADE$(tradeIdx) = RTrim$(Q$(1))
    nicks_TRADE$(tradeIdx) = RTrim$(Q$(2))

    For I = 0 To 7: 
        leagRat_TRADE(tradeIdx, I) = CVI(Q$(I + 3))
    Next I

    For I = 0 To 4: 
        teamRat_TRADE(tradeIdx, I) = CVI(Q$(I + 11))
    Next I

    For I = 0 To 9
        rbNam_TRADE$(tradeIdx, I) = Q$(I * 5 + 16)
        rbAtt_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 17))
        rbAvg_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 18))
        rbRec_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 19))
        rbRecAvg_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 20))
    Next I

    For I = 0 To 5
        wrNam_TRADE$(tradeIdx, I) = Q$(I * 3 + 66)
        wrRec_TRADE(tradeIdx, I) = CVI(Q$(I * 3 + 67))
        wrAvg_TRADE(tradeIdx, I) = CVI(Q$(I * 3 + 68))
    Next I

    For I = 0 To 3
        qbNam_TRADE$(tradeIdx, I) = Q$(I * 5 + 84)
        qbPA_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 85))
        qbPCPct_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 86))
        qbIntPct_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 87))
        qbArmRat_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 88))
    Next I

    For I = 0 To 2
        krNam_TRADE$(tradeIdx, I) = Q$(I * 3 + 104) 
        krNumRet_TRADE(tradeIdx, I) = CVI(Q$(I * 3 + 105))
        krRetAvg_TRADE(tradeIdx, I) = CVI(Q$(I * 3 + 106))
    Next I

    For I = 0 To 2
        prNam_TRADE$(tradeIdx, I) = Q$(I * 3 + 113)
        prRet_TRADE(tradeIdx, I) = CVI(Q$(I * 3 + 114))
        prRetAvg_TRADE(tradeIdx, I) = CVI(Q$(I * 3 + 115))
    Next I

    pntNam_TRADE$(tradeIdx, 0) = Q$(122)
    puntAvg_TRADE(tradeIdx, 0) = CVI(Q$(123))

    For I = 0 To 1
        pkNam_TRADE$(tradeIdx, I) = RTrim$(Q$(I * 5 + 124))
        FGA_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 125))
        FGPct_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 126))
        PAT_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 127))
        PATPct_TRADE(tradeIdx, I) = CVI(Q$(I * 5 + 128))
    Next I

    ydCmpAdj_TRADE!(tradeIdx) = CVI(Q$(134))

    For I = 0 To 14
        sackNam_TRADE$(tradeIdx, I) = Q$(I * 2 + 135)
        numSack_TRADE(tradeIdx, I) = CVI(Q$(I * 2 + 136))
    Next I

    For I = 0 To 9
        intNam_TRADE$(tradeIdx, I) = Q$(I * 2 + 165)
        numInt_TRADE(tradeIdx, I) = CVI(Q$(I * 2 + 166))
    Next I

    Close #1

    If CheckString%(targetTeam$) = 1 then

        dataOK = 0

    Else

        'Get contribution %'s
        Erase C2!

        U = 0

        For I = 9 To 1 Step -1
            If rbAtt_TRADE(tradeIdx, I) <> 0 Then
                rbAtt_TRADE(tradeIdx, I) = rbAtt_TRADE(tradeIdx, I) - rbAtt_TRADE(tradeIdx, I - 1)
            End If
        Next

        For I = 0 To 5: 
            rbRec_TRADE(tradeIdx, I + 10) = wrRec_TRADE(tradeIdx, I)
        Next

        For I = 1 To 15
            If rbRec_TRADE(tradeIdx, I) = 0 Then
                U = U + 1
            Else
                C2!(I) = rbRec_TRADE(tradeIdx, I) - rbRec_TRADE(tradeIdx, I - 1 - U)
                U = 0
            End If
        Next

        For I = 1 To 15: 
            rbRec_TRADE(tradeIdx, I) = C2!(I)
        Next

        For I = 0 To 5: 
            wrRec_TRADE(tradeIdx, I) = rbRec_TRADE(tradeIdx, I + 10)
        Next

        For I = 3 To 1 Step -1
            If qbPA_TRADE(tradeIdx, I) <> 0 Then
                qbPA_TRADE(tradeIdx, I) = qbPA_TRADE(tradeIdx, I) - qbPA_TRADE(tradeIdx, I - 1)
            End If
        Next

        For I = 2 To 1 Step -1
            If krNumRet_TRADE(tradeIdx, I) <> 0 Then
                krNumRet_TRADE(tradeIdx, I) = krNumRet_TRADE(tradeIdx, I) - krNumRet_TRADE(tradeIdx, I - 1)
            End If
        Next

        For I = 2 To 1 Step -1
            If prRet_TRADE(tradeIdx, I) <> 0 Then
                prRet_TRADE(tradeIdx, I) = prRet_TRADE(tradeIdx, I) - prRet_TRADE(tradeIdx, I - 1)
            End If
        Next

        For I = 1 To 1 Step -1
            If FGPct_TRADE(tradeIdx, I) <> 0 Then
                FGPct_TRADE(tradeIdx, I) = FGPct_TRADE(tradeIdx, I) - FGPct_TRADE(tradeIdx, I - 1)
            End If
        Next

        For I = 1 To 1 Step -1
            If PATPct_TRADE(tradeIdx, I) <> 0 Then
                PATPct_TRADE(tradeIdx, I) = PATPct_TRADE(tradeIdx, I) - PATPct_TRADE(tradeIdx, I - 1)
            End If
        Next

        dataOK = 1
        
    End If

End Sub
