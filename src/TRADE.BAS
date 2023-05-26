Sub TRADE ()

    _Title "3-in-1 Football - Trade Players"

    ReDim menuChoices$(1 To 2)

    Do

        Cls

        Call TITLE

        menuChoices$(1) = "TRADE PLAYERS            "
        menuChoices$(2) = "RETURN TO 3 & 1 MAIN MENU"
        Selection% = 1
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 10, 50, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call StartTrade
                Case 2: Cls
            End Select
        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub

'----------------------------------------
'       StartTrade Subroutine
'----------------------------------------
'...explanation...
Sub StartTrade ()

    Shared NY$(), tradeYN$()
    Shared S!()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Call GetDiskID_TEAM(0)

    Color 15, 0

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    S!(0) = 0

    Do
        Locate 3: Print "DOES THIS TEAM HAVE COMPILED STATS (YN)"
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then
        S!(0) = 1
    Else
        Call LoadStatsForTrade(0)
    End If

    Call GetDiskID_TEAM(1)

    Color 15, 0

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    S!(1) = 0

    Do
        Locate 3: Print "DOES THIS TEAM HAVE COMPILED STATS (YN)"
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Call LoadStatsForTrade(1)

    J = 1

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
                Call PlayerStatsPrompt(W1)
                Call TradeKickers(W1)

            Case "K":
                Call PlayerStatsPrompt(W1)
                Call TradeKRs(W1)

            Case "P":
                Call PlayerStatsPrompt(W1)
                Call TradePRs(W1)

            Case "Q":
                Call PlayerStatsPrompt(W1)
                Call TradeQBs(W1)

            Case "R":
                Call PlayerStatsPrompt(W1)
                Call TradeRBs(W1)

            Case "U":
                Call PlayerStatsPrompt(W1)
                Call TradePunters(W1)

            Case "W":
                Call PlayerStatsPrompt(W1)
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

    For J = 0 To 1

        If S!(J) <> 1 Then

            Call TITLE

            For XY = 2 To 23:
                Locate XY, 1: Print Tab(79);
            Next

            Call SaveTradeStats(J)

        End If

    Next J

End Sub

'--- No idea how this was to be used (if at all)
'If S!(0) = 1 Or S!(1) = 1 Then 2221

'----------------------------------------
'           TradeRBs Subroutine
'----------------------------------------
'...explanation...
Sub TradeRBs (W1)

    Shared statsA$()
    Shared tradeA1$(), tradeLC$(), tradeLR$(), tradeRB$()

    Shared S!(), tradeLC!(), tradeLR!(), tradeZ2!()

    Shared tradePA%()
    Shared tradeA0%(), tradeA1%(), tradeA2%(), tradeA3%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0)
    Color 7, 0
    Locate 4, 20
    Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1)
    Color 7, 0

    For I = 0 To 9:
        Locate I + 5, 1
        Print I; " "; tradeA1$(0, I)
        Locate I + 5, 20
        Print I; " "; tradeA1$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0);
            Color 7, 0: Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 9

        Print tradeA1$(0, I)

        Do
            Locate 19, 1
            Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1);
            Color 7, 0: Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I >= 0 And I <= 9

        Print tradeA1$(1, I1)

        Do
            Locate 23, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA1$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA1$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       TC    ###                                 TC    ###"; tradeZ2!(0, I, 0); tradeZ2!(1, I1, 0)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 1); tradeZ2!(1, I1, 1)
    Locate , 5: Print Using " ##.# AVG   "; tradeA1%(0, I) * .3;

    If tradeZ2!(0, I, 0) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 1) / tradeZ2!(0, I, 0); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeA1%(1, I1) * .3;

    If tradeZ2!(1, I1, 0) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 1) / tradeZ2!(1, I1, 0) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 2); tradeZ2!(1, I1, 2)
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradeZ2!(0, I, 3); tradeZ2!(1, I1, 3)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 4); tradeZ2!(1, I1, 4)
    Locate , 5: Print Using " ##.# AVG   "; tradeA3%(0, I) * 3;

    If tradeZ2!(0, I, 3) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 4) / tradeZ2!(0, I, 3); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeA3%(1, I1) * 3;

    If tradeZ2!(1, I1, 3) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 4) / tradeZ2!(1, I1, 3) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 5); tradeZ2!(1, I1, 5)

    I$ = GetKeyPress$

    Swap tradeA1$(0, I), tradeA1$(1, I1)
    Swap tradeA0%(0, I), tradeA0%(1, I1)
    Swap tradeLR!(0, I), tradeLR!(1, I1)
    Swap tradeLR$(0, I, 0), tradeLR$(1, I1, 0)
    Swap tradeLR$(0, I, 1), tradeLR$(1, I1, 1)
    Swap tradeLC!(0, I), tradeLC!(1, I1)
    Swap tradeLC$(0, I, 0), tradeLC$(1, I1, 0)
    Swap tradeLC$(0, I, 1), tradeLC$(1, I1, 1)
    Swap tradeA1%(0, I), tradeA1%(1, I1)
    Swap tradeA2%(0, I), tradeA2%(1, I1)
    Swap tradeA3%(0, I), tradeA3%(1, I1)
    Swap tradeRB$(0, I), tradeRB$(1, I1)

    If S!(0) = 1 Or S!(1) = 1 Or W1 = 0 Then
        For I2 = 0 To 5: tradeZ2!(0, I, I2) = 0: tradeZ2!(1, I1, I2) = 0: Next I2
    Else
        For I2 = 0 To 5: Swap tradeZ2!(0, I, I2), tradeZ2!(1, I1, I2): Next
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA1$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA1$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       TC    ###                                 TC    ###"; tradeZ2!(0, I, 0); tradeZ2!(1, I1, 0)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 1); tradeZ2!(1, I1, 1)
    Locate , 5: Print Using " ##.# AVG   "; tradeA1%(0, I) * .3;

    If tradeZ2!(0, I, 0) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 1) / tradeZ2!(0, I, 0); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeA1%(1, I1) * .3;

    If tradeZ2!(1, I1, 0) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 1) / tradeZ2!(1, I1, 0) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 2); tradeZ2!(1, I1, 2)
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradeZ2!(0, I, 3); tradeZ2!(1, I1, 3)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 4); tradeZ2!(1, I1, 4)
    Locate , 5: Print Using " ##.# AVG   "; tradeA3%(0, I) * 3;

    If tradeZ2!(0, I, 3) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 4) / tradeZ2!(0, I, 3); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeA3%(1, I1) * 3;

    If tradeZ2!(1, I1, 3) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 4) / tradeZ2!(1, I1, 3) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 5); tradeZ2!(1, I1, 5)

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'           TradeWRs Subroutine
'----------------------------------------
'...explanation...
Sub TradeWRs (W1)

    Shared statsA$()
    Shared tradeA2$(), tradeLC$(), tradeWR$()

    Shared S!(), tradeLC!(), tradeZ2!()

    Shared tradeA4%(), tradeA5%()
    Shared tradePA%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0)
    Color 7, 0
    Locate 4, 20
    Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1)
    Color 7, 0

    For I = 0 To 5:
        Locate I + 5, 1: Print I; " "; tradeA2$(0, I): Locate I + 5, 20: Print I; " "; tradeA2$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 5

        Print tradeA2$(0, I)

        Do
            Locate 19, 1
            Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 5

        Print tradeA2$(1, I1)

        Do
            Locate 23, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA2$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA2$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradeZ2!(0, I, 6); tradeZ2!(1, I1, 6)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 7); tradeZ2!(1, I1, 7)
    Locate , 5: Print Using " ##.# AVG   "; tradeA5%(0, I) * 3;

    If tradeZ2!(0, I, 6) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 7) / tradeZ2!(0, I, 6); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeA5%(1, I1) * 3;

    If tradeZ2!(1, I1, 6) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 7) / tradeZ2!(1, I1, 6) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 8); tradeZ2!(1, I1, 8)

    I$ = GetKeyPress$

    Swap tradeA2$(0, I), tradeA2$(1, I1)
    Swap tradeLC!(0, I + 10), tradeLC!(1, I1 + 10)
    Swap tradeLC$(0, I + 10, 0), tradeLC$(1, I1 + 10, 0)
    Swap tradeLC$(0, I + 10, 1), tradeLC$(1, I1 + 10, 1)
    Swap tradeA4%(0, I), tradeA4%(1, I1)
    Swap tradeA5%(0, I), tradeA5%(1, I1)
    Swap tradeWR$(0, I), tradeWR$(1, I1)

    If S!(0) = 1 Or S!(1) = 1 Or W1 = 0 Then
        For I2 = 6 To 8: tradeZ2!(0, I, I2) = 0: tradeZ2!(1, I1, I2) = 0: Next I2
    Else
        For I2 = 6 To 8: Swap tradeZ2!(0, I, I2), tradeZ2!(1, I1, I2): Next
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA2$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA2$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       RC     ##                                 RC     ##"; tradeZ2!(0, I, 6); tradeZ2!(1, I1, 6)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 7); tradeZ2!(1, I1, 7)
    Locate , 5: Print Using " ##.# AVG   "; tradeA5%(0, I) * 3;

    If tradeZ2!(0, I, 6) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 7) / tradeZ2!(0, I, 6); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeA5%(1, I1) * 3;

    If tradeZ2!(1, I1, 6) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 7) / tradeZ2!(1, I1, 6) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 8); tradeZ2!(1, I1, 8)

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'           TradeQBs Subroutine
'----------------------------------------
'...explanation...
Sub TradeQBs (W1)

    Shared statsA$()
    Shared tradeA3$(), tradeLP$(), tradeQB$()

    Shared S!(), tradeLP!(), tradeZ2!()

    Shared tradeA6%(), tradeA7%(), tradeA8%()
    Shared tradePA%(), tradeQX%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0)

    Color 7, 0

    Locate 4, 20
    Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1)

    Color 7, 0

    For I = 0 To 3:
        Locate I + 5, 1
        Print I; " "; tradeA3$(0, I)
        Locate I + 5, 20
        Print I; " "; tradeA3$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 3

        Print tradeA3$(0, I)

        Do
            Locate 19, 1
            Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 3

        Print tradeA3$(1, I1)

        Do
            Locate 23, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7: Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA3$(0, I);
    Color 7, 0
    Locate , 49: Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA3$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       PC    ###                                 PC    ###"; tradeZ2!(0, I, 10); tradeZ2!(1, I1, 10)
    Locate , 5: Print Using "       PA   ####                                 PA   ####"; tradeZ2!(0, I, 9); tradeZ2!(1, I1, 9)
    Locate , 5: Print Using "       PI     ##                                 PI     ##"; tradeZ2!(0, I, 11); tradeZ2!(1, I1, 11)
    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 13); tradeZ2!(1, I1, 13)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 12); tradeZ2!(1, I1, 12)
    Locate , 5: Print Using " ##.# CMP%  "; tradeA7%(0, I);

    If tradeZ2!(0, I, 9) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 10) / tradeZ2!(0, I, 9) * 100; Else Print " -.-";

    Print Using "                           ##.# CMP%  "; tradeA7%(1, I1);

    If tradeZ2!(1, I1, 9) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 10) / tradeZ2!(1, I1, 9) * 100 Else Print " -.-"

    Locate , 5: Print Using " ##.# INT%  "; tradeA8%(0, I);

    If tradeZ2!(0, I, 9) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 11) / tradeZ2!(0, I, 9) * 100; Else Print " -.-";

    Print Using "                           ##.# INT%  "; tradeA8%(1, I1);

    If tradeZ2!(1, I1, 9) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 11) / tradeZ2!(1, I1, 9) * 100 Else Print " -.-"

    I$ = GetKeyPress$

    Swap tradeA3$(0, I), tradeA3$(1, I1)
    Swap tradeLP!(0, I), tradeLP!(1, I1)
    Swap tradeLP$(0, I, 0), tradeLP$(1, I1, 0)
    Swap tradeLP$(0, I, 1), tradeLP$(1, I1, 1)
    Swap tradeA6%(0, I), tradeA6%(1, I1)
    Swap tradeA7%(0, I), tradeA7%(1, I1)
    Swap tradeA8%(0, I), tradeA8%(1, I1)
    Swap tradeQX%(0, I), tradeQX%(1, I1)
    Swap tradeQB$(0, I), tradeQB$(1, I1)

    If S!(0) = 1 Or S!(1) = 1 Or W1 = 0 Then
        For I2 = 9 To 13: tradeZ2!(0, I, I2) = 0: tradeZ2!(1, I1, I2) = 0: Next I2
        For I2 = 0 To 2: tradeZ2!(0, I + 10, I2) = 0: tradeZ2!(1, I1 + 10, I2) = 0: Next
    Else
        For I2 = 9 To 13: Swap tradeZ2!(0, I, I2), tradeZ2!(1, I1, I2): Next I2
        For I2 = 0 To 2: Swap tradeZ2!(0, I + 10, I2), tradeZ2!(1, I1 + 10, I2): Next I2
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA3$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA3$(1, I1)
    Color 7, 0
    Print
    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "       PC    ###                                 PC    ###"; tradeZ2!(0, I, 10); tradeZ2!(1, I1, 10)
    Locate , 5: Print Using "       PA   ####                                 PA   ####"; tradeZ2!(0, I, 9); tradeZ2!(1, I1, 9)
    Locate , 5: Print Using "       PI     ##                                 PI     ##"; tradeZ2!(0, I, 11); tradeZ2!(1, I1, 11)
    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 13); tradeZ2!(1, I1, 13)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 12); tradeZ2!(1, I1, 12)
    Locate , 5: Print Using " ##.# CMP%  "; tradeA7%(0, I);

    If tradeZ2!(0, I, 9) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 10) / tradeZ2!(0, I, 9) * 100; Else Print " -.-";

    Print Using "                           ##.# CMP%  "; tradeA7%(1, I1);

    If tradeZ2!(1, I1, 9) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 10) / tradeZ2!(1, I1, 9) * 100 Else Print " -.-"

    Locate , 5
    Print Using " ##.# INT%  "; tradeA8%(0, I);

    If tradeZ2!(0, I, 9) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 11) / tradeZ2!(0, I, 9) * 100; Else Print " -.-";

    Print Using "                           ##.# INT%  "; tradeA8%(1, I1);

    If tradeZ2!(1, I1, 9) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 11) / tradeZ2!(1, I1, 9) * 100 Else Print " -.-"

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'           TradeKRs Subroutine
'----------------------------------------
'...explanation...
Sub TradeKRs (W1)

    Shared statsA$()
    Shared tradeA4$(), tradeKR$(), tradeLK$()

    Shared S!(), tradeLK!(), tradeZ2!()

    Shared B%()
    Shared tradeA9%(), tradeB%(), tradePA%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0)
    Color 7, 0
    Locate 4, 20
    Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1)
    Color 7, 0

    For I = 0 To 2:
        Locate I + 5, 1
        Print I; " "; tradeA4$(0, I)
        Locate I + 5, 20
        Print I; " "; tradeA4$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 2

        Print tradeA4$(0, I)

        Do
            Locate 19, 1
            Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 2

        Print tradeA4$(1, I1)

        Do
            Locate 23, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA4$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA4$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradeZ2!(0, I, 14); tradeZ2!(1, I1, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 15); tradeZ2!(1, I1, 15)
    Locate , 5: Print Using " ##.# AVG   "; tradeB%(0, I) * 3;

    If tradeZ2!(0, I, 14) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 15) / tradeZ2!(0, I, 14); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeB%(1, I1) * 3;

    If tradeZ2!(1, I1, 14) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 15) / tradeZ2!(1, I1, 14) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 16); tradeZ2!(1, I1, 16)

    I$ = GetKeyPress$

    Swap tradeA4$(0, I), tradeA4$(1, I1)
    Swap tradeA9%(0, I), tradeA9%(1, I1)
    Swap tradeLK!(0, I), tradeLK!(1, I1)
    Swap tradeLK$(0, I, 0), tradeLK$(1, I1, 0)
    Swap tradeLK$(0, I, 1), tradeLK$(1, I1, 1)
    Swap tradeB%(0, I), tradeB%(1, I1)
    Swap tradeKR$(0, I), tradeKR$(1, I1)

    If S!(0) = 1 Or S!(1) = 1 Or W1 = 0 Then
        For I2 = 14 To 16: tradeZ2!(0, I, I2) = 0: tradeZ2!(1, I1, I2) = 0: Next I2
    Else
        For I2 = 14 To 16: Swap tradeZ2!(0, I, I2), tradeZ2!(1, I1, I2): Next I2
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA4$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA4$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradeZ2!(0, I, 14); tradeZ2!(1, I1, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I, 15); tradeZ2!(1, I1, 15)
    Locate , 5: Print Using " ##.# AVG   "; tradeB%(0, I) * 3;

    If tradeZ2!(0, I, 14) > 0 Then Print Using "##.#"; tradeZ2!(0, I, 15) / tradeZ2!(0, I, 14); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeB%(1, I1) * 3;

    If tradeZ2!(1, I1, 14) > 0 Then Print Using "##.#"; tradeZ2!(1, I1, 15) / tradeZ2!(1, I1, 14) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I, 16); tradeZ2!(1, I1, 16)

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'           TradePRs Subroutine
'----------------------------------------
'...explanation...
Sub TradePRs (W1)

    Shared statsA$()
    Shared tradeA5$(), tradePK$(), tradePR$()

    Shared S!(), tradePK!(), tradeZ2!()

    Shared tradePA%()
    Shared tradeB1%(), tradeB2%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0)
    Color 7, 0
    Locate 4, 20
    Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1)
    Color 7, 0

    For I = 0 To 2:
        Locate I + 5, 1
        Print I; " "; tradeA5$(0, I)
        Locate I + 5, 20
        Print I; " "; tradeA5$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 2

        Print tradeA5$(0, I)

        Do
            Locate 19, 1
            Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I1 >= 0 And I1 <= 2

        Print tradeA5$(1, I1)

        Do
            Locate 23, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA5$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA5$(1, I1)
    Color 7, 0
    Print
    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradeZ2!(0, I + 3, 14); tradeZ2!(1, I1 + 3, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I + 3, 15); tradeZ2!(1, I1 + 3, 15)
    Locate , 5: Print Using " ##.# AVG   "; tradeB2%(0, I) * 3;

    If tradeZ2!(0, I, 14) > 0 Then Print Using "##.#"; tradeZ2!(0, I + 3, 15) / tradeZ2!(0, I + 3, 14); Else Print " -.-";

    Print Using "                           ##.# AVG   "; tradeB2%(1, I1) * 3;

    If tradeZ2!(1, I1, 14) > 0 Then Print Using "##.#"; tradeZ2!(1, I1 + 3, 15) / tradeZ2!(1, I1 + 3, 14) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I + 3, 16); tradeZ2!(1, I1 + 3, 16)

    I$ = GetKeyPress$

    Swap tradeA5$(0, I), tradeA5$(1, I1)
    Swap tradePK!(0, I), tradePK!(1, I1)
    Swap tradePK$(0, I, 0), tradePK$(1, I1, 0)
    Swap tradePK$(0, I, 1), tradePK$(1, I1, 1)
    Swap tradeB1%(0, I), tradeB1%(1, I1)
    Swap tradeB2%(0, I), tradeB2%(1, I1)
    Swap tradePR$(0, I), tradePR$(1, I1)

    If S!(0) = 1 Or S!(1) = 1 Or W1 = 0 Then
        For I2 = 14 To 16: tradeZ2!(0, I + 3, I2) = 0: tradeZ2!(1, I1 + 3, I2) = 0: Next I2
    Else
        For I2 = 14 To 16: Swap tradeZ2!(0, I + 3, I2), tradeZ2!(1, I1 + 3, I2): Next I2
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA5$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA5$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      RET     ##                                RET     ##"; tradeZ2!(0, I + 3, 14); tradeZ2!(1, I1 + 3, 14)
    Locate , 5: Print Using "      YDS   ####                                YDS   ####"; tradeZ2!(0, I + 3, 15); tradeZ2!(1, I1 + 3, 15)
    Locate , 5: Print Using " ##.# AVG   "; tradeB2%(0, I) * 3;

    If tradeZ2!(0, I, 14) > 0 Then Print Using "##.#"; tradeZ2!(0, I + 3, 15) / tradeZ2!(0, I + 3, 14); Else Print " -.-";
    Print Using "                           ##.# AVG   "; tradeB2%(1, I1) * 3;

    If tradeZ2!(1, I1, 14) > 0 Then Print Using "##.#"; tradeZ2!(1, I1 + 3, 15) / tradeZ2!(1, I1 + 3, 14) Else Print " -.-"

    Locate , 5: Print Using "      TDS     ##                                TDS     ##"; tradeZ2!(0, I + 3, 16); tradeZ2!(1, I1 + 3, 16)

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'       TradePunters Subroutine
'----------------------------------------
'...explanation...
Sub TradePunters (W1)

    Shared statsA$()
    Shared tradeA6$(), PU$()

    Shared S!()
    Shared tradeZ!()

    Shared tradePA%()
    Shared tradeB3%()

    Do
        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Locate 4, 1
        Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0)
        Color 7, 0
        Locate 4, 20
        Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1)
        Color 7, 0
        Print
        Print tradeA6$(0, 0);
        Locate , 20: Print " "; tradeA6$(1, 0)
        
        Do
            Locate 22, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        If UCase$(I$) = "N" Then

            Call TITLE

            For XY = 2 To 23:
                Locate XY, 1: Print Tab(79);
            Next

            Locate 3
            Locate , 7
            Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA6$(0, 0);
            Color 7, 0
            Locate , 49
            Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA6$(1, 0)
            Color 7, 0
            Print
            Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
            Print
            Locate , 5: Print Using "      PNT     ##                                RET     ##"; tradeZ!(0, 7); tradeZ!(1, 7)
            Locate , 5: Print Using " ##.# AVG   "; tradeB3%(0, 0) * 2 + 28;

            If tradeZ!(0, 7) > 0 Then Print Using "##.#"; tradeZ!(0, 8) / tradeZ!(0, 7); Else Print " -.-";

            Print Using "                           ##.# AVG   "; tradeB3%(1, 0) * 2 + 28;

            If tradeZ!(1, 7) > 0 Then Print Using "##.#"; tradeZ!(1, 8) / tradeZ!(1, 7) Else Print " -.-"

            J$ = GetKeyPress$

            Swap tradeA6$(0, 0), tradeA6$(1, 0)
            Swap tradeB3%(0, 0), tradeB3%(1, 0)
            Swap PU$(0), PU$(1)

            If S!(0) <> 1 And S!(1) <> 1 And W1 <> 0 Then
                Swap tradeZ!(0, 7), tradeZ!(1, 7)
                Swap tradeZ!(0, 8), tradeZ!(1, 8)
            End If

            Call TITLE

            For XY = 2 To 23:
                Locate XY, 1: Print Tab(79);
            Next

            Locate 3
            Locate , 7
            Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA6$(0, 0);
            Color 7, 0
            Locate , 49
            Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA6$(1, 0)
            Color 7, 0
            Print

            Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
            Print
            Locate , 5: Print Using "      PNT     ##                                RET     ##"; tradeZ!(0, 7); tradeZ!(1, 7)
            Locate , 5: Print Using " ##.# AVG   "; tradeB3%(0, 0) * 2 + 28;

            If tradeZ!(0, 7) > 0 Then Print Using "##.#"; tradeZ!(0, 8) / tradeZ!(0, 7); Else Print " -.-";

            Print Using "                           ##.# AVG   "; tradeB3%(1, 0) * 2 + 28;

            If tradeZ!(1, 7) > 0 Then Print Using "##.#"; tradeZ!(1, 8) / tradeZ!(1, 7) Else Print " -.-"

            J$ = GetKeyPress$

        End If

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'       TradeKickers Subroutine
'----------------------------------------
'...explanation...
Sub TradeKickers (W1)

    Shared statsA$()
    Shared tradeA7$(), tradeFG$()

    Shared S!()
    Shared tradeAF!(), tradeAM!(), tradeLF!()
    Shared tradeZ2!()

    Shared tradePA%()
    Shared tradeB4%(), tradeB5%(), tradeB7%(), tradeB8%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 4, 1
    Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0)
    Color 7, 0
    Locate 4, 20
    Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1)
    Color 7, 0

    For I = 0 To 1:
        Locate I + 5, 1
        Print I; " "; tradeA7$(0, I)
        Locate I + 5, 20
        Print I; " "; tradeA7$(1, I)
    Next I

    Do

        Do
            Locate 17, 1
            Color tradePA%(0, 2), tradePA%(0, 1): Print statsA$(0);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I = Val(I$)
        Loop Until I >= 0 And I <= 1

        Print tradeA7$(0, I)

        Do
            Locate 19, 1
            Color tradePA%(1, 2), tradePA%(1, 1): Print statsA$(1);
            Color 7, 0
            Print " PLAYER: ";: Input I$
            I1 = Val(I$)
        Loop Until I >= 0 And I <= 1

        Print tradeA7$(1, I1)
        
        Do
            Locate 22, 1: Print "ANY CHANGE (YN) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA7$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA7$(1, I1)
    Color 7, 0
    Print
    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      FGA  ##                                   FGA  ##"; tradeZ2!(0, 4 * I + 4, 17); tradeZ2!(1, 4 * I1 + 4, 17)
    Locate , 5: Print Using "      FGM  ##                                   FGM  ##"; tradeZ2!(0, 4 * I + 3, 17); tradeZ2!(1, 4 * I1 + 3, 17)
    Locate , 5: Print Using " ###  FG% "; tradeB4%(0, I) * 5 + 25;

    If tradeZ2!(0, 4 * I + 4, 17) > 0 Then Print Using "### "; tradeZ2!(0, I * 4 + 3, 17) / tradeZ2!(0, I * 4 + 4, 17) * 100; Else Print " -.-";

    Print Using "                             ###  FG% "; tradeB4%(1, I1) * 5 + 25;

    If tradeZ2!(1, 4 * I1 + 4, 17) > 0 Then Print Using "### "; tradeZ2!(1, I1 * 4 + 3, 17) / tradeZ2!(1, I1 * 4 + 4, 17) * 100 Else Print " -.-"

    Locate , 5: Print Using "      EPA  ##                                   EPA  ##"; tradeZ2!(0, 4 * I + 6, 17); tradeZ2!(1, 4 * I1 + 6, 17)
    Locate , 5: Print Using "      EPM  ##                                   EPM  ##"; tradeZ2!(0, 4 * I + 5, 17); tradeZ2!(1, 4 * I1 + 5, 17)
    Locate , 5: Print Using " ###  EP% "; tradeB7%(0, I);

    If tradeZ2!(0, 4 * I + 6, 17) > 0 Then Print Using "### "; tradeZ2!(0, 4 * I + 5, 17) / tradeZ2!(0, 4 * I + 6, 17) * 100; Else Print " -.-";

    Print Using "                             ###  EP% "; tradeB7%(1, I1);

    If tradeZ2!(1, 4 * I1 + 6, 17) > 0 Then Print Using "### "; tradeZ2!(1, 4 * I1 + 5, 17) / tradeZ2!(1, 4 * I1 + 6, 17) * 100 Else Print " -.-"

    I$ = GetKeyPress$

    Swap tradeA7$(0, I), tradeA7$(1, I1)
    Swap tradeB4%(0, I), tradeB4%(1, I1)
    Swap tradeLF!(0, I), tradeLF!(1, I1)

    For I2 = 0 To 4
        Swap tradeAM!(0, I, I2), tradeAM!(1, I1, I2)
        Swap tradeAF!(0, I, I2), tradeAF!(1, I1, I2)
    Next I2

    Swap tradeB5%(0, I), tradeB5%(1, I1)
    Swap tradeB7%(0, I), tradeB7%(1, I1)
    Swap tradeB8%(0, I), tradeB8%(1, I1)
    Swap tradeFG$(J, I), tradeFG$(J, I1)

    If S!(0) = 1 Or S!(1) = 1 Or W1 = 0 Then
        tradeZ2!(0, 4 * I + 3, 17) = 0: tradeZ2!(1, 4 * I1 + 3, 17) = 0
        tradeZ2!(0, 4 * I + 4, 17) = 0: tradeZ2!(1, 4 * I1 + 4, 17) = 0
        tradeZ2!(0, 4 * I + 5, 17) = 0: tradeZ2!(1, 4 * I1 + 5, 17) = 0
        tradeZ2!(0, 4 * I + 6, 17) = 0: tradeZ2!(1, 4 * I1 + 6, 17) = 0
    Else
        Swap tradeZ2!(0, 4 * I + 3, 17), tradeZ2!(1, 4 * I1 + 3, 17)
        Swap tradeZ2!(0, 4 * I + 4, 17), tradeZ2!(1, 4 * I1 + 4, 17)
        Swap tradeZ2!(0, 4 * I + 5, 17), tradeZ2!(1, 4 * I1 + 5, 17)
        Swap tradeZ2!(0, 4 * I + 6, 17), tradeZ2!(1, 4 * I1 + 6, 17)
    End If

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 3
    Locate , 7
    Color tradePA%(0, 2), tradePA%(0, 1): Print tradeA7$(0, I);
    Color 7, 0
    Locate , 49
    Color tradePA%(1, 2), tradePA%(1, 1): Print tradeA7$(1, I1)
    Color 7, 0
    Print

    Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
    Print
    Locate , 5: Print Using "      FGA  ##                                   FGA  ##"; tradeZ2!(0, 4 * I + 4, 17); tradeZ2!(1, 4 * I1 + 4, 17)
    Locate , 5: Print Using "      FGM  ##                                   FGM  ##"; tradeZ2!(0, 4 * I + 3, 17); tradeZ2!(1, 4 * I1 + 3, 17)
    Locate , 5: Print Using " ###  FG% "; tradeB4%(0, I) * 5 + 25;

    If tradeZ2!(0, 4 * I + 4, 17) > 0 Then Print Using "### "; tradeZ2!(0, I * 4 + 3, 17) / tradeZ2!(0, I * 4 + 4, 17) * 100; Else Print " -.-";

    Print Using "                             ###  FG% "; tradeB4%(1, I1) * 5 + 25;

    If tradeZ2!(1, 4 * I1 + 4, 17) > 0 Then Print Using "### "; tradeZ2!(1, I1 * 4 + 3, 17) / tradeZ2!(1, I1 * 4 + 4, 17) * 100 Else Print " -.-"

    Locate , 5: Print Using "      EPA  ##                                   EPA  ##"; tradeZ2!(0, 4 * I + 6, 17); tradeZ2!(1, 4 * I1 + 6, 17)
    Locate , 5: Print Using "      EPM  ##                                   EPM  ##"; tradeZ2!(0, 4 * I + 5, 17); tradeZ2!(1, 4 * I1 + 5, 17)
    Locate , 5: Print Using " ###  EP% "; tradeB7%(0, I);

    If tradeZ2!(0, 4 * I + 6, 17) > 0 Then Print Using "### "; tradeZ2!(0, 4 * I + 5, 17) / tradeZ2!(0, 4 * I + 6, 17) * 100; Else Print " -.-";

    Print Using "                             ###  EP% "; tradeB7%(1, I1);

    If tradeZ2!(1, 4 * I1 + 6, 17) > 0 Then Print Using "### "; tradeZ2!(1, 4 * I1 + 5, 17) / tradeZ2!(1, 4 * I1 + 6, 17) * 100 Else Print " -.-"

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'     PlayerStatsPrompt Subroutine
'----------------------------------------
'...explanation...
Sub PlayerStatsPrompt (W1)

    Shared tradePA%()

    Do

        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Locate 5, 5
        Color 15: Print "T";: Color 3: Print "RADE PLAYERS' REPLAY STATS"
        Locate , 5: Color 15: Print "Z";: Color 3: Print "ERO PLAYERS' STATS"
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "T" Or UCase$(I$) = "Z"

    If UCase$(I$) = "T" Then W1 = 1
    If UCase$(I$) = "Z" Then W1 = 0

End Sub

'----------------------------------------
'       DoTheTrade Subroutine
'----------------------------------------
'...explanation...
Sub DoTheTrade (tradeIdx)

    Shared statsA$(), statsAA$()
    Shared lookyDI$(), lookyDS$()
    Shared NY$(), tradeYN$()
    Shared tradeA1$(), tradeA2$(), tradeA3$(), tradeA4$(), tradeA5$(), tradeA6$(), tradeA7$()

    Shared tradeT!(), YC!()

    Shared tradeA0%(), tradeA1%(), tradeA2%(), tradeA3%(), tradeA4%()
    Shared tradeA5%(), tradeA6%(), tradeA7%(), tradeA8%(), tradeA9%()
    Shared tradeB%(), tradeB1%(), tradeB2%(), tradeB3%(), tradeB4%()
    Shared tradeB5%(), tradeB6%(), tradeB7%(), tradeB8%()
    Shared tradeDI%(), tradeDS%(), tradePA%(), tradeQX%()
    Shared tradeYC%()

    tradeYC%(tradeIdx) = YC!(tradeIdx)
    S0 = 0
    S1 = 0
    S2 = 0
    S3 = 0

    For I = 0 To 9: S0 = S0 + tradeA0%(tradeIdx, I): S2 = S2 + tradeA2%(tradeIdx, I): Next

    S4 = 100 / S0

    For I = 0 To 9:
        If tradeA0%(tradeIdx, I) <> 0 Then tradeA0%(tradeIdx, I) = Int(S4 * tradeA0%(tradeIdx, I) + .5) + S1: S1 = tradeA0%(tradeIdx, I)
        If tradeA0%(tradeIdx, I) > 100 Then tradeA0%(tradeIdx, I) = 100
    Next I

    For I = 0 To 5: S2 = S2 + tradeA4%(tradeIdx, I): Next

    S5 = 100 / S2

    For I = 0 To 9:
        If tradeA2%(tradeIdx, I) <> 0 Then tradeA2%(tradeIdx, I) = Int(S5 * tradeA2%(tradeIdx, I) + .5) + S3: S3 = tradeA2%(tradeIdx, I)
        If tradeA2%(tradeIdx, I) > 100 Then tradeA2%(tradeIdx, I) = 100
    Next I

    For I = 0 To 5:
        If tradeA4%(tradeIdx, I) <> 0 Then tradeA4%(tradeIdx, I) = Int(S5 * tradeA4%(tradeIdx, I) + .5) + S3: S3 = tradeA4%(tradeIdx, I)
        If tradeA4%(tradeIdx, I) > 100 Then tradeA4%(tradeIdx, I) = 100
    Next I

    S6 = 0
    S7 = 0

    For I = 0 To 3: S6 = S6 + tradeA6%(tradeIdx, I): Next

    S8 = 100 / S6

    For I = 0 To 3:
        If tradeA6%(tradeIdx, I) <> 0 Then tradeA6%(tradeIdx, I) = Int(S8 * tradeA6%(tradeIdx, I) + .5) + S7: S7 = tradeA6%(tradeIdx, I)
        If tradeA6%(tradeIdx, I) > 100 Then tradeA6%(tradeIdx, I) = 100
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 2: G0 = G0 + tradeA9%(tradeIdx, I): G3 = G3 + tradeB1%(tradeIdx, I): Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 2:
        If tradeA9%(tradeIdx, I) <> 0 Then tradeA9%(tradeIdx, I) = Int(G2 * tradeA9%(tradeIdx, I) + .5) + G1: G1 = tradeA9%(tradeIdx, I)
        If tradeA9%(tradeIdx, I) > 100 Then tradeA9%(tradeIdx, I) = 100
    Next

    For I = 0 To 2:
        If tradeB1%(tradeIdx, I) <> 0 Then tradeB1%(tradeIdx, I) = Int(G5 * tradeB1%(tradeIdx, I) + .5) + G4: G4 = tradeB1%(tradeIdx, I)
        If tradeB1%(tradeIdx, I) > 100 Then tradeB1%(tradeIdx, I) = 100
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 1: G0 = G0 + tradeB5%(tradeIdx, I): G3 = G3 + tradeB8%(tradeIdx, I): Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 1:
        If tradeB5%(tradeIdx, I) <> 0 Then tradeB5%(tradeIdx, I) = Int(G2 * tradeB5%(tradeIdx, I) + .5) + G1: G1 = tradeB5%(tradeIdx, I)
        If tradeB5%(tradeIdx, I) > 100 Then tradeB5%(tradeIdx, I) = 100
    Next

    For I = 0 To 1:
        If tradeB8%(tradeIdx, I) <> 0 Then tradeB8%(tradeIdx, I) = Int(G5 * tradeB8%(tradeIdx, I) + .5) + G4: G4 = tradeB8%(tradeIdx, I)
        If tradeB8%(tradeIdx, I) > 100 Then tradeB8%(tradeIdx, I) = 100
    Next

    For I = 9 To 0 Step -1:
        If tradeA0%(tradeIdx, I) <> 0 Then tradeA0%(tradeIdx, I) = 100: Exit For
    Next

    For I = 5 To 0 Step -1:
        If tradeA4%(tradeIdx, I) <> 0 Then tradeA4%(tradeIdx, I) = 100: Exit For
    Next

    For I = 3 To 0 Step -1:
        If tradeA6%(tradeIdx, I) <> 0 Then tradeA6%(tradeIdx, I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:
        If tradeA9%(tradeIdx, I) <> 0 Then tradeA9%(tradeIdx, I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:
        If tradeB1%(tradeIdx, I) <> 0 Then tradeB1%(tradeIdx, I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If tradeB5%(tradeIdx, I) <> 0 Then tradeB5%(tradeIdx, I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If tradeB8%(tradeIdx, I) <> 0 Then tradeB8%(tradeIdx, I) = 100: Exit For
    Next

    teamYear$ = RTrim$(tradeYN$(tradeIdx))

    Open diskPaths$(0) + "FOOT." + teamYear$ For Random As #1 Len = 934

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

    LSet Q$(1) = statsA$(tradeIdx)
    LSet Q$(2) = statsAA$(tradeIdx)

    For I = 0 To 7: LSet Q$(I + 3) = MKI$(tradeB6%(tradeIdx, I)): Next I
    For I = 0 To 4: LSet Q$(I + 11) = MKI$(tradePA%(tradeIdx, I)): Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = tradeA1$(tradeIdx, I)
        LSet Q$(I * 5 + 17) = MKI$(tradeA0%(tradeIdx, I))
        LSet Q$(I * 5 + 18) = MKI$(tradeA1%(tradeIdx, I))
        LSet Q$(I * 5 + 19) = MKI$(tradeA2%(tradeIdx, I))
        LSet Q$(I * 5 + 20) = MKI$(tradeA3%(tradeIdx, I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = tradeA2$(tradeIdx, I)
        LSet Q$(I * 3 + 67) = MKI$(tradeA4%(tradeIdx, I))
        LSet Q$(I * 3 + 68) = MKI$(tradeA5%(tradeIdx, I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = tradeA3$(tradeIdx, I)
        LSet Q$(I * 5 + 85) = MKI$(tradeA6%(tradeIdx, I))
        LSet Q$(I * 5 + 86) = MKI$(tradeA7%(tradeIdx, I))
        LSet Q$(I * 5 + 87) = MKI$(tradeA8%(tradeIdx, I))
        LSet Q$(I * 5 + 88) = MKI$(tradeQX%(tradeIdx, I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = tradeA4$(tradeIdx, I)
        LSet Q$(I * 3 + 105) = MKI$(tradeA9%(tradeIdx, I))
        LSet Q$(I * 3 + 106) = MKI$(tradeB%(tradeIdx, I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = tradeA5$(tradeIdx, I)
        LSet Q$(I * 3 + 114) = MKI$(tradeB1%(tradeIdx, I))
        LSet Q$(I * 3 + 115) = MKI$(tradeB2%(tradeIdx, I))
    Next I

    LSet Q$(122) = tradeA6$(tradeIdx, 0)
    LSet Q$(123) = MKI$(tradeB3%(tradeIdx, 0))
    LSet Q$(124) = tradeA7$(tradeIdx, 0)
    LSet Q$(125) = MKI$(tradeB4%(tradeIdx, 0))
    LSet Q$(126) = MKI$(tradeB5%(tradeIdx, 0))
    LSet Q$(127) = MKI$(tradeB7%(tradeIdx, 0))
    LSet Q$(128) = MKI$(tradeB8%(tradeIdx, 0))
    LSet Q$(129) = tradeA7$(tradeIdx, 1)
    LSet Q$(130) = MKI$(tradeB4%(tradeIdx, 1))
    LSet Q$(131) = MKI$(tradeB5%(tradeIdx, 1))
    LSet Q$(132) = MKI$(tradeB7%(tradeIdx, 1))
    LSet Q$(133) = MKI$(tradeB8%(tradeIdx, 1))
    LSet Q$(134) = MKI$(tradeYC%(tradeIdx))

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = lookyDS$(tradeIdx, I)
        LSet Q$(I * 2 + 136) = MKI$(tradeDS%(tradeIdx, I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = lookyDI$(tradeIdx, I)
        LSet Q$(I * 2 + 166) = MKI$(tradeDI%(tradeIdx, I))
    Next I

    Put #1, tradeT!(tradeIdx)

    Close #1

    For I = 0 To 9
        tradeA0%(tradeIdx, I) = 0
        tradeA1%(tradeIdx, I) = 0
        tradeA2%(tradeIdx, I) = 0
        tradeA3%(tradeIdx, I) = 0
        tradeA1$(tradeIdx, I) = ""
    Next

    For I = 0 To 5
        tradeA4%(tradeIdx, I) = 0
        tradeA5%(tradeIdx, I) = 0
        tradeA2$(tradeIdx, I) = ""
    Next

    For I = 0 To 3
        tradeA6%(tradeIdx, I) = 0
        tradeA7%(tradeIdx, I) = 0
        tradeA8%(tradeIdx, I) = 0
        tradeQX%(tradeIdx, I) = 0
        tradeA3$(tradeIdx, I) = ""
    Next

    For I = 0 To 2
        tradeA9%(tradeIdx, I) = 0
        tradeB%(tradeIdx, I) = 0
        tradeB1%(tradeIdx, I) = 0
        tradeB2%(tradeIdx, I) = 0
        tradeA4$(tradeIdx, I) = ""
        tradeA5$(tradeIdx, I) = ""
    Next

    tradeA6$(tradeIdx, 0) = ""
    tradeB3%(tradeIdx, I) = 0

    For I = 0 To 1
        tradeB4%(tradeIdx, I) = 0
        tradeB5%(tradeIdx, I) = 0
        tradeB7%(tradeIdx, I) = 0
        tradeB8%(tradeIdx, I) = 0
        tradeA7$(tradeIdx, I) = ""
    Next


End Sub

'----------------------------------------
'       SaveTradeStats Subroutine
'----------------------------------------
'...explanation...
Sub SaveTradeStats (J)

    Shared statsA$(), statsAA$()
    Shared tradeF$(), tradeFG$(), tradeKR$()
    Shared lookyDI$(), lookyDS$()
    Shared PU$(), TN$()
    Shared tradeLC$(), tradeLF$(), tradeLI$(), tradeLK$(), tradeLP$(), tradeLR$()
    Shared tradePK$(), tradePR$()
    Shared tradeQB$(), tradeRB$(), tradeWR$()

    Shared tradeAF!(), tradeAM!()
    Shared tradeLC!(), tradeLF!(), tradeLI!(), tradeLK!(), tradeLP!(), tradeLR!()
    Shared tradePK!()
    Shared tradeZ!(), tradeZ1!(), tradeZ2!()

    Shared tradePA%()
    Shared tradeGI%(), tradeGS%()

    330 '
    Locate 12, 1

    Print "SAVING ";: Color tradePA%(J, 2), tradePA%(J, 1)

    Print statsA$(J);

    Color 7, 0: Print " STATS": Z = 0

    Open diskPaths$(1) + tradeF$(J) For Output As #1

    Print #1, TN$(J)

    For I = 0 To 9: Print #1, tradeRB$(J, I): Next
    For I = 0 To 5: Print #1, tradeWR$(J, I): Next
    For I = 0 To 3: Print #1, tradeQB$(J, I): Next
    For I = 0 To 2: Print #1, tradeKR$(J, I): Next
    For I = 0 To 2: Print #1, tradePR$(J, I): Next

    Print #1, PU$(J)

    For I = 0 To 1: Print #1, tradeFG$(J, I): Next

    For X = 0 To 13
        For Y = 0 To 17:
            Print #1, tradeZ2!(J, X, Y)
        Next
    Next

    For X = 0 To 38: Print #1, tradeZ!(J, X): Next
    For X = 0 To 38: Print #1, tradeZ1!(J, X): Next
    For X = 0 To 9: Print #1, lookyDI$(J, X): For J1 = 0 To 2: Print #1, tradeGI%(J, X, J1): Next: Next
    For X = 0 To 14: Print #1, lookyDS$(J, X): For J1 = 0 To 1: Print #1, tradeGS%(J, X, J1): Next: Next
    For I = 0 To 9: Print #1, tradeLR!(J, I): For J1 = 0 To 1: Print #1, tradeLR$(J, I, J1): Next: Next
    For I = 0 To 15: Print #1, tradeLC!(J, I): For J1 = 0 To 1: Print #1, tradeLC$(J, I, J1): Next: Next
    For I = 0 To 3: Print #1, tradeLP!(J, I): For J1 = 0 To 1: Print #1, tradeLP$(J, I, J1): Next: Next
    For I = 0 To 2: Print #1, tradeLK!(J, I): For J1 = 0 To 1: Print #1, tradeLK$(J, I, J1): Next: Next
    For I = 0 To 2: Print #1, tradePK!(J, I): For J1 = 0 To 1: Print #1, tradePK$(J, I, J1): Next: Next
    For I = 0 To 1: Print #1, tradeLF!(J, I): Print #1, tradeLF$(J, I): For I1 = 0 To 4: Print #1, tradeAM!(J, I, I1): Print #1, tradeAF!(J, I, I1): Next: Next
    For I = 0 To 9: Print #1, tradeLI!(J, I): Print #1, tradeLI$(J, I, 0): Print #1, tradeLI$(J, I, 1): Next

    Close #1

End Sub

'----------------------------------------
'       GetDiskID_TEAMA Subroutine
'----------------------------------------
'...explanation...
Sub GetDiskID_TEAM (J)

    Shared tradeT!()
    Shared tradePA%()

    Shared statsA$(), statsAA$(), NY$(), tradeYN$()

    If J = 0 Then team$ = "A"
    If J = 1 Then team$ = "B"

    Do
        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4

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
        Locate , 12: Print "ENTER DISK ID FOR TEAM "; team$
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 38: Input NY$(J)

        If NY$(J) = "" Then

            Call SelectTeam_TRADE(J, NY$(J), numberTeams)

        Else

            tradeYN$(J) = RTrim$(NY$(J))
            Erase teamNames$
            numberTeams = 0

            Open diskPaths$(0) + "FOOT." + tradeYN$(J) For Random As #1 Len = 934

            Y = LOF(1)

            Field #1, 15 As Q$(0), 640 As X$

            For I1 = 1 To Y / 934

                Get #1, I1

                For I2 = 1 To 15
                    If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                Next I2

                If nullTeam <> 1 Then
                    numberTeams = numberTeams + 1
                    teamNames$(numberTeams) = Q$(0)
                    teamIndex%(numberTeams) = I1
                End If

            Next I1

            Close 1

            Call SelectTeam_TRADE(J, NY$(J), numberTeams)

        End If

        statsA$(J) = RTrim$(Q$(1))
        statsAA$(J) = Q$(2)
        Call ReadTeam(J)

        If BO% <> 1 Then

            Locate 2, 33
            Color tradePA%(J, 2), tradePA%(J, 1): Print statsA$(J)

            Color 15, 4

            Do
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Else
            BO% = 0
        End If

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'       LoadStatsForTrade Subroutine
'----------------------------------------
'...explanation...
Sub LoadStatsForTrade (J)

    Shared tradeF$(), tradeFG$(), tradeKR$()
    Shared lookyDI$(), lookyDS$()
    Shared PU$(), SA$(), TN$()
    Shared tradeLC$(), tradeLF$(), tradeLI$(), tradeLK$(), tradeLP$(), tradeLR$()
    Shared tradePK$(), tradePR$()
    Shared tradeQB$(), tradeRB$(), tradeWR$()

    Shared S!()
    Shared tradeAF!(), tradeAM!()
    Shared tradeLC!(), tradeLF!(), tradeLI!(), tradeLK!(), tradeLP!(), tradeLR!()
    Shared tradePK!()
    Shared tradeZ!(), tradeZ1!(), tradeZ2!()

    Shared tradeGI%(), tradeGS%()

    100 '
    For XX = 4 To 11:
        If Mid$(SA$(J), XX, 1) = " " Or Mid$(SA$(J), XX, 1) = "/" Or Mid$(SA$(J), XX, 1) = "." Then Mid$(SA$(J), XX, 1) = "_"
    Next XX

    For XX = 12 To 14:
        If Mid$(SA$(J), XX, 2) = "ST" Then Mid$(SA$(J), 9, 1) = "_": Mid$(SA$(J), 10, 2) = "ST"
    Next XX

    QQ = 8

    For XX = 11 To 4 Step -1:
        If Mid$(SA$(J), XX, 1) = "_" Then
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

    YR$ = Left$(SA$(J), 2)
    tradeF$(J) = Mid$(SA$(J), 4, QQ) + "." + YR$

    If Not _FileExists(diskPaths$(1) + tradeF$(J)) Then
        S!(J) = 1
    Else
        Open diskPaths$(1) + tradeF$(J) For Input As #1
        Input #1, TN$(J)
        For I = 0 To 9: Input #1, tradeRB$(J, I): Next
        For I = 0 To 5: Input #1, tradeWR$(J, I): Next
        For I = 0 To 3: Input #1, tradeQB$(J, I): Next
        For I = 0 To 2: Input #1, tradeKR$(J, I): Next
        For I = 0 To 2: Input #1, tradePR$(J, I): Next
        Input #1, PU$(J)
        For I = 0 To 1: Input #1, tradeFG$(J, I): Next

        For X = 0 To 13
            For Y = 0 To 17:
                Input #1, tradeZ2!(J, X, Y)
            Next
        Next

        For X = 0 To 38: Input #1, tradeZ!(J, X): Next
        For X = 0 To 38: Input #1, tradeZ1!(J, X): Next
        For X = 0 To 9: Input #1, lookyDI$(J, X): For J1 = 0 To 2: Input #1, tradeGI%(J, X, J1): Next: Next
        For X = 0 To 14: Input #1, lookyDS$(J, X): For J1 = 0 To 1: Input #1, tradeGS%(J, X, J1): Next: Next
        For I = 0 To 9: Input #1, tradeLR!(J, I): For J1 = 0 To 1: Input #1, tradeLR$(J, I, J1): Next: Next
        For I = 0 To 15: Input #1, tradeLC!(J, I): For J1 = 0 To 1: Input #1, tradeLC$(J, I, J1): Next: Next
        For I = 0 To 3: Input #1, tradeLP!(J, I): For J1 = 0 To 1: Input #1, tradeLP$(J, I, J1): Next: Next
        For I = 0 To 2: Input #1, tradeLK!(J, I): For J1 = 0 To 1: Input #1, tradeLK$(J, I, J1): Next: Next
        For I = 0 To 2: Input #1, tradePK!(J, I): For J1 = 0 To 1: Input #1, tradePK$(J, I, J1): Next: Next
        For I = 0 To 1: Input #1, tradeLF!(J, I): Input #1, tradeLF$(J, I): For I1 = 0 To 4: Input #1, tradeAM!(J, I, I1): Input #1, tradeAF!(J, I, I1): Next: Next
        For I = 0 To 9: Input #1, tradeLI!(J, I): Input #1, tradeLI$(J, I, 0): Input #1, tradeLI$(J, I, 1): Next
        Close #1
    End If

End Sub

'----------------------------------------
'       SelectTeam_TRADE Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_TRADE (J, yearNumber$, numberTeams)
    Shared tradeT!()
    Shared tradePA%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        tradeT!(J) = teamIndex%(Selection%)
        teamYear$ = RTrim$(yearNumber$)

        Open diskPaths$(0) + "FOOT." + yearNumber$ For Random As #1 Len = 934

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

        Get #1, tradeT!(J)

    End If

End Sub

'----------------------------------------
'           ReadTeam Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam (J)

    Dim C2!(15)

    Shared statsA$()
    Shared tradeA1$(), tradeA2$(), tradeA3$(), tradeA4$(), tradeA5$(), tradeA6$(), tradeA7$()
    Shared lookyDI$(), lookyDS$()
    Shared SA$()

    Shared YC!()

    Shared tradePA%()
    Shared tradeA0%(), tradeA1%(), tradeA2%(), tradeA3%(), tradeA4%()
    Shared tradeA5%(), tradeA6%(), tradeA7%(), tradeA8%(), tradeA9%()
    Shared tradeB%(), tradeB1%(), tradeB2%(), tradeB3%(), tradeB4%()
    Shared tradeB5%(), tradeB6%(), tradeB7%(), tradeB8%()
    Shared tradeDI%(), tradeDS%()
    Shared tradeQX%(), tradeYC%()

    For I = 0 To 7: tradeB6%(J, I) = CVI(Q$(I + 3)): Next I

    For I = 0 To 4: tradePA%(J, I) = CVI(Q$(I + 11)): Next I

    For I = 0 To 9
        tradeA1$(J, I) = Q$(I * 5 + 16)
        tradeA0%(J, I) = CVI(Q$(I * 5 + 17))
        tradeA1%(J, I) = CVI(Q$(I * 5 + 18))
        tradeA2%(J, I) = CVI(Q$(I * 5 + 19))
        tradeA3%(J, I) = CVI(Q$(I * 5 + 20))
    Next I

    For I = 0 To 5
        tradeA2$(J, I) = Q$(I * 3 + 66)
        tradeA4%(J, I) = CVI(Q$(I * 3 + 67))
        tradeA5%(J, I) = CVI(Q$(I * 3 + 68))
    Next I

    For I = 0 To 3
        tradeA3$(J, I) = Q$(I * 5 + 84)
        tradeA6%(J, I) = CVI(Q$(I * 5 + 85))
        tradeA7%(J, I) = CVI(Q$(I * 5 + 86))
        tradeA8%(J, I) = CVI(Q$(I * 5 + 87))
        tradeQX%(J, I) = CVI(Q$(I * 5 + 88))
    Next I

    For I = 0 To 2
        tradeA4$(J, I) = Q$(I * 3 + 104)
        tradeA9%(J, I) = CVI(Q$(I * 3 + 105))
        tradeB%(J, I) = CVI(Q$(I * 3 + 106))
    Next I

    For I = 0 To 2
        tradeA5$(J, I) = Q$(I * 3 + 113)
        tradeB1%(J, I) = CVI(Q$(I * 3 + 114))
        tradeB2%(J, I) = CVI(Q$(I * 3 + 115))
    Next I

    tradeA6$(J, 0) = Q$(122)
    tradeB3%(J, 0) = CVI(Q$(123))
    tradeA7$(J, 0) = Q$(124)
    tradeB4%(J, 0) = CVI(Q$(125))
    tradeB5%(J, 0) = CVI(Q$(126))
    tradeB7%(J, 0) = CVI(Q$(127))
    tradeB8%(J, 0) = CVI(Q$(128))
    tradeA7$(J, 1) = Q$(129)
    tradeB4%(J, 1) = CVI(Q$(130))
    tradeB5%(J, 1) = CVI(Q$(131))
    tradeB7%(J, 1) = CVI(Q$(132))
    tradeB8%(J, 1) = CVI(Q$(133))
    YC!(J) = CVI(Q$(134))

    For I = 0 To 14
        lookyDS$(J, I) = Q$(I * 2 + 135)
        tradeDS%(J, I) = CVI(Q$(I * 2 + 136))
    Next I

    For I = 0 To 9
        lookyDI$(J, I) = Q$(I * 2 + 165)
        tradeDI%(J, I) = CVI(Q$(I * 2 + 166))
    Next I

    SA$(J) = statsA$(J)

    Close #1

    For I = 0 To 15: C2!(I) = 0: Next

    For I = 9 To 1 Step -1
        If tradeA0%(J, I) <> 0 Then
            tradeA0%(J, I) = tradeA0%(J, I) - tradeA0%(J, I - 1)
        End If
    Next

    For I = 0 To 5: tradeA2%(J, I + 10) = tradeA4%(J, I): Next

    U = 0

    For I = 1 To 15
        If tradeA2%(J, I) = 0 Then
            U = U + 1
        Else
            C2!(I) = tradeA2%(J, I) - tradeA2%(J, I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 15: tradeA2%(J, I) = C2!(I): Next

    For I = 0 To 5: tradeA4%(J, I) = tradeA2%(J, I + 10): Next

    For I = 3 To 1 Step -1
        If tradeA6%(J, I) <> 0 Then
            tradeA6%(J, I) = tradeA6%(J, I) - tradeA6%(J, I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If tradeA9%(J, I) <> 0 Then
            tradeA9%(J, I) = tradeA9%(J, I) - tradeA9%(J, I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If tradeB1%(J, I) <> 0 Then
            tradeB1%(J, I) = tradeB1%(J, I) - tradeB1%(J, I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If tradeB5%(J, I) <> 0 Then
            tradeB5%(J, I) = tradeB5%(J, I) - tradeB5%(J, I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If tradeB8%(J, I) <> 0 Then
            tradeB8%(J, I) = tradeB8%(J, I) - tradeB8%(J, I - 1)
        End If
    Next

End Sub
