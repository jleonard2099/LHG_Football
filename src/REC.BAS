Sub REC (recordOption%)

    _Title "3-in-1 Football - Manage Records"

    Shared BRC$(), TRC$()
    Shared BRC!(), TRC!()

    Select Case recordOption%
        Case 1
            recordsFile$ = "RECORDS.PFB"
        Case 2,3
            recordsFile$ = "RECORDS.CFB"
    End Select

    J = 6

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

    Call TITLE_LEADERS

    If  Not _FileExists(diskPaths$(1) + recordsFile$) Then

        Locate 9, 30: Print "ERROR::: UNABLE TO LOCATE RECORDS"
        I$ = GetKeyPress

    Else

        Open diskPaths$(1) + recordsFile$ For Input As #1

        For I = 0 To 50
            Input #1, BRC!(I)
            Input #1, BRC$(I, 0)
            Input #1, BRC$(I, 1)
            Input #1, BRC$(I, 2)
        Next

        For I = 0 To 50
            Input #1, TRC!(I)
            Input #1, TRC$(I)
        Next

        Close 1

        Do

            Color 0, 0
            Cls

            Locate 9, 30: Color 3, 1: Print "SEASON RECORDS MENU": Print
            Locate , 15: Color 15: Print "V";: Color 7: Print "IEW RECORDS"
            Locate , 15: Color 15: Print "P";: Color 7: Print "RINT RECORDS"
            Locate , 15: Color 15: Print "R";: Color 7: Print "ETURN TO MAIN MENU"

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then
                Select Case UCase$(I$)
                    Case "V"
                        Call ViewRecords

                    Case "P"
                        Call PrintRecords
                End Select
            End If

        Loop Until UCase$(I$) = "R" Or I$ = Chr$(27)

    End If

    Cls

End Sub

'----------------------------------------
'       ViewRecords Subroutine
'----------------------------------------
'...explanation...
Sub ViewRecords ()

    Shared A$(), BRC$()
    Shared TP$(), TP1$(), TRC$()
    Shared BRC!(), TRC!()

    Color 0, 0
    Cls
    J = 20

    Locate 1, 1: Color 15, 4
    Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
    For II = 1 To J: 
        Color 15, 4: Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 4: Print String$(2, 177)
    Next

    Color 15, 4: Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 2, 23: Print "******* INDIVIDUAL RECORDS *******"
    Locate , 4: Print "CATEGORY                PLAYER           TEAM            RECORD"

    Color 7

    For I = 0 To 2
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(3); Tab(28); BRC$(3, 0); Tab(42); BRC$(3, 1); Tab(62); Using "##.#"; BRC!(3)
    Locate , 4: Print TP$(4); Tab(28); BRC$(4, 0); Tab(42); BRC$(4, 1); Tab(62); Using " ###"; BRC!(4);: Print BRC$(4, 2)

    For I = 5 To 7
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(8); Tab(28); BRC$(8, 0); Tab(42); BRC$(8, 1); Tab(62); Using "##.#"; BRC!(8)
    Locate , 4: Print TP$(9); Tab(28); BRC$(9, 0); Tab(42); BRC$(9, 1); Tab(62); Using " ###"; BRC!(9);: Print BRC$(9, 2)

    For I = 10 To 14
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(15); Tab(28); BRC$(15, 0); Tab(42); BRC$(15, 1); Tab(62); Using "##.#"; BRC!(15)
    Locate , 4: Print TP$(16); Tab(28); BRC$(16, 0); Tab(42); BRC$(16, 1); Tab(61); Using "###.#"; BRC!(16)
    Locate , 4: Print TP$(17); Tab(28); BRC$(17, 0); Tab(42); BRC$(17, 1); Tab(62); Using " ###"; BRC!(17);: Print BRC$(17, 2)

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$
    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    For I = 18 To 24
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(25); Tab(28); BRC$(25, 0); Tab(42); BRC$(25, 1); Tab(62); Using "##.#"; BRC!(25)
    Locate , 4: Print TP$(26); Tab(28); BRC$(26, 0); Tab(42); BRC$(26, 1); Tab(62); Using " ###"; BRC!(26);: Print BRC$(26, 2)

    For I = 27 To 29
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(30); Tab(28); BRC$(30, 0); Tab(42); BRC$(30, 1); Tab(62); Using "##.#"; BRC!(30)
    Locate , 4: Print TP$(31); Tab(28); BRC$(31, 0); Tab(42); BRC$(31, 1); Tab(62); Using " ###"; BRC!(31);: Print BRC$(31, 2)

    For I = 32 To 33
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(34); Tab(28); BRC$(34, 0); Tab(42); BRC$(34, 1); Tab(62); Using "##.#"; BRC!(34)

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$
    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    For I = 35 To 36
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(37); Tab(28); BRC$(37, 0); Tab(42); BRC$(37, 1); Tab(61); Using "###.#"; BRC!(37)
    Locate , 4: Print TP$(38); Tab(28); BRC$(38, 0); Tab(42); BRC$(38, 1); Tab(62); Using " ###"; BRC!(38);: Print BRC$(38, 2)

    For I = 39 To 40
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(41); Tab(28); BRC$(41, 0); Tab(42); BRC$(41, 1); Tab(61); Using "###.#"; BRC!(41)

    For I = 42 To 44
        Locate , 4: Print TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    Locate , 4: Print TP$(45); Tab(28); BRC$(45, 0); Tab(42); BRC$(45, 1); Tab(62); Using " ###"; BRC!(45);: Print BRC$(45, 2)

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$
    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 0, 0: Cls: J = 20

    Locate 1, 1: Color 15, 1
    Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
    For II = 1 To J: 
        Color 15, 1: Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 1: Print String$(2, 177)
    Next

    Color 15, 1
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 1: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15: Locate 2, 23: Print "******* OFFENSIVE TEAM RECORDS *******"
    Locate , 4: Print "CATEGORY                   TEAM                  RECORD            "
    Color 7
    Locate , 4: Print TP1$(0); Tab(28); TRC$(0); Tab(53); Using "###.#"; TRC!(0)
    Locate , 4: Print TP1$(1); Tab(28); TRC$(1); Tab(53); Using "#####"; TRC!(1)

    For I = 2 To 3
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    Locate , 4: Print TP1$(4); Tab(28); TRC$(4); Tab(53); Using "#####"; TRC!(4)

    For I = 5 To 6
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    Locate , 4: Print TP1$(7); Tab(28); TRC$(7); Tab(53); Using "#####"; TRC!(7)

    For I = 8 To 9
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    For I = 10 To 11
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I) * 100
    Next I

    Locate , 4: Print TP1$(12); Tab(28); TRC$(12); Tab(53); Using "#####"; TRC!(12)

    For I = 13 To 14
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$
    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    For I = 15 To 17
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    Locate , 4: Print TP1$(18); Tab(28); TRC$(18); Tab(53); Using "#####"; TRC!(18)
    Locate , 4: Print TP1$(19); Tab(28); TRC$(19); Tab(53); Using "###.#"; TRC!(19)

    For I = 20 To 22
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "#####"; TRC!(I)
    Next I

    Locate , 4: Print TP1$(23); Tab(28); TRC$(23); Tab(53); Using "###.#"; TRC!(23)
    Locate , 4: Print TP1$(24); Tab(28); TRC$(24); Tab(53); Using "###.#"; TRC!(24)

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 15: Locate 2, 23: Print "******* DEFENSIVE TEAM RECORDS *******"
    Locate , 4: Print "CATEGORY                   TEAM                  RECORD            "
    Color 7

    Locate , 4: Print TP1$(25); Tab(28); TRC$(25); Tab(53); Using "###.#"; TRC!(25)
    Locate , 4: Print TP1$(26); Tab(28); TRC$(26); Tab(53); Using "#####"; TRC!(26)

    For I = 27 To 29
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    Locate , 4: Print TP1$(30); Tab(28); TRC$(30); Tab(53); Using "#####"; TRC!(30)

    For I = 31 To 32
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    Locate , 4: Print TP1$(33); Tab(28); TRC$(33); Tab(53); Using "#####"; TRC!(33)

    For I = 34 To 35
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    For I = 36 To 37
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I) * 100
    Next

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$
    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4
    Locate , 4: Print TP1$(38); Tab(28); TRC$(38); Tab(53); Using "#####"; TRC!(38)
    Locate , 4: Print TP1$(39); Tab(28); TRC$(39); Tab(53); Using "###.#"; TRC!(39)
    Locate , 4: Print TP1$(40); Tab(28); TRC$(40); Tab(53); Using "#####"; TRC!(40)
    Locate , 4: Print TP1$(41); Tab(28); TRC$(41); Tab(53); Using "###.#"; TRC!(41)

    For I = 42 To 43
        Locate , 4: Print TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$
    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

End Sub

'----------------------------------------
'       PrintRecords Subroutine
'----------------------------------------
'...explanation...
Sub PrintRecords ()

    Shared A$(), BRC$()
    Shared TP$(), TP1$(), TRC$()
    Shared BRC!(), TRC!()

    3200 ' PRINT RECORDS
    LPrint Tab(23); "******* INDIVIDUAL RECORDS *******"
    LPrint
    LPrint "CATEGORY                   PLAYER           TEAM           RECORD"

    For I = 0 To 2
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(3); Tab(28); BRC$(3, 0); Tab(42); BRC$(3, 1); Tab(62); Using "##.#"; BRC!(3)
    LPrint TP$(4); Tab(28); BRC$(4, 0); Tab(42); BRC$(4, 1); Tab(62); Using " ###"; BRC!(4);: LPrint BRC$(4, 2)

    For I = 5 To 7
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(8); Tab(28); BRC$(8, 0); Tab(42); BRC$(8, 1); Tab(62); Using "##.#"; BRC!(8)
    LPrint TP$(9); Tab(28); BRC$(9, 0); Tab(42); BRC$(9, 1); Tab(62); Using " ###"; BRC!(9);: LPrint BRC$(9, 2)

    For I = 10 To 14
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(15); Tab(28); BRC$(15, 0); Tab(42); BRC$(15, 1); Tab(62); Using "##.#"; BRC!(15)
    LPrint TP$(16); Tab(28); BRC$(16, 0); Tab(42); BRC$(16, 1); Tab(61); Using "###.#"; BRC!(16)
    LPrint TP$(17); Tab(28); BRC$(17, 0); Tab(42); BRC$(17, 1); Tab(62); Using " ###"; BRC!(17);: LPrint BRC$(17, 2)

    For I = 18 To 24
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(25); Tab(28); BRC$(25, 0); Tab(42); BRC$(25, 1); Tab(62); Using "##.#"; BRC!(25)
    LPrint TP$(26); Tab(28); BRC$(26, 0); Tab(42); BRC$(26, 1); Tab(62); Using " ###"; BRC!(26);: LPrint BRC$(26, 2)

    For I = 27 To 29
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(30); Tab(28); BRC$(30, 0); Tab(42); BRC$(30, 1); Tab(62); Using "##.#"; BRC!(30)
    LPrint TP$(31); Tab(28); BRC$(31, 0); Tab(42); BRC$(31, 1); Tab(62); Using " ###"; BRC!(31);: LPrint BRC$(31, 2)

    For I = 32 To 33
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(34); Tab(28); BRC$(34, 0); Tab(42); BRC$(34, 1); Tab(62); Using "##.#"; BRC!(34)

    For I = 35 To 36
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(37); Tab(28); BRC$(37, 0); Tab(42); BRC$(37, 1); Tab(61); Using "###.#"; BRC!(37)
    LPrint TP$(38); Tab(28); BRC$(38, 0); Tab(42); BRC$(38, 1); Tab(62); Using " ###"; BRC!(38);: LPrint BRC$(38, 2)

    For I = 39 To 40
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(41); Tab(28); BRC$(41, 0); Tab(42); BRC$(41, 1); Tab(61); Using "###.#"; BRC!(41)

    For I = 42 To 44
        LPrint TP$(I); Tab(28); BRC$(I, 0); Tab(42); BRC$(I, 1); Tab(62); Using "####"; BRC!(I)
    Next

    LPrint TP$(45); Tab(28); BRC$(45, 0); Tab(42); BRC$(45, 1); Tab(62); Using " ###"; BRC!(45);: LPrint BRC$(45, 2)
    LPrint Chr$(12)
    LPrint Tab(23); "******* OFFENSIVE TEAM RECORDS *******"
    LPrint
    LPrint "CATEGORY                      TEAM                 RECORD            "
    LPrint TP1$(0); Tab(28); TRC$(0); Tab(53); Using "###.#"; TRC!(0)
    LPrint TP1$(1); Tab(28); TRC$(1); Tab(53); Using "#####"; TRC!(1)

    For I = 2 To 3
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    LPrint TP1$(4); Tab(28); TRC$(4); Tab(53); Using "#####"; TRC!(4)

    For I = 5 To 6
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    LPrint TP1$(7); Tab(28); TRC$(7); Tab(53); Using "#####"; TRC!(7)

    For I = 8 To 9
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    For I = 10 To 11
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I) * 100
    Next I

    LPrint TP1$(12); Tab(28); TRC$(12); Tab(53); Using "#####"; TRC!(12)

    For I = 13 To 14
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    For I = 15 To 17
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next I

    LPrint TP1$(18); Tab(28); TRC$(18); Tab(53); Using "#####"; TRC!(18)
    LPrint TP1$(19); Tab(28); TRC$(19); Tab(53); Using "###.#"; TRC!(19)

    For I = 20 To 22
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "#####"; TRC!(I)
    Next I

    LPrint TP1$(23); Tab(28); TRC$(23); Tab(53); Using "###.#"; TRC!(23)
    LPrint TP1$(24); Tab(28); TRC$(24); Tab(53); Using "###.#"; TRC!(24)
    LPrint
    LPrint
    LPrint Tab(23); "******* DEFENSIVE TEAM RECORDS *******"
    LPrint
    LPrint "CATEGORY                      TEAM                 RECORD            "
    LPrint TP1$(25); Tab(28); TRC$(25); Tab(53); Using "###.#"; TRC!(25)
    LPrint TP1$(26); Tab(28); TRC$(26); Tab(53); Using "#####"; TRC!(26)

    For I = 27 To 29
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    LPrint TP1$(30); Tab(28); TRC$(30); Tab(53); Using "#####"; TRC!(30)

    For I = 31 To 32
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    LPrint TP1$(33); Tab(28); TRC$(33); Tab(53); Using "#####"; TRC!(33)

    For I = 34 To 35
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    For I = 36 To 37
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I) * 100
    Next

    LPrint TP1$(38); Tab(28); TRC$(38); Tab(53); Using "#####"; TRC!(38)
    LPrint TP1$(39); Tab(28); TRC$(39); Tab(53); Using "###.#"; TRC!(39)
    LPrint TP1$(40); Tab(28); TRC$(40); Tab(53); Using "#####"; TRC!(40)
    LPrint TP1$(41); Tab(28); TRC$(41); Tab(53); Using "###.#"; TRC!(41)

    For I = 42 To 43
        LPrint TP1$(I); Tab(28); TRC$(I); Tab(53); Using "###.#"; TRC!(I)
    Next

    LPrint Chr$(12)

End Sub

