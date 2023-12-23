'----------------------------------------
'         Records_Menu Subroutine
'----------------------------------------
'This subroutine gives the user a number of
'menu selections for viewing, updating and
'printing records
Sub Records_Menu (menuChoice)

    _Title "3-in-1 Football - Analyze Records"

    ReDim menuChoices$(1 To 4)

    Select Case menuChoice

        Case 1
            recordsFile$ = "RECORDS.PFB"

        Case 2, 3
            recordsFile$ = "RECORDS.CFB"

    End Select

    If Not _FileExists(diskPaths$(1) + recordsFile$) Then

        MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(1) + recordsFile$ For Input As #1

        For I = 0 To 50
            Input #1, indRecords!(I)
            Input #1, indRecDesc$(I, 0)
            Input #1, indRecDesc$(I, 1)
            Input #1, indRecDesc$(I, 2)
        Next

        For I = 0 To 50
            Input #1, teamRecords!(I)
            Input #1, teamRecDesc$(I)
        Next

        Close #1

        Do

            Cls

            Call TITLE

            menuChoices$(1) = "VIEW SEASON RECORDS  "
            menuChoices$(2) = "PRINT SEASON RECORDS "
            menuChoices$(3) = "DELETE SEASON RECORDS"
            menuChoices$(4) = "RETURN TO GAME MENU  "
            Selection% = 1
            Count% = 4
            Start% = 1
            ExitCode% = 4
            FileFGColor% = 15: FileBGColor% = 1
            HiLiteFGColor% = 0: HiLiteBGColor% = 7
            Spacing% = 1
            Row% = 8
            menuWidth% = Len(menuChoices$(1))
            Column% = (80 - menuWidth% - 2) / 2
            Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
            _MouseHide

            Color , 0

            If ExitCode% <> 27 Then

                Select Case Selection%

                    Case 1:
                        Call ViewSeasonRecords

                    Case 2:
                        Call PrintSeasonRecords

                    Case 3:
                        If _FileExists(diskPaths$(1) + recordsFile$) Then Kill diskPaths$(1) + recordsFile$

                End Select

            End If

        Loop Until Selection% = 4 Or ExitCode% = 27

    End If

End Sub

'----------------------------------------
'       ViewSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the usere with
'viewing the records on the default display
Sub ViewSeasonRecords ()

    Shared indRecDesc$()
    Shared indRecCategory$(), teamRecCategory$(), teamRecDesc$()
    Shared indRecords!(), teamRecords!()

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1
    Color 15, 4
    Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";

    For II = 1 To J:
        Color 15, 4
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 4: Print String$(2, 177)
    Next

    Color 15, 4
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 2, 23
    Print "******* INDIVIDUAL RECORDS *******"
    Locate , 4
    Print "CATEGORY                PLAYER           TEAM            RECORD"

    Color 7

    For I = 0 To 2
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(3); Tab(28); indRecDesc$(3, 0); Tab(42); indRecDesc$(3, 1); Tab(62); Using "##.#"; indRecords!(3)
    Locate , 4
    Print indRecCategory$(4); Tab(28); indRecDesc$(4, 0); Tab(42); indRecDesc$(4, 1); Tab(62); Using " ###"; indRecords!(4);: Print indRecDesc$(4, 2)

    For I = 5 To 7
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(8); Tab(28); indRecDesc$(8, 0); Tab(42); indRecDesc$(8, 1); Tab(62); Using "##.#"; indRecords!(8)
    Locate , 4
    Print indRecCategory$(9); Tab(28); indRecDesc$(9, 0); Tab(42); indRecDesc$(9, 1); Tab(62); Using " ###"; indRecords!(9);: Print indRecDesc$(9, 2)

    For I = 10 To 14
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(15); Tab(28); indRecDesc$(15, 0); Tab(42); indRecDesc$(15, 1); Tab(62); Using "##.#"; indRecords!(15)
    Locate , 4
    Print indRecCategory$(16); Tab(28); indRecDesc$(16, 0); Tab(42); indRecDesc$(16, 1); Tab(61); Using "###.#"; indRecords!(16)
    Locate , 4
    Print indRecCategory$(17); Tab(28); indRecDesc$(17, 0); Tab(42); indRecDesc$(17, 1); Tab(62); Using " ###"; indRecords!(17);: Print indRecDesc$(17, 2)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    For I = 18 To 24
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(25); Tab(28); indRecDesc$(25, 0); Tab(42); indRecDesc$(25, 1); Tab(62); Using "##.#"; indRecords!(25)
    Locate , 4
    Print indRecCategory$(26); Tab(28); indRecDesc$(26, 0); Tab(42); indRecDesc$(26, 1); Tab(62); Using " ###"; indRecords!(26);: Print indRecDesc$(26, 2)

    For I = 27 To 29
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(30); Tab(28); indRecDesc$(30, 0); Tab(42); indRecDesc$(30, 1); Tab(62); Using "##.#"; indRecords!(30)
    Locate , 4
    Print indRecCategory$(31); Tab(28); indRecDesc$(31, 0); Tab(42); indRecDesc$(31, 1); Tab(62); Using " ###"; indRecords!(31);: Print indRecDesc$(31, 2)

    For I = 32 To 33
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(34); Tab(28); indRecDesc$(34, 0); Tab(42); indRecDesc$(34, 1); Tab(62); Using "##.#"; indRecords!(34)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    For I = 35 To 36
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(37); Tab(28); indRecDesc$(37, 0); Tab(42); indRecDesc$(37, 1); Tab(61); Using "###.#"; indRecords!(37)
    Locate , 4
    Print indRecCategory$(38); Tab(28); indRecDesc$(38, 0); Tab(42); indRecDesc$(38, 1); Tab(62); Using " ###"; indRecords!(38);: Print indRecDesc$(38, 2)

    For I = 39 To 40
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(41); Tab(28); indRecDesc$(41, 0); Tab(42); indRecDesc$(41, 1); Tab(61); Using "###.#"; indRecords!(41)

    For I = 42 To 44
        Locate , 4
        Print indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    Locate , 4
    Print indRecCategory$(45); Tab(28); indRecDesc$(45, 0); Tab(42); indRecDesc$(45, 1); Tab(62); Using " ###"; indRecords!(45);: Print indRecDesc$(45, 2)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21
        Locate I, 4
        Print Space$(72)
    Next

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1

    Color 15, 4
    Print " " + Chr$(214); String$(73, 196); "· ";

    For II = 1 To J
        Color 15, 4
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 4: Print String$(2, 177)
    Next

    Color 15, 4
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 2, 21
    Print "******* OFFENSIVE TEAM RECORDS *******"
    Locate , 4
    Print "CATEGORY                   TEAM                  RECORD            "

    Color 7

    Locate , 4
    Print teamRecCategory$(0); Tab(28); teamRecDesc$(0); Tab(53); Using "###.#"; teamRecords!(0)
    Locate , 4
    Print teamRecCategory$(1); Tab(28); teamRecDesc$(1); Tab(53); Using "#####"; teamRecords!(1)

    For I = 2 To 3
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    Locate , 4
    Print teamRecCategory$(4); Tab(28); teamRecDesc$(4); Tab(53); Using "#####"; teamRecords!(4)

    For I = 5 To 6
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    Locate , 4
    Print teamRecCategory$(7); Tab(28); teamRecDesc$(7); Tab(53); Using "#####"; teamRecords!(7)

    For I = 8 To 9
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    For I = 10 To 11
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I) * 100
    Next I

    Locate , 4
    Print teamRecCategory$(12); Tab(28); teamRecDesc$(12); Tab(53); Using "#####"; teamRecords!(12)

    For I = 13 To 14
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    For I = 15 To 17
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    Locate , 4
    Print teamRecCategory$(18); Tab(28); teamRecDesc$(18); Tab(53); Using "#####"; teamRecords!(18)
    Locate , 4
    Print teamRecCategory$(19); Tab(28); teamRecDesc$(19); Tab(53); Using "###.#"; teamRecords!(19)

    For I = 20 To 22
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "#####"; teamRecords!(I)
    Next I

    Locate , 4
    Print teamRecCategory$(23); Tab(28); teamRecDesc$(23); Tab(53); Using "###.#"; teamRecords!(23)
    Locate , 4
    Print teamRecCategory$(24); Tab(28); teamRecDesc$(24); Tab(53); Using "###.#"; teamRecords!(24)

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 15
    Locate 2, 21
    Print "******* DEFENSIVE TEAM RECORDS *******"
    Locate , 4
    Print "CATEGORY                   TEAM                  RECORD            "

    Color 7

    Locate , 4
    Print teamRecCategory$(25); Tab(28); teamRecDesc$(25); Tab(53); Using "###.#"; teamRecords!(25)
    Locate , 4
    Print teamRecCategory$(26); Tab(28); teamRecDesc$(26); Tab(53); Using "#####"; teamRecords!(26)

    For I = 27 To 29
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    Locate , 4
    Print teamRecCategory$(30); Tab(28); teamRecDesc$(30); Tab(53); Using "#####"; teamRecords!(30)

    For I = 31 To 32
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    Locate , 4
    Print teamRecCategory$(33); Tab(28); teamRecDesc$(33); Tab(53); Using "#####"; teamRecords!(33)

    For I = 34 To 35
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    For I = 36 To 37
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I) * 100
    Next

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

    Color 7
    Locate 4

    Locate , 4
    Print teamRecCategory$(38); Tab(28); teamRecDesc$(38); Tab(53); Using "#####"; teamRecords!(38)
    Locate , 4
    Print teamRecCategory$(39); Tab(28); teamRecDesc$(39); Tab(53); Using "###.#"; teamRecords!(39)
    Locate , 4
    Print teamRecCategory$(40); Tab(28); teamRecDesc$(40); Tab(53); Using "#####"; teamRecords!(40)
    Locate , 4
    Print teamRecCategory$(41); Tab(28); teamRecDesc$(41); Tab(53); Using "###.#"; teamRecords!(41)

    For I = 42 To 43
        Locate , 4
        Print teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    Color 15: Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    'Clear existing categories, leaving border
    For I = 4 To 21:
        Locate I, 4
        Print Space$(72)
    Next

End Sub

'----------------------------------------
'       PrintSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the usere with
'viewing the records on the default printer
Sub PrintSeasonRecords ()

    Shared indRecDesc$()
    Shared indRecCategory$(), teamRecCategory$(), teamRecDesc$()
    Shared indRecords!(), teamRecords!()

    Color 0, 15

    LPrint Tab(23); "******* INDIVIDUAL RECORDS *******"
    LPrint
    LPrint "CATEGORY                   PLAYER           TEAM           RECORD"

    For I = 0 To 2
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(3); Tab(28); indRecDesc$(3, 0); Tab(42); indRecDesc$(3, 1); Tab(62); Using "##.#"; indRecords!(3)
    LPrint indRecCategory$(4); Tab(28); indRecDesc$(4, 0); Tab(42); indRecDesc$(4, 1); Tab(62); Using " ###"; indRecords!(4);: LPrint indRecDesc$(4, 2)

    For I = 5 To 7
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(8); Tab(28); indRecDesc$(8, 0); Tab(42); indRecDesc$(8, 1); Tab(62); Using "##.#"; indRecords!(8)
    LPrint indRecCategory$(9); Tab(28); indRecDesc$(9, 0); Tab(42); indRecDesc$(9, 1); Tab(62); Using " ###"; indRecords!(9);: LPrint indRecDesc$(9, 2)

    For I = 10 To 14
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(15); Tab(28); indRecDesc$(15, 0); Tab(42); indRecDesc$(15, 1); Tab(62); Using "##.#"; indRecords!(15)
    LPrint indRecCategory$(16); Tab(28); indRecDesc$(16, 0); Tab(42); indRecDesc$(16, 1); Tab(61); Using "###.#"; indRecords!(16)
    LPrint indRecCategory$(17); Tab(28); indRecDesc$(17, 0); Tab(42); indRecDesc$(17, 1); Tab(62); Using " ###"; indRecords!(17);: LPrint indRecDesc$(17, 2)

    For I = 18 To 24
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(25); Tab(28); indRecDesc$(25, 0); Tab(42); indRecDesc$(25, 1); Tab(62); Using "##.#"; indRecords!(25)
    LPrint indRecCategory$(26); Tab(28); indRecDesc$(26, 0); Tab(42); indRecDesc$(26, 1); Tab(62); Using " ###"; indRecords!(26);: LPrint indRecDesc$(26, 2)

    For I = 27 To 29
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(30); Tab(28); indRecDesc$(30, 0); Tab(42); indRecDesc$(30, 1); Tab(62); Using "##.#"; indRecords!(30)
    LPrint indRecCategory$(31); Tab(28); indRecDesc$(31, 0); Tab(42); indRecDesc$(31, 1); Tab(62); Using " ###"; indRecords!(31);: LPrint indRecDesc$(31, 2)

    For I = 32 To 33
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(34); Tab(28); indRecDesc$(34, 0); Tab(42); indRecDesc$(34, 1); Tab(62); Using "##.#"; indRecords!(34)

    For I = 35 To 36
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(37); Tab(28); indRecDesc$(37, 0); Tab(42); indRecDesc$(37, 1); Tab(61); Using "###.#"; indRecords!(37)
    LPrint indRecCategory$(38); Tab(28); indRecDesc$(38, 0); Tab(42); indRecDesc$(38, 1); Tab(62); Using " ###"; indRecords!(38);: LPrint indRecDesc$(38, 2)

    For I = 39 To 40
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(41); Tab(28); indRecDesc$(41, 0); Tab(42); indRecDesc$(41, 1); Tab(61); Using "###.#"; indRecords!(41)

    For I = 42 To 44
        LPrint indRecCategory$(I); Tab(28); indRecDesc$(I, 0); Tab(42); indRecDesc$(I, 1); Tab(62); Using "####"; indRecords!(I)
    Next

    LPrint indRecCategory$(45); Tab(28); indRecDesc$(45, 0); Tab(42); indRecDesc$(45, 1); Tab(62); Using " ###"; indRecords!(45);: LPrint indRecDesc$(45, 2)
    LPrint Chr$(12)
    LPrint Tab(23); "******* OFFENSIVE TEAM RECORDS *******"
    LPrint
    LPrint "CATEGORY                      TEAM                 RECORD            "
    LPrint teamRecCategory$(0); Tab(28); teamRecDesc$(0); Tab(53); Using "###.#"; teamRecords!(0)
    LPrint teamRecCategory$(1); Tab(28); teamRecDesc$(1); Tab(53); Using "#####"; teamRecords!(1)

    For I = 2 To 3
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    LPrint teamRecCategory$(4); Tab(28); teamRecDesc$(4); Tab(53); Using "#####"; teamRecords!(4)

    For I = 5 To 6
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    LPrint teamRecCategory$(7); Tab(28); teamRecDesc$(7); Tab(53); Using "#####"; teamRecords!(7)

    For I = 8 To 9
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    For I = 10 To 11
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I) * 100
    Next I

    LPrint teamRecCategory$(12); Tab(28); teamRecDesc$(12); Tab(53); Using "#####"; teamRecords!(12)

    For I = 13 To 14
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    For I = 15 To 17
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next I

    LPrint teamRecCategory$(18); Tab(28); teamRecDesc$(18); Tab(53); Using "#####"; teamRecords!(18)
    LPrint teamRecCategory$(19); Tab(28); teamRecDesc$(19); Tab(53); Using "###.#"; teamRecords!(19)

    For I = 20 To 22
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "#####"; teamRecords!(I)
    Next I

    LPrint teamRecCategory$(23); Tab(28); teamRecDesc$(23); Tab(53); Using "###.#"; teamRecords!(23)
    LPrint teamRecCategory$(24); Tab(28); teamRecDesc$(24); Tab(53); Using "###.#"; teamRecords!(24)
    LPrint
    LPrint
    LPrint Tab(23); "******* DEFENSIVE TEAM RECORDS *******"
    LPrint
    LPrint "CATEGORY                      TEAM                 RECORD            "
    LPrint teamRecCategory$(25); Tab(28); teamRecDesc$(25); Tab(53); Using "###.#"; teamRecords!(25)
    LPrint teamRecCategory$(26); Tab(28); teamRecDesc$(26); Tab(53); Using "#####"; teamRecords!(26)

    For I = 27 To 29
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    LPrint teamRecCategory$(30); Tab(28); teamRecDesc$(30); Tab(53); Using "#####"; teamRecords!(30)

    For I = 31 To 32
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    LPrint teamRecCategory$(33); Tab(28); teamRecDesc$(33); Tab(53); Using "#####"; teamRecords!(33)

    For I = 34 To 35
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    For I = 36 To 37
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I) * 100
    Next

    LPrint teamRecCategory$(38); Tab(28); teamRecDesc$(38); Tab(53); Using "#####"; teamRecords!(38)
    LPrint teamRecCategory$(39); Tab(28); teamRecDesc$(39); Tab(53); Using "###.#"; teamRecords!(39)
    LPrint teamRecCategory$(40); Tab(28); teamRecDesc$(40); Tab(53); Using "#####"; teamRecords!(40)
    LPrint teamRecCategory$(41); Tab(28); teamRecDesc$(41); Tab(53); Using "###.#"; teamRecords!(41)

    For I = 42 To 43
        LPrint teamRecCategory$(I); Tab(28); teamRecDesc$(I); Tab(53); Using "###.#"; teamRecords!(I)
    Next

    LPrint Chr$(12)

End Sub

