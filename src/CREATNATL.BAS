'----------------------------------------
'          CREATENATL Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of national orgs
Sub CREATENATL ()

    _Title "3-in-1 Football - Manage Organizations"

    Shared statsZ1$()

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "CREATE / DEFINE ORG & MEMBERS   "
        menuChoices$(2) = "ADD / DELETE /REVIEW CONFERENCES"
        menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU"
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        I1 = Selection%

        If ExitCode% <> 27 Then

            For X = 1 To MAX_CONFERENCES
                statsZ1$(X) = "XXX"
            Next

            Select Case Selection%

                Case 1:
                    Call DefineNatOrg(organization$)
                    Call ReviewEditNatOrg(organization$)

                Case 2:
                    Do
                        organization$ = ChooseFile$ (diskPaths$(2) + "*.CON", "ORGANIZATION", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Call ReviewEditNatOrg(organization$)
                        End if

                    Loop Until FileExit% = 27 Or FileExit% = 0

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       DefineNatOrg Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the org's
'general naming and setup
Sub DefineNatOrg (organization$)

    Do

        Color 15, 0
        Cls

        Call MenuBox (6, 8, 61, 7, 15, 1)

        Do
            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR ORGANIZATION: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE NCAA-90, SOUTH, MIDWEST"

            Locate 7, 40: Input organization$

            If organization$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'       ReviewEditNatOrg Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'conferences from the org definition.
Sub ReviewEditNatOrg (organization$)

    Shared statsZ1$()

    If _FileExists(diskPaths$(2) + organization$ + ".CON") Then

        Open diskPaths$(2) + organization$ + ".CON" For Input As #1

        Input #1, organization$

        For X = 1 To MAX_CONFERENCES
            Input #1, statsZ1$(X)
        Next

        Close #1

    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; organization$
        Print

        Color 12

        For I = 1 To 10

            Color 2: Print Using "## "; I;
            Color 15: Print statsZ1$(I);

            Locate , 21
            Color 2: Print Using "## "; I + 10;
            Color 15: Print statsZ1$(I + 10);

            Locate , 41
            Color 2: Print Using "## "; I + 20;
            Color 15: Print statsZ1$(I + 20);

            Locate , 61
            Color 2: Print Using "## "; I + 30;
            Color 15: Print statsZ1$(I + 30)

        Next I

        Do

            Locate 21, 1

            Color 14: Print "(1) ";
            Color 15: Print "ADD A CONFERENCE"
            Color 14: Print "(2) ";
            Color 15: Print "REMOVE A CONFERENCE"
            Color 14: Print "(3) ";
            Color 15: Print "END AND SAVE ORGANIZATION"

            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If choice <> 3 Then

            Do
                Locate 22, 35
                Print "SLOT # (1-40) ";
                Input B
            Loop Until B >= 1 And B <= 40

            If choice = 1 Then
                'Add conference
                statsZ1$(B) = ChooseFile$ (diskPaths$(2) + "*.LIB", "CONFERENCE", ExitCode%)
            Else
                'Remove conference
                statsZ1$(B) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveNatOrg(organization$)

End Sub

'----------------------------------------
'        SaveNatOrg Subroutine
'----------------------------------------
'This subroutine writes the organization's
'conference memberships and definition to 
'each of the corresponding files (.CON)
Sub SaveNatOrg (organization$)

    Shared statsZ1$()

    Open diskPaths$(2) + organization$ + ".CON" For Output As #1

    Print #1, organization$

    For X = 1 To MAX_CONFERENCES
        Print #1, statsZ1$(X)
    Next

    Close #1

End Sub
