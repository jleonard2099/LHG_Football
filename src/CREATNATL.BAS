'----------------------------------------
'          CREATENATL Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of national orgs
Sub CREATENATL ()

    _Title "3-in-1 Football - Manage Organizations"

    Shared memberConf$()

    ReDim menuChoices$(1 To 3)

    Do

        Color 15, 0
        Cls

        Call TITLE
        menuChoices$(1) = "CREATE AN ORGANIZATION     "
        menuChoices$(2) = "REVIEW ORGANIZATION MEMBERS"
        menuChoices$(3) = "RETURN TO LEADERS MENU     "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            For X = 1 To MAX_CONFERENCES
                memberConf$(X) = "XXX"
            Next

            Select Case SELECTION%

                Case 1:
                    Call DefineNatOrg(orgName$)

                    If orgName$ <> "" Then
                        orgFile$ = diskPaths$(2) + orgName$ + ".CON"
                        Call ReviewEditNatOrg(orgFile$, orgName$)
                    End If

                Case 2:
                    orgFile$ = _OpenFileDialog$("Choose Organization file", diskPaths$(2), "*.CON", "Organization files [.CON]")
                    'ChooseFile$(diskPaths$(2) + "*.CON", "ORGANIZATION", FileExit%)

                    If orgFile$ <> "" Then
                        If _FileExists(orgFile$) Then
                            Call ReadOrganizationFile (orgFile$, orgName$)
                            Call ReviewEditNatOrg(orgFile$, orgName$)
                        End If
                    End If

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

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
Sub ReviewEditNatOrg (orgFile$, orgName$)

    Shared memberConf$()

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; orgName$
        Print

        Color 12

        For I = 1 To 10

            Color 2: Print Using "## "; I;
            Color 15: Print Left$(memberConf$(I), 17);

            Locate , 21
            Color 2: Print Using "## "; I + 10;
            Color 15: Print Left$(memberConf$(I + 10), 17);

            Locate , 41
            Color 2: Print Using "## "; I + 20;
            Color 15: Print Left$(memberConf$(I + 20), 17);

            Locate , 61
            Color 2: Print Using "## "; I + 30;
            Color 15: Print Left$(memberConf$(I + 30), 17)

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
                Input conferenceIndex
            Loop Until conferenceIndex >= 1 And conferenceIndex <= MAX_CONFERENCES

            If choice = 1 Then
                'Add conference
                targetFile$ = _OpenFileDialog$("Choose League Definition file", diskPaths$(2), "*.TMI", "League Definition files [.TMI]")
                memberConf$(conferenceIndex) = GetFileBaseName$(targetFile$)
                'ChooseFile$(diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)
            Else
                'Remove conference
                memberConf$(conferenceIndex) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveNatOrg(orgFile$, orgName$)

End Sub


'----------------------------------------
'        SaveNatOrg Subroutine
'----------------------------------------
'This subroutine writes the organization's
'conference memberships and definition to 
'each of the corresponding files (.CON)
Sub SaveNatOrg (orgFile$, orgName$)

    Shared memberConf$()

    Open orgFile$ For Output As #1

    Print #1, orgName$

    For X = 1 To MAX_CONFERENCES
        Print #1, memberConf$(X)
    Next

    Close #1

    Cls

End Sub
