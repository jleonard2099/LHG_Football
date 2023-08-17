'----------------------------------------
'          BLANKORG Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a national org, and then proceeds to delete 
' all stats files for all associated teams
Sub BLANKORG ()

    _Title "3-in-1 Football - Erase National Stats"

    Dim H1$(40), Z4$(40)

    organizationFile$ = ChooseFile$ (diskPaths$(2) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS ORGANIZATION. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES"
        BoxWidth = 27
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I2$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(I2$) = "Y" Then

            Open diskPaths$(2) + organizationFile$ + ".CON" For Input As #2

            Input #2, org$

            For X = 1 To 40
                Input #2, Z4$(X)
            Next

            Close 2

            For X = 1 To 40

                If Z4$(X) <> "XXX" Then
                    Cls
                    Exit Sub
                    'Should go back to beginning?
                End If

            Next

            Open diskPaths$(2) + Z4$(X) + ".TMS" For Input As #2

            Input #2, org$, div1$, div2$

            For XW = 1 To 30
                Input #2, H1$(XW)
            Next

            Close 2

            For XW = 1 To 30

                If H1$(XW) <> Chr$(42) Then
                    statFile$ = diskPaths$(2) + H1$(XW)
                    Print "DELETING "; H1$(XW); " STATS"
                    If _FileExists(statFile$) Then Kill statFile$
                    If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                End If

            Next

        End If

    End If 

End Sub
