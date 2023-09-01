'----------------------------------------
'          BLANKCOL Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a college conference, and then proceeds to 
'delete all stats files for all associated 
'teams in the conference
Sub BLANKCOL ()

    _Title "3-in-1 Football - Erase Conference Stats"

    Dim H1$(40)

    conferenceFile$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES"
        BoxWidth = 27
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I2$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(I2$) = "Y" Then

            Open diskPaths$(2) + conferenceFile$ + ".TMS" For Input As #1

            Input #1, conf$, div1$, div2$

            For XW = 1 To 30
                Input #1, H1$(XW)
            Next

            Close #1

            For XW = 1 To 30

                If H1$(XW) <> Chr$(42) Then
                    statFile$ = diskPaths$(2) + H1$(XW)
                    Print "DELETING " + H1$(XW) + " STATS"
                    If _FileExists(statFile$) Then Kill statFile$
                    If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                End If

            Next

        End If 'Done checking Y/N

    End If
    
End Sub

