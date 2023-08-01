Sub BLANKCOL ()

    _Title "3-in-1 Football - Delete Conference Stats"

    Dim H1$(40)

    conferenceFile$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Cls
        Locate 10, 8
        Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES", 26, Pass)
        I2$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(I2$) = "Y" Then

            Open diskPaths$(2) + conferenceFile$ + ".TMS" For Input As #2

            Input #2, LG$, div1$, div2$

            For XW = 1 To 30
                Input #2, H1$(XW)
            Next

            Close 2

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

