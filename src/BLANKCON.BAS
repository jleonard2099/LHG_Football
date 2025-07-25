'----------------------------------------
'          BLANKCON Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a college conference, and then proceeds to
'delete all stats files for all associated
'teams in the conference
Sub BLANKCON ()

    _Title "3-in-1 Football - Erase Conference Stats"

	Shared memberName$()

    conferenceFile$ = _OpenFileDialog$("Choose Conference Stats file", diskPaths$(2), "*.TMS", "Conference Stats files [.TMS]")
    'ChooseFile$ (diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)

    If conferenceFile$ <> "" Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, PRESS 'Y' FOR YES"
        BoxWidth = 31
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        G$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(G$) = "Y" Then

            Call ReadConferenceStats (conferenceFile$, 0)

            For XW = 1 To TEAMS_PER_CONFERENCE

                If memberName$(XW) <> Chr$(42) Then
                    Print "DELETING "; memberName$(XW); " STATS"
                    statFile$ = diskPaths$(1) + memberName$(XW)
                    If _FileExists(statFile$) Then Kill statFile$
                    If _FileExists(statFile$) Then Kill statFile$
                    If _FileExists(statFile$ + "D") Then Kill statFile$ + "D"
                    If _FileExists(statFile$ + "R") Then Kill statFile$ + "R"
                    If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                End If

            Next

            baseFile$ = GetFileBaseName$(conferenceFile$)

            leaderFile$ = diskPaths$(2) + baseFile$ + ".LDR"
            If _FileExists(leaderFile$) Then Kill leaderFile$

            standingsFile$ = diskPaths$(2) + baseFile$ + ".LDS"
            If _FileExists(standingsFile$) Then Kill standingsFile$

        End If 'Done confirming to move forward with deletion

    End If
    
End Sub

