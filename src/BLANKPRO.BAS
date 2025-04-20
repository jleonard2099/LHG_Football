'----------------------------------------
'          BLANKPRO Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a league, and then proceeds to delete all
'stats files for all the associated teams
Sub BLANKPRO ()

    _Title "3-in-1 Football - Erase League Stats"

	Shared memberName$()

	leagueFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.TMS", "League Stats files [.TMS]")
    'ChooseFile$ (diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)

    If leagueFile$ <> "" Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL STATS, PRESS 'Y' FOR YES"
        BoxWidth = 31
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        A$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(A$) = "Y" Then

            Call ReadConferenceStats (leagueFile$, 1)

            For XW = 1 To TEAMS_PER_LEAGUE

                If memberName$(XW) <> Chr$(42) Then
                    Print "DELETING "; memberName$(XW); " STATS"
                    statFile$ = diskPaths$(1) + memberName$(XW)
                    If _FileExists (statFile$) Then Kill statFile$
                    If _FileExists (statFile$ + "R") Then Kill statFile$ + "R"
                    If _FileExists (statFile$ + "S") Then Kill statFile$ + "S"
                    If _FileExists (statFile$ + "D") Then Kill statFile$ + "D"
                End If

            Next

            baseFile$ = GetFileBaseName$(leagueFile$)

            leaderFile$ = diskPaths$(2) + baseFile$ + ".LDR"
            If _FileExists(leaderFile$) Then Kill leaderFile$

            standingsFile$ = diskPaths$(2) + baseFile$ + ".LDS"
            If _FileExists(standingsFile$) Then Kill standingsFile$

        End If 'Done confirming to move forward with deletion

    End If

End Sub

