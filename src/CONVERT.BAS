'----------------------------------------
'        ReadLeagueOld Subroutine
'----------------------------------------
Sub ReadLeagueOld (leagueFile$, league$, div1$, div2$, div3$, wrongFormat%)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    For X = 1 To TEAMS_PER_LEAGUE:
        memberName$(X) = "XXX"
        memberIdx(X) = -1
        memberYear$(X) = "XXX"
    Next

    div1$ = ""
    div2$ = ""
    div3$ = ""

    If _FileExists(leagueFile$) Then

        Open leagueFile$ For Input As #1

        Input #1, statType

        If statType <> 1 Then

            Cls
            Print "THE FILE YOU TRIED TO OPEN IS IN COLLEGE FORMAT"
            Print "EITHER TRY AGAIN OR USE THE COLLEGE OPTION TO EDIT"
            Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"

            I$ = GetKeyPress$

            wrongFormat% = 1

        Else

            Input #1, league$, div1$, div2$, div3$

            For X = 1 To 30:
                Input #1, memberName$(X)
                Input #1, memberIdx(X)
                Input #1, memberYear$(X)
            Next

        End If

        Close #1

    End If

End Sub


'----------------------------------------
'        SaveLeagueOld Subroutine
'----------------------------------------
Sub SaveLeagueOld (leagueFile$, league$, div1$, div2$, div3$)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    Dim statFileNames$(30)

    Open diskPaths$(2) + leagueFile$ + ".TMI" For Output As #1

    Print #1, 1

    Print #1, league$
    Print #1, div1$
    Print #1, div2$
    Print #1, div3$

    For X = 1 To 30:
        Print #1, memberName$(X)
        Print #1, memberIdx(X)
        Print #1, memberYear$(X)
    Next

    Close #1

    Erase memberName$

    For X = 1 To 30

        If memberName$(X) = "XXX" Then
            statFileNames$(X) = Chr$(42)
        Else
            teamName$ = memberName$(X)
            statFileNames$(X) = nameStatFile$(teamName$)
        End If

    Next X

    Open diskPaths$(2) + leagueFile$ + ".TMS" For Output As #2

    Write #2, league$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$

    For X = 1 To 30:
        Write #2, statFileNames$(X)
    Next

    Close #2

End Sub


'----------------------------------------
'        ReadLeagueNew Subroutine
'----------------------------------------
Sub ReadLeagueNew (leagueFile$, league$, div1$, div2$, div3$, div4$, wrongFormat%)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    For X = 1 To TEAMS_PER_LEAGUE:
        memberName$(X) = "XXX"
        memberIdx(X) = -1
        memberYear$(X) = "XXX"
    Next

    div1$ = ""
    div2$ = ""
    div3$ = ""
    div4$ = ""

    If _FileExists(diskPaths$(2) + leagueFile$ + ".TMI") Then

        Open diskPaths$(2) + leagueFile$ + ".TMI" For Input As #1

        Input #1, statType

        If statType <> 1 Then

            Cls
            Print "THE FILE YOU TRIED TO OPEN IS IN COLLEGE FORMAT"
            Print "EITHER TRY AGAIN OR USE THE COLLEGE OPTION TO EDIT"
            Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"

            I$ = GetKeyPress$

            wrongFormat% = 1

        Else

            Input #1, league$, div1$, div2$, div3$, div4$

            For X = 1 To 30:
                Input #1, memberName$(X)
                Input #1, memberIdx(X)
                Input #1, memberYear$(X)
            Next

        End If

        Close #1

    End If

End Sub


'----------------------------------------
'     SaveLeagueConverted Subroutine
'----------------------------------------
'This routine saves the modified data back
'to the original team file
Sub SaveLeagueConverted (leagueFile$, league$, div1$, div2$, div3$, div4$)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    Dim statFileNames$(TEAMS_PER_LEAGUE)

    leagStatFile$ = Left$(leagueFile$, Len(leagueFile$) - 8) + ".TMS"

    Open leagueFile$ For Output As #1

    Print #1, 1

    Print #1, league$
    Print #1, div1$
    Print #1, div2$
    Print #1, div3$
    Print #1, div4$

    For X = 1 To TEAMS_PER_LEAGUE:
        Print #1, memberName$(X)
        Print #1, memberIdx(X)
        Print #1, memberYear$(X)
    Next

    Close #1

    For X = 1 To TEAMS_PER_LEAGUE

        If memberName$(X) = "XXX" Then
            statFileNames$(X) = Chr$(42)
        Else
            teamName$ = memberName$(X)
            statFileNames$(X) = nameStatFile$(teamName$)
        End If

    Next X

    Kill leagStatFile$

    Open leagStatFile$ For Output As #2

    Write #2, league$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$
    Write #2, div4$

    For X = 1 To TEAMS_PER_LEAGUE:
        Write #2, statFileNames$(X)
    Next

    Close #2

End Sub




'----------------------------------------
'         ReadSchedOld SubRoutine
'----------------------------------------
'This subroutine opens the target SCHEDULE
'file and reads the data into corresponding
'arrays for use in the scheduler
Sub ReadSchedOld (targetFile$)

    Shared BS%, NS%

    Shared scheduleAP%(), scheduleNG%()
    Shared homeScores(), visitorScores()
    Shared scheduleYN$()

    Open targetFile$ For Random As #1 Len = (SCHEDULE_SIZE_BYTES - 4)

    fileLength& = LOF(1)
    scheduleAP%(0) = fileLength& / (SCHEDULE_SIZE_BYTES - 4)
    BS% = Int((scheduleAP%(0) - 1) / 20)

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), (SCHEDULE_SIZE_BYTES - 4) - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    For I = 1 To scheduleAP%(0)

        Get #1, I

        For X = 0 To 20:
            scheduleNG%(I, X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            scheduleYN$(I, X) = RTrim$(Q$(22 + X))
        Next

        visitorScores(I) = CVI(Q$(24))
        homeScores(I) = CVI(Q$(25))

    Next

    Close #1

    NS% = 0

End Sub


'----------------------------------------
'         SaveSchedOld SubRoutine
'----------------------------------------
'This subroutine writes all scheduled data,
'as stored in the relevant arrays, back to
'the target schedule file.
Sub SaveSchedOld (saveFile$)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(diskPaths$(3) + saveFile$ + ".SCD") Then Kill diskPaths$(3) + saveFile$ + ".SCD"

    Open diskPaths$(3) + saveFile$ + ".SCD" For Random As #1 Len = (SCHEDULE_SIZE_BYTES - 4)

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), (SCHEDULE_SIZE_BYTES - 4) - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    For I = 1 To scheduleAP%(0)

        For X = 0 To 20:
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1:
            LSet Q$(22 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(24) = MKI$(visitorScores(I))
        LSet Q$(25) = MKI$(homeScores(I))

        Put #1, I

    Next

    Close #1

End Sub


'----------------------------------------
'    SaveSchedFileConverted SubRoutine
'----------------------------------------
'This subroutine writes all scheduled data,
'as stored in the relevant arrays, back to
'the target schedule file.
Sub SaveSchedFileConverted (saveFile$, numberGames)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(saveFile$) Then Kill saveFile$

    Open saveFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    For I = 1 To scheduleAP%(0)

        For X = 0 To 20:
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1:
            LSet Q$(22 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(24) = MKI$(visitorScores(I))
        LSet Q$(25) = MKI$(homeScores(I))

        Put #1, I

    Next

    Close #1

End Sub


'----------------------------------------
'       ConvertSched6to7 Subroutine
'----------------------------------------
' Reads in existing schedule data.
' For each game in the schedule, it
' will migrate to the new format and
' populate with fixed defaults.
' From there, the updated data is saved.
Sub ConvertSched6to7 (targetFile$, silent)

    Shared scheduleNG%()

    newFile$ = targetFile$ + ".NEW"

    If targetFile$ <> "" Then

        Open targetFile$ For Random As #1
        fileLength& = LOF(1)
        Close #1

        numberGames = fileLength& / (SCHEDULE_SIZE_BYTES - 4)

        Call ReadSchedOld(targetFile$)

        'Call SaveSchedFileConverted(newFile$, numberGames)

        'Kill targetFile$

        'Call FCopy(newFile$, targetFile$, Buff$, copyErr%)

        'Kill newFile$

        Kill targetFile$

        Call SaveSchedFileConverted(targetFile$, numberGames)

        If silent = 0 Then
            result& = _MessageBox("Success!", "The schedule file has been converted for use", "ok", "info", 1)
        End If

    End If

End Sub


'----------------------------------------
'      ConvertLeague4to5 Subroutine
'----------------------------------------
' Reads in existing team data, one team
' at-a-time. For each team, it tries
' to calculate what actual stats it can.
' From there, the updated data is saved.
Sub ConvertLeague4to5 (targetFile$, silent)

    newFile$ = targetFile$ + ".NEW"

    If targetFile$ <> "" Then

        Call ReadLeagueOld(targetFile$, league$, div1$, div2$, div3$, notPro)

        If notPro = 1 Then

            MsgText$ = "ERROR!! THIS IS NOT A PRO LEAGUE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call SaveLeagueConverted(newFile$, league$, div1$, div2$, div3$, "")

            Kill targetFile$

            Call FCopy(newFile$, targetFile$, Buff$, copyErr%)

            Kill newFile$

            If silent = 0 Then
                result& = _MessageBox("Success!", "The schedule file has been converted for use", "ok", "info", 1)
            End If

        End If

    End If

End Sub


'----------------------------------------
'      ConvertLeague5to4 Subroutine
'----------------------------------------
Sub ConvertLeague5to4 (targetFile$, silent)

    oldFile$ = targetFile$ + ".OLD"

    If targetFile$ <> "" Then

        Call ReadLeagueNew(targetFile$, league$, div1$, div2$, div3$, div4$, notPro)

        If notPro = 1 Then

            MsgText$ = "ERROR!! THIS IS NOT A PRO LEAGUE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call SaveLeagueConverted(targetFile$, league$, div1$, div2$, div3$, "")

            'Is this all accurate ???
            Kill targetFile$

            Call FCopy(oldFile$, targetFile$, Buff$, copyErr%)

            Kill oldFile$

            If silent = 0 Then
                result& = _MessageBox("Success!", "The schedule file has been converted for use", "ok", "info", 1)
            End If

        End If

    End If

End Sub


'----------------------------------------
'       ConvertAllLeague Subroutine
'----------------------------------------
' Quick and dirty script to convert all
' team files in the "Schedule" folder
Sub ConvertAllLeague ()

    fileSpec$ = diskPaths$(2) + "*.TMI"

    Count% = FileCount%(fileSpec$)
    ReDim foundFiles$(0 To Count%)
    foundFiles$(0) = fileSpec$
    Call ReadFile(foundFiles$())

    For X = 1 To Count%
        target$ = diskPaths$(2) + foundFiles$(X)
        Print "Converting "; target$
        Call ConvertLeague4to5(target$, 1)
    Next X

End Sub


'----------------------------------------
'       ConvertAllSched Subroutine
'----------------------------------------
' Quick and dirty script to convert all
' schedule files in the "Schedule" folder
Sub ConvertAllSched ()

    fileSpec$ = diskPaths$(3) + "*.SCD"

    Count% = FileCount%(fileSpec$)
    ReDim foundFiles$(0 To Count%)
    foundFiles$(0) = fileSpec$
    Call ReadFile(foundFiles$())

    For X = 1 To Count%
        target$ = diskPaths$(3) + foundFiles$(X)
        Print "Converting "; target$
        Call ConvertSched6to7(target$, 1)
    Next X

End Sub

