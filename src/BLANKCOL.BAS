SUB BLANKCOL ()

	_Title "3-in-1 Football - Delete Conference Stats"

	DefInt A-Z

	Dim H1$(40)

	Do

		Cls

		Spec$ = diskPaths$(1) + "*.LIB"
		null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
		Count% = DirCount%

		If Count% = 0 Then 
			Locate 10, 8
			Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
			I$ = GetKeyPress$
			Call CSMsgBox("", 0, Pass)
		Else
			ReDim leagueFiles$(0 To Count%)
			For X = 1 To Count%
				leagueFiles$(X) = Space$(12)
			Next
			leagueFiles$(0) = Spec$
			Call ReadFile(leagueFiles$())

			Call SelectConference (leagueFiles$())
			'-- if we're going to loop,
			'-- we need the exit code from the team load !!!!!

		End If

	'...Until we press ESC
	Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'		SelectConference Subroutine
'----------------------------------------
'...explanation...
Sub SelectConference (leagueFiles$())

Do
	Cls
	Color , 1
	Cls
	Locate , 28
	Color 14
	Print "SELECT CONFERENCE TO ERASE": Color 15
	Selection% = 1
	Start% = 1
	FileFGColr% = 15: FileBGColr% = 1
	HiLiteFGColr% = 0: HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1

	Do

		Locate 25, 1
		Color 14
		Print "ENTER";: Color 15
		Print "-SELECTS CONFERENCE   ";
		Color 14
		Print "HIT ESCAPE KEY TO EXIT";
		Color 15

		_MouseShow "DEFAULT"
		MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
		_MouseHide

		If ExitCode% <> 27 Then 
			targetFile$ = leagueFiles$(Selection%)
			Locate 2, 33: Print targetFile$
			Locate , 33: Print "ANY CHANGE (YN)";
			Color 15, 0
			Do
				I$ = GetKeyPress$
			Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

			If UCase$(I$) = "N" Then 
				targetFile$ = RTrim$(targetFile$)
				WE$ = Left$(targetFile$, Len(targetFile$) - 4)
				LH$ = Left$(WE$, 8)
				Cls
				Locate 10, 8
				Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Pass)
				I2$ = GetKeyPress$
				Call CSMsgBox("", 0, Pass)	
				If I2$ = "Y" Then 
					'Delete everything
					Open diskPaths$(1) + LH$ + ".TMS" For Input As #2
					Input #2, LG$, D1$, D2$
					For XW = 1 To 30
						35 Input #2, H1$(XW)
					Next
					Close 2

					For XW = 1 To 30
						If H1$(XW) <> Chr$(42) Then
							statFile$ = diskPaths$(1) + H1$(XW)
							Print "DELETING "; statFile$; " STATS"
							If _FileExists (statFile$) Then Kill statFile$
							If _FileExists (statFile$ + "S") Then Kill statFile$ + "S"
						End If
					74 Next
					Cls
					Exit Do	
					'SHOULD go to the file selection
				End If	
				'If we are here ,the user did not "Y" at the message box
				'They want to change parameters
				'We should go back to the beginning
			End If	
			'If we are here, the user picked "Y"
			'They want to change parameters
			'We should go back to the beginning	
		End If	
		'We can get here 1 of 2 ways
		'1) we are done deleting stuff
		'2) ESC was pressed from file selection menu	
	Loop Until I$ = "N"

Loop Until ExitCode% = 27

End Sub
