SUB BLANKORG ()

	_Title "3-in-1 Football - Delete National Stats"

	DefInt A-Z

	Dim H1$(40), Z4$(40)

	Do

		CLS

		Spec$ = diskPaths$(1) + "*.CON"
		null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
		Count% = DirCount%

		IF Count% = 0 THEN 
			LOCATE 10, 8
			CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
			I$ = GetKeyPress$
			CALL CSMsgBox("", 0, Pass)
			'GOTO 99
		Else
		
			REDIM conferenceFiles$(0 TO Count%)

			FOR X = 1 TO Count%
			  conferenceFiles$(X) = SPACE$(12)
			NEXT

			conferenceFiles$(0) = Spec$
			CALL ReadFile(conferenceFiles$())
			
			Do
				CLS
				COLOR , 1
				CLS
				LOCATE , 28
				COLOR 14
				PRINT "SELECT ORGANIZATION TO ERASE": COLOR 15
				Selection% = 1
				Start% = 1
				FileFGColr% = 15:FileBGColr% = 1
				HiLiteFGColr% = 0:HiLiteBGColr% = 7
				Rows% = 20
				Columns% = 4
				Spacing% = 6
				Row% = 4
				Column% = 1
				LOCATE 25, 1
				COLOR 14
				PRINT "ENTER"; : COLOR 15
				PRINT "-SELECTS ORGANIZATION   ";
				COLOR 14
				PRINT "HIT ESCAPE KEY TO EXIT";
				COLOR 15

				_MouseShow "DEFAULT"
				MAMenu conferenceFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
				_MouseHide

				IF ExitCode% <> 27 THEN 
					targetFile$ = conferenceFiles$(Selection%)
					LOCATE 2, 33: PRINT targetFile$
					LOCATE , 33: PRINT "ANY CHANGE (YN)";
					COLOR 15, 0
					I1$ = GetKeyPress$
				End If

			Loop Until UCASE$(I1$) = "N"


			IF ExitCode% <> 27 THEN 
				targetFile$ = RTrim$(targetFile$)
				WE$ = LEFT$(targetFile$, LEN(targetFile$) - 4)
				LH$ = LEFT$(WE$, 8)

				CLS
				LOCATE 10, 8

				CALL CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Pass)
				I2$ = GetKeyPress$
				CALL CSMsgBox("", 0, Pass)

				If I2$ = "Y" Then
					OPEN diskPaths$(1) + LH$ + ".CON" FOR INPUT AS #2
						INPUT #2, LG$
						FOR X = 1 TO 40
						INPUT #2, Z4$(X)
						NEXT
					CLOSE 2

					FOR X = 1 TO 40
						IF Z4$(X) <> "XXX" THEN 
							CLS
							Exit Sub
							'Should go back to beginning?
						End If
					Next

					OPEN diskPaths$(1) + Z4$(X) + ".TMS" FOR INPUT AS #2
						INPUT #2, LG$, D1$, D2$
						FOR XW = 1 TO 30
							INPUT #2, H1$(XW)
						NEXT
					CLOSE 2

					FOR XW = 1 TO 30
						IF H1$(XW) <> CHR$(42) THEN 
							statFile$ = diskPaths$(1) + H1$(XW)
							Print "DELETING "; statFile$; " STATS"
							If _FileExists (statFile$) Then Kill statFile$
							If _FileExists (statFile$ + "S") Then Kill statFile$ + "S"
						End If
					Next		

				End If 'Done checking for Y / N

			End If 'Done checking for ESC

		End If 'Done checking file count

	Loop Until ExitCode% = 27 Or Count% = 0

END SUB
