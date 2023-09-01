'----------------------------------------
'           CAREDIT Subroutine
'----------------------------------------
'This routine allows the user to choose a
'career file, then loads in the stats and 
'gives the users options for editing and 
'saving the file.
Sub CAREDIT ()

    _Title "3-in-1 Football - Edit Career Files"

    Shared careerA1$(), careerA2$(), careerA3$(), careerA4$(), careerA5$(), careerA6$(), careerA7$()
    Shared careerDI$(), careerDS$(), careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerRC$(), careerWRS$()

    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Color 15, 0
    Cls

	careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

	If ExitCode% <> 27 And ExitCode% <> 0 Then

		TMR = 6420

		Color 15, 0
		Cls
		Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

		Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #1

		Input #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

		For I = 0 To 49: 
			Input #1, careerA1$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 39: 
			Input #1, careerA2$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 19: 
			Input #1, careerA3$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 14: 
			Input #1, careerA4$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 14: 
			Input #1, careerA5$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 9: 
			Input #1, careerA6$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 9: 
			Input #1, careerA7$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 54: 
			Input #1, careerDI$(I)
			Locate 21, 38
			Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For I = 0 To 54: 
			Input #1, careerDS$(I)
			Locate 21, 38: Color 5
			TMR = TMR - 1: Print Using "####"; TMR
		Next
		
		For N = 1 To 15
			For J = 0 To 49
				For K = 0 To 9: 
					Input #1, careerRBS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 49: 
				Input #1, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 39: 
				For K = 0 To 5: 
					Input #1, careerWRS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 39: 
				Input #1, careerWRS$(N, J, 0)
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 19: 
				For K = 0 To 7: 
					Input #1, careerQBS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 19: 
				Input #1, careerQBS$(N, J, 0)
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 14: 
				For K = 0 To 5: 
					Input #1, careerKRS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 14: 
				Input #1, careerKRS$(N, J, 0)
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 14: 
				For K = 0 To 5: 
					Input #1, careerPRS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 14: 
				Input #1, careerPRS$(N, J, 0)
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N

		For N = 1 To 15: 
			For J = 0 To 9: 
				For K = 0 To 3: 
					Input #1, careerPUS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 9: 
				For K = 0 To 16: 
					Input #1, careerFGS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 54: 
				For K = 0 To 2: 
					Input #1, careerGIS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N
		
		For N = 1 To 15: 
			For J = 0 To 54: 
				For K = 0 To 2: 
					Input #1, careerGSS!(N, J, K)
				Next K
				Locate 21, 38
				Color 5
				TMR = TMR - 1: Print Using "####"; TMR
			Next J
		Next N

		Close #1

		Do

			Cls
			Color 7: Print "EDIT ";
			Color 12: Print "R";
			Color 7: Print "UNNING BACKS, ";
			Color 12: Print "W";
			Color 7: Print "IDE RECEIVERS, ";
			Color 12: Print "Q";
			Color 7: Print "UARTERBACKS, KI";
			Color 12:PRINT "C";
			COLOR 7:PRINT "K RETURNERS, P";:_
			COLOR 12:PRINT "U";
			COLOR 7:PRINT "NT RETURNERS, ";
			Color 12: Print "P";
			Color 7: Print "UNTERS, ";
			Color 12: Print "K";
			Color 7: Print "ICKERS, ";
			Color 12: Print "I";
			Color 7: Print "NTERCEPTIONS, DE";
			Color 12:PRINT "F";
			COLOR 7:PRINT "ENSIVE SACKS, UP";:_
			COLOR 12:PRINT"D";
			COLOR 7:PRINT "ATE CAREER FILE"
			Color 7: Print "OR ";
			Color 12: Print "E";
			Color 7: Print "XIT WITHOUT UPDATING"

			J$ = GetKeyPress$

			Select Case UCase$(J$)

				Case "R"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 16:
						Locate I + 2
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA1$(I), 12)
					Next

					For I = 17 To 33:
						Locate I - 15, 20
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA1$(I), 12)
					Next

					For I = 34 To 50:
						Locate I - 32, 40
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA1$(I), 12)
					Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (51 TO EXIT)"; B9
					Loop Until B9 >= 0 And B9 <= 51

					If B9 <> 51 Then

						If B9 <> 50 Then

							Call PrintCareerStatsRB(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls

								Locate 5, 15: Print "CHANGING RUNNING BACK FILE . . . . . PLEASE WAIT"

								For I = B9 To 49: careerA1$(I) = careerA1$(I + 1): Next

								UPP = (50 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 48:
										For I1 = 0 To 9:
											careerRBS!(J, I, I1) = careerRBS!(J, I + 1, I1)
											careerRBS$(J, I, 0) = careerRBS$(J, I + 1, 0)
											careerRBS$(J, I, 1) = careerRBS$(J, I + 1, 1)
										Next

										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP
									Next
								Next

							End If

						End If

						If exitFlag% <> 1 Then

							careerA1$(49) = ""

							For J = 1 To 15:
								For I1 = 0 To 9:
									careerRBS!(J, 50, I1) = 0
									careerRBS$(J, 50, 0) = ""
									careerRBS$(J, 50, 1) = ""
								Next
							Next

						End If

					End If

				Case "W"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 19:
						Locate I + 2
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA2$(I), 12)
					Next

					For I = 20 To 39:
						Locate I - 18, 20
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA2$(I), 12)
					Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (40 TO EXIT)"; B9
					Loop Until B9 >= 0 And B9 <= 40

					If B9 <> 40 Then

						If B9 <> 39 Then

							Call PrintCareerStatsWR(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls
								Locate 5, 15: Print "CHANGING RECEIVER FILE . . . . . PLEASE WAIT"

								For I = B9 To 38: careerA2$(I) = careerA2$(I + 1): Next

								UPP = (39 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 38:
										For I1 = 0 To 5:
											careerWRS!(J, I, I1) = careerWRS!(J, I + 1, I1)
											careerWRS$(J, I, 0) = careerWRS$(J, I + 1, 0)
										Next
										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP
									Next
								Next

							End If

						End If

						If exitFlag% <> 1 Then
							careerA2$(39) = ""

							For J = 1 To 15:
								For I1 = 0 To 5:
									careerWRS!(J, 39, I1) = 0
									careerWRS$(J, 39, 0) = ""
								Next
							Next

						End If

					End If

				Case "Q"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 9:
						Locate I + 2
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA3$(I), 12)
					Next

					For I = 10 To 19:
						Locate I - 8, 20
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA3$(I), 12)
					Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (20 TO EXIT)"; B9
					Loop Until B9 >= 0 And B9 <= 20

					If B9 <> 20 Then

						If B9 <> 19 Then
							Call PrintCareerStatsQB(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls
								Locate 5, 15: Print "CHANGING QUARTERBACK FILE . . . . . PLEASE WAIT"

								For I = B9 To 18: careerA3$(I) = careerA3$(I + 1): Next

								UPP = (19 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 18:
										For I1 = 0 To 7:
											careerQBS!(J, I, I1) = careerQBS!(J, I + 1, I1)
											careerQBS$(J, I, 0) = careerQBS$(J, I + 1, 0)
										Next

										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP
									Next
								Next

							End If

						End If

					End If

					If exitFlag% <> 1 Then

						careerA3$(19) = ""

						For J = 1 To 15:
							For I1 = 0 To 7:
								careerQBS!(J, 19, I1) = 0
								careerQBS$(J, 19, 0) = ""
							Next
						Next

					End If

				Case "C"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 14: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(careerA4$(I), 12): Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (15 TO EXIT)"; B9
					Loop Until B9 >= 0 And B9 <= 15

					If B9 <> 15 Then

						If B9 <> 14 Then

							Call PrintCareerStatsKR(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls
								Locate 5, 15: Print "CHANGING KICK RETURNER FILE . . . . . PLEASE WAIT"

								For I = B9 To 13: careerA4$(I) = careerA4$(I + 1): Next

								UPP = (14 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 13:
										For I1 = 0 To 5:
											careerKRS!(J, I, I1) = careerKRS!(J, I + 1, I1)
											careerKRS$(J, I, 0) = careerKRS$(J, I + 1, 0)
										Next
										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP
									Next
								Next
							End If

						End If

						If exitFlag% <> 1 Then

							careerA4$(14) = ""

							For J = 1 To 15:
								For I1 = 0 To 5:
									careerKRS!(J, 14, I1) = 0
									careerKRS$(J, 14, 0) = ""
								Next
							Next
						End If

					End If

				Case "U"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 14: 
						Locate I + 2: Color 11
						Print Using "##"; I;
						Print " ";
						Color 7: Print Left$(careerA5$(I), 12)
					Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (15 TO EXIT)"; B9
					Loop Until B9 <=0 And B9 <= 15

					If B9 <> 15 Then

						If B9 <> 14 Then

							Call PrintCareerStatsPR(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls

								Locate 5, 15: Print "CHANGING PUNT RETURNER FILE . . . . . PLEASE WAIT"

								For I = B9 To 13: careerA5$(I) = careerA5$(I + 1): Next

								UPP = (14 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 13:
										For I1 = 0 To 5:
											careerPRS!(J, I, I1) = careerPRS!(J, I + 1, I1)
											careerPRS$(J, I, 0) = careerPRS$(J, I + 1, 0)
										Next
										Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP
									Next
								Next
							End If

						End If

						If exitFlag% <> 1 Then

							careerA5$(14) = ""

							For J = 1 To 15:
								For I1 = 0 To 5:
									careerPRS!(J, 14, I1) = 0
									careerPRS$(J, 14, 0) = ""
								Next
							Next

						End If

					End If

				Case "P"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 9:
						Locate I + 2
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA6$(I), 12)
					Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (10 TO EXIT)"; B9
					Loop Until B9 >= 0 And B9 <= 10

					If B9 <> 10 Then

						If B9 <> 9 Then

							Call PrintCareerStatsP(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls

								Locate 5, 15: Print "CHANGING PUNTER FILE . . . . . PLEASE WAIT"

								For I = B9 To 8: careerA6$(I) = careerA6$(I + 1): Next

								UPP = (9 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 8:
										For I1 = 0 To 3:
											careerPUS!(J, I, I1) = careerPUS!(J, I + 1, I1)
										Next

										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP
									Next

								Next

							End If

						End If

						If exitFlag% <> 1 Then

							careerA6$(9) = ""

							For J = 1 To 15:
								For I1 = 0 To 3:
									careerPUS!(J, 9, I1) = 0
								Next
							Next

						End If

					End If

				Case "K"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 9:
						Locate I + 2: Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerA7$(I), 12)
					Next

					Do
						Locate 22: Color 15:
						Input "ENTER NUMBER OF PLAYER TO DELETE (10 TO EXIT)"; B9
					Loop Until B9 >= 0 Or B9 <= 10

					If B9 <> 10 Then

						If B9 <> 9 Then

							Call PrintCareerStatsK(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls
								Locate 5, 15: Print "CHANGING FIELD GOAL KICKER FILE . . . . . PLEASE WAIT"

								For I = B9 To 8: careerA7$(I) = careerA7$(I + 1): Next

								UPP = (9 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 8:
										For I1 = 0 To 16:
											careerFGS!(J, I, I1) = careerFGS!(J, I + 1, I1)
										Next

										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP
									Next
								Next

							End If

						End If

						If exitFlag% <> 1 Then

							For J = 1 To 15:
								For I1 = 0 To 16:
									careerFGS!(J, 9, I1) = 0
								Next
							Next

						End If

					End If

				Case "I"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 17:
						Locate I + 2
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerDI$(I), 12)
					Next

					For I = 18 To 35:
						Locate I - 16, 20
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerDI$(I), 12)
					Next

					For I = 36 To 54:
						Locate I - 34, 40
						Color 11: Print Using "##"; I;: Print " ";
						Color 7: Print Left$(careerDI$(I), 12)
					Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (55 TO EXIT)"; B9
					Loop Until B9 >= 0 And B9 <= 55

					If B9 <> 55 Then

						If B9 <> 54 Then

							Call PrintCareerStatsINT(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then
								Cls
								Locate 5, 15: Print "CHANGING INTERCEPTION FILE . . . . . PLEASE WAIT"

								For I = B9 To 8: careerDI$(I) = careerDI$(I + 1): Next

								UPP = (54 - B9) * 15
								I = B9

								For J = 1 To 15:
									For I = B9 To 53:
										For I1 = 0 To 2:
											careerGIS!(J, I, I1) = careerGIS!(J, I + 1, I1)
										Next

										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP
									Next
								Next

							End If

						End If

						If exitFlag% <> 1 Then

							careerDI$(54) = ""

							For J = 1 To 15:
								For I1 = 0 To 2:
									careerGIS!(J, 54, I1) = 0
								Next
							Next

						End If

					End If

				Case "F"

					Cls

					Color 12: Print careerFile$

					For I = 0 To 17: 
						Locate I + 2: Color 11
						Print Using "##"; I;
						Print " ";
						Color 7: Print Left$(careerDS$(I), 12)
					Next

					For I = 18 To 35: 
						Locate I - 16, 20: Color 11
						Print Using "##"; I;
						Print " ";
						Color 7: Print Left$(careerDS$(I), 12)
					Next

					For I = 36 To 54: 
						Locate I - 34, 40: Color 11
						Print Using "##"; I;
						Print " ";
						Color 7: Print Left$(careerDS$(I), 12)
					Next

					Do
						Locate 22: Color 15
						Input "ENTER NUMBER OF PLAYER TO DELETE (55 TO EXIT)"; B9
					Loop Until B9 >= 0 And B9 <= 55

					If B9 <> 55 Then

						If B9 <> 54 Then

							Call PrintCareerStatsSACK(exitFlag%, B9, careerFile$)

							If exitFlag% <> 1 Then

								Cls
								Locate 5, 15: Print "CHANGING INTERCEPTION FILE . . . . . PLEASE WAIT"

								For I = B9 To 8: careerDS$(I) = careerDS$(I + 1): Next

								UPP = (54 - B9) * 15
								I = B9

								For J = 1 To 15:

									For I = B9 To 53:

										For I1 = 0 To 2:
											careerGSS!(J, I, I1) = careerGSS!(J, I + 1, I1)
										Next

										UPP = UPP - 1
										Locate 7, 36: Color 12: Print Using "####"; UPP

									Next

								Next

							End If

						End If

						If exitFlag% <> 1 Then

							careerDS$(54) = ""

							For J = 1 To 15: 
								For I1 = 0 To 2: careerGSS!(J, 54, I1) = 0: Next
							Next

						End If

					End If

				Case "D"

					Call UpdateCareerFile(careerFile$)

				Case "E","X"
					'

			End Select

		Loop Until UCase$(J$) = "E" Or UCase$(J$) = "X"
	
	End If 'Done checking for ESC
			
End Sub

'----------------------------------------
'      UpdateCareerFile Subroutine
'----------------------------------------
'This routine updates the existing career
'file with the modified stats values.
Sub UpdateCareerFile (saveFile$)

    Shared careerA1$(), careerA2$(), careerA3$(), careerA4$(), careerA5$(), careerA6$(), careerA7$()
    Shared careerDI$(), careerDS$()
    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()
	
	Cls
	Color 15
	Locate 12, 6: Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

    TMR = 3840

    Open diskPaths$(1) + saveFile$ + ".CAR" For Output As #1

    Write #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    For I = 0 To 49
        Write #1, careerA1$(I)
        Locate 21, 38: Color 5
        TMR = TMR - 1
        Print Using "####"; TMR
    Next

    For I = 0 To 39
        Write #1, careerA2$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 19
        Write #1, careerA3$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 14
        Write #1, careerA4$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 14
        Write #1, careerA5$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 9
        Write #1, careerA6$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 9
        Write #1, careerA7$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 54
        Write #1, careerDI$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 54
        Write #1, careerDS$(I)
        Locate 21, 38
		Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    '---

    For N = 1 To 15

        For J = 0 To 49

            For K = 0 To 9
                Write #1, careerRBS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    '---

    For N = 1 To 15
        For J = 0 To 49
            Write #1, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 39

            For K = 0 To 5
                Write #1, careerWRS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 39
            Write #1, careerWRS$(N, J, 0)
            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 19

            For K = 0 To 7
                Write #1, careerQBS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 19
            Write #1, careerQBS$(N, J, 0)
            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 14

            For K = 0 To 5
                Write #1, careerKRS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15
        For J = 0 To 14
            Write #1, careerKRS$(N, J, 0)
            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 14

            For K = 0 To 5
                Write #1, careerPRS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 14
            Write #1, careerPRS$(N, J, 0)
            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15
        For J = 0 To 9

            For K = 0 To 3
                Write #1, careerPUS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 9

            For K = 0 To 16
                Write #1, careerFGS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 54
            For K = 0 To 2
                Write #1, careerGIS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    '---

    For N = 1 To 15

        For J = 0 To 54
            For K = 0 To 2
                Write #1, careerGSS!(N, J, K)
            Next K

            Locate 21, 38
			Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    Close #1

End Sub

'----------------------------------------
'   PrintCareerStatsRB Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's RB's
Sub PrintCareerStatsRB (exitFlag%, B9, careerFile$)

    Shared careerA1$(), careerRBS$(), careerRC$()
    Shared careerRBS!(), careerRC!()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerA1$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerA1$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
			Color 0, 15

            LPrint careerFile$

            LPrint "RUSHER         YEAR   ATT   YARDS AVERAGE  TDS   REC  YDS  AVG  TDS RUN-LONG-REC"
            VV = 0

            For I = 1 To 15: VV = VV + careerRBS!(I, B9, 7): Next

            LPrint Left$(careerA1$(B9), 13);

            For V = 1 To VV
                LPrint Tab(15); careerRBS!(V, B9, 8);
                LPrint Tab(23); Using "###"; careerRBS!(V, B9, 0);
                LPrint Tab(30); Using "####"; careerRBS!(V, B9, 1);
                If careerRBS!(V, B9, 1) = 0 Or careerRBS!(V, B9, 0) = 0 Then LPrint Tab(37); " 0.0";
                If careerRBS!(V, B9, 0) > 0 Then LPrint Tab(37); Using "##.#"; careerRBS!(V, B9, 1) / careerRBS!(V, B9, 0);
                LPrint Tab(44); Using "###"; careerRBS!(V, B9, 2);
                LPrint Tab(50); Using "###"; careerRBS!(V, B9, 3);
                LPrint Tab(54); Using "####"; careerRBS!(V, B9, 4);
                If careerRBS!(V, B9, 4) = 0 Or careerRBS!(V, B9, 3) = 0 Then LPrint Tab(59); " 0.0";
                If careerRBS!(V, B9, 3) > 0 Then LPrint Tab(59); Using "##.#"; careerRBS!(V, B9, 4) / careerRBS!(V, B9, 3);
                LPrint Tab(65); Using "##"; careerRBS!(V, B9, 5);
                LPrint Tab(71); Using "##"; careerRBS!(V, B9, 6);: LPrint careerRBS$(V, B9, 0);
                LPrint Tab(77); Using "##"; careerRBS!(V, B9, 9);: LPrint careerRBS$(V, B9, 1)
            Next V

            careerRC!(0) = 0
            careerRC!(1) = 0
            careerRC$(0) = ""
            careerRC$(1) = ""

            For V = 1 To VV
                TT1 = TT1 + careerRBS!(V, B9, 0)
                TT2 = TT2 + careerRBS!(V, B9, 1)
                TT3 = TT3 + careerRBS!(V, B9, 2)
                TT4 = TT4 + careerRBS!(V, B9, 3)
                TT5 = TT5 + careerRBS!(V, B9, 4)
                TT6 = TT6 + careerRBS!(V, B9, 5)

                If careerRBS!(V, B9, 6) > careerRC!(0) Then
                    careerRC!(0) = careerRBS!(V, B9, 6)
                    careerRC$(0) = careerRBS$(V, B9, 0)
                Else
                    If careerRBS!(V, B9, 9) > careerRC!(1) Then
                        careerRC!(1) = careerRBS!(V, B9, 9)
                        careerRC$(1) = careerRBS$(V, B9, 1)
                    End If
                End If

            Next V

            LPrint
            LPrint "TOTALS";
            LPrint Tab(18); Using "##"; VV;
            LPrint Tab(22); Using "####"; TT1;
            LPrint Tab(29); Using "#####"; TT2;

            If TT1 = 0 Or TT2 = 0 Then LPrint Tab(37); " 0.0";
            If TT1 > 0 Then LPrint Tab(37); Using "##.#"; TT2 / TT1;

            LPrint Tab(44); Using "###"; TT3;
            LPrint Tab(49); Using "####"; TT4;
            LPrint Tab(53); Using "#####"; TT5;

            If TT4 = 0 Or TT5 = 0 Then LPrint Tab(59); " 0.0";
            If TT4 > 0 Then LPrint Tab(59); Using "##.#"; TT5 / TT4;

            LPrint Tab(64); Using "###"; TT6;
            LPrint Tab(71); Using "##"; careerRC!(0);: LPrint careerRC$(0);
            LPrint Tab(77); Using "##"; careerRC!(1);: LPrint careerRC$(1)

            TT1 = 0
            TT2 = 0
            TT3 = 0
            TT4 = 0
            TT5 = 0
            TT6 = 0

			LPrint Chr$(12)

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsWR Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's WR's
Sub PrintCareerStatsWR (exitFlag%, B9, careerFile$)

    Shared careerA2$(), careerWRS$(), careerRC$()
    Shared careerWRS!(), careerRC!()

    Cls
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerA2$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"
    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerA2$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"

            LPrint careerFile$
            LPrint "RECEIVER        YEAR        REC.      YARDS      AVG.     TD  LONGEST REC."
            VV = 0

            For I = 1 To 15: VV = VV + careerWRS!(I, B9, 4): Next

            LPrint Left$(careerA2$(B9), 13);

            For V = 1 To VV
                LPrint Tab(16); careerWRS!(V, B9, 5);
                LPrint Tab(29); "###"; careerWRS!(V, B9, 0);
                LPrint Tab(40); "####"; careerWRS!(V, B9, 1);
                If careerWRS!(V, B9, 0) = 0 Or careerWRS!(V, B9, 1) = 0 Then LPrint Tab(49); " 0.0";
                If careerWRS!(V, B9, 0) > 0 Then LPrint Tab(49); "##.#"; careerWRS!(V, B9, 1) / careerWRS!(V, B9, 0);
                LPrint Tab(59); "##"; careerWRS!(V, B9, 2);
                LPrint Tab(67); "##"; careerWRS!(V, B9, 3);: LPrint careerWRS$(V, B9, 0)
            Next V

            careerRC!(0) = 0
            careerRC$(0) = ""

            For V = 1 To VV
                TT7 = TT7 + careerWRS!(V, B9, 0)
                TT8 = TT8 + careerWRS!(V, B9, 1)
                TT9 = TT9 + careerWRS!(V, B9, 2)

                If careerWRS!(V, B9, 3) > careerRC!(0) Then
                    careerRC!(0) = careerWRS!(V, B9, 3)
                    careerRC$(0) = careerWRS$(V, B9, 0)
                End If

            Next V

            LPrint "TOTALS";
            LPrint Tab(19); "##"; VV;
            LPrint Tab(28); "####"; TT7;
            LPrint Tab(39); "#####"; TT8;

            If TT7 = 0 Or TT8 = 0 Then LPrint Tab(49); " 0.0";
            If TT7 > 0 Then LPrint Tab(49); "##.#"; TT8 / TT7;

            LPrint Tab(58); "###"; TT9;
            LPrint Tab(67); "##"; careerRC!(0);: LPrint careerRC$(0)

            TT7 = 0
            TT8 = 0
            TT9 = 0

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsQB Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's QB's
Sub PrintCareerStatsQB (exitFlag%, B9, careerFile$)

    Shared careerA3$(), careerQBS$(), careerRC$()
    Shared careerQBS!(), careerRC!()

    Dim careerII(15, 30), careerPP(15, 30), careerYY(15, 30)
    'Shared careerFF!(), careerII(), careerPP(), careerRC!(), careerTT!(), careerYY()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerA3$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerA3$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"

            LPrint careerFile$
            LPrint "QUARTERBACK  YEAR  COMP.    ATT.    INT  TDS   YARDS  PCT. YD/COMP  RATING  LONG.PASS"
            VV = 0

            For I = 1 To 15: VV = VV + careerQBS!(I, B9, 6): Next

            LPrint Left$(careerA3$(B9), 13);

            For V = 1 To VV
                LPrint Tab(13); careerQBS!(V, B9, 7);
                LPrint Tab(21); "###"; careerQBS!(V, B9, 1);
                LPrint Tab(29); "###"; careerQBS!(V, B9, 0);
                LPrint Tab(38); "##"; careerQBS!(V, B9, 2);
                LPrint Tab(43); "##"; careerQBS!(V, B9, 4);
                LPrint Tab(49); "####"; careerQBS!(V, B9, 3);

                If careerQBS!(V, B9, 1) = 0 Or careerQBS!(V, B9, 0) = 0 Then LPrint Tab(56); " 00";
                If careerQBS!(V, B9, 1) > 0 Then LPrint Tab(56); "##"; (careerQBS!(V, B9, 1) / careerQBS!(V, B9, 0)) * 100;
                If careerQBS!(V, B9, 0) = 0 Then LPrint Tab(63); " 0.0";
                If careerQBS!(V, B9, 1) > 0 Then LPrint Tab(63); "##.#"; careerQBS!(V, B9, 3) / careerQBS!(V, B9, 1);

                If careerQBS!(V, B9, 0) > 0 Then careerQBS!(V, B9, 11) = Int(careerQBS!(V, B9, 1) / careerQBS!(V, B9, 0) * 1000) / 10

                If careerQBS!(V, B9, 0) = 0 Then
                    careerQBS!(V, B9, 11) = 0
                    careerQBS!(V, B9, 8) = 0
                    careerQBS!(V, B9, 10) = 0
                    careerQBS!(V, B9, 9) = 0
                Else
                    careerQBS!(V, B9, 8) = Int(careerQBS!(V, B9, 4) / careerQBS!(V, B9, 0) * 1000) / 10
                    careerQBS!(V, B9, 10) = Int(careerQBS!(V, B9, 3) / careerQBS!(V, B9, 0) * 100) / 100
                    careerQBS!(V, B9, 9) = Int(careerQBS!(V, B9, 2) / careerQBS!(V, B9, 0) * 1000) / 10
                End If

                careerPP(V, B9) = careerQBS!(V, B9, 11) - 30
                If careerPP(V, B9) < 0 Then
                    careerPP(V, B9) = 0
                Else
                    If careerPP(V, B9) > 77.5 Then
                        careerPP(V, B9) = 2.375
                    Else
                        careerPP(V, B9) = careerPP(V, B9) * .05
                    End If
                End If

                If careerQBS!(V, B9, 8) > 11.9 Then
                    careerFF!(V, B9) = 2.375
                Else
                    careerFF!(V, B9) = careerQBS!(V, B9, 8) * .2
                End If

                If careerQBS!(V, B9, 9) = 0 Then
                    careerII(V, B9) = 0
                Else
                    If careerQBS!(V, B9, 9) > 9.5 Then
                        careerII(V, B9) = 0
                    Else
                        careerII(V, B9) = 2.375 - (.25 * careerQBS!(V, B9, 9))
                    End If
                End If

                careerQBS!(V, B9, 10) = careerQBS!(V, B9, 10) - 3

                If careerQBS!(V, B9, 10) < 0 Then
                    careerYY(V, B9) = 0
                Else
                    If careerQBS!(V, B9, 10) > 12.5 Then
                        careerYY(V, B9) = 2.375
                    Else
                        D = Int(careerQBS!(V, B9, 10) * 100) / 2
                        If D = careerQBS!(V, B9, 10) / 2 Then
                            careerYY(V, B9) = careerQBS!(V, B9, 10) / 2 * .3 + careerQBS!(V, B9, 10) / 2 * .2
                        Else
                            careerYY(V, B9) = careerQBS!(V, B9, 10) / 2 * .3 + careerQBS!(V, B9, 10) / 2 * .2 + .003
                        End If
                    End If
                End If

                careerQBS!(V, B9, 12) = careerPP(V, B9) + careerFF!(V, B9) + careerII(V, B9) + careerYY(V, B9)

                If careerQBS!(V, B9, 12) > 9.5 Then
                    careerQBS!(V, B9, 12) = 158.3
                Else
                    careerQBS!(V, B9, 12) = careerQBS!(V, B9, 12) * 50 / 3
                End If

                careerQBS!(V, B9, 12) = Int(careerQBS!(V, B9, 12) * 10) / 10

                LPrint Tab(70); "###.#"; careerQBS!(V, B9, 12);
                LPrint Tab(80); "##"; careerQBS!(V, B9, 5);: LPrint careerQBS$(V, B9, 0)

            Next V

            careerRC!(0) = 0
            careerRC$(0) = ""

            For V = 1 To VV
                TT10 = TT10 + careerQBS!(V, B9, 1)
                TT11 = TT11 + careerQBS!(V, B9, 0)
                TT12 = TT12 + careerQBS!(V, B9, 2)
                TT13 = TT13 + careerQBS!(V, B9, 4)
                TT14 = TT14 + careerQBS!(V, B9, 3)

                If careerQBS!(V, B9, 5) > careerRC!(O) Then
                    careerRC!(0) = careerQBS!(V, B9, 5)
                    careerRC$(0) = careerQBS$(V, B9, 0)
                End If

            Next V

            LPrint "TOTALS";
            LPrint Tab(16); "##"; VV;
            LPrint Tab(20); "####"; TT10;
            LPrint Tab(28); "####"; TT11;
            LPrint Tab(37); "###"; TT12;
            LPrint Tab(41); "####"; TT13;
            LPrint Tab(48); "#####"; TT14;

            If TT10 = 0 Or TT11 = 0 Then LPrint Tab(56); " 00";
            If TT10 > 0 Then LPrint Tab(56); "##"; (TT10 / TT11) * 100;
            If TT11 = 0 Then LPrint Tab(63); " 0.0";
            If TT10 > 0 Then LPrint Tab(63); "##.#"; TT14 / TT10;

            If TT11 > 0 Then TT30 = Int(TT10 / TT11 * 1000) / 10
            If TT11 = 0 Then
                TT30 = 0
                TT27 = 0
                TT29 = 0
                TT28 = 0
            Else
                TT27 = Int(TT13 / TT11 * 1000) / 10
                TT29 = Int(TT14 / TT11 * 100) / 100
                TT28 = Int(TT12 / TT11 * 1000) / 10
            End If

            PPT = TT30 - 30

            If PPT < 0 Then
                PPT = 0
            Else
                If PPT > 77.5 Then
                    PPT = 2.375
                Else
                    PPT = PPT * .05
                End If
            End If

            If TT27 > 11.9 Then
                FFT = 2.375
            Else
                FFT = TT27 * .2
            End If

            If TT28 = 0 Then
                IIT = 0
            Else
                If TT28 > 9.5 Then
                    IIT = 0
                Else
                    IIT = 2.375 - (.25 * TT28)
                End If
            End If

            TT29 = TT29 - 3

            If TT29 < 0 Then
                YYT = 0
            Else
                If TT29 > 12.5 Then
                    YYT = 2.375
                Else
                    D = Int(TT29 * 100) / 2
                    If D = TT29 / 2 Then
                        YYT = TT29 / 2 * .3 + TT29 / 2 * .2
                    Else
                        YYT = TT29 / 2 * .3 + TT29 / 2 * .2 + .003
                    End If
                End If
            End If

            TT31 = PPT + FFT + IIT + YYT

            If TT31 > 9.5 Then
                TT31 = 158.3
            Else
                TT31 = TT31 * 50 / 3
            End If

            TT31 = Int(TT31 * 10) / 10

            LPrint Tab(70); "###.#"; TT31;
            LPrint Tab(80); "##"; careerRC!(0);: LPrint careerRC$(0)
            TT10 = 0
            TT11 = 0
            TT12 = 0
            TT13 = 0
            TT14 = 0

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsKR Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's KR's
Sub PrintCareerStatsKR (exitFlag%, B9, careerFile$)

    Shared careerA4$(), careerKRS$(), careerRC$()
    Shared careerKRS!(), careerRC!()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerA4$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerA4$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            LPrint careerFile$
            LPrint "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"
            VV = 0

            For I = 1 To 15: VV = VV + careerKRS!(I, B9, 4): Next

            LPrint Left$(careerA4$(B9), 13);

            For V = 1 To VV
                LPrint Tab(22); careerKRS!(V, B9, 5);
                LPrint Tab(32); "###"; careerKRS!(V, B9, 0);
                LPrint Tab(42); "####"; careerKRS!(V, B9, 1);

                If careerKRS!(V, B9, 0) = 0 Or careerKRS!(V, B9, 1) = 0 Then LPrint Tab(52); " 00";
                If careerKRS!(V, B9, 0) > 0 Then LPrint Tab(52); "##.#"; careerKRS!(V, B9, 1) / careerKRS!(V, B9, 0);

                LPrint Tab(62); "##"; careerKRS!(V, B9, 2);
                LPrint Tab(68); "##"; careerKRS!(V, B9, 3);
                LPrint careerKRS$(V, B9, 0)
            Next V

            careerRC!(0) = 0: careerRC$(0) = ""

            For V = 1 To VV
                TT15 = TT15 + careerKRS!(V, B9, 0)
                TT16 = TT16 + careerKRS!(V, B9, 1)
                TT17 = TT17 + careerKRS!(V, B9, 2)

                If careerKRS!(V, B9, 3) > careerRC!(0) Then
                    careerRC!(0) = careerKRS!(V, B9, 3)
                    careerRC$(0) = careerKRS$(V, B9, 0)
                End If

            Next V

            LPrint "TOTALS";
            LPrint Tab(25); "##"; VV;
            LPrint Tab(32); "###"; TT15;
            LPrint Tab(41); "#####"; TT16;

            If TT15 = 0 Or TT16 = 0 Then LPrint Tab(52); " 00";
            If TT15 > 0 Then LPrint Tab(52); "##.#"; TT16 / TT15;

            LPrint Tab(61); "###"; TT17;
            LPrint Tab(68); "##"; careerRC!(0);: LPrint careerRC$(0)

            TT15 = 0
            TT16 = 0
            TT17 = 0

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsPR Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's PR's
Sub PrintCareerStatsPR (exitFlag%, B9, careerFile$)

    Shared careerA5$(), careerPRS$(), careerRC$()
    Shared careerPRS!(), careerRC!()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerA5$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerA5$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            LPrint careerFile$
            LPrint "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"
            VV = 0

            For I = 1 To 15: VV = VV + careerPRS!(I, B9, 4): Next

            LPrint Left$(careerA5$(B9), 13);

            For V = 1 To VV
                LPrint Tab(22); careerPRS!(V, B9, 5);
                LPrint Tab(32); "###"; careerPRS!(V, B9, 0);
                LPrint Tab(42); "####"; careerPRS!(V, B9, 1);
                If careerPRS!(V, B9, 0) = 0 Or careerPRS!(V, B9, 1) = 0 Then LPrint Tab(52); " 00";
                If careerPRS!(V, B9, 0) > 0 Then LPrint Tab(52); "##.#"; careerPRS!(V, B9, 1) / careerPRS!(V, B9, 0);
                LPrint Tab(62); "##"; careerPRS!(V, B9, 2);
                LPrint Tab(68); "##"; careerPRS!(V, B9, 3);: LPrint careerPRS$(V, B9, 0)
            Next V

            careerRC!(0) = 0
            careerRC$(0) = ""

            For V = 1 To VV
                TT18 = TT18 + careerPRS!(V, B9, 0)
                TT19 = TT19 + careerPRS!(V, B9, 1)
                TT20 = TT20 + careerPRS!(V, B9, 2)
                If careerPRS!(V, B9, 3) > careerRC!(0) Then
                    careerRC!(0) = careerPRS!(V, B9, 3): careerRC$(0) = careerPRS$(V, B9, 0)
                End If
            Next V

            LPrint "TOTALS";
            LPrint Tab(25); "##"; VV;
            LPrint Tab(32); "###"; TT18;
            LPrint Tab(41); "#####"; TT19;

            If TT18 = 0 Or TT19 = 0 Then LPrint Tab(52); " 00";
            If TT18 > 0 Then LPrint Tab(52); "##.#"; TT19 / TT18;

            LPrint Tab(61); "###"; TT20;
            LPrint Tab(68); "##"; careerRC!(0);: LPrint careerRC$(0)
            TT18 = 0
            TT19 = 0
            TT20 = 0

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsP Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's Punter
Sub PrintCareerStatsP (exitFlag%, B9, careerFile$)

    Shared careerA6$(), careerRC$()
    Shared careerPUS!(), careerRC!()

    Cls

    Locate 5, 20: Color 15

    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerA6$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"
    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerA6$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            LPrint careerFile$
            LPrint "PUNTERS           YEAR     ATT.     YARDS      AVG."
            VV = 0

            For I = 1 To 15: VV = VV + careerPUS!(I, B9, 2): Next

            LPrint Left$(careerA6$(B9), 13);

            For V = 1 To VV
                LPrint Tab(18); careerPUS!(V, B9, 3);
                LPrint Tab(28); "###"; careerPUS!(V, B9, 0);
                LPrint Tab(38); "####"; careerPUS!(V, B9, 1);
                If careerPUS!(V, B9, 0) = 0 Or careerPUS!(V, B9, 1) = 0 Then LPrint Tab(47); "00.0"
                If careerPUS!(V, B9, 0) > 0 Then LPrint Tab(47); "##.#"; careerPUS!(V, B9, 1) / careerPUS!(V, B9, 0)
            Next V

            For V = 1 To VV
                TT21 = TT21 + careerPUS!(V, B9, 0)
                TT22 = TT22 + careerPUS!(V, B9, 1)
            Next V

            LPrint "TOTALS";
            LPrint Tab(21); "##"; VV;
            LPrint Tab(27); "####"; TT21;
            LPrint Tab(37); "#####"; TT22;
            If TT21 = 0 Or TT22 = 0 Then LPrint Tab(47); " 00"
            If TT21 > 0 Then LPrint Tab(47); "##.#"; TT22 / TT21
            TT21 = 0
            TT22 = 0

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsK Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's Kickers
Sub PrintCareerStatsK (exitFlag%, B9, careerFile$)

    Shared careerA7$(), careerRC$()
    Shared careerFGS!(), careerRC!(), careerTT!()

    Cls
	
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerA7$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerA7$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            LPrint careerFile$
            Locate 2: LPrint "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49 50- +"
            VV = 0

            For I = 1 To 15: VV = VV + careerFGS!(I, B9, 15): Next

            LPrint Left$(careerA7$(B9), 13);

            For V = 1 To VV
                LPrint Tab(13); careerFGS!(V, B9, 16);
                LPrint Tab(19); "###"; careerFGS!(V, B9, 3);
                LPrint Tab(23); "###"; careerFGS!(V, B9, 2);

                If careerFGS!(V, B9, 2) = 0 Or careerFGS!(V, B9, 3) = 0 Then LPrint Tab(27); "  0";
                If careerFGS!(V, B9, 2) > 0 Then LPrint Tab(27); "###"; (careerFGS!(V, B9, 2) / careerFGS!(V, B9, 3)) * 100;

                LPrint Tab(32); "###"; careerFGS!(V, B9, 1);
                LPrint Tab(36); "###"; careerFGS!(V, B9, 0);

                If careerFGS!(V, B9, 0) = 0 Or careerFGS!(V, B9, 1) = 0 Then LPrint Tab(40); "  0";
                If careerFGS!(V, B9, 0) > 0 Then LPrint Tab(40); "###"; (careerFGS!(V, B9, 0) / careerFGS!(V, B9, 1)) * 100;

                LPrint Tab(46); "##"; careerFGS!(V, B9, 4);
                LPrint Tab(49); "##"; careerFGS!(V, B9, 5);: LPrint "-";
                LPrint Tab(52); "##"; careerFGS!(V, B9, 10);
                LPrint Tab(55); "##"; careerFGS!(V, B9, 6);: LPrint "-";
                LPrint Tab(58); "##"; careerFGS!(V, B9, 11);
                LPrint Tab(61); "##"; careerFGS!(V, B9, 7);: LPrint "-";
                LPrint Tab(64); "##"; careerFGS!(V, B9, 12);
                LPrint Tab(67); "##"; careerFGS!(V, B9, 8);: LPrint "-";
                LPrint Tab(70); "##"; careerFGS!(V, B9, 13);
                LPrint Tab(73); "##"; careerFGS!(V, B9, 9);: LPrint "-";
                LPrint Tab(76); "##"; careerFGS!(V, B9, 14)
            Next V

            For V = 1 To VV
                careerTT!(23) = careerTT!(23) + careerFGS!(V, B9, 0)
                careerTT!(24) = careerTT!(24) + careerFGS!(V, B9, 1)
                careerTT!(34) = careerTT!(34) + careerFGS!(V, B9, 2)
                careerTT!(35) = careerTT!(35) + careerFGS!(V, B9, 3)
                careerTT!(36) = careerTT!(36) + careerFGS!(V, B9, 5)
                careerTT!(37) = careerTT!(37) + careerFGS!(V, B9, 6)
                careerTT!(38) = careerTT!(38) + careerFGS!(V, B9, 7)
                careerTT!(39) = careerTT!(39) + careerFGS!(V, B9, 8)
                careerTT!(40) = careerTT!(40) + careerFGS!(V, B9, 9)
                careerTT!(41) = careerTT!(41) + careerFGS!(V, B9, 10)
                careerTT!(42) = careerTT!(42) + careerFGS!(V, B9, 11)
                careerTT!(43) = careerTT!(43) + careerFGS!(V, B9, 12)
                careerTT!(44) = careerTT!(44) + careerFGS!(V, B9, 13)
                careerTT!(45) = careerTT!(45) + careerFGS!(V, B9, 14)
            Next V

            careerRC!(0) = 0

            For V = 1 To VV
                If careerFGS!(V, B9, 4) >= careerRC!(0) Then
                    careerRC!(0) = careerFGS!(V, B9, 4)
                End If
            Next

            LPrint "TOTALS";
            LPrint Tab(16); VV;
            LPrint Tab(19); "###"; careerTT!(35);
            LPrint Tab(23); "###"; careerTT!(34);

            If careerTT!(34) = 0 Or careerTT!(35) = 0 Then LPrint Tab(27); "  0";
            If careerTT!(34) > 0 Then LPrint Tab(27); "###"; (careerTT!(34) / careerTT!(35)) * 100;

            LPrint Tab(32); "###"; careerTT!(24);
            LPrint Tab(36); "###"; careerTT!(23);

            If careerTT!(23) = 0 Or careerTT!(24) = 0 Then LPrint Tab(40); "  0";
            If careerTT!(23) > 0 Then LPrint Tab(40); "###"; (careerTT!(23) / careerTT!(24)) * 100;

            LPrint Tab(46); "##"; careerRC!(0);
            LPrint Tab(49); "##"; careerTT!(36);: LPrint "-";
            LPrint Tab(52); "##"; careerTT!(41);
            LPrint Tab(55); "##"; careerTT!(37);: LPrint "-";
            LPrint Tab(58); "##"; careerTT!(42);
            LPrint Tab(61); "##"; careerTT!(38);: LPrint "-";
            LPrint Tab(64); "##"; careerTT!(43);
            LPrint Tab(67); "##"; careerTT!(39);: LPrint "-";
            LPrint Tab(70); "##"; careerTT!(44);
            LPrint Tab(73); "##"; careerTT!(40);: LPrint "-";
            LPrint Tab(76); "##"; careerTT!(45)

            For J = 23 To 45: careerTT!(J) = 0: Next

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsINT Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's INT's
Sub PrintCareerStatsINT (exitFlag%, B9, careerFile$)

    Shared careerDI$()
    Shared careerGIS!(), careerTT!()

    Cls
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careercareerDI$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerDI$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            LPrint careerFile$
            LPrint "INTERCEPTORS        YEAR     INTERCEPTIONS"
            VV = 0

            For I = 1 To 15: VV = VV + careerGIS!(I, B9, 1): Next

            LPrint Left$(careerDI$(B9), 13);

            For V = 1 To VV
                LPrint Tab(20); careerGIS!(V, B9, 2);
                LPrint Tab(34); "###"; careerGIS!(V, B9, 0)
            Next V

            careerTT!(32) = 0

            For V = 1 To VV
                careerTT!(32) = careerTT!(32) + careerGIS!(V, B9, 0)
            Next V

            LPrint "TOTALS";
            LPrint Tab(23); "##"; VV;
            LPrint Tab(33); "####"; careerTT!(32)

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub

'----------------------------------------
'   PrintCareerStatsSACK Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's sacks
Sub PrintCareerStatsSACK (exitFlag%, B9, careerFile$)

    Shared careerDS$()
    Shared careerGSS!(), careerTT!()

    Cls
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
	Color 12: Print careerDS$(B9);
	Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
	Color 12: Print careerDS$(B9);
    Color 14: Print " HIT 'E'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            LPrint careerFile$
            LPrint "SACKERS             YEAR     INTERCEPTIONS"
            VV = 0

            For I = 1 To 15: VV = VV + careerGSS!(I, B9, 1): Next

            LPrint Left$(careerDS$(B9), 13);

            For V = 1 To VV
                LPrint Tab(20); careerGSS!(V, B9, 2);
                LPrint Tab(34); "###"; careerGSS!(V, B9, 0)
            Next V

            careerTT!(33) = 0

            For V = 1 To VV
                careerTT!(33) = careerTT!(33) + careerGSS!(V, B9, 0)
            Next V

            LPrint "TOTALS";
            LPrint Tab(23); "##"; VV;
            LPrint Tab(33); "####"; careerTT!(33)

        Case "N"
            'Exit Sub

        Case "E"
            exitFlag% = 1
            'Exit Sub

    End Select

End Sub
