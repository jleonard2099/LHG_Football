SUB CARPRNT ()

	_Title "3-in-1 Football - Print Career Files"

	Shared careerA1$(), careerA2$(), careerA3$(), careerA4$(), careerA5$(), careerA6$(), careerA7$()
	Shared careerDI$(), careerDS$()
	
	Shared careerKRS$(), careerPRS$(), careerQBS$(), careerRBS$(), careerWRS$()
	Shared careerRC$()

	Shared FF()
	
	Shared careerGIS!(), careerGSS!()
	Shared careerFGS!(), careerKRS!(), careerPRS!(), careerPUS!()
	Shared careerQBS!(), careerRBS!(), careerWRS!()
	
	Dim II(15, 30), PP(15, 30), RC(6), TT(50), YY(15, 30)

	Dim GS%(1, 55), GI%(1, 55)

	Color 0, 0
	Cls
	TMR = 6420

	'review edits here but be cautious

	Do
	'Display list of files...

		Spec$ = diskPaths$(1) + "*.CAR"
		Count% = FCount%(Spec$)

		If Count% = 0 Then 
			Cls
			Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
			I$ = GetKeyPress$
			Call CSMsgBox("", 0, Pass)

		Else

			ReDim careerFiles$(0 To Count%)
			For X = 1 To Count%
				careerFiles$(X) = Space$(12)
			Next
			careerFiles$(0) = Spec$
			Call ReadFile(careerFiles$())
			

			Do
				Cls
				Color , 1
				Cls
				Locate , 28
				Color 14
				Print "SELECT FILE TO VIEW": Color 15
				Selection% = 1
				START% = 1
				FileFGColr% = 15: FileBGColr% = 1
				HiLiteFGColr% = 0: HiLiteBGColr% = 7
				Rows% = 20
				Columns% = 4
				Spacing% = 6
				Row% = 4
				Column% = 1
				Locate 25, 1
				Color 14
				Print "ENTER";: Color 15
				Print "-SELECTS FILE";

				_MouseShow "DEFAULT"
				MAMenu careerFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
				_MouseHide

				If ExitCode% <> 27 Then

					targetFile$ = careerFiles$(Selection%)
					Locate 2, 33: Print targetFile$
					Locate , 33: Print "ANY CHANGE (YN)";
					Color 15, 0
					
					Do
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

				End If

			Loop Until UCase$(I$) = "N"

			If ExitCode% <> 27 Or I$ <> Chr$(27) Then

				targetFile$ = RTrim$(targetFile$)
				Z$ = Left$(targetFile$, Len(targetFile$) - 4)
				Color 15
				Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

				Open diskPaths$(1) + Z$ + ".CAR" For Input As #1
				Input #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK
				For I = 0 To 49: Input #1, careerA1$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 39: Input #1, careerA2$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 19: Input #1, careerA3$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 14: Input #1, careerA4$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 14: Input #1, careerA5$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 9: Input #1, careerA6$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 9: Input #1, careerA7$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 54: Input #1, careerDI$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				For I = 0 To 54: Input #1, careerDS$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
				
				For N = 1 To 15: 
					For J = 0 To 49: 
						For K = 0 To 9: 
							Input #1, RBS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 49: 
						Input #1, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 39: 
						For K = 0 To 5: 
							Input #1, WRS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 39: 
						Input #1, careerWRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 19: 
						For K = 0 To 7: 
							Input #1, QBS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 19: 
						Input #1, careerQBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 14: 
						For K = 0 To 5: 
							Input #1, KRS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR:
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 14: 
						Input #1, careerKRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 14: 
						For K = 0 To 5: 
							Input #1, PRS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 14: 
						Input #1, careerPRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 9: 
						For K = 0 To 3: 
							Input #1, PUS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 9: 
						For K = 0 To 16: 
							Input #1, FGS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 54: 
						For K = 0 To 2: 
							Input #1, GIS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				For N = 1 To 15: 
					For J = 0 To 54: 
						For K = 0 To 2: 
							Input #1, GSS(N, J, K)
						Next K
						Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR
					Next J
				Next N

				Close #1

				Open "REPORT" For Output As #2
				Print #2, Z$
				Print #2, "RUSHER         YEAR   ATT   YARDS AVERAGE  TDS   REC  YDS  AVG  TDS RUN-LONG-REC"

				For I = 0 To 50

					If careerA1$(I) <> "" And careerA1$(I) <> "XXX" Then

						VV = RBS(1, I, 7) + RBS(2, I, 7) + RBS(3, I, 7) + RBS(4, I, 7) + RBS(5, I, 7) + RBS(6, I, 7) + RBS(7, I, 7) + RBS(7, I, 7) + RBS(9, I, 7) + RBS(10, I, 7) + RBS(11, I, 7) + RBS(12, I, 7) + RBS(13, I, 7) + RBS(14, I, 7) + RBS(15, I, 7)

						Print #2, Left$(careerA1$(I), 13);

						For V = 1 To VV
							Print #2, Tab(15); RBS(V, I, 8);
							Print #2, Tab(23); Using "###"; RBS(V, I, 0);
							Print #2, Tab(30); Using "####"; RBS(V, I, 1);

							If RBS(V, I, 1) = 0 Or RBS(V, I, 1) = 0 Then Print #2, Tab(37); " 0.0";
							If RBS(V, I, 0) > 0 Then Print #2, Tab(37); Using "##.#"; RBS(V, I, 1) / RBS(V, I, 0);

							Print #2, Tab(44); Using "###"; RBS(V, I, 2);
							Print #2, Tab(50); Using "###"; RBS(V, I, 3);
							Print #2, Tab(54); Using "####"; RBS(V, I, 4);

							If RBS(V, I, 4) = 0 Or RBS(V, I, 3) = 0 Then Print #2, Tab(59); " 0.0";
							If RBS(V, I, 3) > 0 Then Print #2, Tab(59); Using "##.#"; RBS(V, I, 4) / RBS(V, I, 3);

							Print #2, Tab(65); Using "##"; RBS(V, I, 5);
							Print #2, Tab(71); Using "##"; RBS(V, I, 6);: Print #2, careerRBS$(V, I, 0);
							Print #2, Tab(77); Using "##"; RBS(V, I, 9);: Print #2, careerRBS$(V, I, 1)
						Next V

						RC(0) = 0
						RC(1) = 0
						careerRC$(0) = ""
						careerRC$(1) = ""

						For V = 1 To VV
							TT1 = TT1 + RBS(V, I, 0)
							TT2 = TT2 + RBS(V, I, 1)
							TT3 = TT3 + RBS(V, I, 2)
							TT4 = TT4 + RBS(V, I, 3)
							TT5 = TT5 + RBS(V, I, 4)
							TT6 = TT6 + RBS(V, I, 5)

							If RBS(V, I, 6) > RC(0) Then 
								RC(0) = RBS(V, I, 6)
								careerRC$(0) = careerRBS$(V, I, 0)
							Else
								If RBS(V, I, 9) > RC(1) Then 
									RC(1) = RBS(V, I, 9)
									careerRC$(1) = careerRBS$(V, I, 1)
								End If
							End If

						Next V
						
						Print #2, "TOTALS";
						Print #2, Tab(18); Using "##"; VV;
						Print #2, Tab(22); Using "####"; TT1;
						Print #2, Tab(29); Using "#####"; TT2;

						If TT1 = 0 Or TT2 = 0 Then Print #2, Tab(37); " 0.0";
						If TT1 > 0 Then Print #2, Tab(37); Using "##.#"; TT2 / TT1;

						Print #2, Tab(44); Using "###"; TT3;
						Print #2, Tab(49); Using "####"; TT4;
						Print #2, Tab(53); Using "#####"; TT5;

						If TT4 = 0 Or TT5 = 0 Then Print #2, Tab(59); " 0.0";
						If TT4 > 0 Then Print #2, Tab(59); Using "##.#"; TT5 / TT4;

						Print #2, Tab(64); Using "###"; TT6;
						Print #2, Tab(71); Using "##"; RC(0);
						Print #2, careerRC$(0);
						Print #2, Tab(77); Using "##"; RC(1);
						Print #2, careerRC$(1)

						TT1 = 0
						TT2 = 0
						TT3 = 0
						TT4 = 0
						TT5 = 0
						TT6 = 0

						Print #2,

					Next I

				End If

				Print #2,
				Print #2, Z$
				Print #2, "RECEIVER        YEAR        REC.      YARDS      AVG.     TD  LONGEST REC."

				For I = 0 To 40

					If careerA2$(I) <> "" And careerA2$(I) <> "XXX" Then

						VV = WRS(1, I, 4) + WRS(2, I, 4) + WRS(3, I, 4) + WRS(4, I, 4) + WRS(5, I, 4) + WRS(6, I, 4) + WRS(7, I, 4) + WRS(8, I, 4) + WRS(9, I, 4) + WRS(10, I, 4) + WRS(11, I, 4) + WRS(12, I, 4) + WRS(13, I, 4) + WRS(14, I, 4) + WRS(15, I, 4)

						Print #2, Left$(careerA2$(I), 13);

						For V = 1 To VV
							Print #2, Tab(16); WRS(V, I, 5);
							Print #2, Tab(29); Using "###"; WRS(V, I, 0);
							Print #2, Tab(40); Using "####"; WRS(V, I, 1);

							If WRS(V, I, 0) = 0 Or WRS(V, I, 1) = 0 Then Print #2, Tab(49); " 0.0";
							If WRS(V, I, 0) > 0 Then Print #2, Tab(49); Using "##.#"; WRS(V, I, 1) / WRS(V, I, 0);

							Print #2, Tab(59); Using "##"; WRS(V, I, 2);
							Print #2, Tab(66); Using "##"; WRS(V, I, 3);
							Print #2, careerWRS$(V, I, 0)
						Next V

						RC(0) = 0
						careerRC$(0) = ""

						For V = 1 To VV
							TT7 = TT7 + WRS(V, I, 0)
							TT8 = TT8 + WRS(V, I, 1)
							TT9 = TT9 + WRS(V, I, 2)

							If WRS(V, I, 3) > RC(0) Then 
								RC(0) = WRS(V, I, 3)
								careerRC$(0) = careerWRS$(V, I, 0)
							End If

						Next V

						Print #2, "TOTALS";
						Print #2, Tab(19); Using "##"; VV;
						Print #2, Tab(28); Using "####"; TT7;
						Print #2, Tab(39); Using "#####"; TT8;

						If TT7 = 0 Or TT8 = 0 Then Print #2, Tab(49); " 0.0";
						If TT7 > 0 Then Print #2, Tab(49); Using "##.#"; TT8 / TT7;

						Print #2, Tab(58); Using "###"; TT9;
						Print #2, Tab(66); Using "##"; RC(0);
						Print #2, careerRC$(0)

						TT7 = 0
						TT8 = 0
						TT9 = 0

						Print #2,

					End If

				Next I

				Print #2,
				Print #2, Z$
				Print #2, "QUARTERBACK  YEAR  COMP.    ATT.    INT  TDS   YARDS  PCT. YD/COMP  RATING  LONG"

				For I = 0 To 19

					If careerA3$(I) <> "" And careerA3$(I) <> "XXX" Then

						VV = QBS(1, I, 6) + QBS(2, I, 6) + QBS(3, I, 6) + QBS(4, I, 6) + QBS(5, I, 6) + QBS(6, I, 6) + QBS(7, I, 6) + QBS(8, I, 6) + QBS(9, I, 6) + QBS(10, I, 6) + QBS(11, I, 6) + QBS(12, I, 6) + QBS(13, I, 6) + QBS(14, I, 6) + QBS(15, I, 6)

						Print #2, Left$(careerA3$(I), 13);

						For V = 1 To VV

							Print #2, Tab(13); QBS(V, I, 7);
							Print #2, Tab(21); Using "###"; QBS(V, I, 1);
							Print #2, Tab(29); Using "###"; QBS(V, I, 0);
							Print #2, Tab(38); Using "##"; QBS(V, I, 2);
							Print #2, Tab(43); Using "##"; QBS(V, I, 4);
							Print #2, Tab(49); Using "####"; QBS(V, I, 3);

							If QBS(V, I, 1) = 0 Or QBS(V, I, 0) = 0 Then Print #2, Tab(56); " 00";
							If QBS(V, I, 1) > 0 Then Print #2, Tab(56); Using "##"; (QBS(V, I, 1) / QBS(V, I, 0)) * 100;
							If QBS(V, I, 0) = 0 Then Print #2, Tab(63); " 0.0";
							If QBS(V, I, 1) > 0 Then Print #2, Tab(63); Using "##.#"; QBS(V, I, 3) / QBS(V, I, 1);

							If QBS(V, I, 0) > 0 Then QBS(V, I, 11) = Int(QBS(V, I, 1) / QBS(V, I, 0) * 1000) / 10
							
							If QBS(V, I, 0) = 0 Then 
								QBS(V, I, 11) = 0
								QBS(V, I, 8) = 0
								QBS(V, I, 10) = 0
								QBS(V, I, 9) = 0
							Else
								QBS(V, I, 8) = Int(QBS(V, I, 5) / QBS(V, I, 0) * 1000) / 10
								QBS(V, I, 10) = Int(QBS(V, I, 4) / QBS(V, I, 0) * 100) / 100
								QBS(V, I, 9) = Int(QBS(V, I, 3) / QBS(V, I, 0) * 1000) / 10
							End If

							PP(V, I) = QBS(V, I, 11) - 30
							
							If PP(V, I) < 0 Then 
								PP(V, I) = 0
							Else
								If PP(V, I) > 77.5 Then 
									PP(V, I) = 2.375
								Else
									PP(V, I) = PP(V, I) * .05
								End If
							End If

							If QBS(V, I, 8) > 11.9 Then 
								FF(V, I) = 2.375
							Else
								FF(V, I) = QBS(V, I, 8) * .2
							End If

							If QBS(V, I, 9) = 0 Then 
								II(V, I) = 0
							Else
								If QBS(V, I, 9) > 9.5 Then 
									II(V, I) = 0
								Else
									II(V, I) = 2.375 - (.25 * QBS(V, I, 9))
								End If
							End If

							QBS(V, I, 10) = QBS(V, I, 10) - 3

							If QBS(V, I, 10) < 0 Then 
								YY(V, I) = 0
							Else
								If QBS(V, I, 10) > 12.5 Then 
									YY(V, I) = 2.375
								Else
									D = Int(QBS(V, I, 10) * 100) / 2
									If D = QBS(V, I, 10) / 2 Then 
										YY(V, I) = QBS(V, I, 10) / 2 * .3 + QBS(V, I, 10) / 2 * .2
									Else
										YY(V, I) = QBS(V, I, 10) / 2 * .3 + QBS(V, I, 10) / 2 * .2 + .003
									End If
								End If
							End If

							QBS(V, I, 12) = PP(V, I) + FF(V, I) + II(V, I) + YY(V, I)
							If QBS(V, I, 12) > 9.5 Then 
								QBS(V, I, 12) = 158.3
							Else
								QBS(V, I, 12) = QBS(V, I, 12) * 50 / 3
							End If

							QBS(V, I, 12) = Int(QBS(V, I, 12) * 10) / 10

							Print #2, Tab(70); Using "###.#"; QBS(V, I, 12);
							Print #2, Tab(77); Using "##"; QBS(V, I, 5);
							Print #2, careerQBS$(V, I, 0)

						Next V

						RC(0) = 0
						careerRC$(0) = ""

						For V = 1 To VV
							TT10 = TT10 + QBS(V, I, 1)
							TT11 = TT11 + QBS(V, I, 0)
							TT12 = TT12 + QBS(V, I, 2)
							TT13 = TT13 + QBS(V, I, 4)
							TT14 = TT14 + QBS(V, I, 3)

							If QBS(V, I, 5) > RC(0) Then 
								RC(0) = QBS(V, I, 5)
								careerRC$(0) = careerQBS$(V, I, 0)
							End If

						Next V

						Print #2, "TOTALS";
						Print #2, Tab(16); Using "##"; VV;
						Print #2, Tab(20); Using "####"; TT10;
						Print #2, Tab(28); Using "####"; TT11;
						Print #2, Tab(37); Using "###"; TT12;
						Print #2, Tab(41); Using "####"; TT13;
						Print #2, Tab(48); Using "#####"; TT14;

						If TT10 = 0 Or TT11 = 0 Then Print #2, Tab(56); " 00";
						If TT10 > 0 Then Print #2, Tab(56); Using "##"; (TT10 / TT11) * 100;
						If TT11 = 0 Then Print #2, Tab(63); " 0.0";
						If TT10 > 0 Then Print #2, Tab(63); Using "##.#"; TT14 / TT10;

						If TT11 > 0 Then TT30 = Int(TT10 / TT11 * 1000) / 10
						If TT11 = 0 Then 
							TT27 = 0
							TT28 = 0
							TT29 = 0
							TT30 = 0
						Else
							TT27 = Int(TT13 / TT11 * 1000) / 10
							TT29 = Int(TT14 / TT11 * 100) / 100
							TT28 = Int(TT12 / TT11 * 1000) / 10
						End If

						PPT = TT30 - 30
						
						If PPT < 0 Then 
							PPT = 0
						ELse
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

						Print #2, Tab(70); Using "###.#"; TT31;
						Print #2, Tab(77); Using "##"; RC(0);
						Print #2, careerRC$(0)
						
						TT10 = 0
						TT11 = 0
						TT12 = 0
						TT13 = 0
						TT14 = 0

						Print #2,

					End If

				Next I

				Print #2,
				Print #2, Z$
				Print #2, "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

				For I = 0 To 14

					If careerA4$(I) <> "" And careerA4$(I) <> "XXX" Then

						VV = KRS(1, I, 4) + KRS(2, I, 4) + KRS(3, I, 4) + KRS(4, I, 4) + KRS(5, I, 4) + KRS(6, I, 4) + KRS(7, I, 4) + KRS(8, I, 4) + KRS(9, I, 4) + KRS(10, I, 4) + KRS(11, I, 4) + KRS(12, I, 4) + KRS(13, I, 4) + KRS(14, I, 4) + KRS(15, I, 4)

						Print #2, Left$(careerA4$(I), 13);

						For V = 1 To VV
							Print #2, Tab(22); KRS(V, I, 5);
							Print #2, Tab(32); Using "###"; KRS(V, I, 0);
							Print #2, Tab(42); Using "####"; KRS(V, I, 1);

							If KRS(V, I, 0) = 0 Or KRS(V, I, 1) = 0 Then Print #2, Tab(52); " 00";
							If KRS(V, I, 0) > 0 Then Print #2, Tab(52); Using "##.#"; KRS(V, I, 1) / KRS(V, I, 0);

							Print #2, Tab(62); Using "##"; KRS(V, I, 2);
							Print #2, Tab(70); Using "###"; KRS(V, I, 3);
							Print #2, careerKRS$(V, I, 0)
						Next V

						RC(0) = 0
						careerRC$(0) = ""

						For V = 1 To VV
							TT15 = TT15 + KRS(V, I, 0)
							TT16 = TT16 + KRS(V, I, 1)
							TT17 = TT17 + KRS(V, I, 2)

							If KRS(V, I, 3) > RC(0) Then 
								RC(0) = KRS(V, I, 3)
								careerRC$(0) = careerKRS$(V, I, 0)
							End If

						Next V

						Print #2, "TOTALS";
						Print #2, Tab(25); Using "##"; VV;
						Print #2, Tab(32); Using "###"; TT15;
						Print #2, Tab(41); Using "#####"; TT16;

						If TT15 = 0 Or TT16 = 0 Then Print #2, Tab(52); " 00";
						If TT15 > 0 Then Print #2, Tab(52); Using "##.#"; TT16 / TT15;

						Print #2, Tab(61); Using "###"; TT17;
						Print #2, Tab(70); Using "###"; RC(0);: Print #2, careerRC$(0)

						TT15 = 0
						TT16 = 0
						TT17 = 0

						Print #2,
					End If
				
				Next I

				Print #2,
				Print #2, Z$
				Print #2, "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

				For I = 0 To 14
					If careerA5$(I) = "" Or careerA5$(I) = "XXX" Then 1328
					VV = PRS(1, I, 4) + PRS(2, I, 4) + PRS(3, I, 4) + PRS(4, I, 4) + PRS(5, I, 4) + PRS(6, I, 4) + PRS(7, I, 4) + PRS(8, I, 4) + PRS(9, I, 4) + PRS(10, I, 4) + PRS(11, I, 4) + PRS(12, I, 4) + PRS(13, I, 4) + PRS(14, I, 4) + PRS(15, I, 4)

					1306 Print #2, Left$(careerA5$(I), 13);
					For V = 1 To VV
						Print #2, Tab(22); PRS(V, I, 5);
						Print #2, Tab(32); Using "###"; PRS(V, I, 0);
						Print #2, Tab(42); Using "####"; PRS(V, I, 1);
						If PRS(V, I, 0) = 0 Or PRS(V, I, 1) = 0 Then Print #2, Tab(52); " 00";
						If PRS(V, I, 0) > 0 Then Print #2, Tab(52); Using "##.#"; PRS(V, I, 1) / PRS(V, I, 0);
						Print #2, Tab(62); Using "##"; PRS(V, I, 2);
						Print #2, Tab(70); Using "###"; PRS(V, I, 3);: Print #2, careerPRS$(V, I, 0)
					Next V
					RC(0) = 0: careerRC$(0) = ""
					For V = 1 To VV
						TT18 = TT18 + PRS(V, I, 0)
						TT19 = TT19 + PRS(V, I, 1)
						TT20 = TT20 + PRS(V, I, 2)
						If PRS(V, I, 3) > RC(0) Then 1317 Else GoTo 1319

						1317 RC(0) = PRS(V, I, 3): careerRC$(0) = careerPRS$(V, I, 0)

					1319 Next V
					Print #2, "TOTALS";
					Print #2, Tab(25); Using "##"; VV;
					Print #2, Tab(32); Using "###"; TT18;
					Print #2, Tab(41); Using "#####"; TT19;
					If TT18 = 0 Or TT19 = 0 Then Print #2, Tab(52); " 00";
					If TT18 > 0 Then Print #2, Tab(52); Using "##.#"; TT19 / TT18;
					Print #2, Tab(61); Using "###"; TT20;
					Print #2, Tab(70); Using "###"; RC(0);: Print #2, careerRC$(0)
					TT18 = 0: TT19 = 0: TT20 = 0
					Print #2,

				1328 Next I

				Print #2,
				Print #2, Z$
				Print #2, "PUNTERS           YEAR     ATT.     YARDS      AVG."

				For I = 0 To 9

					If careerA6$(I) <> "" And careerA6$(I) <> "XXX" Then

						VV = PUS(1, I, 2) + PUS(2, I, 2) + PUS(3, I, 2) + PUS(4, I, 2) + PUS(5, I, 2) + PUS(6, I, 2) + PUS(7, I, 2) + PUS(8, I, 2) + PUS(9, I, 2) + PUS(10, I, 2) + PUS(11, I, 2) + PUS(12, I, 2) + PUS(13, I, 2) + PUS(14, I, 2) + PUS(15, I, 2)

						Print #2, Left$(careerA6$(I), 13);
						
						For V = 1 To VV
							Print #2, Tab(18); PUS(V, I, 3);
							Print #2, Tab(28); Using "###"; PUS(V, I, 0);
							Print #2, Tab(38); Using "####"; PUS(V, I, 1);
							If PUS(V, I, 0) = 0 Or PUS(V, I, 1) = 0 Then Print #2, Tab(47); "00.0"
							If PUS(V, I, 0) > 0 Then Print #2, Tab(47); Using "##.#"; PUS(V, I, 1) / PUS(V, I, 0)
						Next V
						
						For V = 1 To VV
							TT21 = TT21 + PUS(V, I, 0)
							TT22 = TT22 + PUS(V, I, 1)
						Next V
						
						Print #2, "TOTALS";
						Print #2, Tab(21); Using "##"; VV;
						Print #2, Tab(27); Using "####"; TT21;
						Print #2, Tab(37); Using "#####"; TT22;
						
						If TT21 = 0 Or TT22 = 0 Then Print #2, Tab(47); " 00"
						If TT21 > 0 Then Print #2, Tab(47); Using "##.#"; TT22 / TT21
						
						TT21 = 0: TT22 = 0
						Print #2,

					End If

				Next I

				Print #2,
				Print #2, Z$
				Print #2, "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49 50- +"

				For I = 0 To 9

					If careerA7$(I) <> "" And careerA7$(I) <> "XXX" Then
						VV = FGS(1, I, 15) + FGS(2, I, 15) + FGS(3, I, 15) + FGS(4, I, 15) + FGS(5, I, 15) + FGS(6, I, 15) + FGS(7, I, 15) + FGS(8, I, 15) + FGS(9, I, 15) + FGS(10, I, 15) + FGS(11, I, 15) + FGS(12, I, 15) + FGS(13, I, 15) + FGS(14, I, 15) + FGS(15, I, 15)

						Print #2, Left$(careerA7$(I), 13);

						For V = 1 To VV
							Print #2, Tab(13); FGS(V, I, 16);
							Print #2, Tab(19); Using "###"; FGS(V, I, 3);
							Print #2, Tab(23); Using "###"; FGS(V, I, 2);

							If FGS(V, I, 2) = 0 Or FGS(V, I, 3) = 0 Then Print #2, Tab(27); "  0";
							If FGS(V, I, 2) > 0 Then Print #2, Tab(27); Using "###"; (FGS(V, I, 2) / FGS(V, I, 3)) * 100;

							Print #2, Tab(32); Using "###"; FGS(V, I, 1);
							Print #2, Tab(36); Using "###"; FGS(V, I, 0);

							If FGS(V, I, 0) = 0 Or FGS(V, I, 1) = 0 Then Print #2, Tab(40); "  0";
							If FGS(V, I, 0) > 0 Then Print #2, Tab(40); Using "###"; (FGS(V, I, 0) / FGS(V, I, 1)) * 100;

							Print #2, Tab(46); Using "##"; FGS(V, I, 4);
							Print #2, Tab(49); Using "##"; FGS(V, I, 5);: Print #2, "-";
							Print #2, Tab(52); Using "##"; FGS(V, I, 10);
							Print #2, Tab(55); Using "##"; FGS(V, I, 6);: Print #2, "-";
							Print #2, Tab(58); Using "##"; FGS(V, I, 11);
							Print #2, Tab(61); Using "##"; FGS(V, I, 7);: Print #2, "-";
							Print #2, Tab(64); Using "##"; FGS(V, I, 12);
							Print #2, Tab(67); Using "##"; FGS(V, I, 8);: Print #2, "-";
							Print #2, Tab(70); Using "##"; FGS(V, I, 13);
							Print #2, Tab(73); Using "##"; FGS(V, I, 9);: Print #2, "-";
							Print #2, Tab(76); Using "##"; FGS(V, I, 14)
						Next V

						For V = 1 To VV
							TT(23) = TT(23) + FGS(V, I, 0)
							TT(24) = TT(24) + FGS(V, I, 1)
							TT(34) = TT(34) + FGS(V, I, 2)
							TT(35) = TT(35) + FGS(V, I, 3)
							TT(36) = TT(36) + FGS(V, I, 5)
							TT(37) = TT(37) + FGS(V, I, 6)
							TT(38) = TT(38) + FGS(V, I, 7)
							TT(39) = TT(39) + FGS(V, I, 8)
							TT(40) = TT(40) + FGS(V, I, 9)
							TT(41) = TT(41) + FGS(V, I, 10)
							TT(42) = TT(42) + FGS(V, I, 11)
							TT(43) = TT(43) + FGS(V, I, 12)
							TT(44) = TT(44) + FGS(V, I, 13)
							TT(45) = TT(45) + FGS(V, I, 14)
						Next V

						RC(0) = 0

						For V = 1 To VV
							If FGS(V, I, 4) >= RC(0) Then 
								RC(0) = FGS(V, I, 4)
							End If
						Next

						Print #2, "TOTALS";
						Print #2, Tab(16); VV;
						Print #2, Tab(19); Using "###"; TT(35);
						Print #2, Tab(23); Using "###"; TT(34);

						If TT(34) = 0 Or TT(35) = 0 Then Print #2, Tab(27); "  0";
						If TT(34) > 0 Then Print #2, Tab(27); Using "###"; (TT(34) / TT(35)) * 100;

						Print #2, Tab(32); Using "###"; TT(24);
						Print #2, Tab(36); Using "###"; TT(23);

						If TT(23) = 0 Or TT(24) = 0 Then Print #2, Tab(40); "  0";
						If TT(23) > 0 Then Print #2, Tab(40); Using "###"; (TT(23) / TT(24)) * 100;

						Print #2, Tab(46); Using "##"; RC(0);
						Print #2, Tab(49); Using "##"; TT(36);: Print #2, "-";
						Print #2, Tab(52); Using "##"; TT(41);
						Print #2, Tab(55); Using "##"; TT(37);: Print #2, "-";
						Print #2, Tab(58); Using "##"; TT(42);
						Print #2, Tab(61); Using "##"; TT(38);: Print #2, "-";
						Print #2, Tab(64); Using "##"; TT(43);
						Print #2, Tab(67); Using "##"; TT(39);: Print #2, "-";
						Print #2, Tab(70); Using "##"; TT(44);
						Print #2, Tab(73); Using "##"; TT(40);: Print #2, "-";
						Print #2, Tab(76); Using "##"; TT(45)

						For J = 23 To 45: TT(J) = 0: Next

						Print #2,

					Next I

				End If

				Print #2,
				Print #2, Z$
				Print #2, "INTERCEPTORS        YEAR     INTERCEPTIONS"

				For I = 0 To 54

					If careerDI$(I) <> "" And careerDI$(I) <> "XXX" Then
						VV = GIS(1, I, 1) + GIS(2, I, 1) + GIS(3, I, 1) + GIS(4, I, 1) + GIS(5, I, 1) + GIS(6, I, 1) + GIS(7, I, 1) + GIS(8, I, 1) + GIS(9, I, 1) + GIS(10, I, 1) + GIS(11, I, 1) + GIS(12, I, 1) + GIS(13, I, 1) + GIS(14, I, 1) + GIS(15, I, 1)

						Print #2, Left$(careerDI$(I), 13);

						For V = 1 To VV
							Print #2, Tab(20); GIS(V, I, 2);
							Print #2, Tab(34); Using "###"; GIS(V, I, 0)
						Next V

						TT(32) = 0

						For V = 1 To VV
							TT(32) = TT(32) + GIS(V, I, 0)
						Next V

						Print #2, "TOTALS";
						Print #2, Tab(23); Using "##"; VV;
						Print #2, Tab(33); Using "####"; TT(32)
						Print #2,
					End If

				Next I

				Print #2,
				Print #2, Z$
				Print #2, "SACKERS             YEAR          SACKS"

				For I = 0 To 54

					If careerDS$(I) <> "" And careerDS$(I) <> "XXX" Then
						VV = GSS(1, I, 1) + GSS(2, I, 1) + GSS(3, I, 1) + GSS(4, I, 1) + GSS(5, I, 1) + GSS(6, I, 1) + GSS(7, I, 1) + GSS(8, I, 1) + GSS(9, I, 1) + GSS(10, I, 1) + GSS(11, I, 1) + GSS(12, I, 1) + GSS(13, I, 1) + GSS(14, I, 1) + GSS(15, I, 1)

						Print #2, Left$(careerDS$(I), 13);

						For V = 1 To VV
							Print #2, Tab(20); GSS(V, I, 2);
							Print #2, Tab(34); Using "###"; GSS(V, I, 0)
						Next V

						TT(33) = 0

						For V = 1 To VV
							TT(33) = TT(33) + GSS(V, I, 0)
						Next V

						Print #2, "TOTALS";
						Print #2, Tab(23); Using "##"; VV;
						Print #2, Tab(33); Using "####"; TT(33)
						Print #2,
					End If
					
				Next I

				Close 2

				Open "REPORT" For Input As #2
				Do While Not EOF(2)
					Line Input #2, X$
					LPrint X$
				Loop

				Close 2

				LPrint Chr$(12)

			End If 'Done checking for ESC

		End If 'Done checking file count

	'...Until we press ESC
	Loop Until ExitCode% = 27 Or Count% = 0

End Sub
