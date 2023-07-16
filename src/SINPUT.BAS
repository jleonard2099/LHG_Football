Sub SINPUT ()

    _Title "3-in-1 Football - Stats Input"

    Shared compZ2!()
    Shared AF!(), AM!(), K!(), K2!()
    Shared LC!(), LK!(), LR!(), PK!()

    Shared lookyDI$(), lookyDS$()
    Shared lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$()
    Shared lookyA5$(), lookyA6$(), lookyA7$()

    Dim inputLF!(0 to 1), W6!(1,1), Z!(14), Z1!(14)
    Dim sinputGI%(1,9), sinputGS%(1,14)

    Dim sinputsinputLC$(0 to 15), sinputLK$(0 to 2), sinputLP$(0 to 3), sinputLR$(0 to 9)
    Dim sinputPK$(0 to 2)

    Call GetTeam_SINPUT (T1$, YN$, D)

    statB$ = RTrim$(T1$)

    For XX = 4 To 11
        If Mid$(statB$, XX, 1) = " " Or Mid$(statB$, XX, 1) = "." Then Mid$(statB$, XX, 1) = "_"
    Next XX

    For XX = 12 To 14
        If Mid$(statB$, XX, 2) = "ST" Then Mid$(statB$, 9, 1) = "_": Mid$(statB$, 10, 2) = "ST"
    Next XX

    QQ = 8

    For XX = 11 To 4 Step -1
        If Mid$(statB$, XX, 1) = "_" Then
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

    Z$ = Mid$(statB$, 4, QQ) + "." + Left$(statB$, 2)

    If Not _FileExists(diskPaths$(1) + Z$) Then

        Locate 10, 8
        Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else


        OPEN D$(1)+Z$ FOR INPUT AS #1

        INPUT #1,A$(PX)

        FOR I=0 TO 9:
            INPUT #1,lookyA1$(PX,I)
        NEXT I
        
        FOR I=0 TO 5:
            INPUT #1,lookyA2$(PX,I)
        NEXT I
        
        FOR I = 0 TO 3:
            INPUT #1,lookyA3$(PX,I)
        NEXT I

        FOR I=0 TO 2:
            INPUT #1,lookyA4$(PX,I)
        NEXT I
        
        FOR I=0 TO 2
            INPUT #1,lookyA5$(PX,I)
        NEXT I
        
        INPUT #1,lookyA6$(PX,0)
        
        FOR I = 0 TO 1:
            INPUT #1,lookyA7$(PX,I)
        NEXT I

        FOR J=0 TO 13

            FOR K=0 TO 17:
                INPUT #1,compZ2!(J,K)
            NEXT K

        NEXT J

        FOR L=0 TO 14:
            INPUT #1,Z!(L)
        NEXT L

        FOR L=0 TO 14:
            INPUT #1,Z1!(L)
        NEXT L
        
        FOR J=0 TO 9:
            INPUT #1,lookyDI$(PX,J)
            INPUT #1,sinputGI%(PX,J)
        NEXT
        
        FOR J=0 TO 14:
            INPUT #1,lookyDS$(PX,J)
            INPUT #1,sinputGS%(PX,J)
        NEXT
        
        FOR I=0 TO 9:
            INPUT #1,LR!(I)
            INPUT #1,sinputLR$(I)
        NEXT
        
        FOR I=0 TO 15:
            INPUT #1,LC!(I)
            INPUT #1,sinputLC$(I)
        NEXT
        
        FOR I=0 TO 3:
            INPUT #1,LP!(I)
            INPUT #1,sinputLP$(I)
        NEXT
        
        FOR I=0 TO 2:
            INPUT #1,LK!(I)
            INPUT #1,sinputLK$(I)
        NEXT
        
        FOR I=0 TO 2:
            INPUT #1,PK!(I)
            INPUT #1,sinputPK$(I)
        NEXT
        
        FOR I=0 TO 1:
            INPUT #1,LF!(I)
            FOR I1=0 TO 4:
                INPUT #1,AM!(I,I1)
                INPUT #1,AF!(I,I1)
            NEXT
        NEXT

        CLOSE #1

        FOR J=0 TO 13
            FOR K=0 TO 17:
                K2!(PX,J,K)=compZ2!(J,K)
            NEXT K
        NEXT J

        FOR L=0 TO 12:
            K!(0,L)=Z!(L)
        NEXT L

        FOR L=0 TO 12:
            K!(1,L)=Z1!(L)
        NEXT L

        W6!(0,0)=Z!(13)
        W6!(0,1)=Z!(14)
        W6!(1,0)=Z1!(13)
        W6!(1,1)=Z1!(14)

        '---------------
        ' Team Offense
        '---------------
        Color 15, 0
        Cls

        X$ = "TEAM OFFENSIVE STATS"
        Call TITLE_STAT_HEADER(X$)
            
        LOCATE 4,1

        PRINT "1. STAT FILE NAME ";Z$
        PRINT "2. TM 1ST DOWNS ";K!(0,0)
        PRINT "3. TM RUSH ATT ";K!(0,1)
        PRINT "4. TM RUSH YDS ";INT(K!(0,2))
        PRINT "5. TM PASS ATT ";K!(0,3)
        PRINT "6. TM PASS COM ";K!(0,4)
        PRINT "7. TM PASS INT ";K!(0,5)
        PRINT "8. TM PASS YDS ";INT(K!(0,6))
        PRINT "9. TM PUNTS ";K!(0,7)
        PRINT "10. TM TOTAL PUNT YDS ";INT(K!(0,8))
        PRINT "11. TM FUMBLES ";K!(0,9)
        PRINT "12. TM FUM LOST ";K!(0,10)
        PRINT "13. TM PENALTIES ";K!(0,11)
        PRINT "14. TM PENALTY YDS ";INT(K!(0,12))
        PRINT "15. TM TIMES SACKED ";W6!(0,0)
        PRINT "16. TM YARDS LOST FROM SACKS ";INT(W6!(0,1))

        Do

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 16

            Select Case B9

                Case 1
                    LOCATE 4,1:INPUT "1. STAT FILE NAME ";Z$

                Case 2
                    LOCATE 5,1:INPUT "2. TM 1ST DOWNS ";K!(0,0)

                Case 3
                    LOCATE 6,1:INPUT "3. TM RUSH ATT ";K!(0,1)

                Case 4
                    LOCATE 7,1:INPUT "4. TM RUSH YDS ";K!(0,2)

                Case 5
                    LOCATE 8,1:INPUT "5. TM PASS ATT ";K!(0,3)

                Case 6
                    LOCATE 9,1:INPUT "6. TM PASS COM ";K!(0,4)

                Case 7
                    LOCATE 10,1:INPUT "7. TM PASS INT ";K!(0,5)

                Case 8
                    LOCATE 11,1:INPUT "8. TM PASS YDS ";K!(0,6)

                Case 9
                    LOCATE 12,1:INPUT "9. TM PUNTS ";K!(0,7)

                Case 10
                    LOCATE 13,1:INPUT "10. TM TOTAL PUNT YDS ";K!(0,8)

                Case 11
                    LOCATE 14,1:INPUT "11. TM FUMBLES ";K!(0,9)

                Case 12
                    LOCATE 15,1:INPUT "12. TM FUM LOST ";K!(0,10)

                Case 13
                    LOCATE 16,1:INPUT "13. TM PENALTIES ";K!(0,11)

                Case 14
                    LOCATE 17,1:INPUT "14. TM PENALTY YDS ";K!(0,12)

                Case 15
                    LOCATE 18,1:INPUT "15. TM TIMES SACKED ";W6!(0,0)

                Case 16
                    LOCATE 19,1:INPUT "16. TM YARDS LOST FROM SACKS ";W6!(0,1)

            End Select
            
        Loop Until B9 >= 0



        '---------------
        ' Team Defense
        '---------------
        Cls

        X$ = "TEAM DEFENSIVE STATS"
        Call TITLE_STAT_HEADER(X$)
        
        LOCATE 4,1  
        PRINT "1. TM 1ST DOWNS ";K!(1,0)
        PRINT "2. TM RUSH ATT ";K!(1,1)
        PRINT "3. TM RUSH YDS ";INT(K!(1,2))
        PRINT "4. TM PASS ATT ";K!(1,3)
        PRINT "5. TM PASS COM ";K!(1,4)
        PRINT "6. TM PASS INT ";K!(1,5)
        PRINT "7. TM PASS YDS ";INT(K!(1,6))
        PRINT "8. TM PUNTS ";K!(1,7)
        PRINT "9. TM TOTAL PUNT YDS ";INT(K!(1,8))
        PRINT "10. TM FUMBLES ";K!(1,9)
        PRINT "11. TM FUM LOST ";K!(1,10)
        PRINT "12. TM PENALTIES ";K!(1,11)
        PRINT "13. TM PENALTY YDS ";INT(K!(1,12))
        PRINT "14. TM TIMES SACKED ";W6!(1,0)
        PRINT "15. TM YARDS LOST FROM SACKS ";INT(W6!(1,1))

        Do

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 15

            Select Case B9

                Case 1
                    LOCATE 4,1:INPUT "1. TM 1ST DOWNS ";K!(1,0)

                Case 2
                    LOCATE 5,1:INPUT "2. TM RUSH ATT ";K!(1,1)

                Case 3
                    LOCATE 6,1:INPUT "3. TM RUSH YDS ";K!(1,2)

                Case 4
                    LOCATE 7,1:INPUT "4. TM PASS ATT ";K!(1,3)

                Case 5
                    LOCATE 8,1:INPUT "5. TM PASS COM ";K!(1,4)

                Case 6
                    LOCATE 9,1:INPUT "6. TM PASS INT ";K!(1,5)

                Case 7
                    LOCATE 10,1:INPUT "7. TM PASS YDS ";K!(1,6)

                Case 8
                    LOCATE 11,1:INPUT "8. TM PUNTS ";K!(1,7)

                Case 9
                    LOCATE 12,1:INPUT "9. TM TOTAL PUNT YDS ";K!(1,8)

                Case 10
                    LOCATE 13,1:INPUT "10. TM FUMBLES ";K!(1,9)

                Case 11
                    LOCATE 14,1:INPUT "11. TM FUM LOST ";K!(1,10)

                Case 12
                    LOCATE 15,1:INPUT "12. TM PENALTIES ";K!(1,11)

                Case 13
                    LOCATE 16,1:INPUT "13. TM PENALTY YDS ";K!(1,12)

                Case 14
                    LOCATE 17,1:INPUT "14. TM TIMES SACKED ";W6!(1,0)

                Case 15
                    LOCATE 18,1:INPUT "15. TM YARDS LOST FROM SACKS ";W6!(1,1)

            End Select
            
        Loop Until B9 >= 0


        '---------------
        ' Running Backs
        '---------------
        Do

            Cls
            X$ = "CHANGE RUSHERS"
            Call TITLE_EDITSTATS(2, X$)
            Print

            FOR I1=0 TO 9:
                PRINT USING "##";I1+1;
                PRINT " ";lookyA1$(0,I1)
            NEXT

            Do
                LOCATE 20,1
                Input "# TO CHANGE (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 10

            If I > 0 Then

                I = I - 1

                Cls

                LOCATE 4,1
                PRINT "1. NAME ";lookyA1$(0,I1)
                PRINT "2. RUSHING ATTEMPTS ";K2!(0,I1,0)
                PRINT "3. RUSHING YARDAGE ";INT(K2!(0,I1,1))
                PRINT "4. RUSHING TD'S ";K2!(0,I1,2)
                PRINT "5. RECEPTIONS ";K2!(0,I1,3)
                PRINT "6. RECEIVING YARDAGE ";INT(K2!(0,I1,4))
                PRINT "7. RECEIVING TD'S ";K2!(0,I1,5)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 7


                    Select Case B9

                        Case 1
                            LOCATE 4,1:INPUT "1. NAME ";lookyA1$(0,I1)
                        
                        Case 2
                            LOCATE 5,1:INPUT "2. RUSHING ATTEMPTS ";K2!(0,I1,0)

                        Case 3
                            LOCATE 6,1:INPUT "3. RUSHING YARDAGE ";K2!(0,I1,1)

                        Case 4
                            LOCATE 7,1:INPUT "4. RUSHING TD'S ";K2!(0,I1,2)

                        Case 5
                            LOCATE 8,1:INPUT "5. RECEPTIONS ";K2!(0,I1,3)

                        Case 6
                            LOCATE 9,1:INPUT "6. RECEIVING YARDAGE ";K2!(0,I1,4)

                        Case 7
                            LOCATE 10,1:INPUT "7. RECEIVING TD'S ";K2!(0,I1,5)

                    End Select
            
                 Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Wide Receivers
        '---------------
        Do

            Cls
            X$ = "CHANGE RECEIVERS"
            Call TITLE_EDITSTATS(2, X$)
            Print

            FOR I1=0 TO 5:
                PRINT USING "##";I1+1;
                PRINT " ";lookyA2$(0,I1)
            NEXT

            Do
                LOCATE 20,1
                Input "# TO CHANGE (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 6

            If I > 0 Then

                I = I - 1

                Cls

                LOCATE 4,1
                PRINT "1. NAME ";lookyA2$(0,I1)
                PRINT "2. RECEPTIONS ";K2!(0,I1,6)
                PRINT "3. RECEIVING YARDAGE ";INT(K2!(0,I1,7))
                PRINT "4. RECEIVING TD'S ";K2!(0,I1,8)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    Select Case B9

                        Case 1
                            LOCATE 4,1:INPUT "1. NAME ";lookyA2$(0,I1)

                        Case 2
                            LOCATE 5,1:INPUT "2. RECEPTIONS ";K2!(0,I1,6)

                        Case 3
                            LOCATE 6,1:INPUT "3. RECEIVING YARDAGE ";K2!(0,I1,7)

                        Case 4
                            LOCATE 7,1:INPUT "4. RECEIVING TD'S ";K2!(0,I1,8)

                    End Select

                 Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Quarterbacks
        '---------------
        Do

            Cls
            X$ = "CHANGE QUARTERBACKS"
            Call TITLE_EDITSTATS(2, X$)
            Print

            FOR I1=0 TO 3:
                PRINT USING "##";I1+1;
                PRINT " ";lookyA3$(0,I1)
            NEXT

            Do
                LOCATE 20,1
                Input "# TO CHANGE (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 4

            If I > 0 Then

                I = I - 1

                Cls

                LOCATE 4,1
                PRINT "1. NAME ";lookyA3$(0,I1)
                PRINT "2. PASSES THROWN ";K2!(0,I1,9)
                PRINT "3. PASSES COMPLETED ";K2!(0,I1,10)
                PRINT "4. PASSES INTERCEPTED ";K2!(0,I1,11)
                PRINT "5. TOTAL PASSING YARDS ";INT(K2!(0,I1,12))
                PRINT "6. TOUCHDOWN PASSES ";K2!(0,I1,13)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 6

                    Select Case B9

                        Case 1
                            LOCATE 4,1:INPUT "1. NAME ";lookyA3$(0,I1)

                        Case 2
                            LOCATE 5,1:INPUT "2. PASSES THROWN ";K2!(0,I1,9)

                        Case 3
                            LOCATE 6,1:INPUT "3. PASSES COMPLETED ";K2!(0,I1,10)

                        Case 4
                            LOCATE 7,1:INPUT "4. PASSES INTERCEPTED ";K2!(0,I1,11)

                        Case 5
                            LOCATE 8,1:INPUT "5. TOTAL PASSING YARDS ";K2!(0,I1,12)

                        Case 6
                            LOCATE 9,1:INPUT "6. TOUCHDOWN PASSES ";K2!(0,I1,13)

                    End Select

                 Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Kickoff Returns
        '---------------
        Do

            Cls
            X$ = "CHANGE KICKOFF RETURNS"
            Call TITLE_EDITSTATS(2, X$)
            Print

            FOR I1=0 TO 2:
                PRINT USING "##";I1+1;
                PRINT " ";lookyA4$(0,I1)
            NEXT

            Do
                LOCATE 20,1
                Input "# TO CHANGE (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 3

            If I > 0 Then

                I = I - 1

                Cls

                LOCATE 4,1
                PRINT "1. NAME ";lookyA4$(0,I1)
                PRINT "2. KICKS RETURNED ";K2!(0,I1,14)
                PRINT "3. KICK RETURN YARDAGE ";INT(K2!(0,I1,15))
                PRINT "4. KICK RETURN TD'S ";K2!(0,I1,16)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    Select Case B9

                        Case 1
                            LOCATE 4,1:INPUT "1. NAME ";lookyA4$(0,I1)

                        Case 2
                            LOCATE 5,1:INPUT "2. KICKS RETURNED ";K2!(0,I1,14)

                        Case 3
                            LOCATE 6,1:INPUT "3. KICK RETURN YARDAGE ";K2!(0,I1,15)

                        Case 4
                            LOCATE 7,1:INPUT "4. KICK RETURN TD'S ";K2!(0,I1,16)

                    End Select

                 Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Punt Returns
        '---------------
        Do

            Cls
            X$ = "CHANGE PUNT RETURNS"
            Call TITLE_EDITSTATS(2, X$)
            Print

            FOR I1=3 TO 5:
                PRINT USING "##";I1-2;
                PRINT " ";lookyA5$(0,I1-3)
            NEXT

            Do
                LOCATE 20,1
                Input "# TO CHANGE (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 2

            If I > 0 Then

                I1 = I1 + 3

                Cls

                LOCATE 4,1
                PRINT "1. NAME ";lookyA5$(0,I1-3)
                PRINT "2. PUNTS RETURNED ";K2!(0,I1,14)
                PRINT "3. PUNT RETURN YARDAGE ";INT(K2!(0,I1,15))
                PRINT "4. PUNT RETURN TD'S ";K2!(0,I1,16)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    Select Case B9

                        Case 1
                            LOCATE 4,1:INPUT "1. NAME ";lookyA5$(0,I1-3)

                        Case 2
                            LOCATE 5,1:INPUT "2. PUNTS RETURNED ";K2!(0,I1,14)

                        Case 3
                            LOCATE 6,1:INPUT "3. PUNT RETURN YARDAGE ";K2!(0,I1,15)

                        Case 4
                            LOCATE 7,1:INPUT "4. PUNT RETURN TD'S ";K2!(0,I1,16)

                    End Select

                 Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Punters
        '---------------
        Cls
        X$ = "CHANGE PUNTER"
        Call TITLE_EDITSTATS(2, X$)

        LOCATE 4,1          
        PRINT "1. NAME ";lookyA6$(0,0)
        PRINT "2. PUNTS ";K!(0,7)
        PRINT "3. TOTAL PUNT YARDAGE ";INT(K!(0,8))

        Do

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 3

            Select Case B9

                Case 1
                    LOCATE 4,1:INPUT "1. NAME ";lookyA6$(0,0)

                Case 2
                    LOCATE 5,1:INPUT "2. PUNTS ";K!(0,7)

                Case 3
                    LOCATE 6,1:INPUT "3. TOTAL PUNT YARDAGE ";K!(0,8)

            End Select

        Loop Until B9 = 0



        '---------------
        ' Kickers
        '---------------
        Do

            Cls
            X$ = "CHANGE PLACE KICKERS"
            Call TITLE_EDITSTATS(2, X$)
            Print

            FOR I1=0 TO 1:
                PRINT USING "##";I1+1;
                PRINT " ";lookyA7$(0,I1)
            NEXT

            Do
                LOCATE 20,1
                Input "# TO CHANGE (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 2

            If I > 0 Then

                I = I - 1

                Cls

                LOCATE 4,1
                PRINT "1. NAME ";lookyA7$(0,I1)
                PRINT "2. FIELD GOALS ATT ";K2!(0,4+I1*4,17)
                PRINT "3. FIELD GOALS MADE ";K2!(0,3+I1*4,17)
                PRINT "4. EXTRA POINTS ATT ";K2!(0,6+I1*4,17)
                PRINT "5. EXTRA POINTS MADE ";K2!(0,5+I1*4,17)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            LOCATE 4,1:INPUT "1. NAME ";lookyA7$(0,I1)

                        Case 2
                            LOCATE 5,1:INPUT "2. FIELD GOALS ATT ";K2!(0,4+I1*4,17)

                        Case 3
                            LOCATE 6,1:INPUT "3. FIELD GOALS MADE ";K2!(0,3+I1*4,17)

                        Case 4
                            LOCATE 7,1:INPUT "4. EXTRA POINTS ATT ";K2!(0,6+I1*4,17)

                        Case 5
                            LOCATE 8,1:INPUT "5. EXTRA POINTS MADE ";K2!(0,5+I1*4,17)

                    End Select

                 Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Save Edits
        '---------------
        FOR J=0 TO 13
            FOR K=0 TO 17
                compZ2!(J,K)=K2!(PX,J,K)
            NEXT K
        NEXT J

        FOR L=0 TO 12:
            Z!(L)=K!(0,L)
        NEXT L
        
        FOR L=0 TO 12:
            Z1!(L)=K!(1,L)
        NEXT L

        Z!(13)=W6!(0,0)
        Z!(14)=W6!(0,1)
        Z1!(13)=W6!(1,0)
        Z1!(14)=W6!(1,1)

        OPEN D$(1)+Z$ FOR OUTPUT AS #1
        
        PRINT #1,A$(PX)
        
        FOR I=0 TO 9:
            PRINT #1,lookyA1$(PX,I)
        NEXT I
        
        FOR I=0 TO 5:
            PRINT #1,lookyA2$(PX,I)
        NEXT I
        
        FOR I = 0 TO 3:
            PRINT #1,lookyA3$(PX,I)
        NEXT I
        
        FOR I=0 TO 2:
            PRINT #1,lookyA4$(PX,I)
        NEXT I
        
        FOR I=0 TO 2:
            PRINT #1,lookyA5$(PX,I)
        NEXT I
        
        PRINT #1,lookyA6$(PX,0)
        
        FOR I = 0 TO 1:
            PRINT #1,lookyA7$(PX,I)
        NEXT I
        
        FOR J=0 TO 13
            FOR K=0 TO 17:
                PRINT #1,compZ2!(J,K)
            NEXT K
        NEXT J
        
        FOR L=0 TO 14:
            PRINT #1,Z!(L)
        NEXT L
        
        FOR L=0 TO 14:
            PRINT #1,Z1!(L)
        NEXT L
        
        FOR J=0 TO 9:
            PRINT #1,lookyDI$(PX,J)
            PRINT #1,sinputGI%(PX,J)
        NEXT
        
        FOR J=0 TO 14:
            PRINT #1,lookyDS$(PX,J)
            PRINT #1,sinputGS%(PX,J)
        NEXT
        
        FOR I=0 TO 9:
            PRINT #1,LR!(I)
            PRINT #1,sinputLR$(I)
        NEXT
        
        FOR I=0 TO 15:
            PRINT #1,LC!(I)
            PRINT #1,sinputLC$(I)
        NEXT
        
        FOR I=0 TO 3:
            PRINT #1,LP!(I)
            PRINT #1,sinputLP$(I)
        NEXT
        
        FOR I=0 TO 2:
            PRINT #1,LK!(I)
            PRINT #1,sinputLK$(I)
        NEXT
        
        FOR I=0 TO 2:
            PRINT #1,PK!(I)
            PRINT #1,sinputPK$(I)
        NEXT
        
        FOR I=0 TO 1:
            PRINT #1,LF!(I)
            FOR I1=0 TO 4:
                PRINT #1,AM!(I,I1)
                PRINT #1,AF!(I,I1)
            NEXT
        NEXT

        CLOSE #1

    End If

End Sub


'----------------------------------------
'     GetTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_SINPUT (A$, YN$, teamNbr)

    Shared teamRatings%()

    Do

        validTeam% = 1

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8: Color 15, 4
        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" And YN$ <> "" Then
            Call SelectTeam_SINPUT(YN$, NTMS, teamNbr)
        Else
            YN$ = RTrim$(NY$)
            Erase teamNames$
            NTMS = 0

            Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

            fileLength& = LOF(1)

            Field #1, 15 As Q$(0), 640 As X$

            For I1 = 1 To fileLength& / 934
                Get #1, I1
                For I2 = 1 To 3
                    If Asc(Left$(Q$(0), I2)) <= 32 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 2)) = "UN" Then validTeam% = 0: Exit For
                Next I2

                If validTeam% = 1 Then
                    NTMS = NTMS + 1
                    teamNames$(NTMS) = Q$(0)
                    teamIndex%(NTMS) = I1
                End If

            Next I1

            Close 1

            Call SelectTeam_SINPUT(YN$, NTMS, teamNbr)

        End If

        A$ = RTrim$(Q$(1))

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color teamRatings%(2), teamRatings%(1): Print A$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'     SelectTeam_SINPUT Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_SINPUT (YN$, NTMS, teamNbr)

    Shared teamRatings%()

    Selection% = 1
    Count% = NTMS
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamNbr = teamIndex%(Selection%)
        teamYear$ = RTrim$(YN$)

        Open diskPaths$(0) + "FOOT." + teamYear$ For Random As #1 Len = 934

        Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

        Get #1, teamNbr

        For I1 = 0 To 4
            teamRatings%(I1) = CVI(Q$(I1 + 11))
        Next I1

        Close 1

    Else

        BO% = 1

    End If

End Sub

