' PROGRAM NAME: HELLO.BAS
' COPYRIGHT 1984-2000 BY LANCE HAFFNER ALL RIGHTS RESERVED.
DefInt A-Z
DECLARE SUB GETKEY ()
DECLARE SUB TITLE ()
DECLARE SUB Comline (N, A$(), Max)
   
   DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
   DECLARE SUB box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
   DECLARE FUNCTION GetDrive% ()
   DECLARE SUB TextIn (T$, Max, NumOnly, CapsOn, ExitCode, Colr)
   DECLARE SUB YesNoB (YN$, ExitCode, Colr)
   DECLARE SUB ClearEOL (Colr)
   DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
   DECLARE FUNCTION FUsing$ (Number$, Mask$)
   DECLARE FUNCTION OneColor% (FG, BG)
   DECLARE FUNCTION Peek1% (Segment, Address)
   DECLARE SUB QPrint (X$, Colr, Page)
   DECLARE FUNCTION QPTRIM$ (X$)
   DECLARE SUB CapNum ()
   Dim D$(3), ITEM$(22)
   Dim A$(1 To 15)
   Drive = GetDrive%
   Call BLINKOFF
   DEF FnExt$(X$, Length) = X$ + Space$(Length - Len(X$))   'for display only
   Open "DEV.BAS" For Input As #1
   For I = 0 To 2
   Input #1, D$(I)
   Next
   Close 1
   
 Call Comline(N, A$(), 10)     ' Get what was typed on the command line
 Print "Number of arguments = "; N ' Print out each part of the command line
 Print "Arguments are: "
 For I = 1 To N: Print A$(I): Next I
 
' The following is a sample command line and output for a stand-alone
' executable file (assumes program name is ARG.EXE):
'
' arg one  two   three    four     five      six
 
' Use SUB procedure to get command line and split into arguments.
' Parameters:  NumArgs : Number of command-line arguments found
'              Args$() : Array in which to return arguments
'              MaxArgs : Maximum number of arguments array can return
   If A$(1) = "3" Then GoTo 800
   If A$(1) = "2" Then GoTo 700
1  Call TITLE
   ITEM$(1) = "PLAY SINGLE GAME                "
   ITEM$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
   ITEM$(3) = "REPLAY STATISTICS AND REPORTS   "
   ITEM$(4) = "LEADERS / STANDINGS / RECORDS   "
   ITEM$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
   ITEM$(6) = "HARD DRIVE INSTALLATION         "
   ITEM$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
   ITEM$(8) = "EXIT TO DOS                     "
     SELECTION% = 1
     Count% = 8
     Start% = 1
     SCANCODE% = 8
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     Call box0(7, 24, 16, 57, 2, 31)
     Call ShowCursor
     MMenuVert VarPtr(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     Call HideCursor
     COLOR , 0
     If SCANCODE% = 27 Then End

    If SELECTION% = 1 Then RUN "FOOTBALL.EXE"
    If SELECTION% = 2 Then GoTo 700
    If SELECTION% = 3 Then GoTo 750
    If SELECTION% = 4 Then GoTo 800
    If SELECTION% = 5 Then RUN "SCHEDULE.EXE"
    If SELECTION% = 6 Then RUN "HDCOPY.EXE"
    If SELECTION% = 7 Then GoTo 505
    If SELECTION% = 8 Then CLS: End


505 COLOR 15, 1
      FG = 0
      BG = 7
      For I = 0 To 2
      D$(I) = Left$(D$(I), Len(D$(I)) - 1)
      Next
   Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
CLS:      LOCATE , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print
510 LOCATE , 31: Print "PATHNAME EXAMPLES:": Print
515 LOCATE , 31: Print "A:"
520 LOCATE , 31: Print "C:\FULL\TEAMS"

    LOCATE 7, 1: QPrint "Path for Teams ", 30, -1
    LOCATE , 16: QPrint FnExt$(D$(0), 30), Colr, -1
    LOCATE 9, 1: QPrint "Path for Stats ", 30, -1
    LOCATE , 16: QPrint FnExt$(D$(1), 30), Colr, -1
    LOCATE 11, 1: QPrint "Path for Leaders ", 30, -1
    LOCATE , 18: QPrint FnExt$(D$(2), 30), Colr, -1

GetDiv1:

   LOCATE 7, 16                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30     'specify field parameters
   Call TextIn(D$(0), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   If XCode = 1 Then GoTo GetDiv1:      'can't back up any more, stay here
   If XCode = 2 Then GoTo Done          'all done entering data
   D$(0) = QPTRIM$(D$(0))
GetDiv2:

   LOCATE 9, 16                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
   Call TextIn(D$(1), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   If XCode = 1 Then GoTo GetDiv1:      'can't back up any more, stay here
   If XCode = 2 Then GoTo Done          'all done entering data
   D$(1) = QPTRIM$(D$(1))
GetDiv3:

   LOCATE 11, 18                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
   Call TextIn(D$(2), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   If XCode = 1 Then GoTo GetDiv2:      'can't back up any more, stay here
   If XCode = 2 Then GoTo Done          'all done entering data
   D$(2) = QPTRIM$(D$(2))

Done:

   LOCATE 23, 1
   Print "Is this okay? (Yes/No) ";

   X$ = ""
   Call YesNoB(X$, XCode, Colr)
   LOCATE , 1
   ClearEOL 30
   If XCode = 1 Then GoTo GetDiv3
   If X$ = "N" Then GoTo GetDiv1
   If X$ <> "Y" Then GoTo Done          'insist on either Y or N or up-arrow

580 Open "DEV.BAS" For Output As #1
585 For R = 0 To 2: D$(R) = D$(R) + "\": Next R
590 Print #1, D$(0)
595 Print #1, D$(1)
    Print #1, D$(2)
600 Close #1

    GoTo 1

700 Call TITLE
   ITEM$(1) = "INPUT A NEW TEAM            "
   ITEM$(2) = "EDIT AN EXISTING TEAM       "
   ITEM$(3) = "PRINT TEAM ROSTER / REPORT  "
   ITEM$(4) = "VIEW TEAM ROSTER / REPORT   "
   ITEM$(5) = "DRAFT TO NEW / EXISTING TEAM"
   ITEM$(6) = "TRADE PLAYERS               "
   ITEM$(7) = "RETURN TO 3 IN 1 MAIN MENU  "
     SELECTION% = 1
     Count% = 7
     Start% = 1
     SCANCODE% = 7
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 27
     Call box0(7, 26, 15, 55, 2, 31)
     Call ShowCursor
     MMenuVert VarPtr(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     Call HideCursor
     COLOR , 0
    If SCANCODE% = 27 Then GoTo 1
    If SELECTION% = 1 Then RUN "TINPUT.EXE"
    If SELECTION% = 2 Then RUN "CNGTM.EXE 1"
    If SELECTION% = 3 Then RUN "CNGTM.EXE 2"
    If SELECTION% = 4 Then RUN "CNGTM.EXE 3"
    If SELECTION% = 5 Then RUN "DRAFT.EXE"
    If SELECTION% = 6 Then RUN "TRADE.EXE"
    If SELECTION% = 7 Then GoTo 1

750 Call TITLE
   ITEM$(1) = "VIEW / PRINT COMPILED STATISTICS "
   ITEM$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
   ITEM$(3) = "MERGE SEPARATE STATISTICAL FILES "
   ITEM$(4) = "RETURN TO 3 IN 1 MAIN MENU       "
     SELECTION% = 1
     Count% = 4
     Start% = 1
     SCANCODE% = 4
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 25
     Call box0(7, 24, 12, 58, 2, 31)
     Call ShowCursor
     MMenuVert VarPtr(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     Call HideCursor
     COLOR , 0
     If SCANCODE% = 27 Then GoTo 1
    If SELECTION% = 1 Then RUN "LOOKY.EXE"
    If SELECTION% = 2 Then RUN "CAREER.EXE"
    If SELECTION% = 3 Then RUN "MERGE.EXE"
    If SELECTION% = 4 Then GoTo 1

800 Call TITLE
   ITEM$(1) = "CREATE/ EDIT           "
   ITEM$(2) = "UPDATE STATS/ STANDINGS"
   ITEM$(3) = "VIEW/ PRINT REPORTS    "
   ITEM$(4) = "HEAD-TO-HEAD STATS     "
   ITEM$(5) = "LEAGUE RECORDS         "
   ITEM$(6) = "EXPANDED LEADERS       "
   ITEM$(7) = "DELETE LEAGUE STATS    "
   ITEM$(8) = "CREATE/ EDIT           "
   ITEM$(9) = "UPDATE STATS/ STANDINGS"
   ITEM$(10) = "VIEW/ PRINT REPORTS    "
   ITEM$(11) = "HEAD-TO-HEAD STATS     "
   ITEM$(12) = "CONFERENCE RECORDS     "
   ITEM$(13) = "EXPANDED LEADERS       "
   ITEM$(14) = "DELETE CONFERENCE STATS"
   ITEM$(15) = "CREATE/ EDIT           "
   ITEM$(16) = "UPDATE POLL/ STATS     "
   ITEM$(17) = "VIEW/ PRINT REPORTS    "
   ITEM$(18) = "NATIONAL RECORDS       "
   ITEM$(19) = "DELETE NATIONAL STATS  "
   ITEM$(20) = "RETURN TO 3 IN 1 MENU  "
   ITEM$(21) = "EXIT TO DOS            "
     SELECTION% = 1
     Count% = 21
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 7
     Columns% = 3
     Spacing% = 3
     Row% = 9
     Column% = 2
     Call box0(7, 1, 16, 25, 2, 31)
     Call box0(7, 27, 16, 51, 2, 31)
     Call box0(7, 53, 16, 77, 2, 31)
     COLOR 15, 0: LOCATE 8, 4: Print "PROFESSIONAL LEAGUE"
     LOCATE 8, 30: Print "COLLEGE CONFERENCE"
     LOCATE 8, 55: Print "NATIONAL ORGANIZATION"
     LOCATE 18
     Print "PROFESSIONAL LEAGUE IS USED TO DEFINE A GROUP OF PRO TEAMS AND DIVISIONS"
     Print
     Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A SINGLE CONFERENCE"
     Print
     Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE COLLEGE CONFERENCES TO"
     Print "TRACK AND UPDATE TO GENERATE A NATIONAL POLL, NATIONAL LEADERS."
     Call ShowCursor
20   MAMenu VarPtr(ITEM$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
     Call HideCursor
     If ExitCode% = 27 Then GoTo 1
     If ExitCode% = -71 Then Start% = 1: ExitCode% = 3: SELECTION% = 1: GoTo 20
     If ExitCode% = -79 Then Start% = 15: ExitCode% = 3: SELECTION% = 15: GoTo 20
     If ExitCode% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
     If ExitCode% = -81 Then Start% = Start% + 80: If Start% > 15 Then Start% = 15
     If ExitCode% = -81 Or ExitCode% = -73 Then ExitCode% = 3: SELECTION% = Start%: GoTo 20
     COLOR , 0
    If SELECTION% = 1 Then RUN "CREATPRO.EXE"
    If SELECTION% = 2 Then RUN "COMPPRO.EXE"
    If SELECTION% = 3 Then RUN "SEEPRO.EXE"
    If SELECTION% = 4 Then RUN "HD2HDPRO.EXE"
    If SELECTION% = 5 Then RUN "REC.EXE 1"
    If SELECTION% = 6 Then RUN "EXPANPRO.EXE"
    If SELECTION% = 7 Then RUN "BLANKPRO.EXE"
    If SELECTION% = 8 Then RUN "CREATCOL.EXE"
    If SELECTION% = 9 Then RUN "COMPCOLL.EXE"
    If SELECTION% = 10 Then RUN "SEECOLL.EXE"
    If SELECTION% = 11 Then RUN "HD2HDCOL.EXE"
    If SELECTION% = 12 Then RUN "REC.EXE 2"
    If SELECTION% = 13 Then RUN "EXPANPRO.EXE"
    If SELECTION% = 14 Then RUN "BLANKCOL.EXE"
    If SELECTION% = 15 Then RUN "CREATNAT.EXE"
    If SELECTION% = 16 Then RUN "COMPNATL.EXE"
    If SELECTION% = 17 Then RUN "SEENATL.EXE"
    If SELECTION% = 18 Then RUN "REC.EXE 3"
    If SELECTION% = 19 Then RUN "BLANKORG.EXE"
    If SELECTION% = 20 Then GoTo 1
    If SELECTION% = 21 Then CLS: End

Static Sub CapNum()

   CapStatus = Peek1%(0, &H417) And &H40        'get BIOS status of Caps Lock
   NumStatus = Peek1%(0, &H417) And &H20        'get BIOS status of Num Lock
   H = POS(0): V = CSRLIN                       'save cursor position

   If Cap = CapStatus Then GoTo QNumCheck       'no change since last check

   Cap = CapStatus                              'save Caps status
   LOCATE 25, 69                                'position to print status msg

   If CapStatus Then
      QPrint " CAP ", 112, -1                   'white on black
   Else
      QPrint Space$(5), 7, -1                   'print or erase message
   End If



QNumCheck:

   If Num = NumStatus Then GoTo QCapNumExit     'no change since last check

   Num = NumStatus                              'save Num status
   LOCATE 25, 75                                'position to print status msg

   If NumStatus Then                            'print or erase message
      QPrint " NUM ", 112, -1
   Else
      QPrint Space$(5), 7, -1
   End If



QCapNumExit:

   LOCATE V, H                                  'restore cursor position

End Sub

 Static Sub Comline(NumArgs, Args$(), MaxArgs)
 CONST TRUE = -1, FALSE = 0
 
     NumArgs = 0: In = FALSE
     ' Get the command line using the COMMAND$ function
     Cl$ = Command$
     L = Len(Cl$)
     ' Go through the command line a character at a time
     For I = 1 To L
          C$ = Mid$(Cl$, I, 1)
          ' Test for character being a blank or a tab
          If (C$ <> " " And C$ <> Chr$(9)) Then
          ' Neither blank nor tab; test if you're already inside
          ' an argument
               IF NOT In THEN
               ' You've found the start of a new argument
                    ' Test for too many arguments
                      If NumArgs = MaxArgs Then Exit For
                      NumArgs = NumArgs + 1
                      In = TRUE
               End If
               ' Add the character to the current argument
               Args$(NumArgs) = Args$(NumArgs) + C$
          Else
          ' Found a blank or a tab.
               ' Set "Not in an argument" flag to FALSE
               In = FALSE
          End If
     Next I
 
 End Sub

Sub GETKEY()
LOCATE 23, 1: Print "PRESS ANY KEY TO CONTINUE"
21 I$ = INKEY$: If I$ = "" Then GoTo 21
End Sub

Static Sub TextIn(T$, Max, NumOnly, CapsOn, ExitCode, Colr)

TInitialize:

   Clr = 0                                      'determine monitor type
   If Peek1%(0, &H463) <> &HB4 Then Clr = 1

   X$ = T$                                      'work on a copy of the string
'  X$ = QPTrim$(T$)                             'use this if the incoming
                                                '  string is fixed-length
TC:

   ExitCode = 0: Insrt = 0: Curpo = 1           'initialize flags

   Length = Len(X$)
   If Length > Max Then Exit Sub                'already to big to edit

   X$ = X$ + Space$(Max - Length)               'pad with trailing spaces
   QPrint X$, Colr, -1
   LOCATE , , 1                                 'turn on the cursor

   GoSub TInsertOff                     'set cursor size according to display



TGetKey:

   'CapNum                               'display Cap/Num if active

   'disallow insert if cursor past end
   If Curpo > Length And Insrt <> 0 Then GoSub TInsertOff

   If Curpo > Max Then GoTo TEnter      'field is filled, handle as Enter key

   A$ = INKEY$
   If A$ = "" Then GoTo TGetKey
   If Len(A$) = 1 Then GoTo TRegularKey

   A$ = Right$(A$, 1)                   'it was an extended key, get the code

   On InStr(Chr$(15) + ".GHKMOPRSstu" + Chr$(19), A$) GoTo TShiftTab, TClear, THome, TUp, TLeft, TRight, TEndKey, TDown, TIns, TDel, TCtrlLeft, TCtrlRight, TCtrlEnd, TRestore

   GoTo TGetKey                         'none of the above, get again



TShiftTab:

   ExitCode = 1                         'user wants to go back a field
   GoTo TEnter                          'handle as if it were the Enter key



TClear:

   X$ = ""                              'Alt-C, erase the current string
   GoSub TInsertOff                     'clear insert mode and restore cursor
   LOCATE , POS(0) - (Curpo - 1)
   GoTo TC                              'and start all over again



THome:

   LOCATE , POS(0) - (Curpo - 1)        'put cursor at beginning of line
   Curpo = 1                            'show cursor as being on 1st character
   GoTo TGetKey



TUp:

   ExitCode = 1                         'user wants to go back a field
   GoTo TEnter                          'handle as if it were the Enter key



TLeft:

   If Curpo = 1 Then GoTo TShiftTab     'cursor is on the first character,
                                        'handle as if it were a Shift-Tab
   Curpo = Curpo - 1                    'update Curpo
   LOCATE , POS(0) - 1                  'back up the cursor
   GoTo TGetKey



TRight:

   Curpo = Curpo + 1                    'update Curpo
   LOCATE , POS(0) + 1                  'advance the cursor on the screen
   GoTo TGetKey



TEndKey:

   LOCATE , POS(0) + (Length - Curpo) + 1 'put cursor at the end of the line
   Curpo = Length + 1                   'update Curpo
   GoTo TGetKey



TDown:

   GoTo TEnter



TIns:

   If Insrt Then                        'insert is already on, turn it off
      GoSub TInsertOff
      GoTo TGetKey
   End If

   If Curpo > Length Then GoTo TGetKey  'ignore Ins if cursor is past the end
   If Length = Max Then GoTo TGetKey    'also ignore if field is full

   Insrt = 1                            'set the insert flag
   If Clr Then                          'set cursor size according to display
      LOCATE , , , 0, 7
   Else
      LOCATE , , , 0, 13
   End If

   GoTo TGetKey



TDel:

   If Curpo > Length Then GoTo TGetKey  'ignore Del if cursor is past end

   'slide all characters left one position, add a trailing space and re-print
   Mid$(X$, Curpo) = Mid$(X$, Curpo + 1) + " "
   QPrint Mid$(X$, Curpo), Colr, -1

   Length = Length - 1                  'show string as one character shorter
   GoTo TGetKey



TCtrlLeft:

   If Curpo = 1 Then GoTo TGetKey       'at the beginning, ignore

   A = Curpo                            'save cursor position

   'we're within a word, find beginning
   If Mid$(X$, Curpo - 1, 1) <> " " Then GoTo TSeekLeft2

TSeekLeft1:

   If Curpo = 1 Then GoTo TCtrlLeftExit 'at the beginning, give up

   If Mid$(X$, Curpo - 1, 1) = " " Then
      Curpo = Curpo - 1
      GoTo TSeekLeft1                   'seek previous non-blank character
   End If

TSeekLeft2:

   If Curpo = 1 Then GoTo TCtrlLeftExit 'at the beginning, give up
   If Mid$(X$, Curpo - 1, 1) <> " " Then
      Curpo = Curpo - 1
      GoTo TSeekLeft2                   'seek character preceeded by a blank
   End If

TCtrlLeftExit:

   LOCATE , POS(0) - (A - Curpo)        'position the cursor
   GoTo TGetKey



TCtrlRight:

   A = Curpo                            'save cursor position

TSeekRight1:

   If A > Length Then GoTo TGetKey      'at the end, give up

   If Mid$(X$, A, 1) <> " " Then
      A = A + 1                         'consider next character
      GoTo TSeekRight1                  'seek next blank space
   End If

TSeekRight2:

   If A > Length Then GoTo TGetKey      'at the end, give up

   If Mid$(X$, A, 1) = " " Then
      A = A + 1                         'consider next character
      GoTo TSeekRight2                  'seek next non-blank character
   End If

   LOCATE , POS(0) + (A - Curpo)        'position the cursor

   Curpo = A                            'show cursor as being on the next word
   GoTo TGetKey                         'get another keypress




TCtrlEnd:

   If Curpo > Length Then GoTo TGetKey  'cursor is past the end, ignore

   QPrint Space$(Length - Curpo + 1), Colr, -1  'blank from cursor to the end
   Mid$(X$, Curpo) = Space$(Length - Curpo + 1) 'clear that part of the string
   Length = Curpo - 1                   'show the length being at the cursor
   GoTo TGetKey                         'get another keypress



TRestore:

   LOCATE , POS(0) - (Curpo - 1)        'locate cursor at beginning of line,
   GoTo TInitialize                     '  and start all over again



TRegularKey:

   If A$ < " " Then                     'a control key
      On InStr(Chr$(8) + Chr$(9) + Chr$(13) + Chr$(27), A$) GoTo TBackspace, TTabKey, TEnter, TEscape
      GoTo TGetKey                      'none of the above
   End If

   If CapsOn Then                       'convert to upper case if requested
      If A$ >= "a" And A$ <= "z" Then A$ = Chr$(Asc(A$) And 95)
   End If

   If NumOnly Then                      'disallow non-numeric if requested
      If A$ < "0" Or A$ > "9" Then
         PLAY "L16O3EC"
         GoTo TGetKey
      End If
   End If

   QPrint A$, Colr, -1                  'print character
   LOCATE , POS(0) + 1
   Curpo = Curpo + 1                    'show cursor being ahead

   If Insrt Then GoTo THandleInsert

   Mid$(X$, Curpo - 1, 1) = A$          'assign the character

   'cursor is past end, increase length
   If Curpo > Length + 1 Then Length = Curpo - 1

   'field complete, handle as Enter key
   If Length = Max And Curpo > Length Then GoTo TEnter

   GoTo TGetKey



THandleInsert:

   Length = Length + 1                  'show string being 1 character longer
   Temp$ = Mid$(X$, Curpo - 1)          'using Temp$ avoids a BASIC 7 bug
   Mid$(X$, Curpo) = Temp$              'move characters one position ahead

   Mid$(X$, Curpo - 1, 1) = A$          'assign the current character
   QPrint Mid$(X$, Curpo, Length - Curpo + 1), Colr, -1 're-print X$

   If Length = Max Then GoTo TEnter     'field complete, handle as Enter key
   GoTo TGetKey



TBackspace:

   If Curpo = 1 Then GoTo TGetKey       'can't back up any more, ignore
   Curpo = Curpo - 1                    'show cursor being 1 character before
   LOCATE , POS(0) - 1                  'back up the cursor
   GoTo TDel                            'handle as if it were the Delete key



TTabKey:                                'reserved for your Tab routine if you
                                        '  want to handle it differently
TEnter:

   GoSub TInsertOff                     'clear insert, restore cursor size
   X$ = Left$(X$, Length)               'retain only the current length

   T$ = X$                              'assign the string
   LOCATE , , 0                         'turn off the cursor
   Exit Sub



TEscape:

   ExitCode = 2                         'show that the user pressed Escape
   GoTo TEnter                          'handle as if it were the Enter Key



TInsertOff:

   'clear Insert mode and restore cursor, depending on monitor type

   Insrt = 0
   If Clr Then
      LOCATE , , , 6, 7
   Else
      LOCATE , , , 12, 13
   End If
   Return

End Sub

Sub TITLE()
   COLOR , 0: Key OFF: CLS
   COLOR , 1: LOCATE 1, 1: Print Tab(80);: LOCATE , 32: COLOR 12
   Print "3 IN 1 FOOTBALL V6.3"
   LOCATE 24, 1: Print Tab(80);: LOCATE , 1
   Print "(C) LANCE HAFFNER 1984-2000"; Tab(70); Left$(Time$, 5);
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15
   Print
   COLOR 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER"
             Print "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR LANCE@LHGAMES.COM"
             Print "      LANCE HAFFNER GAMES ON THE INTERNET AT  HTTP://WWW.LHGAMES.COM"

   LOCATE 12, 1
   Print "LANCE HAFFNER GAMES"
   Print "P.O. BOX 100594"
   Print "NASHVILLE, TN 37224"
   Print "LANCE@LHGAMES.COM"
   COLOR 10
End Sub

'----- entry parameters
'
'YN$ = string to be input or edited (use the name of your choice)
'Colr is the combined foreground and background colors that will be used
'
'----- exit parameters
'
'YN$  = final edited amount
'ExitCode indicates how editing was terminated -
'   0 = Enter, Tab, Down-Arrow, Right-Arrow past end, or field filled
'   1 = Shift-Tab, Up-Arrow, or Left-Arrow past beginning
'   2 = Escape key pressed
'
'----- local variables
'
'X$ is a string version of the number while it is being input or edited
'
Static Sub YesNoB(YN$, ExitCode, Colr)

   ExitCode = 0                                 'initialize flag

   X$ = YN$
   If X$ = "" Then X$ = " "
   QPrint X$, Colr, -1
   LOCATE , , 1



YGetKey:

   X$ = INKEY$
   If X$ = "" Then GoTo YGetKey

   If UCase$(X$) = "Y" Or UCase$(X$) = "N" Then GoTo YDone

   If X$ = Chr$(9) Or X$ = Chr$(13) Then GoTo YD2 'Tab or Enter
   If X$ = Chr$(27) Then                        'Escape
      ExitCode = 2
      GoTo YD2
   End If

   If Len(X$) = 2 Then                          'extended key
      X$ = Right$(X$, 1)
      If X$ = "P" Or X$ = "M" Then GoTo YD2     'Down arrow, Right arrow
      If X$ = "H" Or X$ = "K" Or X$ = Chr$(15) Then
         ExitCode = 1                           'Up, Left, Shift-Tab
         GoTo YD2
      End If
   End If

   GoTo YGetKey



YDone:

   If X$ > "Y" Then X$ = Chr$(Asc(X$) - 32)     'capitalize if necessary
   YN$ = X$
   QPrint YN$, Colr, -1

YD2:

   LOCATE , , 0

End Sub

