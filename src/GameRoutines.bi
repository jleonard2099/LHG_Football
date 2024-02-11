Declare Sub FOOTBALL ()
Declare Sub EndOfGame
Declare Sub ADDBACKS ()
Declare Sub AddPossessionTime (minSeconds, maxSeconds)
Declare Sub AdjustCompPct ()
Declare Sub AUTOSETUP
Declare Sub BLITZ ()
Declare Sub BLITZ2 ()
Declare Sub BlitzYardageLong (I8)
Declare Sub BOX2FILE (printChoice, boxName$, hasSpecial)
Declare Sub BOX2SCREEN ()
Declare Sub BOX2SPC (boxName$)
Declare Sub BOXOPTIONS (outputChoice)
Declare Sub CHANGEPOSS ()
Declare Sub CheckGameClock ()
Declare Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, gameIdx)
Declare Sub CHOOSEQB (currTeam)
Declare Sub CHOOSEKICKER (currTeam)
Declare Sub CLIPPING ()
Declare Sub COLKICKOFF ()
Declare Sub COMPCHEAT ()
Declare Sub COMPILER ()
Declare Sub COMPILESTATFILES (teamIdx)
Declare Sub DEFENSE ()
Declare Sub DEFGOALLINE ()
Declare Sub DEFMENU ()
Declare Sub DELAY ()
Declare Sub DisplayField ()
Declare Sub DISPWEATHER ()
Declare Sub EVENTS (specialCount, specialStat)
Declare Sub FAKERUN (choice)
Declare Sub FAKEYARDS (choice)
Declare Sub FGATT ()
Declare Sub FGDIST ()
Declare Sub FGMADE ()
Declare Sub FINDINT ()
Declare Sub FINDSPACE ()
Declare Sub FINDTEAMS (I)
Declare Sub FLEA (ffChance)
Declare Sub FoundSpecial (specialCount, specialStat)
Declare Sub FRAMEOPTION (boxHeight, teamIdx)
Declare Sub FRAMESTATS (J)
Declare Sub FUMBLE ()
Declare Sub FumblePBP ()
Declare Sub GetKeyWhenNoDelay ()
Declare Sub GOALLINE ()
Declare Sub TryForTwoResult ()
Declare Sub HALFSTART ()
Declare Sub HANDOFF ()
Declare Sub INCOMPLETE ()
Declare Sub InitVar ()
Declare Sub KICKOFF ()
Declare Sub KICKSET ()
Declare Sub LGPBP ()
Declare Sub ITSGOOD
Declare Sub MADEPAT
Declare Sub MARKER ()
Declare Sub MARKER1 ()
Declare Sub MEASUREMENT ()
Declare Sub MISSEDFG
Declare Sub MISSPAT
Declare Sub MOVEBALL ()
Declare Sub NAMEEDIT ()
Declare Sub ComputerOffense ()
Declare Sub ONSIDE ()
Declare Sub OPTIONBOX (J)
Declare Sub OPTIONS ()
Declare Sub OVERUSE ()
Declare Sub PASSBALL (passChance)
Declare Sub PASSMENU ()
Declare Sub PASSPBP ()
Declare Sub PBP ()
Declare Sub PBPINT ()
Declare Sub pbplog (lineNumber)
Declare Sub pbplog (lineNumber%)
Declare Sub PCPBP ()
Declare Sub PLACEPBP ()
Declare Sub POSTGAME ()
Declare Sub PUNTPBP ()
Declare Sub PUNTYARDS ()
Declare Sub QBLINE ()
Declare Sub QBSNEAK ()
Declare Sub QBTHROW ()
Declare Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx)
Declare Sub REVGAIN ()
Declare Sub RUNMENU ()
Declare Sub RUNPBP ()
Declare Sub RUNTDPBP ()
Declare Sub SACK ()
Declare Sub SAVESTATS ()
Declare Sub SCOREBOARD ()
Declare Sub SCOREUPD ()
Declare Sub ScoreSummary
Declare Sub SCOUT ()
Declare Sub SCOUTREPT ()
Declare Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)
Declare Sub SETSCREEN ()
Declare Sub SETWEATHER ()
Declare Sub SHORTYARDAGE ()
Declare Sub SQUIB ()
Declare Sub STATS2SCREEN ()
Declare Sub STUFFED ()
Declare Sub TAKESAFETY ()
Declare Sub TEAMBOX (boxHeight, teamIdx)
Declare Sub TICKER ()
Declare Sub TickerSchedule ()
Declare Sub TICKERUPDATE ()
Declare Sub Touchback
Declare Sub TwoMinMenu (menuChoice)
Declare Sub WHICHRECEIVER
Declare Sub YARDAGE ()
Declare Sub YARDLINE ()


Declare Function FindMarkerColumn (X) 'Used only in "Marker" subroutine
Declare Function GetTeamYear% (yearFromName$)
Declare Function TickerTime% (timeInterval, X)
