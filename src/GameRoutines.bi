Declare Sub FOOTBALL ()
Declare Sub INFO ()
Declare Sub InitVar ()
Declare Sub AUTOSETUP (validTeam)
Declare Sub LetterToNumber (promptVal$, J)
Declare Sub TEAMBOX (boxHeight, teamIdx)
Declare Sub FRAMEOPTION (boxHeight, teamIdx)
Declare Sub Rolld100 (randNumVar)
Declare Sub DELAY ()
Declare Sub CoinToss
Declare Sub PBP ()
Declare Sub TICKER ()
Declare Sub TICKERUPDATE ()
Declare Sub TickerSchedule ()
Declare Sub SCOUT ()
Declare Sub HALFSTART ()
Declare Sub CHOOSEKICKER (currTeam)
Declare Sub PLACEPBP ()
Declare Sub DISPWEATHER ()
Declare Sub CHANGEPOSS ()
Declare Sub SETSCREEN ()
Declare Sub Scoreboard
Declare Sub DisplayField ()
Declare Sub YARDLINE ()
Declare Sub GOALLINE ()
Declare Sub NAMEEDIT ()
Declare Sub SCOUTREPT ()
Declare Sub ADDBACKS ()
Declare Sub ONSIDE ()
Declare Sub INCOMPLETE ()
Declare Sub QBSNEAK ()
Declare Sub OffPlayOveruse ()
Declare Sub DefPlayOveruse ()
Declare Sub BLITZ ()
Declare Sub BlitzYardageLong (I8)
Declare Sub BLITZ2 ()
Declare Sub COMPCHEAT (defEra)
Declare Sub FumbleRecoverPBP ()
Declare Sub CheckGameClock ()
Declare Sub GetDefensiveEra (defEra)
Declare Sub COMPDEFENSE (defEra)
Declare Sub HISTDEFENSE (defEra)
Declare Sub MARKER ()
Declare Sub MARKER1 ()
Declare Sub MOVEBALL ()
Declare Sub StatusBoard ()
Declare Sub ColorChange ()
Declare Sub DisplayClockDown
Declare Sub DEFMENU (defEra)
Declare Sub DEF_CheckGoalLine ()
Declare Sub Turnover_PAT
Declare Sub GetPATChoice (userChoice)
Declare Sub LongGainPBP ()
Declare Sub RunForTD_PBP ()
Declare Sub SETWEATHER ()
Declare Sub FGATT ()
Declare Sub FGMADE ()
Declare Sub ComputerOffense ()
Declare Sub MEASUREMENT ()
Declare Sub ReceiverLossPBP (yardString$)
Declare Sub WHICHRECEIVER
Declare Sub TurnoverOnDowns
Declare Sub PenaltyOnKickoff ()
Declare Sub PenaltyOnPunt (autoFD)
Declare Sub PenaltyOnPlaceKick (autoFD)
Declare Sub PenaltyChoice (decision, autoFD)
Declare Sub PenaltyCharge (teamIdx, ydsCharged!)
Declare Sub FirstDownAfterPlay
Declare Sub CHOOSEQB (currTeam)
Declare Sub TimeoutCalled (teamIdx, periodOver)
Declare Sub KickoffChoices (deferOpt)
Declare Sub PuntNotReturned (fairCatchPossible, puntType, isPenalty)
Declare Sub TAKESAFETY ()
Declare Sub FINDINT (defPlayIdx)
Declare Sub YARDAGE ()
Declare Sub PuntReturnYards ()
Declare Sub PuntFakeRunYards (choice)
Declare Sub CheckFumble_1 ()
Declare Sub CheckFumble_2 ()
Declare Sub FLEA (ffChance)
Declare Sub REVGAIN ()
Declare Sub FGFakeRunYards (choice)
Declare Sub HANDOFF ()
Declare Sub DisplayDefense
Declare Sub PASSBALL (direction, fieldSide)
Declare Sub AdjustCompPct ()
Declare Sub SCOREUPD ()
Declare Sub COMPILER ()
Declare Sub TwoMinMenu (menuChoice)
Declare Sub FieldGoalChance ()
Declare Sub RUNMENU ()
Declare Sub PASSMENU ()
Declare Sub SHORTYARDAGE ()
Declare Sub PASSPBP ()
Declare Sub SACK ()
Declare Sub QBLINE ()
Declare Sub SQUIB ()
Declare Sub PUNTPBP ()
Declare Sub STUFFED ()
Declare Sub MISSEDFG
Declare Sub ITSGOOD
Declare Sub MISSPAT
Declare Sub MADEPAT
Declare Sub KickoffReturn (kickNotReturned, isPenalty)
Declare Sub KickoffLine
Declare Sub KickoffDistance ()
Declare Sub QBTHROW ()
Declare Sub RUNPBP ()
Declare Sub PassCompletePBP (direction, fieldSide)
Declare Sub PassYardagePBP (yardString$)
Declare Sub Touchback
Declare Sub AddPossessionTime (minSeconds, maxSeconds)
Declare Sub FumblePBP ()
Declare Sub TryForTwoResult ()
Declare Sub OPTIONS ()
Declare Sub EVENTS (specialCount, specialStat)
Declare Sub FoundSpecial (specialCount, specialStat)
Declare Sub POSTGAME ()
Declare Sub BOXOPTIONS (outputChoice)
Declare Sub BOX2FILE (printChoice, boxName$, hasSpecial)
Declare Sub BOX2SPC (boxName$)
Declare Sub BOX2SCREEN ()
Declare Sub ScoreSummary
Declare Sub STATS2SCREEN ()
Declare Sub SAVESTATS ()
Declare Sub COMPILESTATFILES (teamIdx)
Declare Sub pbplog (lineNumber)
Declare Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, gameIdx)
Declare Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)
Declare Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx, dataOK)
Declare Sub GetKeyWhenNoDelay ()
Declare Sub EndOfGame

Declare Function d100% ()
Declare Function fgLimit%
Declare Function FindMarkerColumn (X)
Declare Function GetSneakChance%
Declare Function GetTeamYear% (yearFromName$)
Declare Function TickerTime% (timeInterval, X)
