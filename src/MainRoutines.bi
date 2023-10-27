Declare Sub ALIGN ()
Declare Sub AddCareerYear (finalUpdate, careerFile$, statFile$)
Declare Sub AlignStats (pathRoadStats$, statFile$)
Declare Sub BLANKCOL ()
Declare Sub BLANKORG ()
Declare Sub BLANKPRO ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CAREDIT ()
Declare Sub CAREER ()
Declare Sub CARPRNT ()
Declare Sub CARVIEW ()
Declare Sub CATEGORY_BOX (J)
Declare Sub CNGTM ()
Declare Sub CREATCOL ()
Declare Sub CREATNAT ()
Declare Sub CREATPRO ()
Declare Sub CareerPrintCategory (RT, B9, categoryID%, escPressed%)
Declare Sub ChangeGlobalSettings ()
Declare Sub ChangeTeamRatings (A$, mascot$, YN$, S$, teamNbr, YC!)
Declare Sub CheckContributions
Declare Sub CheckStatsColl (numberGames%, TM$, WT$, finalUpdate$)
Declare Sub CheckStatsNatl (numberGames%, TM$, WT$, finalUpdate$)
Declare Sub CheckStatsPro (numberGames%, TM$, WT$, finalUpdate$)
Declare Sub ChooseDraftTeam (yearNumber$, numberTeams, teamIdx, teamName$)
Declare Sub ChooseTradeTeam (J)
Declare Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)
Declare Sub CompileCollege ()
Declare Sub CompileLeague ()
Declare Sub CompileNatlOrg ()
Declare Sub ConfigureDrives ()
Declare Sub ContinueTrade ()
Declare Sub CreateCareerFile ()
Declare Sub DRAFT ()
Declare Sub DefineConference (conference$, div1$, div2$)
Declare Sub DefineLeaguePro (LG$, div1$, div2$, div3$)
Declare Sub DefineNatOrg (organization$)
Declare Sub DeleteBoxscores (fileChoice%, fileSpec$)
Declare Sub DisplayLeaders (XX, QW, TVN)
Declare Sub DoTheDraft (draftNew, doneDrafting, YN$, teamName$, teamIdx)
Declare Sub DoTheMerge (pathCurrentStats$, statFile$, B$)
Declare Sub DoTheTrade (tradeIdx)
Declare Sub DraftChoices (YN$, teamName$)
Declare Sub DraftExistingTeam (YN$, teamIdx)
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame ()
Declare Sub EditParameters ()
Declare Sub EditStats ()
Declare Sub EditTeam (A$, mascot$, YN$, S$, teamNbr, YC!)
Declare Sub ExpandedLeaders_Menu (tStats)
Declare Sub ExistingTeamParameters (YN$, teamName$, teamIdx)
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub GetListLength (printOrView%, listLength, maxItems)
Declare Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)
Declare Sub HD2HDCOL ()
Declare Sub HD2HDPRO ()
Declare Sub Hd2HdColl_Conf (X2)
Declare Sub Hd2HdColl_Report (A$, LG$, LG2$, DV$(), DV2$())
Declare Sub Hd2HdColl_Screen (A$, LG$, LG2$, DV$(), DV2$())
Declare Sub Hd2HdPro_League (X2)
Declare Sub Hd2HdPro_Report (A$, LG$, LG2$, DV$(), DV2$())
Declare Sub Hd2HdPro_Screen (A$, LG$, LG2$, DV$(), DV2$())
Declare Sub InputTeam ()
Declare Sub LOOKY ()
Declare Sub LeaderStatsColl_Screen (FF$)
Declare Sub LeaderStatsNat_Screen (FF$, div1$, div2$, NRT)
Declare Sub LeaderStatsPro_Screen (league$)
Declare Sub Leaders_Menu ()
Declare Sub LoadLeaderFiles_Coll (FF$, targetFile$)
Declare Sub LoadLeaderFiles_Natl (FF$, div1$, div2$, NRT, targetFile$)
Declare Sub LoadLeaderFiles_Pro (league$, WE$)
Declare Sub LoadScheduleInfo (targetFile$)
Declare Sub LoadStatsForTrade (J)
Declare Sub LoadTeamStats (statFile$, teamName$, totalGames%, teamPower, SS)
Declare Sub MERGE ()
Declare Sub Main_Menu (menuSelection%)
Declare Sub MergeCareerFiles ()
Declare Sub MergeStats ()
Declare Sub NEWLDR (statType, skipMenu)
Declare Sub NewGame (visitor$, visitorQB$, home$, homeQB$, YN$, NTMS, gameIndex%)
Declare Sub NewSchedule (scheduleFile$)
Declare Sub NewTeamParameters (YN$, teamName$, teamIdx)
Declare Sub POSTLEADERS (confName$)
Declare Sub POSTNATIONAL (WE$)
Declare Sub PlayerStatsPrompt (W1)
Declare Sub PostLeadersColl (WE$)
Declare Sub PostLeadersPro (leaderFile$)
Declare Sub PrintCareerStatsINT (exitFlag%, B9)
Declare Sub PrintCareerStatsK (exitFlag%, B9)
Declare Sub PrintCareerStatsKR (exitFlag%, B9)
Declare Sub PrintCareerStatsP (exitFlag%, B9)
Declare Sub PrintCareerStatsPR (exitFlag%, B9)
Declare Sub PrintCareerStatsQB (exitFlag%, B9)
Declare Sub PrintCareerStatsRB (exitFlag%, B9)
Declare Sub PrintCareerStatsSACK (exitFlag%, B9)
Declare Sub PrintCareerStatsWR (exitFlag%, B9)
Declare Sub PrintLeaders (XX, LG, TVN)
Declare Sub PrintRoster (A$, mascot$, B8%)
Declare Sub PrintSeasonRecords ()
Declare Sub PrintViewReselect (userOpt$)
Declare Sub ProcessLeaders (careerFile$)
Declare Sub QBRatingPro (I)
Declare Sub QBRatingColl (I)
Declare Sub QBRating_Expanded (CT%)
Declare Sub ReadScheduleStats (statFile$, numberGames%)
Declare Sub ReadScheduleTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadSourceTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadStatsColl (statFile$, numberGames%, TM$)
Declare Sub ReadStatsNat (statFile$, numberGames%, TM$)
Declare Sub ReadStatsPro (statFile$, numberGames%, TM$)
Declare Sub ReadTradeTeam (J)
Declare Sub Records_Menu (menuChoice)
Declare Sub Replay_Menu ()
Declare Sub ReportFilePrompt_Coll (reportFile$)
Declare Sub ReportFilePrompt_Natl (reportFile$)
Declare Sub ReportFilePrompt_Pro (reportFile$)
Declare Sub ResetStatPrompt (statFile$)
Declare Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)
Declare Sub ReviewEditConference (conference$, div1$, div2$)
Declare Sub ReviewEditLeague (league$, div1$, div2$, div3$)
Declare Sub ReviewEditNatOrg (organization$)
Declare Sub SAVELEADER (leaderFile$, tStats)
Declare Sub SCHEDULE ()
Declare Sub SEECOLL ()
Declare Sub SEENATL ()
Declare Sub SEEPRO ()
Declare Sub SaveChangedTeam (A$, mascot$, YN$, S$, teamNbr, YC%, teamAttendance&)
Declare Sub SaveConferences (conference$, div1$, div2$)
Declare Sub SaveLeague (LG$, div1$, div2$, div3$)
Declare Sub SaveNatOrg (organization$)
Declare Sub SavePrintCollLeaders (reportFile$, FF$, destination)
Declare Sub SavePrintNatLeaders (reportFile$, FF$, div1$, div2$, NRT, destination)
Declare Sub SavePrintProLeaders (destination, reportFile$, league$)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeam (A$, mascot$, YN$, S$, teamNumber, YC%, teamAttendance&)
Declare Sub SaveTeamDraft (draftNew, yearNumber$, teamName$, teamIdx)
Declare Sub SaveTradeStats (J)
Declare Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%)
Declare Sub SelectTradeTeam (J, yearNumber$, numberTeams)
Declare Sub SortLeadersConf ()
Declare Sub SortLeadersNat ()
Declare Sub SortLeadersPro
Declare Sub SortOptions (statType, printOrView%, sortType%)
Declare Sub SortPlayer (stpl, PPP, sortType%)
Declare Sub SortSchedule ()
Declare Sub SortTeam (sortMax, stpl, sortType%)
Declare Sub StatOptions_Coll (FF$)
Declare Sub StatOptions_Natl (FF$, div1$, div2$, NRT)
Declare Sub StatOptions_Pro (league$)
Declare Sub StatsByCategoryColl (numberGames%, TM$, WT$, finalUpdate$)
Declare Sub StatsByCategoryNatl (numberGames%, TM$, WT$, finalUpdate$)
Declare Sub StatsByCategoryPro (numberGames%, TM$, WT$, finalUpdate$)
Declare Sub StatsToPrinter (teamName$, totalGames%, statFormat, teamPower, SS)
Declare Sub StatsToScreen (teamName$, totalGames%, statFormat, teamPower, SS)
Declare Sub TRADE ()
Declare Sub TeamMgt_Menu ()
Declare Sub TeamStatsToPrinter ()
Declare Sub TradeKRs (W1)
Declare Sub TradeKickers (W1)
Declare Sub TradePRs (W1)
Declare Sub TradePunters (W1)
Declare Sub TradeQBs (W1)
Declare Sub TradeRBs (W1)
Declare Sub TradeWRs (W1)
Declare Sub UpdateCareerFile (saveFile$)
Declare Sub UpdateCareerLeaders (careerFile$)
Declare Sub UpdateExpandedLeaders (statType, tStats)
Declare Sub ViewExpandedStats (statType)
Declare Sub ViewPrintCareerRecords ()
Declare Sub ViewRoster (A$, mascot$, B8%)
Declare Sub ViewSeasonRecords ()
Declare Sub ViewStatSummary (statFile$, teamName$, totalGames%, teamPower, SS)

Declare Function GetQBName$ (targetGame%, I5)
Declare Function FindStatsColl% (XW, statFile$, numberGames%)
Declare Function FindStatsNat% (XW, statFile$, numberGames%, NRT)
Declare Function FindStatsPro% (XW, statFile$, numberGames%)
Declare Function QBRating! (I, I1, teamType)
