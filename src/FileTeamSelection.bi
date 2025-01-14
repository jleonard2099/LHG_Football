Declare Sub ChooseTeam (yearNumber$, numberTeams, chosenIdx, teamName$)
Declare Sub SelectTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadExtraData (teamYear$, teamIdx, dataOK)
Declare Sub UserFilePrompt (reportFile$)
Declare Sub ScheduleFilePrompt (scheduleFile$)

Declare Sub ReadTeamStatsComp (teamName$, statFile$)
Declare Sub ReadTeamStats (teamName$, statFile$)
Declare Sub ReadScheduleStats (statFile$, XW, numberGames)
Declare Sub ReadGameRecords (recordFile$)
Declare Sub ReadSeasonRecords (recordFile$)
Declare Sub ReadConferenceStats (statFile$, statType)
Declare Sub ReadOrganizationFile (organizationFile$)

Declare Function nameStatFile$ (teamName$)
Declare Function CheckString% (targetString$)
Declare Function ChooseFile$ (fileSpec$, fileDesc$, ExitCode%)

Const TEAM_SIZE_BYTES = 934
Const DATA_SIZE_BYTES = 50
Const TEAM_FILE_NAME$ = "FOOT"
Const DATA_FILE_NAME$ = "STADIUMS"

Dim previousYear$

'-- transition away from this
Dim Shared fileLength&

Dim teamAttendance&

Dim ydsCompAdj

Dim mascot$, stadium$, teamNick$

Dim leagueRatings(7), teamRatings(5)

Dim rushContrib(0 To 9), rushAvg(9), rbRecepts(0 To 15), rbRecAvg(9)
Dim recContrib(5), wrRecAvg(5)
Dim passContrib(0 To 3), compPct(3), qbIntPct(3)
Dim krContrib(2), krRetAvg(2), prContrib(2), prRetAvg(2), puntAvg(1)
Dim pkFGA(1), pkFGContrib(1), pkPAT(1), pkPATContrib(1)
Dim numInts(9), numSacks(14), armRating(3)

Dim rbName$(10), wrName$(6), qbName$(4), krName$(3), prName$(3)
Dim punterName$(3), pkName$(2), intName$(9), sackName$(15)
