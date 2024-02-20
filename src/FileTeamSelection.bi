Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam (selectedIdx)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadExtraData (teamYear$, teamIdx)
Declare Function nameStatFile$ (teamName$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

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

Dim leagueRatings%(7), teamRatings%(5)

Dim rushContrib(0 To 9), rushAvg(9), rbRecepts(0 To 15), rbRecAvg(9)
Dim recContrib(5), wrRecAvg(5)
Dim passContrib(0 To 3), compPct(3), qbIntPct(3)
Dim krContrib(2), krRetAvg(2), prContrib(2), prRetAvg(2), puntAvg(1)
Dim pkFGA(1), pkFGContrib(1), pkPAT(1), pkPATContrib(1)
Dim numInts(9), numSacks(14), armRating(3)

Dim rbName$(9), wrName$(5), qbName$(4), krName$(2), prName$(2)
Dim punterName$(3), pkName$(1), intName$(9), sackName$(15)

