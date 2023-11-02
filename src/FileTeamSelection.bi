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

Dim Shared A$

'-- transition away from this
Dim Shared fileLength&

Dim teamAttendance&

Dim ydsCompAdj 'YC! is used elsewhere

Dim mascot$, stadium$, teamNick$

Dim leagueRatings%(7), teamRatings%(5)

Dim rushAtt(9), rushAvg(9), rbRecepts(15), rbRecAvg(9)
Dim wrRecept(5), wrRecAvg(5)
Dim passAtt(0 To 3), compPct(3), qbIntPct(3)
Dim krRet(2), krRetAvg(2), prRet(2), prRetAvg(2), puntAvg(1)
Dim pkFGA(1), pkFGPct(1), pkPAT(1), pkPATPct(1)
Dim numInts(9), numSacks(14), armRating(3)

Dim rbName$(9), wrName$(5), qbName$(4), krName$(2), prName$(2)
Dim punterName$(3), pkName$(1), intName$(9), sackName$(15)
