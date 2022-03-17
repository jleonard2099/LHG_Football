
* PHASE 1 - PRELIMINARY WORK

** GOAL: ADDRESS MISSING / EXTRANEOUS SOURCE FILES

[X] Football
	no missing source files / references


** GOAL: COMPILE FULL GAMES FROM SOURCES

[X] Re-create dependent functions with no QB64 equivalent in native BASIC
	[X] ClearEOL
	[X] FCopy
	[X] MAMenu
	[X] MMenuVert
	[X] MScrnSave/MScrnRest
	[X] QPrint/MQPrint	
	[X] ReadFile + FCount
	[X] Text In
	[X] YesNoB
	
[X] Replace dependent functions with QB64 equivalents

[X] Remove obsolete code
	Remove checks for disk drives
	Remove references to SortStr (not needed)
		They are all used to sort file lists
		My function for getting file lists does a sort

[X] Make code compatible with QB64

[ ] Attempt first compilation of each file
