# LH Games 3-in-1 Football #

### Game Description ###
**· With stats compiler for each player and team! · YOU choose from 14 offensive plays and 6 defensive formations! ·**

### About Project ###
With permission from Lance Haffner himself, this effort is being made to bring his original source code from the DOS world into the modern world. This is due in part to cross-platform compilers for BASIC. But this effort restores his original vision for a cross-platform sports sim empire, also opening the door for any manner of improvements to accuracy and portability.

Several functions from the original code were linked from Crescent Software libraries. To make a faster route to compilation of the code, most of these were re-written as copycats (where code was not available or ASM routines were used). Functions which relied on packing color into 1 byte were updated to use FG/BG separately. This is in anticipation of future work that will aim to reduce as many of these routines as possible

### About LH Games ###
Since 1983 Lance Haffner Games has been publishing the finest quality text-based computer sports games. Recognized for their statistical accuracy; national media outlets have used Haffner's games to play matchups between great teams of the century. The Ft. Worth Star-Telegram, WTVF-TV (Nashville), The Nashville Banner, and ESPN have used Lance Haffner's games to simulate real and "what if" matchups using current teams and great teams from the past.

LH Games were some of the premier simulators of the 80's. They were a staple of of the mail order days and  served as one of the earliest computer sports simulation. Available for several platforms at the time thanks to their use of BASIC, they fast became known for their impressive accuracy and surprisingly quick speed. Not only that, they allowed very detailed and thorough team, conference and season management - yet somehow maintained an entertaining feel with gameplay that was never overly detailed. 

### Visuals ###
![football box art](fb-box.jpg)
![football in action](fb-action.jpg)

# The Game #
### Installation ###
No installation required. Pre-compiled binaries and required reference files are in /bin.

### Usage ###
Run Hello.exe to start with the main game menu.

Teams files will not be included at this time. However, if you're nice enough, the people at the [Haffner Files Facebook group](https://www.facebook.com/groups/183455342454939) will be glad to help you.

### Support ###
Contact me at my [profile page](https://github.com/jleonard2099/jleonard2099/tree/main) for now.
Bugs and/or feature requests can be submitted using GitHub (see the "Issues" tab at the top of page). This will be the easiest way for me to implement ideas.

# Project Status #
## Current ##
Code has been re-worked to compile in a modern environment. Code is in progress of being re-factored for menu navigation, along with consolidating so it can be supported for Linux / Mac compile in QB64.

Things are almost complete, code has been compiled and tested. Anticipate finished product before 2023.

## Road Map ##
### Phase 1 ###
Nearly complete. The focus has been completely on refactoring code and first steps towards consistency / cleanup.
- Make wording of menus and options consistent for all products
- Make subroutines for all repeated code

### Phase 2 ###
This phase will start to identify these as new games, and involve these short and simple goals:
- Update standards for code
	- variable names, config files, etc.
	- document purpose of functions
- Improve file, path and error handling to provide helpful messages
- Further improve code efficiency and consistency
	- global Team Selection routine 
	- consistent operation for the same functions in each game
- Updating the games to support modern rules
- Bug fixes / etc. as reported by longtime users and playtesters

### Phase 3 ###
This final phase will involve the following:
- Updating code to work with databases instead of flat files.
- Bringing modern interface concepts to the design (including GUI)

# The Fine Print #

### License ###
Affero GNU GPL v3

### Contributing ###
Contact me directly if interested in contributing. Only considering people who share my vision for these games and are able to honor Lance's work and legacy.

### Authors / Acknowledgment ###
- [Manual images created by Cool Clips](http://search.coolclips.com/)
- [Game icons created by Joseph Gan (Icon-Icons)](https://icon-icons.com/users/XHgnBHIfJk0ZlajSy0A0x/icon-sets/)
- I am responsible for maintaining / updating most of the code and will spend equal effort cleaning up code as improving it
- Jason Lamb, for lending his expertise in BASIC to helping me learn some standards and rewriting the menu functions.
- Lance Haffner, for his hard work over 2 decades giving people endless hours of playable sports games and making dreams come true.
- [Lance Haffner Sports Sims](https://www.facebook.com/groups/115923978469802) Facebook group admin Philip Lupi, for his hard work keeping Lance's games alive and active with fans old and new.
