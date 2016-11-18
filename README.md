# ada_checkers_official
------You can download the main.exe file and it will run the app!------

FILES:

main.adb 
- This is the main file, compile this and run this to run the game

gameboard.ads
gameboard.adb
- These files contain information relevant to the gameboard and other methods necessary for the game

Prereq: You must have GNAT or Ada installed on your machine, google it. libre.adacore.com There is a free download license for educational purposes such as this, on their site

The objects we use for this game are made from interfaces, which are Ada Spec Files(.ads) The objects are defined by packages, there should be a corresponding Ada Body File(.adb)

To clarify, an object like a set of numbers NumberSet.ads, must have a NumberSet.adb

.ads - holds var definitions, function, and procedure defintions meanwhile the .adb file must use all the exact same variables, functions, and procedures as the .ads
