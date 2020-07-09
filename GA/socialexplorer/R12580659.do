/* 
* TODO: 1. Place the .txt data file and the dictionary file you downloaded in the work folder, or enter the full path to these files!
*       2. You may have to increase memory using the 'set mem' statement. It is commented out in the code bellow.
*
* If you have any questions or need assistance contact info@socialexplorer.com.
*/

///set mem 512m
clear
set more off
infile using "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/socialexplorer/R12580659.dct", using("/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/socialexplorer/R12580659_SL050.txt")


keep FIPS NAME A00001_001 A00002_002 A04001_010 A03001_001 A03001_002 A03001_003 A03001_004 A03001_005 A03001_006 A03001_007 A03001_008

rename FIPS fips
destring fips, replace
rename NAME county_name2
rename A00001_001 county_totalpop1
rename A03001_001 county_totalpop2
rename A03001_002 county_whitepop
rename A03001_003 county_blackpop
rename A03001_004 county_AIpop
rename A03001_005 county_asianpop
rename A03001_006 county_NHpop
rename A03001_007 county_otherpop
rename A03001_008 county_tworacepop
rename A00002_002 county_popdensity
rename A04001_010 county_hispanicpop

compress
save "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/socialexplorer/gacountypopulation.dta", replace
