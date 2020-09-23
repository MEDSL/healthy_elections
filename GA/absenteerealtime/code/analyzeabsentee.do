/////////////////////////////////////////
/*
Kevin DeLuca: kevindeluca@g.harvard.edu
September 2020

MIT-HEALTHY ELECTIONS
Code to import and analyze GA absentee file
*/
/////////////////////////////////////////




//////////////////////////////////////////////////////////////////////////////////
/*
INSTRUCTIONS
 
1. download the latest absetnee file from the GA SOS website: https://elections.sos.ga.gov/Elections/voterabsenteefile.do
	1a Name the zipfile "YYYY-MM-DD.zip", and save it in the directory "~/healthy_elections/GA/absenteerealtime/absenteefiles"
	1b There must be a zipfile with the appropriate date in the right folder or the program will not run
2. change the directory to the local path for "~/healthy_elections/GA/absenteerealtime/"
3. change the date global (below) to the date you want to look at
4. run the code and be amazed at the results
*/
//////////////////////////////////////////////////////////////////////////////////


clear
set more off
graph drop _all




*CHANGE THIS DIRECTORY HERE
global mainPath = "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/absenteerealtime"

*CHANGE THIS DATE VARIABLE
global analyzedate = "2020-09-17"




*no statewide file for this one, need to append all the csvs together
cd "$mainPath/absenteefiles/"
unzipfile "$mainPath/absenteefiles/$analyzedate.zip"

local files: dir "$mainPath/absenteefiles/" files "*.csv"
di `files'
local m=1
foreach file in `files'{
	import delimited "$mainPath/absenteefiles/`file'", clear
// 	tostring(countyprecinct), replace
// 	tostring(street), replace
// 	tostring(municipalprecinct), replace
// 	tostring(mailingstreet), replace
// 	tostring(mailingaptunit), replace
	tostring(ballotstatus statusreason ballotreturndate), replace
	keep county voterregistration city zipcode applicationstatus ballotstatus statusreason applicationdate ballotissueddate ballotreturndate ballotstyle ballotassisted challengedprovisional votecenterid
	
	tempfile csv`m'
	save `csv`m''
	display `m'
	local m = `m'+1
	erase "$mainPath/absenteefiles/`file'"
}

local m = `m'-1
use `csv1', clear
forval x=2(1)`m'{
	append using `csv`x''
}
duplicates drop
gen year=2020
rename voterregistration registration_number

*extract application date, return date, and ballotstatus, etc.


gen analyzedate = "$analyzedate"
gen analyzedate2 = date(analyzedate,"YMD")

gen date_applicationdate = date(applicationdate,"MDY")
gen date_ballotissueddate = date(ballotissueddate,"MDY")
gen date_ballotreturndate = date(ballotreturndate,"MDY")
format date_applicationdate %tdnn/dd/yyyy
format date_ballotissueddate %tdnn/dd/yyyy
format date_ballotreturndate %tdnn/dd/yyyy
gen date_applicationdate2 = date_applicationdate
gen date_ballotissueddate2 = date_ballotissueddate
gen date_ballotreturndate2 = date_ballotreturndate

compress


*plots - start all plots in september

*applications by date
gen c = 1
preserve
drop if date_applicationdate>analyzedate2+1
collapse (sum) c, by(applicationstatus date_applicationdate date_applicationdate2)
drop if date_applicationdate==.
drop if date_applicationdate2<22159|date_applicationdate2>22222
graph twoway (scatter c date_applicationdate if applicationstatus=="A", c(l)), title("Ballot Applications by Date") ytitle("Number of Applications") xtitle("Application Date") name(applied)
graph export "$mainPath/results/$analyzedate_applied.pdf", replace
restore

*issued by date
preserve
drop if date_ballotissueddate>analyzedate2+1
collapse (sum) c, by(applicationstatus date_ballotissueddate date_ballotissueddate2)
drop if applicationstatus=="R"
drop if date_ballotissueddate2==.
drop if date_ballotissueddate2<22159|date_ballotissueddate2>22222
graph twoway (scatter c date_ballotissueddate if applicationstatus=="A", c(l)), title("Ballots Issued by Date") ytitle("Ballots Issued") xtitle("Issue Date") name(issued)
graph export "$mainPath/results/$analyzedate_issued.pdf", replace
restore

*returned by date
preserve
drop if date_ballotreturndate>analyzedate2+1
collapse (sum) c, by(applicationstatus date_ballotreturndate date_ballotreturndate2)
drop if date_ballotreturndate2==.
drop if date_ballotreturndate2<22159|date_ballotreturndate2>22222
graph twoway (scatter c date_ballotreturndate if applicationstatus=="A", c(l)), title("Ballots Returned by Date") ytitle("Ballots Returned") xtitle("Return Date") name(returned)
graph export "$mainPath/results/$analyzedate_returned.pdf", replace
restore






