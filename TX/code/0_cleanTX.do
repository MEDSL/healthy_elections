/////////////////////////////////////
/*
Kevin DeLuca
6/15/20

MEDSL - Healthy Elections
Clean up the available TX county-level data
last updated: 6/16/20
*/
/////////////////////////////////////

clear
set more off
graph drop _all

*directories
global mainPath = "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/TX"
cd "$mainPath"


*********

*start by creating dataset of individual mail-in/early voters
*this will be useful once we have the voter file - has voter IDs so can merge onto demographic info, then aggregate up to precinct or county level

*dem primary early/mail-in voters
forval x=2(1)28{
	import delimited "$mainPath/2020/primary_dem/feb`x'dem.csv", clear
	format id_voter %12.0f
	gen datestr = "02/`x'/2020"
	gen date = date(datestr,"MDY")
	format date %tdnn/dd/YY
	capture tostring precinct, replace force
	rename voting_method mode
	gen party = "democratic"
	tempfile feb`x'dem
	save `feb`x'dem', replace
}

forval x=2(1)28{
	import delimited "$mainPath/2020/primary_rep/feb`x'rep.csv", clear
	format id_voter %12.0f
	gen datestr = "02/`x'/2020"
	gen date = date(datestr,"MDY")
	format date %tdnn/dd/YY
	capture tostring precinct, replace force
	rename voting_method mode
	gen party = "republican"
	tempfile feb`x'rep
	save `feb`x'rep', replace
}

use `feb2dem', clear
append using `feb2rep'
forval x=3(1)28{
	append using `feb`x'dem'
	append using `feb`x'rep'
}
duplicates drop //some true duplicates here, probably mistakes in the system

gen electiontype="primary"
gen year=2020
gen date2=date

compress
//save "$mainPath/code/dta/early_mailin_voters.dta", replace

*collapse down to county-level, for merging later
gen n_earlyvotes = 1
collapse (sum) n_earlyvotes, by(county mode date date2 datestr party electiontype year)

*ts set for functions
egen ids = group(county mode party)

tsset ids date
tsfill

replace n_earlyvotes=0 if n_earlyvotes==.
*cumulative votes
sort ids date
gen n_early_cumulative = n_earlyvotes
local N = _N
forval x=2(1)`N'{
	qui replace n_early_cumulative = n_early_cumulative[`x']+n_early_cumulative[`x'-1] in `x' if ids[`x']==ids[`x'-1]
	qui replace county=county[`x'-1] in `x' if ids[`x']==ids[`x'-1]
	qui replace datestr=datestr[`x'-1] in `x' if ids[`x']==ids[`x'-1]
	qui replace mode=mode[`x'-1] in `x' if ids[`x']==ids[`x'-1]
	qui replace party=party[`x'-1] in `x' if ids[`x']==ids[`x'-1]
}
replace year=2020
replace electiontype="primary"
replace date2=date
drop if county==""

compress
save "$mainPath/code/dta/early_mailin_voters_bydatemodecounty.dta", replace

collapse (sum) n_earlyvotes, by(county mode party electiontype year)
compress
save "$mainPath/code/dta/early_mailin_voters_bymodecounty.dta", replace

collapse (sum) n_earlyvotes, by(mode party electiontype year)
compress
save "$mainPath/code/dta/early_mailin_voters_bymode.dta", replace

*/
*********




*********

*get 2016 data, for comparisons (county-level, for those available)
clear
import excel "$mainPath/2016/earlyvotestats_county_primary2016.xlsx", firstrow
rename County county
gen party = "democratic" if Party=="Democratic"
replace party = "republican" if Party=="Republican"
rename DATE date
rename CumulativeInPersonVoters votes1
rename CumulativeByMailVoters votes2
replace county=strupper(county)
gen date2 = date

keep electiontype county party date* votes*

drop if county=="TOTAL"
//keep if date2==20510

reshape long votes, i(electiontype county party date date2) j(modenum)
gen mode = "MAIL-IN" if modenum==2
replace mode = "IN-PERSON" if modenum==1
rename votes n_early_cumulative

drop modenum
gen year=2016

save "$mainPath/code/dta/earlyvote_county_primary2016.dta", replace

*/
*********



STOP


*********

*put the county-level polling places into one big spreadsheet

clear
local files: dir "$mainPath/2020/pollplaces" files "*.csv"
di `files'

foreach file in `files'{
	import delimited "$mainPath/2020/pollplaces/`file'", clear
	//split precinct, p(,)
	gen year=2020
	save "$mainPath/code/dta/temp/`file'.dta", replace
}


use "$mainPath/code/dta/temp/anderson.csv.dta", clear
foreach file in `files'{
	append using "$mainPath/code/dta/temp/`file'.dta", force
}
duplicates drop
export delimited "$mainPath/2020/allpollplaces.csv", replace


*/
*********



STOP

*********

*get county level final results, by county for 2020

clear 
import excel "$mainPath/2020/countybycountycanvassresultsrep.xlsx", firstrow
gen party="republican"
rename OFFICENAME office
rename TOTALVOTESPEROFFICEPERCOUNT votes
replace votes = subinstr(votes,",","",.)
destring votes, replace
drop ELECTIONDATENAME
rename COUNTYNAME county

*collapse by office, for now - keep presidential primary results, by county
collapse (sum) votes, by(party county office)
keep if office=="PRESIDENT/VICE-PRESIDENT"
tempfile repcanvas
save `repcanvas', replace

clear 
import excel "$mainPath/2020/countybycountycanvassresultsdem.xlsx", firstrow
gen party="democratic"
rename OFFICENAME office
rename TOTALVOTESPEROFFICEPERCOUNT votes
replace votes = subinstr(votes,",","",.)
destring votes, replace
drop ELECTIONDATENAME
rename COUNTYNAME county

*collapse by office, for now - keep presidential primary results, by county
collapse (sum) votes, by(party county office)
keep if office=="PRESIDENT/VICE-PRESIDENT"
append using `repcanvas'


