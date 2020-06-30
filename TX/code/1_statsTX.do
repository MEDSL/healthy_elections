/////////////////////////////////////
/*
Kevin DeLuca
6/15/20

MEDSL - Healthy Elections
Merge data, do some basic plots, export data to be plotted MEDSL style
last updated: 6/30/20
*/
/////////////////////////////////////

clear
set more off
graph drop _all

*directories
global mainPath = "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/TX"
cd "$mainPath"




*********

*2020 primary results...
*get county level final results, by county for 2020

clear 
import excel "$mainPath/2020/countybycountycanvassresultsrep.xlsx", firstrow
gen party="republican"
rename CANDIDATENAME candidate
rename OFFICENAME office
rename TOTALVOTESPEROFFICEPERCOUNT votes
replace votes = subinstr(votes,",","",.)
destring votes, replace
drop ELECTIONDATENAME
rename COUNTYNAME county
tempfile repcanvas
save `repcanvas', replace

clear 
import excel "$mainPath/2020/countybycountycanvassresultsdem.xlsx", firstrow
gen party="democratic"
rename CANDIDATENAME candidate
rename OFFICENAME office
rename TOTALVOTESPEROFFICEPERCOUNT votes
replace votes = subinstr(votes,",","",.)
destring votes, replace
drop ELECTIONDATENAME
rename COUNTYNAME county
append using `repcanvas'

preserve
*collapse by office, for now - keep presidential primary results, by county
collapse (sum) votes, by(party county office)
keep if office=="PRESIDENT/VICE-PRESIDENT"

*total voters
collapse (sum) votes, by (party)
rename votes totalvotes_pres
tempfile totalvoters
save `totalvoters'
restore


keep if office=="PRESIDENT/VICE-PRESIDENT"
collapse (sum) votes, by(party candidate)
merge m:1 party using `totalvoters', nogen
gen voteshare = votes/totalvotes_pres
keep if voteshare>.11

replace voteshare=voteshare*100
tab candidate, summ(votes)
tab party, summ(totalvotes_pres)
tab candidate, summ(voteshare)






*********

*covid cases in TX over time

import delimited "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/general/outside_data/us-counties-nyt.csv", clear
keep if state=="Texas"

tab county cases if date=="2020-03-03"

collapse (sum) cases deaths, by(date)
gen date2 = date(date,"YMD")
gen date3 = date2

format date2 %tdnn/dd
graph twoway (scatter cases date2 if date3<=21977, c(l)), title("Confirmed COVID19 Cases in Texas") subtitle("2/12 - 3/3") xtitle("Date") ylabel(0(5)20) ytitle("Total Confirmed Cases") xlabel(21957(4)21977)
graph export "$mainPath/code/results/covidcasesTX.pdf", replace
graph export "$mainPath/code/results/covidcasesTX.png", replace
//graph twoway (scatter cases date2 if date3<21977, c(l)) (scatter deaths date2 if date3<21977, c(l)), legend(on label(1 "Cases") label(2 "Deaths")) title("Confirmed COVID19 Cases and Death, Texas") xtitle("Date") ylabel(0(20)100)
*export as csv for R code:
export delimited "$mainPath/code/results/csvs/covidcasesTX.csv", replace






*********

*compare mail in voting stats, 2016 vs 2020

use "$mainPath/code/dta/earlyvote_county_primary2016.dta", clear
append using "$mainPath/code/dta/early_mailin_voters_bydatemodecounty.dta"

bysort year: egen temp = max(date)
gen daysbefore = date-temp
drop temp

sort party county mode daysbefore year

preserve
*can't really compare...just keep stats for counties where we have both years' stats
bysort county: egen temp = min(year)
keep if temp==2016
drop temp

tab county

sort party county mode date
collapse (sum) n_early_cumulative, by(daysbefore year party mode)
sort party mode daysbefore year

*some graphs, will recreate in R w/ correct style later

*mail in early votes
graph twoway (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2020&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2016&party=="democratic", c(l)), title("Democratic") subtitle("(Select Counties)") ytitle("Cumulative Mail-In Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(demcomparison)

graph twoway (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2020&party=="republican", c(l)) (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2016&party=="republican", c(l)), title("Republican") subtitle("(Select Counties)") ytitle("Cumulative Mail-In Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(repcomparison)

graph combine demcomparison repcomparison, ycomm name(combinedcomparison) title("Texas Presidential Primaries")
graph export "$mainPath/code/results/combinedcomparison.pdf", replace
graph export "$mainPath/code/results/combinedcomparison.png", replace


*in person early votes
graph twoway (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&year==2020&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&year==2016&party=="democratic", c(l)), title("Democratic") subtitle("(Select Counties)") ytitle("Cumulative In-Person Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(demcomparison2)

graph twoway (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&year==2020&party=="republican", c(l)) (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&year==2016&party=="republican", c(l)), title("Republican") subtitle("(Select Counties)") ytitle("Cumulative In-Person Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(repcomparison2)

graph combine demcomparison2 repcomparison2, ycomm name(combinedcomparisoninperson) title("Texas Presidential Primaries")
graph export "$mainPath/code/results/combinedcomparisoninperson.pdf", replace
graph export "$mainPath/code/results/combinedcomparisoninperson.png", replace
*export as csv for R code:
export delimited "$mainPath/code/results/csvs/combinedcomparisonmode.csv", replace


*all early votes (mail in + in person)
collapse (sum) n_early_cumulative, by(daysbefore year party)

graph twoway (scatter n_early_cumulative daysbefore if daysbefore>-11&year==2020&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if year==2016&party=="democratic", c(l)), title("Democratic") subtitle("(Select Counties)") ytitle("Cumulative Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(demcomparison3)

graph twoway (scatter n_early_cumulative daysbefore if daysbefore>-11&year==2020&party=="republican", c(l)) (scatter n_early_cumulative daysbefore if year==2016&party=="republican", c(l)), title("Republican") subtitle("(Select Counties)") ytitle("Cumulative Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(repcomparison3)

graph combine demcomparison3 repcomparison3, ycomm name(combinedcomparisonall) title("Texas Presidential Primaries") subtitle("Early In-Person and Mail-In Votes")
graph export "$mainPath/code/results/combinedcomparisonall.pdf", replace
graph export "$mainPath/code/results/combinedcomparisonall.png", replace
*export as csv for R code:
export delimited "$mainPath/code/results/csvs/combinedcomparisonall.csv", replace

restore





*********

preserve
*early votes, by party and over time, 2020 only
keep if year==2020

collapse (sum) n_early_cumulative, by(daysbefore year party mode)

graph twoway (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&party=="republican", c(l)), title("Mail-In Voters in TX") subtitle("2020 Presidential Primaries") ytitle("Cumulative Mail-In Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "Democrat") label(2 "Republican")) ylabel(0(25000)125000) name(partisan2020mailin)
graph export "$mainPath/code/results/partisan2020mailin.pdf", replace
graph export "$mainPath/code/results/partisan2020mailin.png", replace

graph twoway (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&party=="republican", c(l)), title("Early In-Person Voters in TX") subtitle("2020 Presidential Primaries") ytitle("Cumulative Early In-Person Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "Democrat") label(2 "Republican")) name(partisan2020inperson)
graph export "$mainPath/code/results/partisan2020inperson.pdf", replace
graph export "$mainPath/code/results/partisan2020inperson.png", replace
*export as csv for R code:
export delimited "$mainPath/code/results/csvs/partisan2020.csv", replace

collapse (sum) n_early_cumulative, by(daysbefore year party)

graph twoway (scatter n_early_cumulative daysbefore if party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if party=="republican", c(l)), title("All Early Votes in TX") subtitle("2020 Presidential Primaries, All Counties") ytitle("Cumulative Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "Democrat") label(2 "Republican")) name(partisan2020all)
graph export "$mainPath/code/results/partisan2020all.pdf", replace
graph export "$mainPath/code/results/partisan2020all.png", replace
*export as csv for R code:
export delimited "$mainPath/code/results/csvs/partisan2020all.csv", replace

restore






