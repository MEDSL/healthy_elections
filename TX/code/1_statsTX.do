/////////////////////////////////////
/*
Kevin DeLuca
6/15/20

MEDSL - Healthy Elections
Merge data, do some basic plots
last updated: 6/15/20
*/
/////////////////////////////////////

clear
set more off
graph drop _all

*directories
global mainPath = "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/TX"
cd "$mainPath"


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

sort party county mode date
collapse (sum) n_earlyvotes n_early_cumulative, by(daysbefore year party mode)
sort party mode daysbefore year

*some graphs, will recreate in R w/ correct style

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


*all early votes (mail in + in person)
collapse (sum) n_early_cumulative, by(daysbefore year party)

graph twoway (scatter n_early_cumulative daysbefore if year==2020&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if year==2016&party=="democratic", c(l)), title("Democratic") subtitle("(Select Counties)") ytitle("Cumulative Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(demcomparison3)

graph twoway (scatter n_early_cumulative daysbefore if year==2020&party=="republican", c(l)) (scatter n_early_cumulative daysbefore if year==2016&party=="republican", c(l)), title("Republican") subtitle("(Select Counties)") ytitle("Cumulative Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(repcomparison3)

graph combine demcomparison3 repcomparison3, ycomm name(combinedcomparisonall) title("Texas Presidential Primaries") subtitle("Early In-Person and Mail-In Votes")
graph export "$mainPath/code/results/combinedcomparisonall.pdf", replace
graph export "$mainPath/code/results/combinedcomparisonall.png", replace

export delimited "$mainPath/code/results/compareto2016.csv", replace
restore







preserve
*early votes, by party and over time, 2020 only
keep if year==2020

collapse (sum) n_earlyvotes n_early_cumulative, by(daysbefore year party mode)

graph twoway (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&party=="republican", c(l)), title("Mail-In Voters in TX") subtitle("2020 Presidential Primaries") ytitle("Cumulative Mail-In Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "Democrat") label(2 "Republican")) ylabel(0(25000)125000) name(partisan2020mailin)
graph export "$mainPath/code/results/partisan2020mailin.pdf", replace
graph export "$mainPath/code/results/partisan2020mailin.png", replace

graph twoway (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if daysbefore>-11&mode=="IN-PERSON"&party=="republican", c(l)), title("Early In-Person Voters in TX") subtitle("2020 Presidential Primaries") ytitle("Cumulative Early In-Person Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "Democrat") label(2 "Republican")) name(partisan2020inperson)
graph export "$mainPath/code/results/partisan2020inperson.pdf", replace
graph export "$mainPath/code/results/partisan2020inperson.png", replace

export delimited "$mainPath/code/results/partisan2020.csv", replace

collapse (sum) n_early_cumulative, by(daysbefore year party)

graph twoway (scatter n_early_cumulative daysbefore if party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if party=="republican", c(l)), title("All Early Votes in TX") subtitle("2020 Presidential Primaries, All Counties") ytitle("Cumulative Early Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "Democrat") label(2 "Republican")) name(partisan2020all)
graph export "$mainPath/code/results/partisan2020all.pdf", replace
graph export "$mainPath/code/results/partisan2020all.png", replace

restore







*merge on COVID data, see if there is a relationship between mail in rates/early voting and covid infection rates (they were very low though, in texas during this time period. A better look will be the July primary runoff elections, though I'm not sure turnout is going to be easy to understand during this sort of weird election anyway)

