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
graph twoway (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2020&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2016&party=="democratic", c(l)), title("Democratic") subtitle("(Select Counties)") ytitle("Cumulative Mail-In Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(demcomparison)

graph twoway (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2020&party=="republican", c(l)) (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&year==2016&party=="republican", c(l)), title("Republican") subtitle("(Select Counties)") ytitle("Cumulative Mail-In Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "2020") label(2 "2016")) name(repcomparison)

graph combine demcomparison repcomparison, ycomm name(combinedcomparison) title("Texas Presidential Primaries")
graph export "$mainPath/code/results/combinedcomparison.pdf", replace

keep if mode=="MAIL-IN"
export delimited "$mainPath/code/results/mailincompare2016.csv", replace
restore


*early votes, by party and over time, 2020 only
keep if year==2020

collapse (sum) n_earlyvotes n_early_cumulative, by(daysbefore year party mode)

graph twoway (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&party=="democratic", c(l)) (scatter n_early_cumulative daysbefore if mode=="MAIL-IN"&party=="republican", c(l)), title("Mail-In Voters in TX") subtitle("2020 Presidential Primaries") ytitle("Cumulative Mail-In Votes") xtitle("Days Before Early Voting Ends") legend(on label(1 "Democrat") label(2 "Republican")) ylabel(0(25000)125000) name(partisan2020)
graph export "$mainPath/code/results/partisan2020.pdf", replace

keep if mode=="MAIL-IN"
export delimited "$mainPath/code/results/mailinpartisan2020.csv", replace
