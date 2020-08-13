/////////////////////////
/*
Kevin DeLuca
8/12/20
Healthy Elections - GA
Plots
*/
/////////////////////////

clear 
set more off


*****
import delimited "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/reportdata/countylevelstats.csv", clear 

*county-level rejection rates by race, 2020
graph twoway (scatter bhreject_2020combopri whreject_2020combopri [w=votedpop_2020combopri], msymbol(circle_hollow))  (function y = x, ra(bhreject_2020combopri) lcolor(gray) clpat(dash)), ytitle("Black Rejection Rate") xtitle("White Rejection Rate") aspectratio(1) // title("Rejection Rates by Race, 2020 Combined Primary") subtitle("County Level by Race Rejection Rates")

*overall VBM rates, 2016 PPP vs 2020 combined
graph twoway (scatter bymail_2020combopri bymail_2016prepri [w=votedpop_2020combopri], msymbol(circle_hollow))  (function y = x, lcolor(gray) clpat(dash)), ytitle("County % VBM - 2020 Combined Primary") xtitle("County % VBM - 2016 Presidential Primary") ylab(0(0.2)1) xlab(0(0.2)1) aspectratio(1) //title("VBM Rates, 2016 Pres Primary and 2020 Combined Primary")

*overall rejection rates, 2016 PPP vs 2020 combined
graph twoway (scatter rejected_2020combopri rejected_2016prepri [w=votedpop_2020combopri], msymbol(circle_hollow)) (function y = x, lcolor(gray) clpat(dash)),  ytitle("Rejection Rate - 2020 Combined Primary") xtitle("Rejection Rate - 2016 Presidential Primary") aspectratio(1) ylab(0(0.2)1) xlab(0(0.2)1) //title("VBM Rejection Rates, 2016 Pres Primary and 2020 Combined Primary")

*electionday voting, 2016 PPP vs 2020 combined
graph drop _all
graph twoway (scatter electionday_2020combopri electionday_2016prepri [w=votedpop_2020combopri], msymbol(circle_hollow)) (function y = x, lcolor(gray) clpat(dash)), ytitle("Election Day - 2020 Combined Primary") xtitle("Election Day - 2016 Presidential Primary") ylab(0(0.2)1) xlab(0(0.2)1)  aspectratio(1) //title("Election Day Voting Rates, 2016 Pres Primary and 2020 Combined Primary")





*****
import delimited "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/reportdata/rejectbycounty.csv", clear

graph bar rejected if rejected>=0.0142, over(county, label(angle(45))) ytitle("Proportion Rejected") blabel(total, format(%5.3f))








*****
import delimited "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/reportdata/rejectratesbyage.csv", clear

graph twoway (scatter rejected age if year==2016, c(l)), title("Rejection Rates by Age in 2016") xtitle("Age in Years") ytitle("VBM Rejection Rates") ylab(0(0.1)0.4)

graph twoway (scatter rejected age if year==2020, c(l)), title("Rejection Rates by Age in 2020") xtitle("Age in Years") ytitle("VBM Rejection Rates") ylab(0(0.005)0.025)












*****
import delimited "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/reportdata/ballotissuedates.csv", clear

gen temp = ballotissueweek2
format temp %tdnn/dd
drop ballotissueweek
rename temp ballotissueweek

*cumulative ballots issued by week
graph twoway (scatter cumulative_issued ballotissueweek if ballotissueweek2>22020, c(l)), ytitle("Total Ballots Issued") xtitle("Week Ballot was Issued to Voters")  ylab(0(250000)1750000)

*proportion returned by week
graph twoway (scatter returned ballotissueweek if ballotissueweek2>22020, c(l)), ytitle("Proportion of Ballots Returned") xtitle("Week Ballot was Issued to Voters") name(ballotreturnedweek) ylab(0(0.2)1)

*final voter/ballot status by issue date
graph twoway (scatter electionday ballotissueweek if ballotissueweek2>22020, c(l)) (scatter rejected ballotissueweek if ballotissueweek2>22020, c(l)) (scatter lateballot ballotissueweek if ballotissueweek2>22020, c(l)), legend(on label(1 "Voted on Eleciton Day") label(2 "Rejected") label(3 "Late Ballot") col(3)) ytitle("Proportion of Ballots/Voters") xtitle("Week Ballot was Issued to Voter") ylab(0(0.05)0.15)








*****
import delimited "/Users/cantstopkevin/Documents/HarvardDesktop/MEDSL/github/healthy_elections/GA/reportdata/returnedballotissuedates.csv", clear

gen temp = ballotissueweek2
format temp %tdnn/dd
drop ballotissueweek
rename temp ballotissueweek

*returned ballots only - final voter/ballot status by issue date
graph twoway (scatter electionday ballotissueweek if ballotissueweek2>22020, c(l)) (scatter rejected ballotissueweek if ballotissueweek2>22020, c(l)) (scatter lateballot ballotissueweek if ballotissueweek2>22020, c(l)), legend(on label(1 "Voted on Eleciton Day") label(2 "Rejected") label(3 "Late Ballot") col(3)) ytitle("Proportion of Ballots/Voters") xtitle("Week Ballot was Issued to Voter") ylab(0(0.05)0.15)
