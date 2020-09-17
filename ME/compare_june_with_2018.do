import delimited 2018_primary.txt,  clear delimit("|")

gen domestic = 1
replace domestic = 0 if reqtype == "UR"
replace domestic = 0 if reqtype == "FW"
replace domestic = 0 if reqtype == "VP"
keep if domestic == 1

** count number of requests
count

** count number issued
gen issued = issdate ~= ""
count if issued == 1
quietly summ issued
local issued = r(mean)*r(N)

** count number returned
gen returned = recdate ~= ""
count if returned == 1
quietly summ returned
local returned = r(mean)*r(N)

** number of returned ballots counted
gen counted = returned == 1 & accorrej == "ACC"
count if counted == 1
quietly summ counted
local counted = r(mean)*r(N)

** number of returned ballots rejected
gen rejected = returned == 1 & accorrej == "REJ"
count if rejected == 1
quietly summ rejected
local rejected = r(mean)*r(N)


display "Counted pct. = " 100*`counted'/`returned'
display "Rejected pct. = " 100*`rejected'/`returned'
