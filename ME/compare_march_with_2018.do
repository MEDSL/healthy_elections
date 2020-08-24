global dir = "d"

import delimited 2018_primary.txt,  clear delimit("|")

gen domestic = 1
replace domestic = 0 if reqtype == "UR"
replace domestic = 0 if reqtype == "FW"
replace domestic = 0 if reqtype == "VP"
keep if domestic == 1

gen issued = issdate ~= ""
gen returned = recdate ~= ""
gen counted = returned == 1 & accorrej == "ACC"
gen rejected = returned == 1 & accorrej == "REJ"
drop if returned == 0

collapse (mean) rejected2018 = rejected (count) n2018 = rejected,by(municipality)
save $dir:\scratch\rejected2018, replace


import delimited absentee-voter-file030320.txt,  clear delimit("|")

gen domestic = 1
replace domestic = 0 if reqtype == "UR"
replace domestic = 0 if reqtype == "FW"
replace domestic = 0 if reqtype == "VP"
keep if domestic == 1

gen issued = issdate ~= ""
gen returned = recdate ~= ""
gen counted = returned == 1 & accorrej == "ACC"
gen rejected = returned == 1 & accorrej == "REJ"
drop if returned == 0

collapse (mean) rejectedmarch = rejected (count) nmarch = rejected,by(municipality)
save $dir:\scratch\rejectedmarch, replace

clear

use $dir:\scratch\rejectedmarch,
merge 1:1 municipality using $dir:\scratch\rejected2018
