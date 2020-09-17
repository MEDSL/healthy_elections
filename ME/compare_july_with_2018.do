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


import delimited july_r.txt,  clear delimit("|")

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

collapse (mean) rejectedjuly = rejected (count) njuly = rejected,by(municipality)
save $dir:\scratch\rejectedjuly, replace

clear

use $dir:\scratch\rejectedjuly,
merge 1:1 municipality using $dir:\scratch\rejected2018
