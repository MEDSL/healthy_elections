#########################33 Catalist Table Creator ########################33
library(extrafont)
library(showtext)
font_import("F:/MEDSL/healthy_elections/general/fonts/styrene_b_ttf", prompt = F)
font_import( prompt = F)
windowsFonts(A = windowsFont("StyreneB-Regular"))
windowsFonts(A = windowsFont("styrene_b"))
library(readxl)
library(tidyverse)
####################
setwd("F:/MEDSL/healthy_elections/WI")
catalist_data <- read.csv("returns_catalist_cleaned.csv")
catalist_data2 <- catalist_data[, -grep("null", colnames(catalist_data))]
catalist_data2 <- catalist_data2[, -grep("unknown", colnames(catalist_data2))]
names(catalist_data2)
catalist_data2$town_names <- catalist_data2$ward %>% strsplit(" - ") %>% sapply("[", 2)
length(unique(catalist_data2$town_names))
###let's do a stargazer table 
library(stargazer)
catalist_data2$mail2white <- catalist_data2$mail_white+catalist_data2$absentee_white
catalist_data2$mail2black <- catalist_data2$mail_black+catalist_data2$absentee_black
catalist_data2$mail2hispanic <- catalist_data2$mail_hispanic+catalist_data2$absentee_hispanic
catalist_data2$mail2other <- catalist_data2$mail_other+catalist_data2$absentee_other
catalist_data2$mail2_18to29 <- catalist_data2$mail_1829+catalist_data2$absentee_1829
catalist_data2$mail2_30to44 <- catalist_data2$mail_3044+catalist_data2$absentee_3044
catalist_data2$mail2_45to59 <- catalist_data2$mail_4559+catalist_data2$absentee_4559
catalist_data2$mail2_60plus <- catalist_data2$mail_60plus+catalist_data2$absentee_60plus
catalist_data2$total_dum <- 0
catalist_data2$total_dum[str_detect(catalist_data2$ward,"TOTAL" )] <- 1
summary(catalist_data2$total_dum)
sum(catalist_data2$total)
sum(catalist_data2$early,na.rm=TRUE)
sum(catalist_data2$day,na.rm=TRUE)
sum(catalist_data2$absentee,na.rm=TRUE)
catalist_data2b <- catalist_data2[, -grep("day", colnames(catalist_data2))]
catalist_data2b2020 <- subset(catalist_data2b, year=="2020")
nrow(catalist_data2b2020)
length(unique(catalist_data2b2020$ward))
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="TOWN OF CLAYBANKS"] <- "TOWN OF CLAY BANKS"
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="TOWN OF CLAYTON-WARD 2"] <- "TOWN OF CLAYTON"
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="TOWN OF CLAYTON-WARD 3"] <- "TOWN OF CLAYTON"
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="TOWN OF LAND O'LAKES"] <- "TOWN OF LAND O-LAKES"
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="TOWN OF SPINGDALE"] <- "TOWN OF SPRINGDALE"
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="VILLAGE OF MOUNT STERLING"] <- "VILLAGE OF MT. STERLING"
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="VILLAGE OF MT. PLEASANT"] <- "VILLAGE OF MOUNT PLEASANT"
catalist_data2b2020$town_names[catalist_data2b2020$town_names=="CITY OF BUFFALO"] <- "CITY OF BUFFALO CITY"

#VILLAGE OF INGRAM not present 
# VILLAGE OF CURTISS not present 
# VILLAGE OF GRANTON

############################################3
wi2020town <- wi2020 %>% group_by(town_name) %>% summarize(total_vote = sum(total,na.rm=T))
catalist_data2age_sum <- catalist_data2b2020 %>% 
  group_by(town_names) %>% 
  dplyr::summarize(early_1829=sum(early_1829,na.rm=T),early_3044=sum(early_3044,na.rm=T),
                    early_4559=sum(early_4559,na.rm=T),early_60plus=sum(early_60plus,na.rm=T),
                    mail2_18to29=sum(mail2_18to29,na.rm=T),mail2_30to44=sum(mail2_30to44,na.rm=T),
                    mail2_4559=sum(mail2_45to59,na.rm=T),mail2_60plus=sum(mail2_60plus,na.rm=T))
wi2020town2 <- merge(wi2020town, catalist_data2age_sum,by.x="town_name",by.y="town_names",all.x=T,all.y=F)
View(wi2020town2)
nrow(wi2020town)
nrow(wi2020town2)
###let's find mode totals 
wi2020town2$early <- wi2020town2$early_1829+wi2020town2$early_3044+wi2020town2$early_4559+wi2020town2$early_60plus
wi2020town2$mail <- wi2020town2$mail2_18to29+wi2020town2$mail2_30to44+wi2020town2$mail2_4559+wi2020town2$mail2_60plus
wi2020town2$off_results <- 0
wi2020town2$off_results[wi2020town2$total_vote < (wi2020town2$early+wi2020town2$mail)] <- 1
wi2020town2 <- wi2020town2 %>% replace(is.na(.), 0)
View(wi2020town2)
summary(wi2020town2$off_results)

###########################################################################################3
library(dplyr)
catalist_data2age_sum <- catalist_data2 %>% group_by(year) %>% dplyr::summarize(day_1829=sum(day_1829,na.rm=T),day_3044=sum(day_3044,na.rm=T),
                                                      day_4559=sum(day_4559,na.rm=T),day_60plus=sum(day_60plus,na.rm=T),
                                                      early_1829=sum(early_1829,na.rm=T),early_3044=sum(early_3044,na.rm=T),
                                                      early_4559=sum(early_4559,na.rm=T),early_60plus=sum(early_60plus,na.rm=T),
                                                      mail2_18to29=sum(mail2_18to29,na.rm=T),mail2_30to44=sum(mail2_30to44,na.rm=T),
                                                      mail2_4559=sum(mail2_45to59,na.rm=T),mail2_60plus=sum(mail2_60plus,na.rm=T))
catalist_data2age_sum


stargazer(catalist_data2 %>% select(),covariate.labels = c("Logged Donations","Weighted Donations",
                                                                                               "Weighted Donations per cap",
                                                                                               "Logged Donations per cap",
                                                                                               "Marginal-Majority",
                                                                                               "Leg. Control",
                                                                                               "Speaker Power",
                                                                                               "Governor Power (BPI)",
                                                                                               "Polarization",
                                                                                               "Leg. Seats Up",
                                                                                               "Leg. Prof.",
                                                                                               "Log GDP per cap.",
                                                                                               "GDP pct. chg.",
                                                                                               "Log Population",
                                                                                               "POTUS Approval"),
          summary = TRUE, omit.summary.stat = c("p25","p75"))