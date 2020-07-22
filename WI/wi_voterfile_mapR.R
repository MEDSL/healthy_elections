######################################################################################
##################### WI Voterfile Cleanr and MappR #################################
######################################################################################
library(extrafont)
library(showtext)
font_import("F:/MEDSL/healthy_elections/general/fonts/styrene_b_ttf", prompt = F)
font_import( prompt = F)
windowsFonts(A = windowsFont("StyreneB-Regular"))
windowsFonts(A = windowsFont("styrene_b"))
library(readxl)
library(tidyverse)
library(foreign)
library(dplyr)
library(rgdal)
library(stringi)
library(stringr)
library(ggplot2)
options(stringsAsFactors = FALSE)
setwd("F:/MEDSL/healthy_elections/WI")
###################################################
wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
wi_abs_file <- read.csv("F:/voterfile/wi/wi_abs_export.csv")
wi_abs_file$voterregnumber <- str_pad(wi_abs_file$voterregnumber, width=10,pad="0",side="left")
wi_bisg <- readRDS("wi_bisg_results.Rdata")
nrow(wi_bisg) # 6491178
wi_abs_file <- merge(wi_abs_file, wi_bisg, by.x= "voterregnumber" , by.y="Voter.Reg.Number",all.x=T,all.y=F)
### we will now want to sum by race and such for the racial comparison 
##let's subset by election type 
wi_abs_file2 <- subset(wi_abs_file, electionname == "2020 Spring Election and Presidential Preference Vote")
length(which(is.na(wi_abs_file2$pred.whi)==T))
sum(is.na(wi_abs_file2$pred.whi)==T)/nrow(wi_abs_file2) #  0.2047895 missing race. 
sort(unique(wi_abs_file2$ballotstatusreason))
table(wi_abs_file2$ballotstatusreason,wi_abs_file2$ballotstatus)
###assigning now 
wi_abs_file2$ballot_rejected <- NA
wi_abs_file2$ballot_rejected[wi_abs_file2$ballotstatusreason!="" & wi_abs_file2$ballotstatusreason!= "Returned" ] <- 1
wi_abs_file2$ballot_rejected[wi_abs_file2$ballotstatusreason!="" & wi_abs_file2$ballotstatusreason== "Returned" ] <- 0
wi_abs_file2$voter_dum <- 0
wi_abs_file2$voter_dum[is.na(wi_abs_file2$ballot_rejected)==F] <- 1
sum(wi_abs_file2$ballot_rejected,na.rm=T)/sum(wi_abs_file2$voter_dum)
wi_abs_file2 <- subset(wi_abs_file2, voter_dum==1)
###let's find out now 
table(wi_abs_file2$ballot_rejected,wi_abs_file2$early)

100-(920880/(920880+135842))*100
(195523/(195523+1033))*100
sum(wi_abs_file2$early)/nrow(wi_abs_file2)

sum(wi_abs_file2$voter_dum)
wi_abs_file2$ballot_accepted <- NA
wi_abs_file2$ballot_accepted[wi_abs_file2$ballotstatusreason==""]
###we will need to do pred race with surname alone 
table(wi_abs_file2$ballotstatusreason,wi_abs_file2$early)
wi_abs_file2nameonly <- subset(wi_abs_file2, is.na(pred.whi)==T)
colnames(wi_abs_file2nameonly)[colnames(wi_abs_file2nameonly)=="lastname"] <- "surname"
wi_abs_file2nameonly$surname <- str_to_upper(wi_abs_file2nameonly$surname)
wi_abs_file2nameonly <- subset(wi_abs_file2nameonly, select=-c(pred.whi,pred.bla,pred.his,pred.asi,pred.oth))
wi_abs_file2nameonly <- predict_race(wi_abs_file2nameonly, surname.only = T)
wi_abs_file2 <- subset(wi_abs_file2, is.na(pred.whi)==FALSE)
wi_abs_file2$geo_impute <- 1
wi_abs_file2nameonly$geo_impute <- 0
colnames(wi_abs_file2)[colnames(wi_abs_file2)=="lastname"] <- "surname"
library(gtools)
wi_abs_file2 <- smartbind(wi_abs_file2,wi_abs_file2nameonly )
setwd(wi_vf_wd)
saveRDS(wi_abs_file2, "wi_abs_file2.Rdata" )
###now let's find the props by race 
sum(wi_abs_file2$abs_dum*wi_abs_file2$pred.whi*wi_abs_file2$ballot_rejected)/sum(wi_abs_file2$pred.whi)
sum(wi_abs_file2$abs_dum*wi_abs_file2$pred.bla*wi_abs_file2$ballot_rejected)/sum(wi_abs_file2$pred.bla)
sum(wi_abs_file2$abs_dum*wi_abs_file2$pred.his*wi_abs_file2$ballot_rejected)/sum(wi_abs_file2$pred.his)
sum(wi_abs_file2$abs_dum*wi_abs_file2$pred.asi*wi_abs_file2$ballot_rejected)/sum(wi_abs_file2$pred.asi)
sum(wi_abs_file2$abs_dum*wi_abs_file2$pred.oth*wi_abs_file2$ballot_rejected)/sum(wi_abs_file2$pred.oth)
##analyzing reasons for rejection
reason_reject_df <- wi_abs_file2 %>% group_by(early,ballotstatusreason) %>% tally()
reason_reject_df <- subset(reason_reject_df, early==0)
reason_reject_df <- subset(reason_reject_df, ballotstatusreason!="Returned")
reason_reject_df$rej_pct <- (reason_reject_df$n/sum(reason_reject_df$n))*100
reason_reject_df$category2 <- reason_reject_df$ballotstatusreason
reason_reject_df$category2[reason_reject_df$category2=="Voter Refused" | reason_reject_df$category2=="Voter Request"] <- "other"
reason_reject_df$category2[reason_reject_df$category2=="Ballot Not Received" | reason_reject_df$category2=="Not Returned"] <- 
  "Ballot Not Received"
reason_reject_df$category2[reason_reject_df$category2=="Ballot Returned After Deadline" | reason_reject_df$category2=="Ballot Not Returned By Deadline"] <- 
  "Ballot Returned After Deadline"
reason_reject_df$category2[reason_reject_df$category2=="Rejected at Polls/MBOC" | reason_reject_df$category2=="Returned, to be Rejected"] <- 
  "Rejected"
reason_reject_df$category2[reason_reject_df$category2=="Voter Spoiled" | reason_reject_df$category2=="Ineligible" ] <- "other"
reason_reject_df2 <- reason_reject_df %>% group_by(category2) %>% summarise(total_votes=sum(n))
reason_reject_df2$rej_pct <- (reason_reject_df2$total_votes/(sum(reason_reject_df2$total_votes)))*100
###we'll want to create a new category to analyze by race, by late return, not delivered, and other 
wi_abs_file2$reason2 <- "Other"
wi_abs_file2$reason2[wi_abs_file2$ballotstatusreason=="Ballot Returned After Deadline" | wi_abs_file2$ballotstatusreason=="Ballot Not Returned By Deadline"] <-
  "Ballot Returned After Deadline"
wi_abs_file2$reason2[wi_abs_file2$ballotstatusreason=="Ballot Not Received" | wi_abs_file2$ballotstatusreason=="Not Returned"] <-
  "Ballot Not Received"
wi_abs_file2$reason2[wi_abs_file2$ballotstatusreason=="Returned"] <- "Returned"
wi_abs_file2vbm_only <- subset(wi_abs_file2, early==0 )
nrow(wi_abs_file2vbm_only)/nrow(wi_abs_file2)
wi_abs_file2vbm_only$late_return <- 0
wi_abs_file2vbm_only$late_return[wi_abs_file2vbm_only$reason2=="Ballot Returned After Deadline"] <- 1
wi_abs_file2vbm_only$not_return <- 0
wi_abs_file2vbm_only$not_return[wi_abs_file2vbm_only$reason2=="Ballot Not Received"] <- 1
wi_abs_file2vbm_only$other_return <- 0
wi_abs_file2vbm_only$other_return[wi_abs_file2vbm_only$reason2=="Other"] <- 1
wi_abs_file2vbm_only <- subset(wi_abs_file2vbm_only, reason2!="Returned")
sum(wi_abs_file2vbm_only$pred.whi*wi_abs_file2vbm_only$late_return)/sum(wi_abs_file2vbm_only$pred.whi)
sum(wi_abs_file2vbm_only$pred.whi*wi_abs_file2vbm_only$not_return)/sum(wi_abs_file2vbm_only$pred.whi)
sum(wi_abs_file2vbm_only$pred.whi*wi_abs_file2vbm_only$other_return)/sum(wi_abs_file2vbm_only$pred.whi)
##blacks
sum(wi_abs_file2vbm_only$pred.bla*wi_abs_file2vbm_only$late_return)/sum(wi_abs_file2vbm_only$pred.bla)
sum(wi_abs_file2vbm_only$pred.bla*wi_abs_file2vbm_only$not_return)/sum(wi_abs_file2vbm_only$pred.bla)
sum(wi_abs_file2vbm_only$pred.bla*wi_abs_file2vbm_only$other_return)/sum(wi_abs_file2vbm_only$pred.bla)
#hispanics
sum(wi_abs_file2vbm_only$pred.his*wi_abs_file2vbm_only$late_return)/sum(wi_abs_file2vbm_only$pred.his)
sum(wi_abs_file2vbm_only$pred.his*wi_abs_file2vbm_only$not_return)/sum(wi_abs_file2vbm_only$pred.his)
sum(wi_abs_file2vbm_only$pred.his*wi_abs_file2vbm_only$other_return)/sum(wi_abs_file2vbm_only$pred.his)
#asians
sum(wi_abs_file2vbm_only$pred.asi*wi_abs_file2vbm_only$late_return)/sum(wi_abs_file2vbm_only$pred.asi)
sum(wi_abs_file2vbm_only$pred.asi*wi_abs_file2vbm_only$not_return)/sum(wi_abs_file2vbm_only$pred.asi)
sum(wi_abs_file2vbm_only$pred.asi*wi_abs_file2vbm_only$other_return)/sum(wi_abs_file2vbm_only$pred.asi)
#others
sum(wi_abs_file2vbm_only$pred.oth*wi_abs_file2vbm_only$late_return)/sum(wi_abs_file2vbm_only$pred.oth)
sum(wi_abs_file2vbm_only$pred.oth*wi_abs_file2vbm_only$not_return)/sum(wi_abs_file2vbm_only$pred.oth)
sum(wi_abs_file2vbm_only$pred.oth*wi_abs_file2vbm_only$other_return)/sum(wi_abs_file2vbm_only$pred.oth)
57697+22300+26699
3385+5928+3006
3810+3186+ 1916
2570+1092+1098
1587+782+786
106696+12319+8912+4760+3155
sum(wi_abs_file2vbm_only$pred.oth*wi_abs_file2vbm_only$late_return)+sum(wi_abs_file2vbm_only$pred.asi*wi_abs_file2vbm_only$late_return)+
  sum(wi_abs_file2vbm_only$pred.his*wi_abs_file2vbm_only$late_return)+sum(wi_abs_file2vbm_only$pred.bla*wi_abs_file2vbm_only$late_return)+
  sum(wi_abs_file2vbm_only$pred.whi*wi_abs_file2vbm_only$late_return)
sum(wi_abs_file2vbm_only$pred.oth*wi_abs_file2vbm_only$not_return)+sum(wi_abs_file2vbm_only$pred.asi*wi_abs_file2vbm_only$not_return)+
  sum(wi_abs_file2vbm_only$pred.his*wi_abs_file2vbm_only$not_return)+sum(wi_abs_file2vbm_only$pred.bla*wi_abs_file2vbm_only$not_return)+
  sum(wi_abs_file2vbm_only$pred.whi*wi_abs_file2vbm_only$not_return)
sum(wi_abs_file2vbm_only$pred.oth*wi_abs_file2vbm_only$other_return)+sum(wi_abs_file2vbm_only$pred.asi*wi_abs_file2vbm_only$other_return)+
  sum(wi_abs_file2vbm_only$pred.his*wi_abs_file2vbm_only$other_return)+sum(wi_abs_file2vbm_only$pred.bla*wi_abs_file2vbm_only$other_return)+
  sum(wi_abs_file2vbm_only$pred.whi*wi_abs_file2vbm_only$other_return)


View(reason_reject_df2)
sort(unique(reason_reject_df$ballotstatusreason))
107561+12404+8954+4781+3174
26122+1071227+66838+56950+32141+26122
sum(is.na(wi_abs_file$pred.whi)) # 517866 after the 10 digit string padding. Far superior. 
sum(is.na(wi_abs_file$pred.whi))/nrow(wi_abs_file) # 21 % missing after str padding  
nrow(wi_abs_file) - sum(is.na(wi_abs_file$pred.whi)) # 1892333 matched successfully

###These checks are able to 
#summary(nchar(wi_abs_file$voterregnumber))
#summary(nchar(wi_bisg$Voter.Reg.Number))
#quantile(nchar(wi_bisg$Voter.Reg.Number), seq(0,1,by=0.05)) # most are 10 chars long 
#quantile(nchar(wi_abs_file$voterregnumber), seq(0,1,by=0.05)) ### looks like these are way too short
#sort(unique(wi_abs_file$voterregnumber))[1:100]
#sort(unique(wi_bisg$Voter.Reg.Number))[1:100] # I see, these look pretty padded compared to the abs file ; 9 digits long ? 
###
#sort(unique(wi_bisg$Voter.Reg.Number))[6491078:6491178] #yep, looks like it should be 9 digits long 


wi_vf <- readRDS("wi_voterfile_cleaned.Rdata")
names(wi_abs_file) ##good, looks like it was read in successfully 
nrow(wi_abs_file)
wi_abs_file <- subset(wi_abs_file, electionname=="2020 Spring Election and Presidential Preference Vote" | 
                        electionname=="2020 Spring Primary")
length(unique(wi_abs_file$voterregnumber)) - 62537
length(which(wi_abs_file$electionname=="2020 Spring Primary"))
#let's do the dplyr cmd we thought of here 
wi_abs_file$dum_prez <- 1
wi_abs_file <- wi_abs_file %>% group_by(voterregnumber) %>% mutate(present = sum(dum_prez))
length(which(wi_abs_file$present>1))
wi_abs_file_dup <- subset(wi_abs_file, present > 1)
wi_abs_file <- subset(wi_abs_file, present == 1)
wi_abs_file_dup <- subset(wi_abs_file_dup, electionname=="2020 Spring Election and Presidential Preference Vote")
wi_abs_file <- rbind(wi_abs_file,wi_abs_file_dup)
nrow(wi_abs_file)

names(wi_vf)
wi_vf <- wi_vf[,c(1:25,28:29,39,41,96)]
#wi_vf$County[wi_vf$County=="Milwaukee County Supervisory District 14"] <-  "Milwaukee County"
wi_vf <- subset(wi_vf, County != "" & County != "Milwaukee County Supervisory District 14")
wi_vf <- subset(wi_vf, County != "" & County != "At Polls")
wi_vf <- subset(wi_vf, County != "Town of Delafield")
wi_vf <- subset(wi_vf, County != "City of Ashland")
wi_vf$Voter.Reg.Number <- str_pad(wi_vf$Voter.Reg.Number, width=10, side="left",pad="0")
###good. Should now be able to do the loop 
colnames(wi_abs_file)[colnames(wi_abs_file)=="county.x"] <- "County"
counnty_wi_vec <- sort(unique(wi_abs_file$County))
wi_vf_abs_all <- data.frame(stringsAsFactors = F)
for(i in 1:length(counnty_wi_vec)){
  svMisc::progress((i/length(counnty_wi_vec))*100)
  wi_temp <- subset(wi_vf, County==counnty_wi_vec[i])
  wi_abs_temp <- subset(wi_abs_file, County==counnty_wi_vec[i])
  wi_temp2 <- merge(wi_temp, wi_abs_temp, by.x="Voter.Reg.Number", by.y="voterregnumber", all.x=T)
  if(nrow(wi_vf_abs_all)==0){
    wi_vf_abs_all <- wi_temp2
  }else{
    wi_vf_abs_all <- rbind(wi_vf_abs_all, wi_temp2)
  }
}
setwd(wi_vf_wd)
sum(is.na(wi_vf_abs_all$pred.whi))/nrow(wi_vf_abs_all) # not apparently working...hm. 
table(wi_vf_abs_all$April2020,wi_vf_abs_all$ballotdeliverymethod)
saveRDS(wi_vf_abs_all, "wi_vf_abs_all07212020.Rdata")

#wi_vf_abs_all <- readRDS("wi_vf_abs_all.Rdata")
##cleaning up a bit and dropping non obs 
length(which(wi_vf_abs_all$April2020=="" & is.na(wi_vf_abs_all$ballotstatusreason)==T))
wi_vf_abs_all <- subset(wi_vf_abs_all, April2020 != "")

nrow(wi_vf_abs_all) # 2361222
wi_vf_abs_all <- wi_vf_abs_all[,-c(66:77)]
wi_vf_abs_all <- merge(wi_vf_abs_all, wi_bisg, by="Voter.Reg.Number", all.x=T)
saveRDS(wi_vf_abs_all, "wi_vf_abs_all07212020.Rdata")

wi_vf_abs_all$priorvoter <- 0
wi_vf_abs_all$priorvoter[wi_vf_abs_all$February2020!="" | wi_vf_abs_all$November2018!="" | wi_vf_abs_all$October2018!=""|
                           wi_vf_abs_all$November2016!=""|wi_vf_abs_all$April2016!=""] <- 1
wi_vf_abs_all$temp_id <- paste0(wi_vf_abs_all$Voter.Reg.Number,sep=" ",wi_vf_abs_all$LastName, sep=" ", wi_vf_abs_all$firstname, sep=" ",
                                wi_vf_abs_all$Address1)
wi_vf_abs_all <- wi_vf_abs_all[!duplicated(wi_vf_abs_all$temp_id), ]
nrow(wi_vf_abs_all)

table(wi_vf_abs_all$applicationsource, wi_vf_abs_all$priorvoter)
table(wi_vf_abs_all$April2020, wi_vf_abs_all$priorvoter)
table(wi_vf_abs_all$ballotstatusreason, wi_vf_abs_all$priorvoter) # 109    2118 ; not bad given the size of the vf 
##good, these line up 
wi_vf_abs_all$problem_return <- NA
wi_vf_abs_all$problem_return[wi_vf_abs_all$ballotstatusreason=="Returned"] <- 0
wi_vf_abs_all$problem_return[wi_vf_abs_all$ballotstatusreason!="Returned" & is.na(wi_vf_abs_all$ballotstatusreason)==F] <- 1
sum(wi_vf_abs_all$problem_return,na.rm=T) # 36758
43949/length(which(wi_vf_abs_all$priorvoter==0 & wi_vf_abs_all$ballotstatus!="")) # 0.9619164 returned w/out issue for newvoters 
1064000/length(which(wi_vf_abs_all$priorvoter==1 & wi_vf_abs_all$ballotstatus!="")) #0.9701028 returned w/out issue for old voters 
table(wi_vf_abs_all$April2020,wi_vf_abs_all$problem_return)
table(wi_vf_abs_all$ballotstatusreason,wi_vf_abs_all$April2020)
###let's find results by race 
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.whi,na.rm=T)/sum(wi_vf_abs_all$pred.whi,na.rm=T) # 0.01928071
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.bla,na.rm=T)/sum(wi_vf_abs_all$pred.bla,na.rm=T) # 0.04074668
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.his,na.rm=T)/sum(wi_vf_abs_all$pred.his,na.rm=T) # 0.02794595
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.asi,na.rm=T)/sum(wi_vf_abs_all$pred.asi,na.rm=T) # 0.02523697
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.oth,na.rm=T)/sum(wi_vf_abs_all$pred.oth,na.rm=T) # 0.02412683
sum(wi_vf_abs_all$problem_return,na.rm=T)/length(which(is.na(wi_vf_abs_all$problem_return)==F)) # 0.03211127
###let's check the rate of early voting and such 
names(wi_vf_abs_all)
sum(wi_vf_abs_all$early,na.rm=T)
194254/1555263
sum(wi_vf_abs_all$abs_dum,na.rm=T)
1142480/1555263
unique(wi_vf_abs_all$applicationsource)
wi_vf_abs_all$early2 <- NA
wi_vf_abs_all$early2[wi_vf_abs_all$applicationsource=="Voted in Person"] <- 1
wi_vf_abs_all$early2[wi_vf_abs_all$applicationsource!="Voted in Person" & is.na(wi_vf_abs_all$applicationsource)==F] <- 0
wi_vf_abs_all$vbm_dum <- NA
wi_vf_abs_all$vbm_dum[wi_vf_abs_all$early2==1] <- 0
wi_vf_abs_all$vbm_dum[wi_vf_abs_all$early2==0] <- 1
length(which(wi_vf_abs_all$vbm_dum==1 & wi_vf_abs_all$problem_return==0))
914502/1555263

table(wi_vf_abs_all$ballotstatusreason,wi_vf_abs_all$April2020)#looks like some people encountered problems getting ballot in, so 
#they instead voted in person 
11904/(11904+2868) # 0.8058489 of those with issues for deadline ended up voting in person 
750/(750+1468) # 0.3381425 of those who did not return ballot  ended up voting in person 


# 878606  voted absentee w/out problem, at polls w/out problem, or at pools w/problem ; 273325 voted abs but had problem 
878606/table(wi_vf_abs_all$April2020)
878606/(1169729 + 436267)
abs_only <- subset(wi_vf_abs_all, April2020=="Absentee")
table(abs_only$priorvoter,abs_only$problem_return)
30845/(30845+13782) # new voters 
815380/(815380 + 259543) # 0.7585474
846225/length(which(wi_vf_abs_all$April2020=="Absentee"))

odd_sub <- subset(wi_vf_abs_all, April2020=="Absentee" & ballotstatusreason=="")
View(odd_sub)
##Application date is a garbage field; way too many people over the age of 100 

###hm, we are definitely not getting the type of matching that we're expecting. Accorindg to these data, we should be seeing 
#69131 new voters, and of these, 45249 as voting early or by mail. However, we are instead seeing just 3415 in the voterfile. 
# 1272206 unique IDs in the wi abs file 
# 1605198 unique IDs in the WI voterfile for those voting in 2020 
#let's check new voters 
newvoters_vf <- subset(wi_vf2, newvoter==1)
nrow(newvoters_vf)
sort(unique(newvoters_vf$Voter.Reg.Number))[69019:69119]

##ok, looks like 203,090 voted early from absentee , and 2,188 from in person ,
sort(unique(wi_vf$April2020))
length(which(wi_vf$April2020=="Absentee"))/length(which(wi_vf$April2020!=""))
length(which(wi_vf$April2020=="Absentee")) - 203090
966168/length(which(wi_vf$April2020!=""))
length(which(wi_vf$April2020!=""))
(203090+2188)/length(which(wi_vf$April2020!=""))
###ok, now let's get the map for the early voting and race 
wi_abs_file$early <- NA
wi_abs_file$early[wi_abs_file$ballotdeliverymethod=="Voted In Person"] <- 1
wi_abs_file$early[wi_abs_file$ballotdeliverymethod!="Voted In Person" & wi_abs_file$ballotdeliverymethod!="" ] <- 0
summary(wi_abs_file$early)
wi_abs_file$abs_dum <- 0
wi_abs_file$abs_dum[wi_abs_file$ballotdeliverymethod!= ""] <- 1

wi_vf_collap_early <- wi_abs_file %>% group_by(County,early) %>% summarise(white_sum=sum(pred.whi,na.rm=T),
                                                                     nonwhite_sum=sum(1-pred.whi,na.rm=T)) 
wi_vf_collap_early <- subset(wi_vf_collap_early, is.na(early)==F)
View(wi_vf_collap_early)
###let's make into wide 
wi_vf_collap_early1 <- subset(wi_vf_collap_early, early==1)
wi_vf_collap_early0 <- subset(wi_vf_collap_early, early==0)
colnames(wi_vf_collap_early1)[3:4] <- c("white_early","nonwhite_early")
colnames(wi_vf_collap_early0)[3:4] <- c("white_vbm","nonwhite_vbm")
wi_vf_collap_early1 <- subset(wi_vf_collap_early1, select=-c(early))
wi_vf_collap_early0 <- subset(wi_vf_collap_early0, select=-c(early))
wi_vf_collap_early_wide <- merge(wi_vf_collap_early1, wi_vf_collap_early0,by="County",all.x=T,all.y=T)
wi_vf_collap_early_wide$nonwhite_early[is.na(wi_vf_collap_early_wide$nonwhite_early)==T] <- 0
wi_vf_collap_early_wide$white_early[is.na(wi_vf_collap_early_wide$white_early)==T] <- 0
wi_vf_collap_early_wide$total_white <- wi_vf_collap_early_wide$white_early+wi_vf_collap_early_wide$white_vbm
wi_vf_collap_early_wide$total_nonwhite <- wi_vf_collap_early_wide$nonwhite_early+wi_vf_collap_early_wide$nonwhite_vbm
wi_vf_collap_early_wide$total_absballots <- wi_vf_collap_early_wide$total_nonwhite+wi_vf_collap_early_wide$total_white 
View(wi_vf_collap_early_wide)
saveRDS(wi_vf_collap_early_wide, "wi_vf_collap_early_wide.Rdata")
###good. Now let's merge onto the map 
sort(unique(wi_abs_file$County))

sort(unique(wi_vf$County))
###good. Now we should be able to merge on the data 
length(unique(wi_abs_file$voterregnumber)) # 1272206
length(which(wi_abs_file$electionname=="2020 Partisan Primary"))
length(which(wi_abs_file$electionname=="2020 Spring Election and Presidential Preference Vote"))
wi_abs_slimmed <- subset(wi_abs_file, electionname=="2020 Spring Election and Presidential Preference Vote" )
wi_abs_slimmed <- subset(wi_abs_slimmed, select=c(voterregnumber,ballotdeliverymethod,dateballotsent,dateballotreturned,
                                                  pred.whi,pred.bla,pred.his,pred.asi,pred.oth))
##let's see how this goes 
wi_vf2 <- merge(wi_vf, wi_abs_slimmed,by.x="Voter.Reg.Number" , by.y="voterregnumber",all.x=T)




nrow(wi_vf2)-nrow(wi_vf) # 9129 extra 
length(unique(wi_vf2$Voter.Reg.Number))
wi_vf2 <- wi_vf2[!duplicated(wi_vf2$Voter.Reg.Number), ]
######now let's get rid of some of the older vote cols, those before 2016
wi_vf2 <- wi_vf2[,-c(43:95)]
###now let's check the voting methods 
table(wi_vf2$April2020)
1169256/(1169256+436248) #good, the numbers add up
table(wi_vf2$ballotdeliverymethod)
table(wi_abs_slimmed$ballotdeliverymethod)
###seems strange that not all of the records were matched. Ah well. 
nrow(wi_abs_slimmed) - 5673
####let's create a time series plot
wi_abs_slimmed <- subset(wi_abs_slimmed, dateballotsent!="" )
wi_abs_slimmed$dum=1
wi_abs_slimmed$return_dum <- 1
wi_abs_slimmed$return_dum[wi_abs_slimmed$dateballotreturned==""] <- 0
ts_wi_ballots <- wi_abs_slimmed %>% group_by(dateballotsent) %>% summarise(total_sent=sum(dum,na.rm=T), total_return=sum(return_dum,na.rm=T))
ts_wi_ballots$DATE <- as.Date(ts_wi_ballots$dateballotsent, tryFormats = c("%m/%d/%Y")) #good, this works 
ts_wi_ballots2 <- ts_wi_ballots[1:73, ] 
ts_wi_ballots2 <- ts_wi_ballots2[35:73, ]
###ballot return plot 
ballot_return_ts_plot <- ggplot(ts_wi_ballots2) +
  geom_line(aes(x=DATE,y=total_sent,col="#3791FF"),size=1.4) +
  geom_line(aes(x=DATE,y=total_return,col="#C0BA79"),size=1.4)
ballot_return_ts_plot <- ballot_return_ts_plot + theme_minimal() +   
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Date") + ylab("Number of ballots") + 
  scale_color_discrete(labels=c("Total sent","Total returned")) + labs(color="") + guides(size=NULL)
ballot_return_ts_plot # the three spikes occur on: 3/18, 3/25, 3/30
ggsave("wi_ballot_ts_plot.png", plot = ballot_return_ts_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 400)
###we should now also merge on the data for race estimates 
wi_bisg <- readRDS("wi_bisg_results.Rdata")
names(wi_bisg)
wi_vf2 <- merge(wi_vf2, wi_bisg, by="Voter.Reg.Number",all.x=T)
nrow(wi_vf2)
rm(wi_vf)
###lets get 2016 data 
wi2016 <- subset(wi_vf2, April2016!="")
wi2016$dum=1
wi2016 <- wi2016 %>% group_by(April2016,County) %>% summarise(total_ballots=sum(dum),white_sum=sum(pred.whi,na.rm=T)) 
wi2016$CNTY_NAME <- str_to_upper(wi2016$County)
wi2016$CNTY_NAME <- str_remove(wi2016$CNTY_NAME, " COUNTY")
wi2016$nonwhite_sum <- wi2016$total_ballots - wi2016$white_sum
wi2016 <- subset(wi2016, April2016!="X")
wi2016 <- wi2016 %>% group_by(County) %>% mutate(county_total=sum(total_ballots,na.rm=T))
wi2016$mode_pct <- (wi2016$total_ballots/wi2016$county_total)*100
wi2016 <- subset(wi2016, April2016=="Absentee")
wi2016 <- subset(wi2016, select = -c(April2016))
for(i in 2:7){
  temp_name <- paste0(colnames(wi2016)[i], sep="_", "2016")
  colnames(wi2016)[i] <- temp_name
}
names(wi2016)
##let's find the vbm pct 



####ok, now let's find the abs ballots and such cast by county and all of that . Subset such that April 2020 is filled 
wi_vf2 <- subset(wi_vf, April2020!="")
wi_vf2 <- merge(wi_vf2,wi_bisg,by="Voter.Reg.Number")
wi_vf2$dum=1
wi_vf_collapsed <- wi_vf2 %>% group_by(April2020,County) %>% summarise(total_ballots=sum(dum),white_sum=sum(pred.whi,na.rm=T)) 
wi_vf_collapsed$CNTY_NAME <- str_to_upper(wi_vf_collapsed$County)
wi_vf_collapsed$CNTY_NAME <- str_remove(wi_vf_collapsed$CNTY_NAME, " COUNTY")
wi_vf_collapsed$nonwhite_sum <- wi_vf_collapsed$total_ballots - wi_vf_collapsed$white_sum
###totals for race 
wi_vf_collapsed <- wi_vf_collapsed %>% group_by(County) %>% mutate(white_cty_pop=sum(white_sum,na.rm=T))
wi_vf_collapsed <- wi_vf_collapsed %>% group_by(County) %>% mutate(nonwhite_cty_pop=sum(nonwhite_sum,na.rm=T))

wi_vf_collapsed$white_pct <- (wi_vf_collapsed$white_sum/wi_vf_collapsed$total_ballots)*100
wi_vf_collapsed <- wi_vf_collapsed %>% group_by(County) %>% mutate(county_total=sum(total_ballots,na.rm=T))
wi_vf_collapsed$mode_pct <- (wi_vf_collapsed$total_ballots/wi_vf_collapsed$county_total)*100
wi_vf_collapsed$white_mode_pct <- (wi_vf_collapsed$white_sum/wi_vf_collapsed$county_total)*100
wi_vf_collapsed <- subset(wi_vf_collapsed, County!="")
wi_vf_collapsed_abs <- subset(wi_vf_collapsed, April2020=="Absentee")
wi_vf_collapsed_ip <- subset(wi_vf_collapsed, April2020!="Absentee")
wi_vf_collapsed_ip <- subset(wi_vf_collapsed_ip, select=c(County,total_ballots, white_sum,white_pct,white_mode_pct,mode_pct))
colnames(wi_vf_collapsed_ip) <- c("County","ip_ballots","white_ip","white_ip_pct","white_ip2pct","ip_pct")
wi_vf_collapsed_wide <- merge(wi_vf_collapsed_abs,wi_vf_collapsed_ip,by="County")
wi_vf_collapsed_wide$CNTY_NAME[wi_vf_collapsed_wide$CNTY_NAME=="ST. CROIX"] <- "SAINT CROIX"
wi_vf_collapsed_wide$nonwhite_vbm_pct <- (wi_vf_collapsed_wide$nonwhite_sum/wi_vf_collapsed_wide$nonwhite_cty_pop)*100
wi_vf_collapsed_wide$white_vbm_pct <- (wi_vf_collapsed_wide$white_sum/wi_vf_collapsed_wide$white_cty_pop)*100
wi_vf_collapsed_wide$race_diff <- (wi_vf_collapsed_wide$white_vbm_pct - wi_vf_collapsed_wide$nonwhite_vbm_pct)
###lets fix up the early file now 
wi_vf_collap_early_wide$CNTY_NAME <- str_to_upper(wi_vf_collap_early_wide$County)
wi_vf_collap_early_wide$CNTY_NAME <- str_remove(wi_vf_collap_early_wide$CNTY_NAME, " COUNTY")
wi_vf_collap_early_wide$CNTY_NAME[wi_vf_collap_early_wide$CNTY_NAME=="ST. CROIX"] <- "SAINT CROIX"
##lets merge
wi_vf_collapsed_wide_all <- merge(wi_vf_collapsed_wide, wi_vf_collap_early_wide, by="CNTY_NAME")


View(wi_vf_collapsed_wide)
sort(unique(wi_vf_collapsed_wide_all$CNTY_NAME))

#### good. Now let's get in the data by race and such 
wi_county_shp <- readOGR("F:/MEDSL/healthy_elections/WI/shpfiles", "wi_counties_diss")
wi_county_shp$CNTY_NAME <- str_to_upper(wi_county_shp$CNTY_NAME)
wi_county_shp <- merge(wi_county_shp,wi_vf_collapsed_wide_all, by="CNTY_NAME" )
###let's assign colors now
library(medslcleanR2)
medsl_purple <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")
quantile(wi_county_shp$mode_pct,seq(0,1,by=0.25))
wi_county_shp$color_abs <- medsl_purple[1]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 50 & wi_county_shp$mode_pct <60 ] <- medsl_purple[2]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 60 & wi_county_shp$mode_pct <70 ] <- medsl_purple[3]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 70 & wi_county_shp$mode_pct <80 ] <- medsl_purple[4]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 80  ] <- medsl_purple[5]
###assigning colors for 2016
wi_county_shp$color2016abs <- medsl_purple[1]
wi_county_shp$color2016abs[wi_county_shp$mode_pct_2016 >= 50 & wi_county_shp$mode_pct_2016 <60 ] <- medsl_purple[2]
wi_county_shp$color2016abs[wi_county_shp$mode_pct_2016 >= 60 & wi_county_shp$mode_pct_2016 <70 ] <- medsl_purple[3]
wi_county_shp$color2016abs[wi_county_shp$mode_pct_2016 >= 70 & wi_county_shp$mode_pct_2016 <80 ] <- medsl_purple[4]
wi_county_shp$color2016abs[wi_county_shp$mode_pct_2016 >= 80  ] <- medsl_purple[5]

##### now will do the race colors 
wi_county_shp$color_whitevbm <- medsl_purple[1]
wi_county_shp$color_whitevbm[wi_county_shp$white_vbm_pct >= 50 & wi_county_shp$white_vbm_pct <60 ] <- medsl_purple[2]
wi_county_shp$color_whitevbm[wi_county_shp$white_vbm_pct >= 60 & wi_county_shp$white_vbm_pct <70 ] <- medsl_purple[3]
wi_county_shp$color_whitevbm[wi_county_shp$white_vbm_pct >= 70 & wi_county_shp$white_vbm_pct <80 ] <- medsl_purple[4]
wi_county_shp$color_whitevbm[wi_county_shp$white_vbm_pct >= 80  ] <- medsl_purple[5]
wi_county_shp$color_nonwhitevbm <- medsl_purple[1]
wi_county_shp$color_nonwhitevbm[wi_county_shp$nonwhite_vbm_pct >= 50 & wi_county_shp$nonwhite_vbm_pct <60 ] <- medsl_purple[2]
wi_county_shp$color_nonwhitevbm[wi_county_shp$nonwhite_vbm_pct >= 60 & wi_county_shp$nonwhite_vbm_pct <70 ] <- medsl_purple[3]
wi_county_shp$color_nonwhitevbm[wi_county_shp$nonwhite_vbm_pct >= 70 & wi_county_shp$nonwhite_vbm_pct <80 ] <- medsl_purple[4]
wi_county_shp$color_nonwhitevbm[wi_county_shp$nonwhite_vbm_pct >= 80  ] <- medsl_purple[5]
wi_county_shp <- merge(wi_county_shp, wi2016,by="County")
###let's do early now 
wi_county_shp$county_early_pct <- ((wi_county_shp$white_early+wi_county_shp$nonwhite_early)/wi_county_shp$county_total)*100
wi_county_shp$white_early_pct <- ((wi_county_shp$white_early)/wi_county_shp$white_cty_pop)*100
wi_county_shp$nonwhite_early_pct <- (wi_county_shp$nonwhite_early/wi_county_shp$nonwhite_cty_pop)*100
###assigning colors 
quantile(wi_county_shp$county_early_pct,seq(0,1,by=0.05))
medsl_blues <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")
wi_county_shp$color_early <- medsl_blues[1]
wi_county_shp$color_early[wi_county_shp$county_early_pct >= 5 & wi_county_shp$county_early_pct < 10] <- medsl_blues[2]
wi_county_shp$color_early[wi_county_shp$county_early_pct >= 10 & wi_county_shp$county_early_pct < 15] <- medsl_blues[3]
wi_county_shp$color_early[wi_county_shp$county_early_pct >= 15 & wi_county_shp$county_early_pct < 20] <- medsl_blues[4]
wi_county_shp$color_early[wi_county_shp$county_early_pct >= 20] <- medsl_blues[5]
###will now do by race 
wi_county_shp$color_earlyW <- medsl_blues[1]
wi_county_shp$color_earlyW[wi_county_shp$white_early_pct >= 5 & wi_county_shp$white_early_pct < 10] <- medsl_blues[2]
wi_county_shp$color_earlyW[wi_county_shp$white_early_pct >= 10 & wi_county_shp$white_early_pct < 15] <- medsl_blues[3]
wi_county_shp$color_earlyW[wi_county_shp$white_early_pct >= 15 & wi_county_shp$white_early_pct < 20] <- medsl_blues[4]
wi_county_shp$color_earlyW[wi_county_shp$white_early_pct >= 20] <- medsl_blues[5]
wi_county_shp$color_earlyNW <- medsl_blues[1]
wi_county_shp$color_earlyNW[wi_county_shp$nonwhite_early_pct >= 5 & wi_county_shp$nonwhite_early_pct < 10] <- medsl_blues[2]
wi_county_shp$color_earlyNW[wi_county_shp$nonwhite_early_pct >= 10 & wi_county_shp$nonwhite_early_pct < 15] <- medsl_blues[3]
wi_county_shp$color_earlyNW[wi_county_shp$nonwhite_early_pct >= 15 & wi_county_shp$nonwhite_early_pct < 20] <- medsl_blues[4]
wi_county_shp$color_earlyNW[wi_county_shp$nonwhite_early_pct >= 20] <- medsl_blues[5]
wi_county_shp$race_early_diff <- wi_county_shp$white_early_pct-wi_county_shp$nonwhite_early_pct
sum(wi_county_shp$nonwhite_early)/sum(wi_county_shp$nonwhite_cty_pop) # 0.1158097
sum(wi_county_shp$white_early)/sum(wi_county_shp$white_cty_pop) # 0.1342469

saveRDS(wi_county_shp, "wi_county_mod_Shpfile.Rdata")
###mapping early 
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_early_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$county_total), wi_county_shp$color_early, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_blues,
       legend = c("< 5%" , "5 - 10%", "10 - 15%", "15 - 20%", "20%+"), title="Early %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
###for white early 
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_earlywhite_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$white_cty_pop), wi_county_shp$color_earlyW, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_blues,
       legend = c("< 5%" , "5 - 10%", "10 - 15%", "15 - 20%", "20%+"), title="Early %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
jpeg("wi_cty_earlynonwhite_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$nonwhite_cty_pop), wi_county_shp$color_earlyW, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_blues,
       legend = c("< 5%" , "5 - 10%", "10 - 15%", "15 - 20%", "20%+"), title="Early %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()


#####ok, will map

quantile(wi_vf_collapsed_abs$nonwhite_pct, seq(0,1,0.05))

setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_abs_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$county_total), wi_county_shp$color_abs, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_purple,
       legend = c("< 50%" , "50 - 60%", "60 - 70%", "70 - 80%", "80%+"), title="VBM %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()

###2016 map 
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_abs2016_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$county_total_2016), wi_county_shp$color2016abs, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_purple,
       legend = c("< 50%" , "50 - 60%", "60 - 70%", "70 - 80%", "80%+"), title="VBM %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
summary(wi_county_shp$nonwhite_vbm_pct)
summary(wi_county_shp$white_vbm_pct)
#white map here 
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_whiteabs_map.jpeg", res=500, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
wi_whiteabs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$white_cty_pop), wi_county_shp$color_whitevbm, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_purple,
       legend = c("< 50%" , "50 - 60%", "60 - 70%", "70 - 80%", "80%+"), title="VBM %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
jpeg("wi_cty_nonwhiteabs_map.jpeg", res=500, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$nonwhite_cty_pop), wi_county_shp$color_nonwhitevbm, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_purple,
       legend = c("< 50%" , "50 - 60%", "60 - 70%", "70 - 80%", "80%+"), title="VBM %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
######################################################################
######################### First time voting analysis here ################
names(wi_vf2)
wi_vf2$prior_voter <- 0
wi_vf2$prior_voter[wi_vf2$February2020!="" | wi_vf2$November2018!="" | wi_vf2$October2018!="" | wi_vf2$November2016!="" |
                     wi_vf2$April2016!=""] <- 1
summary(wi_vf2$prior_voter)
wi_vf2$prior_vbm <- 0
wi_vf2$prior_vbm[wi_vf2$February2020=="Absentee" | wi_vf2$November2018=="Absentee" | wi_vf2$October2018=="Absentee" |
                   wi_vf2$November2016=="Absentee" | wi_vf2$April2016=="Absentee"] <- 1
summary(wi_vf2$prior_vbm) # 39.01 % had experience voting by mail before 

table(wi_vf2$prior_vbm)
table(wi_vf2$prior_voter)

table(wi_vf2$prior_vbm,wi_vf2$prior_voter)
table(wi_vf2$April2020,wi_vf2$prior_voter)
newvoter_df <- wi_vf2 %>% group_by(County,prior_voter) %>% summarise(white_sum=sum(pred.whi,na.rm=T), nonwhite_sum=sum(1 - pred.whi, na.rm=T))
newvoter_df_old <- subset(newvoter_df, prior_voter==1)
newvoter_df_new <- subset(newvoter_df, prior_voter==0)
colnames(newvoter_df_old)[3:4] <- c("white_old","nonwhite_old")
colnames(newvoter_df_new)[3:4] <- c("white_new","nonwhite_new")
newvoter_df_old <- subset(newvoter_df_old, select=-c(prior_voter))
newvoter_df_new <- subset(newvoter_df_new, select=-c(prior_voter))
####let's merge on now 
newvoter_wide <- merge(newvoter_df_old,newvoter_df_new, by="County")
nrow(newvoter_wide)
###let's merge onto the county shp file now 
wi_county_shp <- merge(wi_county_shp, newvoter_wide, by.x="County.x", by.y="County")
names(wi_county_shp)
wi_county_shp$newvoter_pct <- ((wi_county_shp$white_new+wi_county_shp$nonwhite_new)/wi_county_shp$county_total)*100
wi_county_shp$white_new_pct <- (wi_county_shp$white_new/wi_county_shp$white_cty_pop)*100
wi_county_shp$nonwhite_new <- (wi_county_shp$nonwhite_new/wi_county_shp$nonwhite_cty_pop)*100
wi_county_shp$nonwhite_new_num <- (wi_county_shp$nonwhite_new*wi_county_shp$nonwhite_cty_pop)/100 
###now let's create the colors for new voters 
quantile(wi_county_shp$newvoter_pct,seq(0,1,by=0.05)) 2.7 - 7.4 
#2.7 - 3.5 , 3.5 - 4 , 4 - 5.5 , 5.5 - 7, 7 + 
medsl_reds <- c("#EDD0CB","#F59181","#F6573E","#CD3C2B","#8D2115")
wi_county_shp$color_new <- medsl_reds[1] 
wi_county_shp$color_new[wi_county_shp$newvoter_pct >= 3.5 & wi_county_shp$newvoter_pct < 4] <- medsl_reds[2] 
wi_county_shp$color_new[wi_county_shp$newvoter_pct >= 4 & wi_county_shp$newvoter_pct < 5.5] <- medsl_reds[3] 
wi_county_shp$color_new[wi_county_shp$newvoter_pct >= 5.5 & wi_county_shp$newvoter_pct < 7] <- medsl_reds[4] 
wi_county_shp$color_new[wi_county_shp$newvoter_pct >= 7] <- medsl_reds[5] 
#white new 
wi_county_shp$color_new_white <- medsl_reds[1] 
wi_county_shp$color_new_white[wi_county_shp$white_new_pct >= 3.5 & wi_county_shp$white_new_pct < 4] <- medsl_reds[2] 
wi_county_shp$color_new_white[wi_county_shp$white_new_pct >= 4 & wi_county_shp$white_new_pct < 5.5] <- medsl_reds[3] 
wi_county_shp$color_new_white[wi_county_shp$white_new_pct >= 5.5 & wi_county_shp$white_new_pct < 7] <- medsl_reds[4] 
wi_county_shp$color_new_white[wi_county_shp$white_new_pct >= 7] <- medsl_reds[5] 
## nonwhite new 
wi_county_shp$color_new_nonwhite <- medsl_reds[1] 
wi_county_shp$color_new_nonwhite[wi_county_shp$nonwhite_new >= 3.5 & wi_county_shp$nonwhite_new < 4] <- medsl_reds[2] 
wi_county_shp$color_new_nonwhite[wi_county_shp$nonwhite_new >= 4 & wi_county_shp$nonwhite_new < 5.5] <- medsl_reds[3] 
wi_county_shp$color_new_nonwhite[wi_county_shp$nonwhite_new >= 5.5 & wi_county_shp$nonwhite_new < 7] <- medsl_reds[4] 
wi_county_shp$color_new_nonwhite[wi_county_shp$nonwhite_new >= 7] <- medsl_reds[5] 

length(which(wi_county_shp$newvoter_pct < 4))
########### plots here 
jpeg("wi_cty_newvotes_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$county_total), wi_county_shp$color_new, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_reds,
       legend = c("< 3.5%" , "3.5 - 4%", "4 - 5.5%", "5.5 - 7%", "7%+"), title="New voter %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_newvotes_white_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$white_cty_pop), wi_county_shp$color_new_white, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_reds,
       legend = c("< 3.5%" , "3.5 - 4%", "4 - 5.5%", "5.5 - 7%", "7%+"), title="New voter %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_newvotes_nonwhite_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$nonwhite_cty_pop), wi_county_shp$color_new_nonwhite, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_reds,
       legend = c("< 3.5%" , "3.5 - 4%", "4 - 5.5%", "5.5 - 7%", "7%+"), title="New voter %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()

sum(wi_county_shp$white_new)/sum(wi_county_shp$white_cty_pop) # 0.04140706
sum(wi_county_shp$nonwhite_new_num)/sum(wi_county_shp$nonwhite_cty_pop) # 0.05376849
wi_county_shp$new_race_diff <- wi_county_shp$white_new_pct - wi_county_shp$nonwhite_new
###let's do tables by race 
wi_vf2$vbm_dum <- 0
wi_vf2$vbm_dum[wi_vf2$April2020== "Absentee" ] <- 1
wi_vf2$newvoter <- 0
wi_vf2$newvoter[wi_vf2$prior_voter==0] <- 1
1-sum(wi_vf2$prior_voter*wi_vf2$pred.whi,na.rm=T)/sum(wi_vf2$pred.whi,na.rm=T)
1-sum(wi_vf2$prior_voter*wi_vf2$pred.bla,na.rm=T)/sum(wi_vf2$pred.bla,na.rm=T)
1-sum(wi_vf2$prior_voter*wi_vf2$pred.his,na.rm=T)/sum(wi_vf2$pred.his,na.rm=T)
1-sum(wi_vf2$prior_voter*wi_vf2$pred.asi,na.rm=T)/sum(wi_vf2$pred.asi,na.rm=T)
1-sum(wi_vf2$prior_voter*wi_vf2$pred.oth,na.rm=T)/sum(wi_vf2$pred.oth,na.rm=T)
###now let's do by mode 
(sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.whi,na.rm=T)/sum(wi_vf2$pred.whi,na.rm=T))/(sum(wi_vf2$pred.whi*wi_vf2$newvoter,na.rm=T))
sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.bla,na.rm=T)/sum(wi_vf2$pred.bla,na.rm=T)
sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.his,na.rm=T)/sum(wi_vf2$pred.his,na.rm=T)
sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.asi,na.rm=T)/sum(wi_vf2$pred.asi,na.rm=T)
sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.oth,na.rm=T)/sum(wi_vf2$pred.oth,na.rm=T)
###by new voters 
(sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.whi,na.rm=T))/(sum(wi_vf2$pred.whi*wi_vf2$newvoter,na.rm=T))
(sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.bla,na.rm=T))/(sum(wi_vf2$pred.bla*wi_vf2$newvoter,na.rm=T))
(sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.his,na.rm=T))/(sum(wi_vf2$pred.his*wi_vf2$newvoter,na.rm=T))
(sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.asi,na.rm=T))/(sum(wi_vf2$pred.asi*wi_vf2$newvoter,na.rm=T))
(sum(wi_vf2$vbm_dum*wi_vf2$newvoter*wi_vf2$pred.oth,na.rm=T))/(sum(wi_vf2$pred.oth*wi_vf2$newvoter,na.rm=T))

wi_county_shp2 <- wi_county_shp

100-65.9
100-63.7
100-62.6
newvoter_vbm_df <- wi_vf2 %>% group_by(County,prior_vbm) %>% tally() 
newvoter_df$county_tot
newvoter_df <- as.data.frame.matrix(newvoter_df) 
colnames(newvoter_df) <- c("")
View(newvoter_df)
45243/(45243+23876)
1123849/(1123849+412241)
sum(wi_vf2$prior_vbm)
sort(unique(wi_vf2[,29]))

1 - 0.9569

head(wi_abs_file)

head(wi_abs_slimmed)

wi_abs_early <- subset(wi_abs_file, ballotdeliverymethod == "Voted In Person")
wi_abs_early$dum = 1
wi_abs_early <- subset(wi_abs_early, select = c(voterregnumber,county,dum))
nrow(wi_abs_early)
#wi_abs_early <- merge(wi_abs_early, wi_bisg, by.x="voterregnumber",by.y="Voter.Reg.Number",all.x=T)
#sum(is.na(wi_abs_early$pred.whi))
#for some reason, the voter reg numbers are not merging. I'll look this up later 
wi_abs_early <- wi_abs_early %>% group_by(county) %>% summarise(total_early=sum(dum,na.rm=T))

wi_county_shp <- merge(wi_county_shp, wi_abs_early, by.x="County",by.y="county" )
wi_county_shp$total_early[is.na(wi_county_shp$total_early)==T] <- 0
wi_county_shp$early_pct <- (wi_county_shp$total_early/wi_county_shp$county_total_2016)*100
summary(wi_county_shp$early_pct)
medsl_red <- c("#EDD0CB","#F59181","#F6573E","#CD3C2B","#8D2115")
wi_county_shp$color_early <- medsl_red[1]
wi_county_shp$color_early[wi_county_shp$early_pct >= 5 & wi_county_shp$early_pct < 10 ] <- medsl_red[2]
wi_county_shp$color_early[wi_county_shp$early_pct >= 10 & wi_county_shp$early_pct < 15 ] <- medsl_red[3]
wi_county_shp$color_early[wi_county_shp$early_pct >= 15 & wi_county_shp$early_pct < 20 ] <- medsl_red[4]
wi_county_shp$color_early[wi_county_shp$early_pct >= 20 ] <- medsl_red[5]

jpeg("wi_cty_early_map.jpeg", res=500, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$county_total), wi_county_shp$color_early, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_red,
       legend = c("< 5%" , "5 - 10%", "10 - 15%", "15 - 20%", "20%+"), title="Early Vote %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()

saveRDS(wi_county_shp, "wi_county_voterfile_collapsed_shp.Rdata")


####let's check the new voter data 
View(wi_vf2)
newvoter_vf <- subset(wi_vf2, select=c(Voter.Reg.Number,newvoter))
###let's try to merge now 
wi_abs_file2vbm_onlyB <- merge(wi_abs_file2vbm_only,newvoter_vf,by.x="voterregnumber", by.y="Voter.Reg.Number",all.x=T)
sum(wi_abs_file2vbm_onlyB$newvoter,na.rm=T)
sum(is.na(wi_abs_file2vbm_onlyB$newvoter)) # 108139 missing 
length(which(wi_vf2$newvoter==1 & wi_vf2$April2020=="Absentee")) # 45243 were new and voted absentee 


sum(is.na(wi_county_shp$total_early))

table(wi_abs_slimmed$ballotdeliverymethod)
