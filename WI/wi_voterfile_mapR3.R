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
###### Code cleaning through the wi abs file 
wi_abs_file <- read.csv("F:/voterfile/wi/wi_abs_export.csv")
wi_abs_file$voterregnumber <- str_pad(wi_abs_file$voterregnumber, width=10,pad="0",side="left")
wi_abs_file <- subset(wi_abs_file, electionname=="2020 Spring Election and Presidential Preference Vote" | 
                        electionname=="2020 Spring Primary")
wi_abs_file$dum_prez <- 1
###Identifying the duplicates 
wi_abs_file <- wi_abs_file %>% group_by(voterregnumber) %>% mutate(present = sum(dum_prez))
length(which(wi_abs_file$present>1))
wi_abs_file_dup <- subset(wi_abs_file, present > 1)
wi_abs_file <- subset(wi_abs_file, present == 1)
wi_abs_file_dup <- subset(wi_abs_file_dup, electionname=="2020 Spring Election and Presidential Preference Vote")
###binding the duplicated data 
wi_abs_file <- rbind(wi_abs_file,wi_abs_file_dup)
length(which(wi_abs_file$ballotstatusreason=="Returned"))/nrow(wi_abs_file)# 0.8868039 were returned . What are other options? 
#So this equals returned and counted. WE will need to look at the other categories 
length(which(wi_abs_file$dateballotreturned==""))/nrow(wi_abs_file) # 0.09906817 prop of ballots with blank return address, so not submitted
###will now summarize and get the not returned by county
wi_abs_file$not_returned <- 0
wi_abs_file$not_returned[wi_abs_file$dateballotreturned==""] <- 1
wi_abs_file$county <- str_to_upper(wi_abs_file$county)
wi_abs_file$county <- str_remove_all(wi_abs_file$county, " COUNTY")
wi_abs_file$early_dum <- 0  
wi_abs_file$early_dum[wi_abs_file$ballotdeliverymethod=="Voted In Person"] <- 1  
wi_abs_file$vbm_dum <- 0
wi_abs_file$vbm_dum[wi_abs_file$ballotdeliverymethod!="Voted In Person" & wi_abs_file$ballotdeliverymethod!=""] <- 1
###now that we coded ballot method, we will do counted 
wi_abs_file$counted <- 0
wi_abs_file$counted[wi_abs_file$ballotstatusreason=="Returned"] <- 1
###dummies for returned and mail and returned and early 
wi_abs_file$mail_returned <- 0
wi_abs_file$mail_returned[wi_abs_file$not_returned==0 & wi_abs_file$vbm_dum==1] <- 1
wi_abs_file$early_returned <- 0
wi_abs_file$early_returned[wi_abs_file$not_returned==0 & wi_abs_file$early_dum==1] <- 1
##now for cojunted and method 
wi_abs_file$mail_counted <- 0
wi_abs_file$mail_counted[wi_abs_file$vbm_dum==1 & wi_abs_file$counted==1] <- 1
wi_abs_file$early_counted <- 0
wi_abs_file$early_counted[wi_abs_file$early_dum==1 & wi_abs_file$counted==1] <- 1

wi_abs_file$ballotreasontype <- str_to_lower(wi_abs_file$ballotreasontype)
###let's get reasons down 
wi_abs_file$rejection_reason <- "other"
wi_abs_file$rejection_reason[wi_abs_file$ballotreasontype=="absentee ballot received after deadline"] <- "received late"
wi_abs_file$rejection_reason[wi_abs_file$ballotreasontype=="absentee ballot postmarked after election day" | 
                               wi_abs_file$ballotreasontype == "post marked after election day"] <- "postmarked late"
wi_abs_file$rejection_reason[str_detect(wi_abs_file$ballotreasontype, "certification")] <- "certification issue"
wi_abs_file$rejection_reason[str_detect(wi_abs_file$ballotreasontype, " poi ") | str_detect(wi_abs_file$ballotreasontype, " por ")] <-
  "proof of identity/residence issue"
wi_abs_file$rejection_reason[wi_abs_file$ballotreasontype=="superseding ballot accepted" | 
                               wi_abs_file$ballotreasontype=="superseding ballot returned" | 
                               wi_abs_file$ballotreasontype=="vote already recorded for this voter"] <- "superseding ballot"
wi_abs_file$rejection_reason[wi_abs_file$ballotreasontype==""] <- "accepted"

table(wi_abs_file$rejection_reason)
###looking at not returned specifically and rekected 
wi_notcounted <- subset(wi_abs_file, rejection_reason!= "accepted" | not_returned == 1 )
wi_notcounted$full_addrs <- paste0(wi_notcounted$address1, sep=" ", wi_notcounted$address2)
wi_notcounted_addrs <- subset(wi_notcounted, select = c(full_addrs, county))
wi_notcounted_addrs <- wi_notcounted_addrs[!duplicated(wi_notcounted_addrs$full_addrs), ]
nrow(wi_notcounted_addrs)
wi_notcounted_addrs <- as.data.frame(wi_notcounted)
write.dbf(wi_notcounted_addrs, "wi_notcounted2020addrs.dbf")
write.csv(wi_notcounted_addrs, "wi_notcounted2020vf.csv",row.names = FALSE)

write.csv(wi2016abs, "wi2016abs_vf.csv",row.names = FALSE)
nrow(wi_notcounted)
View(wi_notcounted)


##let's save the work so far 
saveRDS(wi_abs_file, "wi_abs_file08032020.Rdata")

sort(unique(wi_abs_file$ballotreasontype)) # is the field for rejection 
table(wi_abs_file$rejection_reason)
length(which(wi_abs_file$rejection_reason!="accepted"))

###let's check to make sure the results line up 
length(which(wi_abs_file$ballotstatusreason=="Ballot Not Returned By Deadline" | 
               wi_abs_file$ballotstatusreason=="Ballot Returned After Deadline" & wi_abs_file$not_returned==0))/938411

###now we will summarize by county
wi_notreturned <- wi_abs_file %>% group_by(county) %>% summarize(total_requests=sum(dum_prez,na.rm=T), early_returned=sum(early_returned),
                                                                 mail_returned=sum(mail_returned),mail_counted=sum(mail_counted),
                                                                 early_counted=sum(early_counted),counted=sum(counted),
                                                                 not_returned=sum(not_returned))
wi_notreturned$returned_pct <- ((wi_notreturned$total_requests-wi_notreturned$not_returned)/(wi_notreturned$total_requests))*100
summary(wi_notreturned$returned_pct) #good 
wi_notreturned$counted_pct <- ((wi_notreturned$counted)/(wi_notreturned$total_requests-wi_notreturned$not_returned))*100
wi_notreturned$counted_pct[wi_notreturned$counted_pct>100] <- 100
###now let's do counted pct by mode 
wi_notreturned$mail_counted_pct <- ((wi_notreturned$mail_counted)/wi_notreturned$mail_returned)*100
wi_notreturned$mail_counted_pct[wi_notreturned$mail_counted_pct>100] <- 100
wi_notreturned$early_counted_pct <- ((wi_notreturned$early_counted)/wi_notreturned$early_returned)*100
setwd("F:/voterfile/wi")
saveRDS(wi_notreturned, "wi_abs_counted_stats_county.Rdata")

summary(wi_notreturned$early_counted_pct)

summary(wi_notreturned$counted_pct)

View(wi_notreturned)


sort(unique(wi_abs_file$ballotdeliverymethod))
sort(unique(wi_abs_file$ballotstatusreason))





############################################
wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
saveRDS(wi_vf_abs_all, "wi_vf_abs_all07222020.Rdata")
wi_vf_abs_all <- readRDS("wi_vf_abs_all07222020.Rdata")

###let's do mode voting by race here 
(sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.whi,na.rm=T))/(sum(wi_vf_abs_all$pred.whi, na.rm=T))
(sum(wi_vf_abs_all$early2*wi_vf_abs_all$pred.whi,na.rm=T))/(sum(wi_vf_abs_all$pred.whi, na.rm=T))
100 - 58.7 - 12.5
#black
(sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.bla,na.rm=T))/(sum(wi_vf_abs_all$pred.bla, na.rm=T))
(sum(wi_vf_abs_all$early2*wi_vf_abs_all$pred.bla,na.rm=T))/(sum(wi_vf_abs_all$pred.bla, na.rm=T))
100 - 60.3 - 11.5
#hispanic
(sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.his,na.rm=T))/(sum(wi_vf_abs_all$pred.his, na.rm=T))
(sum(wi_vf_abs_all$early2*wi_vf_abs_all$pred.his,na.rm=T))/(sum(wi_vf_abs_all$pred.his, na.rm=T))
100-62.6 - 9.1
#asian
(sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.asi,na.rm=T))/(sum(wi_vf_abs_all$pred.asi, na.rm=T))
(sum(wi_vf_abs_all$early2*wi_vf_abs_all$pred.asi,na.rm=T))/(sum(wi_vf_abs_all$pred.asi, na.rm=T))
100 - 64.4-9.2
#other
(sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.oth,na.rm=T))/(sum(wi_vf_abs_all$pred.oth, na.rm=T))
(sum(wi_vf_abs_all$early2*wi_vf_abs_all$pred.oth,na.rm=T))/(sum(wi_vf_abs_all$pred.oth, na.rm=T))
100-59.6-10.3
##quickly do total by race 
sum(wi_vf_abs_all$pred.whi,na.rm=T) # 1345038
sum(wi_vf_abs_all$pred.bla,na.rm=T) # 81255
sum(wi_vf_abs_all$pred.his,na.rm=T) # 91350
sum(wi_vf_abs_all$pred.asi,na.rm=T) # 51849
sum(wi_vf_abs_all$pred.oth,na.rm=T) # 36496
nrow(wi_vf_abs_all)
##let's get total pcts 
sum(wi_vf_abs_all$vbm_dum,na.rm=T)/nrow(wi_vf_abs_all)
sum(wi_vf_abs_all$early2,na.rm=T)/nrow(wi_vf_abs_all)
100-59.2-12.1

####let's move the return rates here 
table(wi_vf_abs_all$vbm_dum ,wi_vf_abs_all$problem_return)
#let's find out how many had a problem, initially coded as vbm, but then voted At polls for April 2020
length(which(wi_vf_abs_all$April2020=="At Polls" & wi_vf_abs_all$vbm_dum==1 & wi_vf_abs_all$problem_return==1)) # 22075
22075/36256 # the proportion of those who managed to vote in person given problems with vbm 
length(which(wi_vf_abs_all$ballotstatusreason=="Not Returned" | wi_vf_abs_all$ballotstatusreason=="Ballot Not Returned By Deadline" |
               wi_vf_abs_all$ballotstatusreason=="Ballot Not Received" | wi_vf_abs_all$ballotstatusreason=="Ballot Returned After Deadline"))
25741/nrow(wi_vf_abs_all)
unique(wi_vf_abs_all$ballotstatusreason)
914502/(914502+36256)
###getting the rejected reasons here 
table(wi_vf_abs_all$problem_return,wi_vf_abs_all$vbm_dum)
###we'll want to create a new category to analyze by race, by late return, not delivered, and other 
#reason complex 
wi_vf_abs_all$reason1 <- NA
wi_vf_abs_all$reason1[wi_vf_abs_all$ballotstatusreason=="Ballot Returned After Deadline" | 
                        wi_vf_abs_all$ballotstatusreason=="Ballot Not Returned By Deadline"] <-
  "Ballot Returned After Deadline"
wi_vf_abs_all$reason1[wi_vf_abs_all$ballotstatusreason=="Ballot Not Received" | wi_vf_abs_all$ballotstatusreason=="Not Returned"] <-
  "Ballot Not Received"
wi_vf_abs_all$reason1[wi_vf_abs_all$ballotstatusreason=="Returned"] <- "Returned"
wi_vf_abs_all$reason1[wi_vf_abs_all$ballotstatusreason=="Rejected at Polls/MBOC" | wi_vf_abs_all$ballotstatusreason=="Returned, to be Rejected"|
                        wi_vf_abs_all$ballotstatusreason=="Ineligible"] <- "Rejected"
wi_vf_abs_all$reason1[wi_vf_abs_all$ballotstatusreason=="Undeliverable" | wi_vf_abs_all$ballotstatusreason=="Voter Moved"] <-
                        "Delivery Issue"
wi_vf_abs_all$reason1[is.na(wi_vf_abs_all$ballotstatusreason)==F & is.na(wi_vf_abs_all$reason1)==T ] <- "Other"
(table(wi_vf_abs_all$reason1)/(sum(is.na(wi_vf_abs_all$reason1)==F)-  1107949))*100
#### reason simplified 
wi_vf_abs_all$reason2 <- NA
wi_vf_abs_all$reason2[wi_vf_abs_all$ballotstatusreason=="Ballot Returned After Deadline" | 
                        wi_vf_abs_all$ballotstatusreason=="Ballot Not Returned By Deadline"] <-
  "Ballot Returned After Deadline"
wi_vf_abs_all$reason2[wi_vf_abs_all$ballotstatusreason=="Ballot Not Received" | wi_vf_abs_all$ballotstatusreason=="Not Returned"] <-
  "Ballot Not Received"
wi_vf_abs_all$reason2[wi_vf_abs_all$ballotstatusreason=="Returned"] <- "Returned"
wi_vf_abs_all$reason2[is.na(wi_vf_abs_all$ballotstatusreason)==F & is.na(wi_vf_abs_all$reason2)==T ] <- "Other"
(table(wi_vf_abs_all$reason2)/1144707)*100
sum(is.na(wi_vf_abs_all$ballotstatusreason)==F) # 1144707
1144707 - 1107949
###ok, let's do the by race here 
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.whi*wi_vf_abs_all$vbm_dum,na.rm=T)/
  (sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.whi,na.rm=T)) # num: 28116.87 , 789470
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.bla*wi_vf_abs_all$vbm_dum,na.rm=T)/
  (sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.bla,na.rm=T)) # num: 3041.161 , 48984.43
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.his*wi_vf_abs_all$vbm_dum,na.rm=T)/
  (sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.his,na.rm=T)) #2692.949 , 57145.11
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.asi*wi_vf_abs_all$vbm_dum,na.rm=T)/
  (sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.asi,na.rm=T)) # 1458.734 , 33407.34
sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.oth*wi_vf_abs_all$vbm_dum,na.rm=T)/
  (sum(wi_vf_abs_all$vbm_dum*wi_vf_abs_all$pred.oth,na.rm=T)) # 946.2824 , 21751.16
28117+ 3041 + 2693 +  1459 + 946 # numerator 
21751 + 33407 + 57145 + 48984 + 789470 # denom 
36256/950757
###will now create a series of dummy variables for reason rejected 
wi_vf_abs_all$not_received <- NA
wi_vf_abs_all$not_received[wi_vf_abs_all$reason2!="Ballot Not Received"] <- 0
wi_vf_abs_all$not_received[wi_vf_abs_all$reason2=="Ballot Not Received"] <- 1
wi_vf_abs_all$late_return <- NA
wi_vf_abs_all$late_return[wi_vf_abs_all$reason2!="Ballot Returned After Deadline"] <- 0
wi_vf_abs_all$late_return[wi_vf_abs_all$reason2=="Ballot Returned After Deadline"] <- 1
wi_vf_abs_all$other_rej <- NA
wi_vf_abs_all$other_rej[wi_vf_abs_all$reason2!="Other"] <- 0
wi_vf_abs_all$other_rej[wi_vf_abs_all$reason2=="Other"] <- 1
####now let's do summaries by race 
sum(wi_vf_abs_all$late_return*wi_vf_abs_all$pred.whi,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.whi,na.rm=T) # 0.4233828
sum(wi_vf_abs_all$not_received*wi_vf_abs_all$pred.whi,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.whi,na.rm=T)  # 0.2680826
sum(wi_vf_abs_all$other_rej*wi_vf_abs_all$pred.whi,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.whi,na.rm=T) #  0.3085346
#black
sum(wi_vf_abs_all$late_return*wi_vf_abs_all$pred.bla,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.bla,na.rm=T)  # 0.2452842
sum(wi_vf_abs_all$not_received*wi_vf_abs_all$pred.bla,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.bla,na.rm=T) # 0.5047406
sum(wi_vf_abs_all$other_rej*wi_vf_abs_all$pred.bla,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.bla,na.rm=T) # 0.2499753
#hispanic
sum(wi_vf_abs_all$late_return*wi_vf_abs_all$pred.his,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.his,na.rm=T) # 0.3667753
sum(wi_vf_abs_all$not_received*wi_vf_abs_all$pred.his,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.his,na.rm=T) # 0.3629748
sum(wi_vf_abs_all$other_rej*wi_vf_abs_all$pred.his,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.his,na.rm=T) # 0.2702499
#asian
sum(wi_vf_abs_all$late_return*wi_vf_abs_all$pred.asi,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.asi,na.rm=T) # 0.4395738
sum(wi_vf_abs_all$not_received*wi_vf_abs_all$pred.asi,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.asi,na.rm=T) # 0.2756252
sum(wi_vf_abs_all$other_rej*wi_vf_abs_all$pred.asi,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.asi,na.rm=T) # 0.284801
#other
sum(wi_vf_abs_all$late_return*wi_vf_abs_all$pred.oth,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.oth,na.rm=T) # 0.4035098
sum(wi_vf_abs_all$not_received*wi_vf_abs_all$pred.oth,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.oth,na.rm=T) # 0.2928273
sum(wi_vf_abs_all$other_rej*wi_vf_abs_all$pred.oth,na.rm=T)/
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.oth,na.rm=T) # 0.3036629

sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.oth,na.rm=T)+sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.asi,na.rm=T)+
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.his,na.rm=T)+sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.bla,na.rm=T)+
  sum(wi_vf_abs_all$problem_return*wi_vf_abs_all$pred.whi,na.rm=T)
  
sum(wi_vf_abs_all$late_return,na.rm=T)/36256
sum(wi_vf_abs_all$not_received,na.rm=T)/36256
sum(wi_vf_abs_all$other_rej,na.rm=T)/36256
###let's get the results for new voters now 
(table(wi_vf_abs_all$reason1,wi_vf_abs_all$priorvoter)/(58+446+135+780+430))*100
(table(wi_vf_abs_all$reason1,wi_vf_abs_all$priorvoter)/(873+7884+1621+14085+10446))*100
#newvoters for denom: 
1- (43949/(43949+58+446+135+780+430))
1- (1064000/(1064000+873+7884+1621+14085+10446))
###
table(wi_vf_abs_all$reason1, wi_vf_abs_all$vbm_dum)
length(which(wi_vf_abs_all$April2020=="At Polls" & wi_vf_abs_all$vbm_dum==1 & wi_vf_abs_all$problem_return==1 & 
               wi_vf_abs_all$priorvoter==1))/length(which(wi_vf_abs_all$vbm_dum==1 & wi_vf_abs_all$problem_return==1 & 
                                                            wi_vf_abs_all$priorvoter==1))
length(which(wi_vf_abs_all$April2020=="At Polls" & wi_vf_abs_all$vbm_dum==1 & wi_vf_abs_all$problem_return==1 & 
               wi_vf_abs_all$priorvoter==0))/length(which(wi_vf_abs_all$vbm_dum==1 & wi_vf_abs_all$problem_return==1 & 
                                                            wi_vf_abs_all$priorvoter==0))


#not quite getting the total # 
sum(is.na(wi_vf_abs_all$pred.whi))
1140137 + 55033+49339+27356+26722 + 307401
nrow(wi_vf_abs_all)
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

###let's run the final map here for election day voting 
View(wi_county_shp2)
wi_county_shp2$ip_ed_pct2 <- (wi_county_shp2$ip_ballots/wi_county_shp2$county_total)*100
quantile(wi_county_shp2$ip_ed_pct2, seq(0,1,by=0.05))
wi_county_shp2$color_ed <- medsl_blues[1]
wi_county_shp2$color_ed[wi_county_shp2$ip_ed_pct2 >= 20 & wi_county_shp2$ip_ed_pct2 < 30 ] <- medsl_blues[2]
wi_county_shp2$color_ed[wi_county_shp2$ip_ed_pct2 >= 30 & wi_county_shp2$ip_ed_pct2 < 40 ] <- medsl_blues[3]
wi_county_shp2$color_ed[wi_county_shp2$ip_ed_pct2 >= 40 & wi_county_shp2$ip_ed_pct2 < 50 ] <- medsl_blues[4]
wi_county_shp2$color_ed[wi_county_shp2$ip_ed_pct2 >= 50 ] <- medsl_blues[5]
#election day vote % 
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_elecday_map.jpeg", res=500, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp2, log(wi_county_shp2$county_total), wi_county_shp2$color_ed, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_blues,
       legend = c("< 20%" , "20 - 30%", "30 - 40%", "40 - 50%", "50%+"), title="Election Day Vote %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
###calculating election day voting rates by race ; total_nonwhite refers to nonwhite abs ballots 
wi_county_shp2$white_elecday_pct <- (100 - wi_county_shp2$white_vbm_pct)
wi_county_shp2$nonwhite_elecday_pct <- (100 - wi_county_shp2$nonwhite_vbm_pct)
summary(wi_county_shp2$nonwhite_elecday_pct)  
####color assignment for elec day by race 
wi_county_shp2$color_ed_nonwhite <- medsl_blues[1]
wi_county_shp2$color_ed_nonwhite[wi_county_shp2$nonwhite_elecday_pct >= 20 & wi_county_shp2$nonwhite_elecday_pct < 30 ] <- medsl_blues[2]
wi_county_shp2$color_ed_nonwhite[wi_county_shp2$nonwhite_elecday_pct >= 30 & wi_county_shp2$nonwhite_elecday_pct < 40 ] <- medsl_blues[3]
wi_county_shp2$color_ed_nonwhite[wi_county_shp2$nonwhite_elecday_pct >= 40 & wi_county_shp2$nonwhite_elecday_pct < 50 ] <- medsl_blues[4]
wi_county_shp2$color_ed_nonwhite[wi_county_shp2$nonwhite_elecday_pct >= 50 ] <- medsl_blues[5]
wi_county_shp2$color_ed_white <- medsl_blues[1]
wi_county_shp2$color_ed_white[wi_county_shp2$white_elecday_pct >= 20 & wi_county_shp2$white_elecday_pct < 30 ] <- medsl_blues[2]
wi_county_shp2$color_ed_white[wi_county_shp2$white_elecday_pct >= 30 & wi_county_shp2$white_elecday_pct < 40 ] <- medsl_blues[3]
wi_county_shp2$color_ed_white[wi_county_shp2$white_elecday_pct >= 40 & wi_county_shp2$white_elecday_pct < 50 ] <- medsl_blues[4]
wi_county_shp2$color_ed_white[wi_county_shp2$white_elecday_pct >= 50 ] <- medsl_blues[5]
###should be able to map now 
setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_nonwhite_elecday_map.jpeg", res=500, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp2, log(wi_county_shp2$nonwhite_cty_pop), wi_county_shp2$color_ed_nonwhite, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_blues,
       legend = c("< 20%" , "20 - 30%", "30 - 40%", "40 - 50%", "50%+"), title="Election Day Vote %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()

setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_white_elecday_map.jpeg", res=500, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp2, log(wi_county_shp2$white_cty_pop), wi_county_shp2$color_ed_white, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_blues,
       legend = c("< 20%" , "20 - 30%", "30 - 40%", "40 - 50%", "50%+"), title="Election Day Vote %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()
wi_county_shp2$race_elecday_diff <- wi_county_shp2$white_elecday_pct - wi_county_shp2$nonwhite_elecday_pct
summary(wi_county_shp2$race_elecday_diff)
saveRDS(wi_county_shp2, "wi_county_voterfile_collapsed_shp2.Rdata")

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

table(wi_vf$April2020)
mil_vf <- subset(wi_vf, County=="Milwaukee County")
names(mil_vf)
mil_vf <- merge(mil_vf, wi_bisg, by="Voter.Reg.Number")
mil_vf <- subset(mil_vf, Voter.Status=="Active")

nrow(mil_vf)
table(mil_vf$Voter.Status, mil_vf$April2020)
mil_vf$voted <- 0
mil_vf$voted[mil_vf$April2020!=""] <- 1
mil_vf$pred.nonwhite <- 1 - mil_vf$pred.whi
sum(mil_vf$pred.whi*mil_vf$voted,na.rm=T)/sum(mil_vf$pred.whi,na.rm=T)
sum(mil_vf$pred.nonwhite*mil_vf$voted,na.rm=T)/sum(mil_vf$pred.nonwhite,na.rm=T)


sort(unique(mil_vf))
(30597 + 136577)/(30597 + 136577 + 224632)

###reading in the absentee file for 2016 
setwd("F:/voterfile/wi")
library(readxl)
wi2016abs <- read_xlsx("Absentee_file_2016.xlsx")
head(wi2016abs)
View(wi2016abs)
sort(unique(wi2016abs$`Election Name`))
wi2016abs <- subset(wi2016abs, `Election Name` =="2016 Spring Election and Presidential Preference Vote")
sort(unique(wi2016abs$`Ballot Delivery Method`))
wi2016abs$early <- 0
wi2016abs$early[wi2016abs$`Ballot Delivery Method`=="Voted In Person"] <- 1 
wi2016abs$vbm_dum <- 0
wi2016abs$vbm_dum[wi2016abs$early==0] <- 1
###let's code not returned 
wi2016abs$not_returned <- 0
length(which(wi2016abs$`Date Ballot Returned`==""))
sum(is.na(wi2016abs$`Date Ballot Returned`)) # not returned coding 
sort(unique(wi2016abs$`Date Ballot Returned`))
wi2016abs$not_returned[is.na(wi2016abs$`Date Ballot Returned`)==TRUE] <- 1
###now will do counted 
wi2016abs$counted <- 0
wi2016abs$counted[wi2016abs$`Ballot Status Reason`=="Returned"] <- 1
wi2016abs$mail_returned <- 0
wi2016abs$mail_returned[wi2016abs$vbm_dum==1 & wi2016abs$not_returned==0] <- 1 
wi2016abs$early_returned <- 0
wi2016abs$early_returned[wi2016abs$early==1 & wi2016abs$not_returned==0] <- 1
wi2016abs$mail_counted <- 0
wi2016abs$mail_counted[wi2016abs$vbm_dum==1 & wi2016abs$counted==1] <- 1
wi2016abs$early_counted <- 0
wi2016abs$early_counted[wi2016abs$early==1 & wi2016abs$counted==1] <- 1
##fixing county 
wi2016abs$county <- str_to_upper(wi2016abs$County)
wi2016abs$county <- str_remove_all(wi2016abs$county, " COUNTY")
wi2016abs$dum_prez = 1
###calculating late ballots rejected in 2016
length(which(wi2016abs$`Ballot Status Reason`=="Ballot Not Returned By Deadline" | wi2016abs$`Ballot Status Reason`=="Returned - Late" |
               wi2016abs$`Ballot Status Reason`=="Ballot Returned After Deadline" & wi2016abs$not_returned==0))
10903/(nrow(wi2016abs)- sum(wi2016abs$not_returned))

###now let's bring down the coding from 2020 
wi2016notreturned <- wi2016abs %>% group_by(county) %>% summarize(total_requests=sum(dum_prez,na.rm=T), early_returned=sum(early_returned),
                                                                 mail_returned=sum(mail_returned),mail_counted=sum(mail_counted),
                                                                 early_counted=sum(early_counted),counted=sum(counted),
                                                                 not_returned=sum(not_returned))
wi2016notreturned$returned_pct <- ((wi2016notreturned$total_requests-wi2016notreturned$not_returned)/(wi2016notreturned$total_requests))*100
summary(wi2016notreturned$returned_pct) #good 
wi2016notreturned$counted_pct <- ((wi2016notreturned$counted)/(wi2016notreturned$total_requests-wi2016notreturned$not_returned))*100
wi2016notreturned$counted_pct[wi2016notreturned$counted_pct>100] <- 100
###now let's do counted pct by mode 
wi2016notreturned$mail_counted_pct <- ((wi2016notreturned$mail_counted)/wi2016notreturned$mail_returned)*100
wi2016notreturned$mail_counted_pct[wi2016notreturned$mail_counted_pct>100] <- 100
wi2016notreturned$early_counted_pct <- ((wi2016notreturned$early_counted)/wi2016notreturned$early_returned)*100
setwd("F:/voterfile/wi")
##let's change the names 
names(wi2016notreturned)
for(i in 2:ncol(wi2016notreturned)){
  temp_name <- paste0(colnames(wi2016notreturned)[i],sep="","2016")
  colnames(wi2016notreturned)[i] <- temp_name
}
for(i in 2:ncol(wi_notreturned)){
  temp_name <- paste0(colnames(wi_notreturned)[i],sep="","2020")
  colnames(wi_notreturned)[i] <- temp_name
}

saveRDS(wi2016notreturned, "wi2016abs_counted_stats_county.Rdata")
sort(unique(wi_notreturned$county))
sort(unique(wi2016notreturned$county))
##let's merge now 
wi_notreturned_all <- merge(wi_notreturned,wi2016notreturned,by="county")
nrow(wi_notreturned_all)
#now that these are all complete, should be able to compare 
sum(wi_notreturned_all$not_returned2016)/sum(wi_notreturned_all$total_requests2016) # 234705 requested 
sum(wi_notreturned_all$not_returned2020)/sum(wi_notreturned_all$total_requests2020) # 1263100 requested 
1- 0.09906817
1- 0.05489444
sum(wi_notreturned_all$mail_counted2016)
sum(wi_notreturned_all$mail_counted2020)
###now let's save
saveRDS(wi_notreturned_all, "wi_returned_rates_all.Rdata")
write.csv(wi_notreturned_all, "wi_returned_rates_all.csv",row.names = F)

#### let's look at the rejection reasons 
table(wi2016abs$`Ballot Status Reason`)
#######################

wi2016abs$reason1 <- NA
wi2016abs$reason1[wi2016abs$`Ballot Status Reason`=="Ballot Returned After Deadline" | 
                        wi2016abs$`Ballot Status Reason`=="Ballot Not Returned By Deadline"] <-
  "Ballot Returned After Deadline"
wi2016abs$reason1[wi2016abs$`Ballot Status Reason`=="Ballot Not Received" | wi2016abs$`Ballot Status Reason`=="Not Returned"] <-
  "Ballot Not Received"
wi2016abs$reason1[wi2016abs$`Ballot Status Reason`=="Returned"] <- "Returned"
wi2016abs$reason1[wi2016abs$`Ballot Status Reason`=="Rejected at Polls/MBOC" | wi2016abs$`Ballot Status Reason`=="Returned, to be Rejected"|
                        wi2016abs$`Ballot Status Reason`=="Ineligible"] <- "Rejected"
wi2016abs$reason1[wi2016abs$`Ballot Status Reason`=="Undeliverable" | wi2016abs$`Ballot Status Reason`=="Voter Moved"] <-
  "Delivery Issue"
wi2016abs$reason1[is.na(wi2016abs$`Ballot Status Reason`)==F & is.na(wi2016abs$reason1)==T ] <- "Other"
#### reason simplified 
wi2016abs$reason2 <- NA
wi2016abs$reason2[wi2016abs$`Ballot Status Reason`=="Ballot Returned After Deadline" | 
                        wi2016abs$`Ballot Status Reason`=="Ballot Not Returned By Deadline"] <-
  "Ballot Returned After Deadline"
wi2016abs$reason2[wi2016abs$`Ballot Status Reason`=="Ballot Not Received" | wi2016abs$`Ballot Status Reason`=="Not Returned"] <-
  "Ballot Not Received"
wi2016abs$reason2[wi2016abs$`Ballot Status Reason`=="Returned"] <- "Returned"
wi2016abs$reason2[is.na(wi2016abs$`Ballot Status Reason`)==F & is.na(wi2016abs$reason2)==T ] <- "Other"
table(wi2016abs$reason1)
table(wi2016abs$reason2)
###now let's look at the reasons for 2020 
table(wi_abs_file$ballotstatusreason)
66701+2647 # 69348 returned too late 
sum(wi_notreturned_all$mail_returned2020)
69348/938411 # these are the rejected ballots for returning too late 


wi2016abs$counted <- 0
wi2016abs$counted[wi2016abs$reason2=="Returned"] <- 1
table(wi2016abs$reason1,wi2016abs$early)
wi2016abs$full_addrs <- paste0(wi2016abs$Address1, sep=" ", wi2016abs$Address2)
wi2016addrs <- subset(wi2016abs, select=c(county, full_addrs))
wi2016addrs <- wi2016addrs[!duplicated(wi2016addrs$full_addrs), ]
nrow(wi2016addrs)
str(wi2016addrs)
wi2016addrs <- as.data.frame(wi2016addrs)
write.dbf(wi2016addrs, "wi2016addrs.dbf")
##let's look at the address field and geocode for 2016 
sort(unique(wi_abs_file$ballotreasontype)) # is the field for rejection 
table(wi_abs_file$ballotreasontype)



sum(wi_notreturned_all$mail_returned2020+wi_notreturned_all$early_returned2020) # 1137967 ballots returned 


###let's get the results by county 
wi2016county_abs <- wi2016abs %>% group_by(County) %>% summarise(counted_votes = sum(counted), counted_vbm=sum(counted*vbm_dum),
                                                                 counted_early=sum(counted*early),rejected=sum(1-counted))
wi2016county_abs$CNTY_NAME <- str_to_upper(wi2016county_abs$County)
wi2016county_abs$CNTY_NAME <- str_remove_all(wi2016county_abs$CNTY_NAME, " COUNTY")
wi2016county_abs$CNTY_NAME[wi2016county_abs$CNTY_NAME=="ST. CROIX"] <- "SAINT CROIX"
View(wi2016county_abs)
###let's read in the county data 
setwd("F:/MEDSL/healthy_elections/WI")
wi_county_shp <- readRDS("wi_county_turnout_shp.Rdata")
wi_county_shp <- merge(wi_county_shp, wi2016county_abs, by="CNTY_NAME")
wi_county_shp$ed2016 <- (wi_county_shp$vote2016dem+wi_county_shp$vote2016gop) - (wi_county_shp$counted_vbm+wi_county_shp$counted_early)
View(wi_county_shp)
137328/2131109
76125/2131109
###let's do percents now 
wi_county_shp$ed_pct <- (wi_county_shp$ed2016/(wi_county_shp$vote2016dem+wi_county_shp$vote2016gop))*100
wi_county_shp$vbm_pct <- (wi_county_shp$counted_vbm/(wi_county_shp$vote2016dem+wi_county_shp$vote2016gop))*100
wi_county_shp$early_pct <- (wi_county_shp$counted_early/(wi_county_shp$vote2016dem+wi_county_shp$vote2016gop))*100
library(reldist)
gini(wi_county_shp$total_pop)
gini(wi_county_shp$vote2016dem)
gini(wi_county_shp$vote2016gop)
####mapping and assigning colors now 
medsl_blues <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")
wi_county_shp$color2016ed <- medsl_blues[1]
wi_county_shp$color2016ed[wi_county_shp$ed_pct >= 20 & wi_county_shp$ed_pct < 30] <- medsl_blues[2]
wi_county_shp$color2016ed[wi_county_shp$ed_pct >= 30 & wi_county_shp$ed_pct < 40] <- medsl_blues[3]
wi_county_shp$color2016ed[wi_county_shp$ed_pct >= 40 & wi_county_shp$ed_pct < 50] <- medsl_blues[4]
wi_county_shp$color2016ed[wi_county_shp$ed_pct >= 50] <- medsl_blues[5]

jpeg("wi_electionday2016.jpeg", res=300, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
ed_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color2016ed, weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_blues,
       legend = c("< 20%" , "20 to 30%", "30 to 40%", "40 to 50%", "50%+"), title="Elec. Day %",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()
###let's do a scatterplot here 
wi_county_shp_vf <- readRDS("wi_county_mod_Shpfile.Rdata")
#for now, let's get the results just by county, not race 
wi_county_shp_vf$county_early <- wi_county_shp_vf$white_early + wi_county_shp_vf$nonwhite_early
wi_county_shp_vf$vbm_pct <- (wi_county_shp_vf$total_absballots/wi_county_shp_vf$county_total)*100
wi_county_shp_vf <- subset(wi_county_shp_vf, select=c(CNTY_NAME,county_total,total_absballots,ip_ballots,county_early,ip_pct,
                                                      county_early_pct,vbm_pct))
wi_county_shp_vf <- wi_county_shp_vf@data
colnames(wi_county_shp_vf) <- c("CNTY_NAME","total_ballots2020","total_absballots2020","ip_ballots2020","earlyballots2020","ip_pct2020",
                                "early_pct2020","vbm_pct2020")
wi_county_shp <- merge(wi_county_shp, wi_county_shp_vf , by="CNTY_NAME")

###now let's do a Geom plot, except let's do it by county name 
library(ggplot2)
gg_ed_plot<-ggplot(wi_county_shp@data, aes(x=ed_pct, y=ip_pct2020)) +
  geom_point(size=-1) + 
  geom_text(label=(wi_county_shp$CNTY_NAME)) + xlab("2016 ED %") + ylab("2020 ED %") + theme_bw() + ylim(0,100) + xlim(80,100) +
  theme(title = element_text(size = rel(1.4), family="Styrene B"))
###lets create a new plot by points 
gg_ed_plot<-ggplot(wi_county_shp@data, aes(x=ed_pct, y=ip_pct2020)) +
  geom_point(aes(size=ip_ballots2020),color=medsl_blues[4]) + 
   xlab("2016 ED %") + ylab("2020 ED %") + theme_bw() + ylim(0,100) + xlim(0,100) +
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + guides(size=FALSE)

gg_ed_plot
ggsave("electionday_scatter.jpeg", plot = gg_ed_plot, scale = 1, 
       width = 5, height = 4, units = c("in"), dpi = 600)
cor(wi_county_shp$ip_pct2020,wi_county_shp$ed_pct)
###create new maps for vbm
medsl_purple <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")
wi_county_shp$color2020vbm <- medsl_purple[1]
wi_county_shp$color2020vbm[wi_county_shp$vbm_pct2020 > 50 & wi_county_shp$vbm_pct2020 <= 60] <- medsl_purple[2]
wi_county_shp$color2020vbm[wi_county_shp$vbm_pct2020 > 60 & wi_county_shp$vbm_pct2020 <= 70] <- medsl_purple[3]
wi_county_shp$color2020vbm[wi_county_shp$vbm_pct2020 > 70 & wi_county_shp$vbm_pct2020 <= 80] <- medsl_purple[4]
wi_county_shp$color2020vbm[wi_county_shp$vbm_pct2020 > 80 ] <- medsl_purple[5]
###create map here 
jpeg("wi_vbm2020.jpeg", res=600, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
vbm_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color2020vbm, weight_mod = 4.1, size_correct = F,
                       title = ""  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_purple,
       legend = c("< 50%" , "50 - 60%", "60 - 70%", "70 - 80%", "80%+"), title="VBM %",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()
###scatterplot 
gg_vbm_plot<-ggplot(wi_county_shp@data, aes(x=vbm_pct, y=vbm_pct2020)) +
  geom_point(aes(size=mail_returned2020),color=medsl_purple[4]) + 
  xlab("2016 VBM %") + ylab("2020 VBM %") + theme_bw() + ylim(0,100) + xlim(0,100) +
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + guides(size=FALSE)
gg_vbm_plot
ggsave("vbm_scatter.jpeg", plot = gg_vbm_plot, scale = 1, 
       width = 5, height = 4, units = c("in"), dpi = 600)
cor(wi_county_shp$vbm_pct,wi_county_shp$vbm_pct2020)


###now let's do early ballots 
quantile(wi_county_shp$early_pct,seq(0,1,by=0.05))
wi_county_shp$color2016early <- medsl_blues[1]
wi_county_shp$color2016early[wi_county_shp$early_pct > 5 & wi_county_shp$early_pct <= 10] <- medsl_blues[2]
wi_county_shp$color2016early[wi_county_shp$early_pct > 10 & wi_county_shp$early_pct <= 15] <- medsl_blues[3]
wi_county_shp$color2016early[wi_county_shp$early_pct > 15 & wi_county_shp$early_pct <= 20] <- medsl_blues[4]
wi_county_shp$color2016early[wi_county_shp$early_pct > 20] <- medsl_blues[5]
###let's plot here 
jpeg("wi2016early.jpeg", res=600, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
vbm_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color2016early, weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_blues,
       legend = c("< 5%" , "5 - 10%", "10 - 15%", "15 - 20%", "20%+"), title="Early %",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()
###scatterplot here 
gg_early_plot<-ggplot(wi_county_shp@data, aes(x=early_pct, y=early_pct2020)) +
  geom_point(aes(size=early_counted2020), color=medsl_blues[4]) + 
   xlab("2016 Early %") + ylab("2020 Early %") + theme_bw()  + xlim(0,100) + ylim(0,100) +
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + guides(size=FALSE)
gg_early_plot
ggsave("early_scatter.jpeg", plot = gg_early_plot, scale = 1, 
       width = 5, height = 4, units = c("in"), dpi = 600)
cor(wi_county_shp$early_pct,wi_county_shp$early_pct2020)
###getting rejected pcts 
wi_county_shp$rejected_pct <-  ((wi_county_shp$rejected)/(wi_county_shp$rejected+wi_county_shp$counted_votes))*100
summary(wi_county_shp$rejected_pct)
wi_vf_abs_all$inperson_dum <- 0
wi_vf_abs_all$inperson_dum[wi_vf_abs_all$April2020=="At Polls"] <- 1
wi2020data <- wi_vf_abs_all %>% group_by(County.x) %>% summarise(rejected=sum(problem_return,na.rm=T), 
                                                                 counted_vbm=sum((1-problem_return)*vbm_dum,na.rm=T),
                                                                 counted_early=sum((1-problem_return)*early2,na.rm=T),
                                                                 election_day=sum(inperson_dum,na.rm=T))
wi2020data$rejected_pct <- (wi2020data$rejected/(wi2020data$rejected+wi2020data$counted_vbm))*100
wi2020data$CNTY_NAME <- str_to_upper(wi2020data$County.x)
wi2020data$CNTY_NAME <- str_remove_all(wi2020data$CNTY_NAME, " COUNTY")
for(i in 2:6){
  temp_name <- paste0(colnames(wi2020data)[i],sep="","2020")
  colnames(wi2020data)[i] <- temp_name
}
# 23,196 said to be rejected 
sum(wi2020data$rejected2020)/(sum(wi2020data$rejected2020+wi2020data$counted_vbm2020))
wi2020data <- subset(wi2020data, select=-c(County.x))
wi2020data$CNTY_NAME[wi2020data$CNTY_NAME=="ST. CROIX"] <- "SAINT CROIX"
#exclude voter request, NAs, and not returned to get articles numbers 
saveRDS(wi2020data, "wi_rej2020.Rdata")

sort(unique(wi_notreturned_all$county))
wi_notreturned_all$county[wi_notreturned_all$county=="ST. CROIX"] <- "SAINT CROIX"



summary(wi2020data$rejected_pct)
sum(wi2020data$rejected)
View(wi2020data)
wi_county_shp <- merge(wi_county_shp, wi_notreturned_all, by.x="CNTY_NAME", by.y="county")
names(wi_county_shp)
###let's create the cartogram plots now 
quantile(wi_notreturned_all$returned_pct2020, seq(0,1,by=0.05))
#breaks by 80, 85, 90, 95

wi_county_shp$color_returned <- medsl_blues[1]
wi_county_shp$color_returned[wi_county_shp$returned_pct2020 > 80 & wi_county_shp$returned_pct2020 <= 85 ] <- medsl_blues[2]
wi_county_shp$color_returned[wi_county_shp$returned_pct2020 > 85 & wi_county_shp$returned_pct2020 <= 90 ] <- medsl_blues[3]
wi_county_shp$color_returned[wi_county_shp$returned_pct2020 > 90 & wi_county_shp$returned_pct2020 <= 95 ] <- medsl_blues[4]
wi_county_shp$color_returned[wi_county_shp$returned_pct2020 > 95 ] <- medsl_blues[5]
##now let's dow this for 2016
wi_county_shp$color2016returned <- medsl_blues[1]
wi_county_shp$color2016returned[wi_county_shp$returned_pct2016 > 80 & wi_county_shp$returned_pct2016 <= 85 ] <- medsl_blues[2]
wi_county_shp$color2016returned[wi_county_shp$returned_pct2016 > 85 & wi_county_shp$returned_pct2016 <= 90 ] <- medsl_blues[3]
wi_county_shp$color2016returned[wi_county_shp$returned_pct2016 > 90 & wi_county_shp$returned_pct2016 <= 95 ] <- medsl_blues[4]
wi_county_shp$color2016returned[wi_county_shp$returned_pct2016 > 95 ] <- medsl_blues[5]
###now let's do the carto plots 
jpeg("wi2016returned.jpeg", res=600, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
vbm_carto <- carto_plot(wi_county_shp, log(wi_county_shp$total_requests2016), wi_county_shp$color2016returned, 
                        weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_blues,
       legend = c("< 80%" , "80 - 85%", "85 - 90%", "90 - 95%", "95%+"), title="Returned %",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()
jpeg("wi2020returned.jpeg", res=600, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
vbm_carto <- carto_plot(wi_county_shp, log(wi_county_shp$total_requests2020), wi_county_shp$color_returned, 
                        weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_blues,
       legend = c("< 80%" , "80 - 85%", "85 - 90%", "90 - 95%", "95%+"), title="Returned %",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()


sum(wi_notreturned_all$mail_returned2020)/sum(wi_notreturned_all$total_requests2020)
###let's read in the remaining bisg data 
###we will want the entire wi voterfile 

wi_bisg_notcounted <- read.csv("wi2020notcounted_bisg.csv")
wi2016bisg <- read.csv("wi2016abs_bisg.csv")
###full voterfile 
wi_vf <- readRDS("wi_voterfile_cleaned.Rdata")
wi_vf$Voter.Reg.Number <- str_pad(wi_vf$Voter.Reg.Number,10,side="left",pad="0")
wi_vf$prior_voter <- NA
for(i in 25:95){
  wi_vf$prior_voter[wi_vf[,i]!="" & is.na(wi_vf$prior_voter)==T ] <- 1
}
wi_vf$prior_voter[is.na(wi_vf$prior_voter)==T] <- 0
summary(wi_vf$prior_voter) #good, now have the prior voter complete 
wi_vf <- subset(wi_vf, select=c(Voter.Reg.Number,prior_voter))
wi_bisg_notcounted$voterregnumber <- str_pad(wi_bisg_notcounted$voterregnumber,10,side="left",pad="0")
wi_bisg_notcounted <- merge(wi_bisg_notcounted, wi_vf, by.x="voterregnumber", by.y="Voter.Reg.Number",all.x=T)
sum(is.na(wi_bisg_notcounted$prior_voter))#looks like everything merged 
View(wi_bisg_notcounted)
wi_bisg$Voter.Reg.Number <- str_pad(wi_bisg$Voter.Reg.Number,width = 10,side="left",pad="0")
wi_abs_file_bisg <- merge(wi_abs_file, wi_bisg, by.y="Voter.Reg.Number", by.x="voterregnumber", all.x=T)
wi_abs_file_bisg_lefto <- subset(wi_abs_file_bisg,is.na(pred.whi)==T )
wi_abs_file_bisg_lefto <- subset(wi_abs_file_bisg_lefto, select= -c(pred.whi,pred.bla,pred.his,pred.oth,pred.asi))
wi_abs_file_bisg <- subset(wi_abs_file_bisg, is.na(pred.whi)==F)
wi_abs_file_bisg_lefto <- predict_race(wi_abs_file_bisg_lefto, surname.only=T)
wi_abs_file_bisg <- rbind(wi_abs_file_bisg, wi_abs_file_bisg_lefto)
wi_abs_file_bisg <- merge(wi_abs_file_bisg, wi_vf, by.x="voterregnumber", by.y="Voter.Reg.Number",all.x=T)
wi_abs_file_bisg <- wi_abs_file_bisg[!duplicated(wi_abs_file_bisg$voterregnumber),]
nrow(wi_abs_file_bisg)
###now can run the analysis 
sum(colSums(wi_abs_file_bisg[,55:59]))
names(wi_abs_file_bisg)
table(wi_abs_file_bisg$not_returned,wi_abs_file_bisg$prior_voter)
table(wi_abs_file_bisg$prior_voter)/nrow(wi_abs_file_bisg)
##getting sums 
38445/(38445+10045)
1089443/(1089443+109912)
sum(wi_abs_file_bisg$prior_voter)
nrow(wi_abs_file_bisg)
sum(is.na(wi_abs_file_bisg$pred.whi))
wi_abs_file_bisg <- wi_abs_file_bisg
length(unique(wi_abs_file_bisg$voterregnumber))
###analyzing by race
names(wi_abs_file)
race_tbl <- subset(wi_abs_file_bisg,not_returned==1)
race_tbl <- race_tbl[,55:59]
nrow(race_tbl)/nrow(wi_abs_file)
round(colSums(race_tbl,na.rm=T),0)
sum(round(colSums(race_tbl,na.rm=T),0))
(colSums(race_tbl,na.rm=T)/colSums(wi_abs_file_bisg[,55:59]))*100
119957/nrow(wi_abs_file_bisg)
###let's now run the results for 2016 
wi2016bisg_nc <- subset(wi2016bisg, not_returned==1)
round(colSums(wi2016bisg_nc[,60:64],na.rm=T))
sum(round(colSums(wi2016bisg_nc[,60:64],na.rm=T)))
nrow(wi2016bisg_nc)/nrow(wi2016bisg)
(colSums(wi2016bisg_nc[,60:64],na.rm=T)/colSums(wi2016bisg[,60:64],na.rm=T))*100
nrow(wi2016bisg_nc)
############ rejection rates by race 
race_tblr <- subset(wi_abs_file_bisg,counted==0 & not_returned == 0)
race_tblr <- race_tblr[,55:59]
wi_abs_file_bisg_returned <- subset(wi_abs_file_bisg, not_returned==0)
round(colSums(race_tblr,na.rm=T),0)
(colSums(race_tblr,na.rm=T)/colSums(wi_abs_file_bisg_returned[,55:59]))*100
nrow(race_tblr)/nrow(wi_abs_file_bisg_returned)
##now for 2016 
wi2016bisg_returned <- subset(wi2016bisg, not_returned == 0 )
race_tblr2016 <- subset(wi2016bisg,counted==0 & not_returned == 0)
race_tblr2016 <- race_tblr2016[,60:64]
round(colSums(race_tblr2016,na.rm=T))
(colSums(race_tblr2016,na.rm=T)/colSums(wi2016bisg_returned[,60:64],na.rm=T))*100
nrow(race_tblr2016)/nrow(wi2016bisg_returned)
###now first time voter status
wi_abs_file_bisg_returned2 <- subset(wi_abs_file_bisg, not_returned==0)
table(wi_abs_file_bisg_returned2$prior_voter,wi_abs_file_bisg_returned2$counted)
1016/38445
18982/1089443

####################
View(wi2016bisg)
###mapping the rejection pcts 
wi_county_shp$rejection2020pct <- 100 - wi_county_shp$counted_pct2020
wi_county_shp$rejection2016pct <- 100 - wi_county_shp$counted_pct2016
###let's now check and assign colors 
medsl_reds <- c("#EDD0CB","#F59181","#F6573E","#CD3C2B","#8D2115")
quantile(wi_county_shp$rejection2016pct, seq(0,1,by=0.05))
# < 10, 7.5, 5, 2.5, 0 
wi_county_shp$reject2020colors <- medsl_reds[1]
wi_county_shp$reject2020colors[wi_county_shp$rejection2020pct > 0 & wi_county_shp$rejection2020pct <= 2.5] <- medsl_reds[2]
wi_county_shp$reject2020colors[wi_county_shp$rejection2020pct > 2.5 & wi_county_shp$rejection2020pct <= 5] <- medsl_reds[3]
wi_county_shp$reject2020colors[wi_county_shp$rejection2020pct > 5 & wi_county_shp$rejection2020pct <= 7.5] <- medsl_reds[4]
wi_county_shp$reject2020colors[wi_county_shp$rejection2020pct > 7.5 ] <- medsl_reds[5]
###2016 colors 
wi_county_shp$reject2016colors <- medsl_reds[1]
wi_county_shp$reject2016colors[wi_county_shp$rejection2016pct > 0 & wi_county_shp$rejection2016pct <= 2.5] <- medsl_reds[2]
wi_county_shp$reject2016colors[wi_county_shp$rejection2016pct > 2.5 & wi_county_shp$rejection2016pct <= 5] <- medsl_reds[3]
wi_county_shp$reject2016colors[wi_county_shp$rejection2016pct > 5 & wi_county_shp$rejection2016pct <= 7.5] <- medsl_reds[4]
wi_county_shp$reject2016colors[wi_county_shp$rejection2016pct > 7.5 ] <- medsl_reds[5]
###plots now 
jpeg("wi2016reject.jpeg", res=600, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
vbm_carto <- carto_plot(wi_county_shp, log(wi_county_shp$total_requests2016), wi_county_shp$reject2016colors, 
                        weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_reds,
       legend = c("0%" , "0 - 2.5%", "2.5 - 5%", "5 - 7.5%", "7.5%+"), title="Rejection %",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()

jpeg("wi2020reject.jpeg", res=600, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
vbm_carto <- carto_plot(wi_county_shp, log(wi_county_shp$total_requests2020), wi_county_shp$reject2020colors, 
                        weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_reds,
       legend = c("0%" , "0 - 2.5%", "2.5 - 5%", "5 - 7.5%", "7.5%+"), title="Rejection %",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()

1 - sum(wi_county_shp$counted2016)/sum(wi_county_shp$total_requests2016 - wi_county_shp$not_returned2016)
1 - sum(wi_county_shp$counted2020)/sum(wi_county_shp$total_requests2020 - wi_county_shp$not_returned2020)

saveRDS(wi_county_shp, "wi_county_turnout_shp.Rdata")
####let's run the final analysis of race by mode 
setwd("F:/voterfile/wi")
wi_vf_abs_all <- readRDS("wi_vf_abs_all07222020.Rdata")
sort(unique(wi_vf_abs_all$ballotreasontype))
sort(unique(wi_vf_abs_all$ballotstatusreason))
wi_vf_abs_all$counted <- 0
wi_vf_abs_all$counted[is.na(wi_vf_abs_all$ballotstatusreason)==T] <- 1
wi_vf_abs_all$counted[wi_vf_abs_all$ballotstatusreason==""] <- 1
wi_vf_abs_all$counted[wi_vf_abs_all$ballotreasontype=="Returned"] <- 1
nrow(wi_vf_abs_all)
###second version 
wi_vf_abs_all2 <- wi_vf_abs_all[,c(1:7,28,76:80)]
nrow(wi_abs_file_bisg)
summary(nchar(wi_vf_abs_all2$Voter.Reg.Number))
wi_vf_abs_all2 <- merge(wi_vf_abs_all2,wi_abs_file_bisg,by.y="voterregnumber",by.x="Voter.Reg.Number",all.x=T )
View(wi_vf_abs_all2)
nrow(wi_vf_abs_all2) - 455786
wi_vf_abs_all2$counted[is.na(wi_vf_abs_all2$counted)==T] <- 1



sum(wi_vf_abs_all2$counted)



sum(wi_vf_abs_all$problem_return,na.rm=T)
#white
sum(wi_abs_file_bisg_returned2$mail_counted*wi_abs_file_bisg_returned2$pred.whi,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.whi.x,na.rm=T)
sum(wi_abs_file_bisg_returned2$early_counted*wi_abs_file_bisg_returned2$pred.whi,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.whi.x,na.rm=T)
100-13.08-60.04
#black
sum(wi_abs_file_bisg_returned2$mail_counted*wi_abs_file_bisg_returned2$pred.bla,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.bla.x,na.rm=T)
sum(wi_abs_file_bisg_returned2$early_counted*wi_abs_file_bisg_returned2$pred.bla,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.bla.x,na.rm=T)
100-50.25-11.65
#hispanic
sum(wi_abs_file_bisg_returned2$mail_counted*wi_abs_file_bisg_returned2$pred.his,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.his.x,na.rm=T)
sum(wi_abs_file_bisg_returned2$early_counted*wi_abs_file_bisg_returned2$pred.his,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.his.x,na.rm=T)
100-7.66-45.05
###Asian
sum(wi_abs_file_bisg_returned2$mail_counted*wi_abs_file_bisg_returned2$pred.asi,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.asi.x,na.rm=T)
sum(wi_abs_file_bisg_returned2$early_counted*wi_abs_file_bisg_returned2$pred.asi,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.asi.x,na.rm=T)
100-7.91-48.42
###other
sum(wi_abs_file_bisg_returned2$mail_counted*wi_abs_file_bisg_returned2$pred.oth,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.oth.x,na.rm=T)
sum(wi_abs_file_bisg_returned2$early_counted*wi_abs_file_bisg_returned2$pred.oth,na.rm=T)/
  sum(wi_vf_abs_all2$counted*wi_vf_abs_all2$pred.oth.x,na.rm=T)
100-10.16-53.46
length(which(wi_vf_abs_all$April2016!=""))
###we will be able to calculate the totals by using th edenominators from catalist 
round(sum(wi2016bisg_returned$mail_counted*wi2016bisg_returned$pred.whi,na.rm=T),0)
round(sum(wi2016bisg_returned$early_counted*wi2016bisg_returned$pred.whi,na.rm=T),0)
(65463/1824136)*100 # mail
(118706/1824136)*100 # early 
100-6.51-3.59
###black 2016
round(sum(wi2016bisg_returned$mail_counted*wi2016bisg_returned$pred.bla,na.rm=T),0)
round(sum(wi2016bisg_returned$early_counted*wi2016bisg_returned$pred.bla,na.rm=T),0)
(4121/112952)*100 # mail
(6408/112952)*100 # mail
100-5.67-3.65
#Hispanic 
round(sum(wi2016bisg_returned$mail_counted*wi2016bisg_returned$pred.his,na.rm=T),0)
round(sum(wi2016bisg_returned$early_counted*wi2016bisg_returned$pred.his,na.rm=T),0)
(2973/96459)*100 # mail
(5561/96459)*100 # mail
100-5.77-3.08
#Asian 
round(sum(wi2016bisg_returned$mail_counted*wi2016bisg_returned$pred.asi,na.rm=T),0)
round(sum(wi2016bisg_returned$early_counted*wi2016bisg_returned$pred.asi,na.rm=T),0)
(1762/50487)*100 # mail
(3853/50487)*100 # mail
100-7.63-3.49
##other 
round(sum(wi2016bisg_returned$mail_counted*wi2016bisg_returned$pred.oth,na.rm=T),0)
round(sum(wi2016bisg_returned$early_counted*wi2016bisg_returned$pred.oth,na.rm=T),0)
(1630/46210)*100 # mail
(2775/46210)*100 # mail
100-3.53-6.01

###let's read in the milwaukee data again 
setwd("F:/MEDSL/healthy_elections/WI")
milwaukee_sampled_df <- read.csv("master_df_mil_opened2geo_distance_df.csv")
quantile(milwaukee_sampled_df$dist_change)
sum(is.na(milwaukee_sampled_df$dist_change))
nrow(milwaukee_sampled_df)
View(milwaukee_sampled_df)


View(wi_county_shp)
names(wi_county_shp)

###let's do some scatter plots for the remaining maps 

gg_return_plot<-ggplot(wi_county_shp@data, aes(x=returned_pct2020, y=returned_pct2016)) +
  geom_point(aes(size=mail_counted2020), color=medsl_blues[4]) + 
  xlab("2016 Return %") + ylab("2020 Return %") + theme_bw()  + xlim(0,100) + ylim(0,100) +
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + guides(size=FALSE)
gg_return_plot
ggsave("return_scatter.jpeg", plot = gg_return_plot, scale = 1, 
       width = 5, height = 4, units = c("in"), dpi = 600)

###let's do rejections 

gg_reject_plot<-ggplot(wi_county_shp@data, aes(x=rejection2020pct, y=rejection2016pct)) +
  geom_point(aes(size=mail_returned2020), color=medsl_reds[4]) + 
  xlab("2016 Reject %") + ylab("2020 Reject %") + theme_bw()  + xlim(0,100) + ylim(0,100) +
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + guides(size=FALSE)
gg_reject_plot
ggsave("reject_scatter.jpeg", plot = gg_reject_plot, scale = 1, 
       width = 5, height = 4, units = c("in"), dpi = 600)

###creating sample file for voter file 
set.seed(1337)
wi_abs_file_smpl <- dplyr::sample_n(as.data.frame(wi_abs_file), size=1000, replace=FALSE)
nrow(wi_abs_file_smpl)
names(wi_abs_file_smpl)
wi_abs_file_smpl <- subset(wi_abs_file_smpl, select=c(voterregnumber,lastname,firstname,voterstatus,voterstatusreason,address1,
                                                      address2,ballotdeliverymethod,ballotstatusreason,ballotreasontype,electionname,
                                                      county))
wi_abs_file_smpl$lastname <- str_to_upper(wi_abs_file_smpl$lastname)
View(wi_abs_file_smpl)
write.csv(wi_abs_file_smpl, "wisconsin_voterfile_sample.csv",row.names = FALSE)

wisconsin_vf <- read.csv("F:/voterfile/wi/wisconsin_voterfile_sample.csv")
wisconsin_vf$full_addrs <- paste0(wisconsin_vf$address1, sep=", ", wisconsin_vf$address2)
wisconsin_vf$full_addrs <- str_to_upper(wisconsin_vf$full_addrs)
wisconsin_vf <- subset(wisconsin_vf, select=c(full_addrs, county))
wisconsin_vf <- wisconsin_vf[!duplicated(wisconsin_vf$full_addrs), ]
nrow(wisconsin_vf)
saveRDS(wi_county_shp, "wi_county_shp08072020.Rdata")
head(wisconsin_vf)
write.dbf(wisconsin_vf, "wisconsin_vf_sample.dbf")
saveRDS(wi_abs_file_bisg, "wi_abs_file_bisg08072020.Rdata")
