#####################################################################################################
######################### WI Milwaukee Distance and Participation Analysis #########################################
##################################################################################################

#reading in data 
setwd("F:/MEDSL/healthy_elections/WI")
master_df_mil_opened2 <- readRDS("milwaukee_analysis_df07092020.Rdata")

master_df_mil_opened2$voted2016prim <- 0
master_df_mil_opened2$voted2016prim[master_df_mil_opened2$April2016!=""] <- 1
master_df_mil_opened2$voted2018ge <- 0
master_df_mil_opened2$voted2018ge[master_df_mil_opened2$November2018!=""] <- 1
###ok, lets merge on the bisg results 
wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
wi_bisg <- readRDS("wi_bisg_results.Rdata")
names(wi_bisg)
wi_bisg2 <- subset(wi_bisg, select = c(Voter.Reg.Number,pred.whi))
master_df_mil_opened2 <-merge(master_df_mil_opened2, wi_bisg2,by="Voter.Reg.Number")
nrow(master_df_mil_opened2)###good 
master_df_mil_opened2$voted2020all <- 0
master_df_mil_opened2$voted2020all[master_df_mil_opened2$April2020!=""] <- 1
master_df_mil_opened2$voted2020abs <- 0
master_df_mil_opened2$voted2020abs[master_df_mil_opened2$April2020=="Absentee"] <- 1
master_df_mil_opened2$voted2020ip <- 0
master_df_mil_opened2$voted2020ip[master_df_mil_opened2$April2020=="At Polls"] <- 1
###ok, good. Now we should be able to run the analysis, though we will first most likely want to create a spatial df 
master_df_mil_opened2_coords1 <- subset(master_df_mil_opened2, select=c(X,Y))
master_df_mil_opened2_spdf <- SpatialPointsDataFrame(coords = master_df_mil_opened2_coords1, data = master_df_mil_opened2,
                                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
####now will run the analysis of the results that Charles would like. 
names(master_df_mil_opened2_spdf)
table(master_df_mil_opened2_spdf$Voter.Status)
sum(master_df_mil_opened2$pred.whi*master_df_mil_opened2$voted2020all,na.rm=T)/sum(master_df_mil_opened2$pred.whi,na.rm = T)
length(which(master_df_mil_opened2$dist_change==0))/nrow(master_df_mil_opened2) # % who did not have polling place close 
length(which(master_df_mil_opened2$dist_change==0 & master_df_mil_opened2$voted2020all==1))/
  length(which(master_df_mil_opened2$dist_change==0)) # 0.4974304 
length(which(master_df_mil_opened2$dist_change>0 & master_df_mil_opened2$voted2020all==1))/
  length(which(master_df_mil_opened2$dist_change>0)) # 0.3664594 
##now let's find out proportions of voting by mail 
length(which(master_df_mil_opened2$dist_change==0 & master_df_mil_opened2$April2020=="Absentee"))/
  length(which(master_df_mil_opened2$dist_change==0 & master_df_mil_opened2$voted2020all==1)) # 0.4974304 
length(which(master_df_mil_opened2$dist_change>0 & master_df_mil_opened2$April2020=="Absentee"))/
  length(which(master_df_mil_opened2$dist_change>0 & master_df_mil_opened2$voted2020all==1)) # 0.4974304 
nrow(master_df_mil_opened2_spdf)
quantile(master_df_mil_opened2$dist_change,seq(0,1,by=.05))
master_df_mil_opened2$rank_distchange <- 0
master_df_mil_opened2$rank_distchange[master_df_mil_opened2$dist_change>0 & master_df_mil_opened2$dist_change <= 1.9] <- 1
master_df_mil_opened2$rank_distchange[master_df_mil_opened2$dist_change>1.9 & master_df_mil_opened2$dist_change <= 7.74] <- 2
master_df_mil_opened2$rank_distchange[master_df_mil_opened2$dist_change>7.74] <- 3
###now let's do a table for participation via collapse 
mil_partic_race <- master_df_mil_opened2 %>% group_by(voted2020all,rank_distchange) %>% summarise(white_sum=sum(pred.whi,na.rm=T),
                                                                                                  nonwhite_sum=sum(1-pred.whi,na.rm=T))
mil_partic_race <- mil_partic_race %>% group_by(rank_distchange) %>% mutate(white_dist_n=sum(white_sum,na.rm=T), 
                                                                            nonwhite_dist_n=sum(nonwhite_sum,na.rm=T))
mil_partic_race$percent_white <- (mil_partic_race$white_sum/mil_partic_race$white_dist_n)*100
mil_partic_race$percent_nonwhite <- (mil_partic_race$nonwhite_sum/mil_partic_race$nonwhite_dist_n)*100
###let's do the above now for the vote modes 
mil_partic_race_mode <- master_df_mil_opened2 %>% 
  group_by(voted2020all,rank_distchange) %>% 
  summarise(white_ed_sum=sum(pred.whi*voted2020ip,na.rm=T),nonwhite_ed_sum=sum((1-pred.whi)*voted2020ip,na.rm=T),
            white_vbm_sum=sum(pred.whi*voted2020abs,na.rm=T),nonwhite_vbm_sum=sum((1-pred.whi)*voted2020abs,na.rm=T))
mil_partic_race <- merge(mil_partic_race, mil_partic_race_mode, by=c("voted2020all","rank_distchange"))
mil_partic_race$white_ed_pct <- (mil_partic_race$white_ed_sum/mil_partic_race$white_dist_n)*100
mil_partic_race$nonwhite_ed_pct <- (mil_partic_race$nonwhite_ed_sum/mil_partic_race$nonwhite_dist_n)*100
mil_partic_race$white_vbm_pct <- (mil_partic_race$white_vbm_sum/mil_partic_race$white_dist_n)*100
mil_partic_race$nonwhite_vbm_pct <- (mil_partic_race$nonwhite_vbm_sum/mil_partic_race$nonwhite_dist_n)*100


View(mil_partic_race)
1-0.8135016
1-0.8126434
###test with a normal logit 
master_df_mil_opened2$dum=1
master_df_mil_opened2 <- master_df_mil_opened2 %>% group_by(PollingPlaceAddress) %>% mutate(poll_n=sum(dum,na.rm=T))
master_df_mil_opened2 <- subset(master_df_mil_opened2, poll_n >= 30)
library(lme4)
test_nonspace_logit <- glmer(voted2020all ~ pred.whi + closed + dist_change  + voted2016prim + voted2018ge + 
                               (1|PollingPlaceAddress),
                             data=master_df_mil_opened2, family = binomial(link = "logit") )
summary(test_nonspace_logit)
test_nonspace_logit_fe <- glm(voted2020all ~ pred.whi + closed + dist_change  + voted2016prim + voted2018ge + 
                                as.factor(PollingPlaceAddress),
                              data=master_df_mil_opened2, family = binomial(link = "logit") )
summary(test_nonspace_logit_fe)
