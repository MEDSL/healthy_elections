#########################################################################################
################## The Voter intention survey cleaner ##################################
##################################################################################
library(extrafont)
library(showtext)
font_import("F:/MEDSL/blogs/styrene_b")
windowsFonts(A = windowsFont("styrene_b"))
library(dplyr)
library(BAMMtools)
library(rgeos)
library(broom)
library(brms)
library(stringi)
library(stringr)
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(srvyr)
library(haven)
library(rstan)
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
#pkgbuild::has_build_tools(debug = TRUE)
#dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat(if( grepl("^darwin", R.version$os)) "\nCXX14FLAGS=-O3 -march=native -mtune=native -arch x86_64 -ftemplate-depth-256" else 
  if (.Platform$OS.type == "windows") "\nCXX14FLAGS=-O3 -mtune=native -mmmx -msse -msse2 -msse3 -mssse3 -msse4.1 -msse4.2" else
    "CXX14FLAGS = -fPIC",
  file = M, sep = "\n", append = TRUE)

###################################################
##function to convert long to wide, multiple values 
myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
####theme files
medsl_brands <- c("#3791FF","#59CBF5","#C0BA79","#F6573E","#156DD0","#C72654","#FF6878")

theme_medsl2gen <- theme(title = element_text(size = rel(1.2), family="Styrene B"),
                          plot.caption = element_text(hjust=0),panel.grid.minor = element_blank())
###there are 14 breaks on the x axis. we will want to make it so that they are the same. 
medsl_purple <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")
###create auto caption 
caption_date2 <- paste0("Data source: MIT Voter Intention Survey \nGraph Source: MIT Elections Data and Science Lab\nGraph date:",
                       sep=" ", format(Sys.Date(),format="%m/%d/%Y"))




###############3
setwd("C:/Users/johna/OneDrive/Documents/GitHub/healthy_elections/voteintention")
list.files()
vi_survey <- read.csv("fallsurvey1.CSV")
names(vi_survey)
####we will probably want a simple design by age, race, and party. We can do this with the vf without too much 
#issue 
vi_survey$vbm_use <- 0
vi_survey$vbm_use[vi_survey$q4==3] <- 1
vi_survey$age <- 2020 - vi_survey$birthyr
vi_survey$age2 <- vi_survey$age^2
vi_survey$democrat <- 0
vi_survey$democrat[vi_survey$pid7 < 4] <- 1
vi_survey$gop <- 0
vi_survey$gop[vi_survey$pid7 > 4] <- 1
vi_survey$other <- 0
vi_survey$other[vi_survey$pid7==4] <- 1
vi_survey$state_fip <- str_pad(vi_survey$inputstate,width=2,pad="0",side="left")
###new race var 
vi_survey$race3 <- 1
vi_survey$race3[vi_survey$race==2] <- 2
vi_survey$race3[vi_survey$race >= 3] <- 3
###this command figures out what the names and class of the parameters will be 
get_prior(vbm_use ~ (1|gender) + (1|race3) + age + democrat + gop + other+  (1|state_fip), data=vi_survey,
          family=bernoulli)
bayes_vbm_all <- brm(vbm_use ~ (1|gender) + (1|race3) + age + democrat + gop + other+ (1|state_fip), data=vi_survey,
                     family=bernoulli,
                     prior=c(set_prior("normal(0,0.2)", class="b"),
                             set_prior("normal(0,0.2)", class="sd",group="gender"),
                             set_prior("normal(0,0.2)", class="sd",group="race3"),
                             set_prior("normal(0,0.2)", class="b", coef="age"),
                             set_prior("normal(-0.2,0.2)", class="b", coef = "democrat"),
                             set_prior("normal(-0.4,0.2)", class="b", coef="other"),
                             set_prior("normal(-0.9,0.2)", class="b", coef="gop"),
                             set_prior("normal(0,0.2)", class="sd",group="state_fip")))  
summary(bayes_vbm_all)
library(tidybayes)
bayes_vbm_all %>% 
  gather_samples(`sd_..*`, regex=TRUE) %>%
  ungroup() %>%
  mutate(group=stringr::str_replace_all(term, c("sd_" = "", "__Intercept"=""))) %>%
  ggplot(aes(y=group, x=estimate)) + 
  ggridges::geom_density_ridges(aes(height=..density..),
                                rel_min_height=0.01, stat="density",
                                scale=1.5)
###saving model 
saveRDS(bayes_vbm_all, "bayes_vbm_allV3.rds")
bayes_vbm_all <- readRDS("bayes_vbm_allV2.rds")
###possible to do this by coef? 

#####we will now want to read in the NC voterfile, and get the necessary info. 
#noteL looks like the votefile being read in can crash R. We will need to convert it to a csv most likely. This 
#can take time. Will practice with a smaller version for now. 
library(data.table)
###let's try to read in chunks of 1 million

nc_vf1 <- 
  fread("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/NC/ncvoter_Statewide10032020.txt",
        nrows=2000000,data.table = FALSE)
nc_vf2 <- 
  fread("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/NC/ncvoter_Statewide10032020.txt",
        nrows=2000000,data.table = FALSE,skip=2000000)
nc_vf3 <- 
  fread("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/NC/ncvoter_Statewide10032020.txt",
        nrows=2000000,data.table = FALSE,skip=4000000)
nc_vf4 <- 
  fread("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/NC/ncvoter_Statewide10032020.txt",
        nrows=3000000,data.table = FALSE,skip=6000000)
colnames(nc_vf2) <- colnames(nc_vf1)
colnames(nc_vf3) <- colnames(nc_vf1)
colnames(nc_vf4) <- colnames(nc_vf1)
#merge together now? 
nc_vf_all <- rbind(nc_vf1,nc_vf2,nc_vf3,nc_vf4)
nc_vf_all$race3 <- 1
nc_vf_all$race3[nc_vf_all$race_code=="B"] <- 2
nc_vf_all$race3[nc_vf_all$race_code!="W" & nc_vf_all$race_code!="B"] <- 3
table(nc_vf_all$race3)
sort(unique(nc_vf_all$party_cd))
nc_vf_all$democrat <- 0
nc_vf_all$democrat[nc_vf_all$party_cd=="DEM"] <- 1
nc_vf_all$gop <- 0
nc_vf_all$gop[nc_vf_all$party_cd=="REP"] <- 1
nc_vf_all$other <- 0
nc_vf_all$other[nc_vf_all$gop==0 & nc_vf_all$democrat==0] <- 1


###now age 
colnames(nc_vf_all)[colnames(nc_vf_all)=="birth_age"] <- "age"
##drop prior data 
rm(nc_vf1,nc_vf2,nc_vf3,nc_vf4)
nc_vf_all$gender <- 1
nc_vf_all$gender[nc_vf_all$gender_code=="F" | nc_vf_all$gender=="U"] <- 2
nc_vf_all <- subset(nc_vf_all, age <= 100)
nc_vf_all <- subset(nc_vf_all, voter_status_desc != "DENIED" & voter_status_desc != "REMOVED")
#Let's see if we can't get tallies of these then; for now, let's drop gender 
nc_vf_sum <- nc_vf_all %>% group_by(gender,race3,age,democrat,gop,other) %>% tally()
summary(nc_vf_sum$race3)
summary(nc_vf_sum$age)
nc_vf_sum$state_fip="37"
####will now go with expand grid in order to create the dummy data 
#nc_expanded_df <- expand.grid(gender=1:2,race3=1:3,age=18:100,democrat=1,gop=1,state_fip="37")
set.seed(3461)
pred_obj_nc<- predict(bayes_vbm_all, newdata=nc_vf_sum, allow_new_levels=TRUE, 
            nsamples=1000, summary=FALSE)
pred_obj_ncmci <- apply(pred_obj_nc, 2, mean_se)#good, this gives us the proportions 
pred_obj_ncmci <- data.frame(matrix(unlist(pred_obj_ncmci), nrow=length(pred_obj_ncmci), byrow=T))
####good, now we can get to the rest 
pred_obj_ncmci <- as.data.frame(pred_obj_ncmci)
colnames(pred_obj_ncmci) <- c("mean","low_se","up_se")
pred_obj_ncmci$std <- pred_obj_ncmci$mean - pred_obj_ncmci$low_se
pred_obj_ncmci$low95ci <- pred_obj_ncmci$mean -(1.96*pred_obj_ncmci$std)
pred_obj_ncmci$up95ci <- pred_obj_ncmci$mean + (1.96*pred_obj_ncmci$std)
####binding the data 
nc_vf_sum <- cbind(nc_vf_sum,pred_obj_ncmci)
nc_vf_sum$vbm_count_mean <- nc_vf_sum$n*nc_vf_sum$mean
nc_vf_sum$vbm_count95low <- nc_vf_sum$n*nc_vf_sum$low95ci 
nc_vf_sum$vbm_count95up <- nc_vf_sum$n*nc_vf_sum$up95ci
###summing by age 
nc_vf_sum2 <- nc_vf_sum %>% group_by(race3,democrat,gop,other) %>% 
  summarise(total=sum(n),vbm_count_mean=sum(vbm_count_mean),vbm_count95low=sum(vbm_count95low),
            vbm_count95up=sum(vbm_count95up))
View(nc_vf_sum2)
###now saving 
saveRDS(nc_vf_sum2, "nc_state_wide_mrp_ests.rds")

####let's get the results by county now 
county_vec <- sort(unique(nc_vf_all$county_desc))
nc_vf_sum_county <- nc_vf_all %>% group_by(gender,race3,age,democrat,gop,other,county_desc ) %>% tally()

master_county_results <- data.frame()
for (i in 1:length(county_vec)) {
  nc_vf_sum_sub <- subset(nc_vf_sum_county, county_desc==county_vec[i])
  temp_pred_obj_nc<- predict(bayes_vbm_all, newdata=nc_vf_sum_sub, allow_new_levels=TRUE, 
                        nsamples=1000, summary=FALSE)
  temp_pred_obj_nc <- apply(temp_pred_obj_nc, 2, mean_se)
  ###########
  temp_pred_obj_nc <- data.frame(matrix(unlist(temp_pred_obj_nc), nrow=length(temp_pred_obj_nc), byrow=T))
  colnames(temp_pred_obj_nc) <- c("mean","low_se","up_se")
  temp_pred_obj_nc$std <- temp_pred_obj_nc$mean - temp_pred_obj_nc$low_se
  temp_pred_obj_nc$low95ci <- temp_pred_obj_nc$mean -(1.96*temp_pred_obj_nc$std)
  temp_pred_obj_nc$up95ci <- temp_pred_obj_nc$mean + (1.96*temp_pred_obj_nc$std)
  ###########
  nc_vf_sum_sub <- cbind(nc_vf_sum_sub,temp_pred_obj_nc)
  #####creating vars of intreest 
  nc_vf_sum_sub$vbm_count_mean <- nc_vf_sum_sub$n*nc_vf_sum_sub$mean
  nc_vf_sum_sub$vbm_count95low <- nc_vf_sum_sub$n*nc_vf_sum_sub$low95ci 
  nc_vf_sum_sub$vbm_count95up <- nc_vf_sum_sub$n*nc_vf_sum_sub$up95ci
  #########################
  nc_vf_sum_sub2 <- nc_vf_sum_sub %>% group_by(race3,democrat,gop,other,county_desc) %>% 
    summarise(total=sum(n),vbm_count_mean=sum(vbm_count_mean),vbm_count95low=sum(vbm_count95low),
              vbm_count95up=sum(vbm_count95up))
  nc_vf_sum_sub2 <- as.data.frame(nc_vf_sum_sub2)
  if(nrow(master_county_results)==0){
    master_county_results <- nc_vf_sum_sub2
  }else{
    master_county_results <- rbind(master_county_results,nc_vf_sum_sub2)
  }

}
saveRDS(master_county_results, "mrp_nc_county_resultsV1.rds")

#####let's try a predicted_draw 
nc_state_draws <- bayes_vbm_all %>%
  add_predicted_draws(newdata=nc_vf_sum, allow_new_levels=TRUE, n=1)
View(nc_state_draws)
###now let's create the range of estimates 
sum(nc_vf_sum$n)
sum(nc_vf_sum$vbm_count)*.68
###we now have the MRP output, so let's get the plot by 





sum(nc_vf_sum$vbm_count)*.68
bayes_vbm_all %>%
  add_predicted_samples(newdata=nc_vf_sum, allow_new_levels=TRUE, n=500)


###this is the how to intention vote that charles tweeted 
master_results_survey <- data.frame(stringsAsFactors = FALSE)


vi_survey <- subset(vi_survey, select=-c(q3_other,starttime,endtime))
vi_survey_long <- vi_survey %>% gather(key=question, value=response, q1:q12)
vi_survey_long$response2 <- as.factor(vi_survey_long$response)

###let's try to create the loop now 
q_vec <- sort(unique(vi_survey_long$question))
for(i in 1:34){
  temp_data <- vi_survey_long[vi_survey_long$question==q_vec[i],]
  temp_data <- subset(temp_data, is.na(response2)==F)
  temp_data$dum=0
  temp_data$dum[is.na(temp_data$response)==F] <- 1
  num_responsdents <- temp_data %>% group_by(state_fip) %>% summarise(total_n=sum(dum))
  num_levels <- length(unique(temp_data$response2))
  print(num_levels)
  tryCatch({ 
    temp_summs <- temp_data %>% as_survey(weights=weight) %>%
      group_by(state_fip,question,response2) %>% summarise(pct=survey_mean(vartype="ci",na.rm=T))
    temp_summs$question <- q_vec[i]
    temp_summs <- merge(temp_summs, num_responsdents, by="state_fip")
    if(nrow(master_results_survey)==0){
      master_results_survey <- temp_summs
    }else{
      master_results_survey <- rbind(master_results_survey, temp_summs)
    } },error = function(e) e)
}
for(i in 4:6){
  master_results_survey[,i] <- (master_results_survey[,i])*100
  master_results_survey[,i] <- round(master_results_survey[,i], 2)
}
saveRDS(master_results_survey, "results_summed_by_state2.rds")
write.csv(master_results_survey,"results_summed_by_state.csv",row.names = F )
###for some reason, question 2 a is not part of this. I'll have to check and figure out what happened 



#### we now want to create stacked bar plots 
question_list <- list("q1"=c("Already voted","Yes","Probably","No","Undecided"),"q2"=c("In person", "By mail"),
                      "q2a"=c("Dropbox/official loc.","Mailed"), "q4"=c("In person, election day","In person, early", "By mail", "Don't know"),
                      "q3"=c("Worried about catching COVID \nif voted in person","Wanted to get voting out of the way",
                             "Contacted by campaign and encouraged to vote early",
                             "sick or disabled, and voting in person \nwould be difficult/dangerous",
                             "Plan to be out of town on election day","Wanted to vote when it was convenient","Other"),
                      "q4a"=c("Dropbox/official loc.","Mailed"),"q5"=c("Very likely","Somewhat likely","Somewhat unlikely","Very unlikely"),
                      "q6a"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6b"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6c"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6d"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6e"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6f"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6g"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6h"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q6i"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7a"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7b"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7c"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7d"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7e"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7f"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7g"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7h"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q7i"=c("Very worried","Somewhat worried","A little worried","Not worried at all"),
                      "q8a"=c("Extremely likely","Somewhat likely","Somewhat unlikely","Extremely unlikely"),
                      "q8b"=c("Extremely likely","Somewhat likely","Somewhat unlikely","Extremely unlikely"),
                      "q8c"=c("Extremely likely","Somewhat likely","Somewhat unlikely","Extremely unlikely"),
                      "q9a"=c("Extremely likely","Somewhat likely","Somewhat unlikely","Extremely unlikely"),
                      "q9b"=c("Extremely likely","Somewhat likely","Somewhat unlikely","Extremely unlikely"),
                      "q9c"=c("Extremely likely","Somewhat likely","Somewhat unlikely","Extremely unlikely"),
                      "q10"=c("Very confident","Somewhat confident","Not too confident","Not at all confident","Don't know"),
                      "q11"=c("Yes","No"),"q12"=c("In person, election day","In person, early", "By mail"))
###this appears to work 
question_list[q_vec[1]]
q_responses <- as.vector(unlist(question_list[q_vec[1]]))
class(q_responses)

###list for q names 
q_name_list <- list("q1"=c("Q1: Intention to vote in the November general election"),"q2"=c('Q2: Early voting mode'),
                    "q2a"=c("Q2a: Ballot return method"),"q4"=c("Q4: November vote mode intention"),
                    "q3"=c('Reason for voting early'),
                    "q4a"=c('Preference to return ballot'),"q5"=c('Likelihood of voting in person'),
                    "q6a"=c('Worries of in person voting: \nTraveling to and from the polling place'),
                    "q6b"=c('Worries of in person voting: \nPlacing ballot in ballot box'),
                    "q6c"=c('Worries of in person voting: \nHanding ID to poll worker'),
                    "q6d"=c("Worries of in person voting: \nBeing near poll workers"),
                    "q6e"=c('Worries of in person voting: \nTouching door handles when entering/exiting polling place'),
                    "q6f"=c('Worries of in person voting: \nBeing near other voters while completing ballot'),
                    "q6g"=c('Worries of in person voting: \nTouching voting machine/pen to mark ballot'),
                    "q6h"=c('Worries of in person voting: \nBeing near other voters'),
                    "q7a"=c('Worries of mail voting: \nPostage to mail back ballot'),
                    "q7b"=c('Worries of mail voting: \nHow to vote if later prefer to vote on Election Day'),
                    "q7c"=c('Worries of mail voting: \nHow to vote if lose/spoil ballot'),
                    "q7d"=c('Worries of mail voting: \nSomeone will request and steal my ballot'),
                    "q7e"=c('Worries of mail voting: \nBallot received from state late'),
                    "q7f"=c("Worries of mail voting: \nSomeone will intercept and change ballot"),
                    "q7g"=c('Worries of mail voting: \nBallot will not be received and processed'),
                    "q7h"=c('Worries of mail voting: \nLate arrival of ballot to be counted'),
                    "q7i"=c('Worries of mail voting: \nElection officials will not count ballot once returned'),
                    "q8a"=c('How likely ballot will be counted if: \nVote by mail/absentee'),
                    "q8b"=c('How likely ballot will be counted if: \nVote in person on Election Day'),
                    "q8c"=c('How likely ballot will be counted if: \nVote in person early'),
                    "q9a"=c("How likely ballot will be secret if: \nVote by mail/absentee"),
                    "q9b"=c("How likely ballot will be secret if: \nVote in person on Election Day"),
                    "q9c"=c('How likely ballot will be secret if: \nVote in person early'),
                    "q10"=c('Confidence vote will be counted as intended'),
                    "q11"=c("Voted in previous primary"),
                    "q12"=c('Primary voting method'))

state_codes <- read.csv("merge_on_statecodes.csv")
state_codes$state_fips <- str_pad(state_codes$state_fips,width=2,pad="0",side="left")
state_codes <- subset(state_codes, select=c(state,state_po,state_fips))
colnames(state_codes)[colnames(state_codes)=="state_fips"] <- "state_fip"
master_results_survey <- merge(master_results_survey, state_codes, by="state_fip",all.y=FALSE)
###good, now we can 


temp_sub <- subset(master_results_survey, question==q_vec[i])
temp_sub$response_fac <- factor(temp_sub$response,labels = as.vector(unlist(question_list[q_vec[1]])))

total_n_vals <- subset(temp_sub, response==1)
total_n_vals <- total_n_vals$total_n

###new version with change x marks 
temp_survey_plot <- ggplot(temp_sub, aes(fill=response_fac, y=pct, x=state_po)) + 
  geom_bar(position="stack", stat="identity") + labs(title=as.vector(unlist(q_name_list[[q_vec[1]]])),x="State",y="%",fill="",
                                                     caption = caption_date2) +
  scale_fill_manual(values = medsl_brands[1:length(unique(temp_sub$response))],drop=F)  + theme_minimal() + 
  scale_x_discrete(breaks=c( "AZ",'FL','MI',"NC","OH","PA","WI"),labels=c(paste0("AZ",sep="","\nN=",sep="",total_n_vals[1]),
                                                                          paste0("FL",sep="","\nN=",sep="",total_n_vals[2]),
                                                                          paste0("MI",sep="","\nN=",sep="",total_n_vals[3]),
                                                                          paste0("NC",sep="","\nN=",sep="",total_n_vals[4]),
                                                                          paste0("OH",sep="","\nN=",sep="",total_n_vals[5]),
                                                                          paste0("PA",sep="","\nN=",sep="",total_n_vals[6]),
                                                                          paste0("WI",sep="","\nN=",sep="",total_n_vals[7])) )
temp_survey_plot <- temp_survey_plot + theme_medsl2gen 
temp_survey_plot

temp_survey_plot




###good, now will create the loop and such 
setwd("F:/MEDSL/healthy_elections/voteintention/plots")
for(i in 1:length(q_vec)){
  tryCatch({
    
  ##subsetting the data 
    temp_sub <- subset(master_results_survey, question==q_vec[i])
    temp_sub$response_fac <- factor(temp_sub$response,labels = as.vector(unlist(question_list[q_vec[i]])))
  ###getting a vec of the N's by state 
    total_n_vals <- subset(temp_sub, response==1)
    total_n_vals <- total_n_vals$total_n
  
  ###new version with change x marks 
    temp_survey_plot <- ggplot(temp_sub, aes(fill=response_fac, y=pct, x=state_po)) + 
      geom_bar(position="stack", stat="identity") + labs(title=as.vector(unlist(q_name_list[[q_vec[i]]])),x="State",y="%",fill="",
                                                         caption = caption_date2) +
      scale_fill_manual(values = medsl_brands[1:length(unique(temp_sub$response))],drop=F)  + theme_minimal() + 
      scale_x_discrete(breaks=c( "AZ",'FL','MI',"NC","OH","PA","WI"),labels=c(paste0("AZ",sep="","\nN=",sep="",total_n_vals[1]),
                                                                              paste0("FL",sep="","\nN=",sep="",total_n_vals[2]),
                                                                              paste0("MI",sep="","\nN=",sep="",total_n_vals[3]),
                                                                              paste0("NC",sep="","\nN=",sep="",total_n_vals[4]),
                                                                              paste0("OH",sep="","\nN=",sep="",total_n_vals[5]),
                                                                              paste0("PA",sep="","\nN=",sep="",total_n_vals[6]),
                                                                              paste0("WI",sep="","\nN=",sep="",total_n_vals[7])) ) 
  
    temp_survey_plot <- temp_survey_plot + theme_medsl2gen 
    temp_survey_plot
    plot_name <- paste0(q_vec[i],sep="_","stacked_barplot.png")
    ggsave(plot_name, plot = temp_survey_plot, scale = 1,
           width = 9, height = 6, units = c("in"), dpi = 600)
    }, error = function(e) e)
}




##############################################




temp_summs$q_response <- paste0(temp_summs$question, sep=" ", temp_summs$response)
###wide test 
test_wide <- spread(temp_summs, q_response,pct:total_n)
test_wide <- myspread(temp_summs, response, c(pct,pct_low,pct_upp))
##close, but we will need to get rid of the nas 
test_wide2 <- test_wide %>% group_by(state_fip) %>% summarise_all(mean(na.rm=T))

##this seems to have gotten rid of the na vals, early there 
test_wide2 <- test_wide %>% group_by(state_fip) %>% summarise(across(`1_pct`:`4_pct_upp`, ~ mean(.x, na.rm = TRUE)))

for(i in 2:ncol(test_wide2)){
  test_wide2[,i] <- (test_wide2[,i])*100
  test_wide2[,i] <- round(test_wide2[,i],2)
}


###now we want to see if we can export via stargazer 
test_gather <- gather(test_wide2, key="estimate", value="type" ,`1_pct`:`1_pct_upp`)
test_gather <- gather(test_wide2, key="estimate", value="value" ,2:4)
## we might then be able to follow up with a group by mutate 

View(test_gather)
library(stargazer)

?stargazer
library(xtable)
?xtable
View(master_results_survey)
###we might be able to do a gather command 

###seeing if we cant do this quicker 

summarise(svy, col.grad = survey_mean(col.grad),
          api00 = survey_mean(api00, vartype = "ci"))




vi_survey_q4 <- vi_survey %>% as_survey(weights=weight) %>%
  group_by(state_fip,q4) %>%
  summarise(pct=survey_mean(na.rm=T, vartype="ci"))
View(temp_summs)

vi_survey_long_sum <- vi_survey_long %>% as_survey(weights=weight) %>%
  group_by(question,level,state_fip) %>% summarise(pct=survey_mean(vartype="ci",na.rm=T))

View(vi_survey_long)
df %>% gather(key = factor, value = level, -value) %>%
  group_by(factor, level) %>%
  summarize(mean = mean(value))


get(vi_survey, 1)

vi_survey_q4 <- vi_survey %>% as_survey(weights=weight) %>% group_by(state_fip,q4) %>% summarise(n=survey_total())



View(vi_survey_q4)



dstrata %>%
  group_by(awards) %>%
  summarise(api00 = survey_mean(api00))