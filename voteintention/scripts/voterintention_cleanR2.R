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
library(stringi)
library(stringr)
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(srvyr)
library(haven)
library(modelsummary)
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
setwd("F:/MEDSL/healthy_elections/voteintention")
list.files()
vi_survey <- read.csv("fallsurvey1.CSV")
View(vi_survey)
vi_survey$state_fip <- str_pad(vi_survey$inputstate,width=2,pad="0",side="left")


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
master_results_survey <- readRDS("results_summed_by_state2.rds")

####example from modelsummary 
data(trees)
models <- list()
models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)

# simple table
msummary(models)

# confidence intervals, p values, or t-stats instead of standard errors
msummary(models, statistic = 'conf.int', conf_level = 0.99)
msummary(models, statistic = 'p.value', conf_level = 0.99)
msummary(models, statistic = 'statistic', conf_level = 0.99)

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