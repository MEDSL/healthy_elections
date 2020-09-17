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

###############3
setwd("F:/MEDSL/healthy_elections/voteintention")
list.files()
vi_survey <- read.csv("fallsurvey1.CSV")
View(vi_survey)
vi_survey$state_fip <- str_pad(vi_survey$inputstate,width=2,pad="0",side="left")
vi_survey_q1 <- vi_survey %>% as_survey(weights=weight) %>% group_by(state_fip,q1) %>% summarise(n=survey_total())
for(i in 3:5){
  vi_survey_q1[i] <- round(vi_survey_q1[i],2)
}
vi_survey_q1b <- vi_survey %>% as_survey(weights=weight) %>% group_by(state_fip,q1) %>% summarise(n=survey_total())
vi_survey_q1b <- vi_survey_q1b %>% group_by(state_fip) %>% mutate(total_n = sum(n))


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
    group_by(state_fip,question,response) %>% summarise(pct=survey_mean(vartype="ci",na.rm=T))
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
saveRDS(master_results_survey, "results_summed_by_state.rds")
write.csv(master_results_survey,"results_summed_by_state.csv",row.names = F )

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