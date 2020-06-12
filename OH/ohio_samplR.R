###################Ohio Sampler ######################
library(wru)
library(tidyverse)
library(vroom)
library(dplyr)
setwd("F:/MEDSL/healthy_elections/OH/Historical Voter File")
oh_files <- list.files()
#####Let's run a loop here
master_df <- data.frame()
for(i in 1:length(oh_files)){
  oh_temp <- read.delim(oh_files[i],sep=",")
  colnames(oh_temp)[colnames(oh_temp)=="LAST_NAME"] <- "surname"
  #oh_temp <- subset(oh_temp, select=c()) #might just create new file here
  oh_temp <- predict_race(oh_temp, census.surname = T, surname.only = T)
  if(nrow(master_df)==0){
    master_df <- oh_temp
  }else{
    master_df <- rbind(master_df,oh_temp)
  }
}