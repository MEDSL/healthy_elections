###################Ohio Sampler ######################
library(wru)
library(tidyverse)
library(vroom)
library(dplyr)
setwd("F:/MEDSL/healthy_elections/OH/Historical Voter File") # replace as necessary 
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
###read in teh county data 
county_data <- read.csv("F:/MEDSL/healthy_elections/county_acs_demos.csv")
oh_data <- subset(county_data, Geo_STUSAB=="oh")
oh_data <- oh_data[,1:6]
oh_data$pop_pct <- oh_data$total_pop/sum(oh_data$total_pop)
oh_data$county <- str_to_upper(str_remove_all(oh_data$Geo_NAME, "County"))
oh_data$county <- trimws(oh_data$county)
oh_data$county[oh_data$county=="VAN WERT"] <- "VANWERT"
####let's now sample the dataframe 
sampled_df <- data.frame()
for(i in 1:length(oh_files)){
  county_name <- str_remove(oh_files[i], ".txt")
  temp_oh_data <- subset(oh_data, county==county_name)
  oh_temp <- read.delim(oh_files[i],sep=",")
  colnames(oh_temp)[colnames(oh_temp)=="LAST_NAME"] <- "surname"
  oh_temp <- subset(oh_temp, select=c(SOS_VOTERID,surname,RESIDENTIAL_ADDRESS1,RESIDENTIAL_CITY, RESIDENTIAL_ZIP)) 
  #just want data so as to run the geocode 
  size_sampl <- temp_oh_data$pop_pct*10000
  if(size_sampl*nrow(oh_temp)<2){
    size_sampl <- 2
  }else{
    size_sampl <- round(size_sampl,0)
  }
  temp_sample <- sample_n(oh_temp, size = size_sampl )
  temp_sample$county <- county_name
  #oh_temp <- subset(oh_temp, select=c()) #might just create new file here
  if(nrow(sampled_df)==0){
    sampled_df <- temp_sample
  }else{
    sampled_df <- rbind(sampled_df,temp_sample)
  }
} #FRANKLIN missing, and Butler
saveRDS(sampled_df)
nrow(sampled_df)
length(unique(sampled_df$county))
length(unique(oh_data$county))
