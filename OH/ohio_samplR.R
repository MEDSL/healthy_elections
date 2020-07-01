###################Ohio Sampler ######################
library(wru)
library(tidyverse)
library(vroom)
library(dplyr)
library(foreign)
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
  if(size_sampl<2){
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
saveRDS(sampled_df, "ohio_voterfile_sample.Rdata")
sampled_df <- readRDS("ohio_voterfile_sample.Rdata")
nrow(sampled_df)
sampled_df$full_addr <- paste0(sampled_df$RESIDENTIAL_ADDRESS1, sep=" ", sampled_df$RESIDENTIAL_CITY, sep=" ", "OH", sep=" ",
                               sampled_df$RESIDENTIAL_ZIP)
saveRDS(sampled_df, "ohio_voterfile_sample.Rdata")
write.dbf(sampled_df, "ohio_sample_geocodes.dbf")
########reading in the csv 
options(stringsAsFactors = F)
setwd("F:/MEDSL/healthy_elections/OH") # replace as necessary 
oh_geocode <- read.csv("oh_sample.csv")
oh_geocode_miss <- subset(oh_geocode, X==0 ) # 572 obs 
oh_geocode_coded <- subset(oh_geocode, X != 0)
saveRDS(oh_geocode_coded, "oh_geocode_coded.Rdata")
#####geocoding script 
##step 1: load the API key 
api.key<-"AIzaSyCt9YWkR50q1qmQohRl6tEF0tBh-ICek1s"
#(origin = "38.1621328+24.0029257") takes lat long format 
#this means it takes x2 first, then x1 
wd1 <- ""
api.key=""
set.api.key(api.key)
register_google(api.key, "standard") #registers the API key for the purpose of the analysis  

##create storage 
addr_df <- matrix(NA, nrow = nrow(oh_geocode_miss), ncol = 3)
for(i in 1:nrow(addr_df)){
  svMisc::progress((i/nrow(addr_df))*100)
  tryCatch({
    storage_addr <- geocode(oh_geocode_miss$full_addr[i], output="latlon")
    addr_df[i,1]<-storage_addr$lon # the longitude
    addr_df[i,2]<-storage_addr$lat # the latitude
    addr_df[i,3]<-oh_geocode_miss$SOS_VOTERI[i] # the voter reg id to merge onto 
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")}
  )} 
setwd(wd1)
saveRDS(addr_df, "oh_google_geocoded_addrs.Rdata")
colnames(addr_df)[1:2] <- c("lon","lat")
addr_df <- readRDS("oh_google_geocoded_addrs.Rdata")

###here will be the overlay section 
library(rgdal)

temp_coor <- subset(addr_df, select = c(lon,lat))

addr_df_temp <- SpatialPointsDataFrame(coords = temp_coor, data = addr_df,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
cbgs <- readOGR(wd, "CB2010") #the general file name 
addr_df_temp$tract_fips <- over(addr_df_temp,cbg2010)$id

####read in the big three county records 
setwd("F:/MEDSL/healthy_elections/OH") # replace as necessary 
big3 <- read.csv("bigthreecounties.csv")
big3 <- big3[2:3]
foreign::write.dbf(big3, "big3_addr0h.dbf")
View(big3)
#####reading in the data for the big 3 geocoded 
geocoded_big3 <- read.csv("F:/voterfile/ohio_big3_addrs.csv")
View(geocoded_big3)
geocoded_big3$off <- 0
geocoded_big3$off[geocoded_big3$X==0] <- 1
sum(geocoded_big3$off)/nrow(geocoded_big3)# 3% are off 
write.csv(geocoded_big3, "ohio_big3_addrs.csv",row.names = F)
length(which(geocoded_big3$X==0))
