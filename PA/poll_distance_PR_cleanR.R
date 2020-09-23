##################################################################
################## Template for finding poll distances ###########
library(rgdal)
library(sp)
library(dplyr)
library(foreign)
library(geosphere) #this has the distgeo fxn
distGeo_mod <- function(lon1,lat1,lon2,lat2){
  distGeo(c(lon1,lat1),c(lon2,lat2))
  #return(value)
}
options(stringsAsFactors = FALSE)
#this function is for where the user has two spatial objects 
poll_dist_fxn1 <- function(spat_vf,spat_polls){
  #step 0: pull in the coordinate fields for the poll info 
  voter_poll_dyad_df <- data.frame(stringsAsFactors = FALSE)
  spat_polls$longitude <- coordinates(spat_polls)[1]
  spat_polls$latitude <- coordinates(spat_polls)[2]
  #step 1: create loop to go through the observations for the voter file coordinates 
  for(i in 1:nrow(spat_vf)){
    temp_vf <- spat_vf[i,] #pull ith obs 
    #step 2: extract coordinates 
    temp_vf$long <- coordinates(temp_vf)[1]
    temp_vf$lat <- coordinates(temp_vf)[2]
    temp_vf <- temp_vf@data
    #step 3: merge single vf coordinate onto all of the polling locations 
    temp_merge <- merge(temp_vf, spat_polls, by=NULL)
    #step 4: find distance between every polling place and voter coord 
    temp_merge$euc_distance <- NA
    temp_merge$euc_distance <- mapply(distGeo_mod, temp_merge$long, temp_merge$lat, temp_merge$longitude, temp_merge$latitude )
    #the mapply fxn applies the fxn for every row 
    #step 5: take the row with the minimal distance 
    temp_merge <- temp_merge %>% slice(which.min(euc_distance))
    if(nrow(voter_poll_dyad_df)==0){
      voter_poll_dyad_df <- temp_merge
    }else{
      voter_poll_dyad_df <- rbind(voter_poll_dyad_df, temp_merge)
    }
  }
  return(voter_poll_dyad_df)
}

####for non spatial objects, though the coordinate fields are present for both  
poll_dist_fxn2 <- function(spat_vf,spat_polls,spat_vf_long,spat_vf_lat,spat_polls_long,spat_polls_lat){
  #step 0: pull in the coordinate fields 
  voter_poll_dyad_df <- data.frame(stringsAsFactors = FALSE)
  spat_polls$longitude_poll <- spat_polls_long
  spat_polls$latitude_poll <- spat_polls_lat
  spat_vf$longitude_voter <- spat_vf_long
  spat_vf$latitude_voter <- spat_vf_lat
  #step 1: create loop to go through the observations for the voter file coordinates 
  for(i in 1:nrow(spat_vf)){
    #svMisc::progress((i/nrow(spat_vf))*100)
    print(i)
    temp_vf <- spat_vf[i,] #pull ith obs 
    #step 2: merge single vf coordinate onto all of the polling locations 
    temp_merge <- merge(temp_vf, spat_polls, by=NULL)
    #step 3: find distance between every polling place and voter coord 
    temp_merge$euc_distance <- NA
    temp_merge$euc_distance <- mapply(distGeo_mod, temp_merge$longitude_voter, 
                                      temp_merge$latitude_voter, temp_merge$longitude_poll, 
                                      temp_merge$latitude_poll )
    #the mapply fxn applies the fxn for every row ; can use just this part if voters - polls already matched 
    #step 4: take the row with the minimal distance 
    temp_merge <- temp_merge %>% slice(which.min(euc_distance))
    #step 5: bind the data into the master data frame 
    if(nrow(voter_poll_dyad_df)==0){
      voter_poll_dyad_df <- temp_merge
    }else{
      voter_poll_dyad_df <- rbind(voter_poll_dyad_df, temp_merge)
    }
  }   
  return(voter_poll_dyad_df)
  
}


###note that this should be run for both polling places before and after. If the user already knows the matched polling places before and after,
#then simply run the data with the mapply(distGeo_mod,...) section 
we_drop_pa <- "C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/PA"
setwd(we_drop_pa)
pa_addrs <- read.csv("pa_addrs_geocoded/pa_addrs_geocoded.csv")
allegheny_polls <- read.csv("PA polling distance/2016alleghenygeocoded.csv")

View(allegheny_polls)
View(pa_addrs)
allegheny_addrs <- subset(pa_addrs,county=="42003")
nrow(allegheny_addrs)
nrow(allegheny_addrs) %/% 100
u <- 1
total_df <- data.frame(stringsAsFactors = FALSE)
while(u < 8986){
  start=u
  end1 <- u+100
  temp_sub <- allegheny_addrs[u:end1,]
  allegheny_addrs3 <- poll_dist_fxn2(temp_sub,allegheny_polls,temp_sub$X,temp_sub$Y,
                                     allegheny_polls$lon,allegheny_polls$lat)
  if(nrow(total_df)==0){
    total_df <- allegheny_addrs3 
  }else{
    total_df <- rbind(total_df,allegheny_addrs3)
  }
  start <- end1 + 1
  print(nrow(total_df))
}


allegheny_addrs3 <- poll_dist_fxn2(allegheny_addrs,allegheny_polls,allegheny_addrs$X,allegheny_addrs$Y,
                                  allegheny_polls$lon,allegheny_polls$lat)


nrow(allegheny_addrs3)
### a third verison that will save 
poll_dist_fxn3 <- function(spat_vf,spat_polls,spat_vf_long,spat_vf_lat,spat_polls_long,spat_polls_lat){
  #step 0: pull in the coordinate fields 
  voter_poll_dyad_df <- data.frame(stringsAsFactors = FALSE)
  spat_polls$longitude_poll <- spat_polls_long
  spat_polls$latitude_poll <- spat_polls_lat
  spat_vf$longitude_voter <- spat_vf_long
  spat_vf$latitude_voter <- spat_vf_lat
  #step 1: create loop to go through the observations for the voter file coordinates 
  for(i in 1:nrow(spat_vf)){
    #svMisc::progress((i/nrow(spat_vf))*100)
    print(i)
    temp_vf <- spat_vf[i,] #pull ith obs 
    #step 2: merge single vf coordinate onto all of the polling locations 
    temp_merge <- merge(temp_vf, spat_polls, by=NULL)
    #step 3: find distance between every polling place and voter coord 
    temp_merge$euc_distance <- NA
    temp_merge$euc_distance <- mapply(distGeo_mod, temp_merge$longitude_voter, 
                                      temp_merge$latitude_voter, temp_merge$longitude_poll, 
                                      temp_merge$latitude_poll )
    #the mapply fxn applies the fxn for every row ; can use just this part if voters - polls already matched 
    #step 4: take the row with the minimal distance 
    temp_merge <- temp_merge %>% slice(which.min(euc_distance))
    #step 5: bind the data into the master data frame 
    if(nrow(voter_poll_dyad_df)==0){
      voter_poll_dyad_df <- temp_merge
    }else if((nrow(voter_poll_dyad_df)%%1000)==0){
      voter_poll_dyad_df <- rbind(voter_poll_dyad_df, temp_merge)
      sv_name <- paste0("voter_poll_dyad_df",sep="_",i,sep="",".rds")
      saveRDS(voter_poll_dyad_df, sv_name)
    }else{
      voter_poll_dyad_df <- rbind(voter_poll_dyad_df, temp_merge)
      
    }
  }   
  return(voter_poll_dyad_df)
  
}
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/PA/allegheny_polls")


allegheny_addrs3 <- poll_dist_fxn3(allegheny_addrs,allegheny_polls,allegheny_addrs$X,allegheny_addrs$Y,
                                   allegheny_polls$lon,allegheny_polls$lat)
####another version of the addrs; from 
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/PA/allegheny_polls/part2")
allegheny_addrs_part2 <- allegheny_addrs[-c(1:485000), ]
nrow(allegheny_addrs_part2)
allegheny_addrs3 <- poll_dist_fxn3(allegheny_addrs_part2,allegheny_polls,allegheny_addrs_part2$X,
                                   allegheny_addrs_part2$Y,
                                   allegheny_polls$lon,allegheny_polls$lat)
saveRDS(allegheny_addrs3, "pa_alleghany_dist2016part2.rds")
write.csv(allegheny_addrs3, "pa_alleghany_dist2016part2.csv",row.names = F)
###let's read in the data from the 
pa_first_half <- readRDS("voter_poll_dyad_df_485001.rds")
allegheny_addrs_all_geo <- rbind(pa_first_half,allegheny_addrs3)
saveRDS(allegheny_addrs_all_geo, "alleghany2016dist_results_all.rds")
write.csv(allegheny_addrs_all_geo, "alleghany2016dist_results_all.csv",row.names = F)

#############reading in the data for 2016 phillie 

setwd("C:/Users/johna/OneDrive/Documents/GitHub/healthy_elections/PA")
phillie2016addrs <- read.csv("phillie2016_vgeocoded.csv")
setwd(we_drop_pa)
pa_addrs <- read.csv("pa_addrs_geocoded/pa_addrs_geocoded.csv")
phillie_voters_addrs <- subset(pa_addrs, county=="42101")
#####ok, this will take a while 
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/PA/phillie_polls")
phillie_results1 <- poll_dist_fxn3(phillie_voters_addrs,phillie2016addrs,phillie_voters_addrs$X,phillie_voters_addrs$Y,
                                   phillie2016addrs$lon,phillie2016addrs$lat)
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/PA/phillie_polls")
list.files()
phillie_results1 <- readRDS("voter_poll_dyad_df_770001.rds")
nrow(phillie_results1)
###3picking up where we left off after pc restarted on 8/16
phillie_voters_addrs2 <- phillie_voters_addrs[-c(1:770001), ]
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/PA/phillie_polls/part2")
phillie_results2 <- poll_dist_fxn3(phillie_voters_addrs2,phillie2016addrs,phillie_voters_addrs2$X,
                                   phillie_voters_addrs2$Y,
                                   phillie2016addrs$lon,phillie2016addrs$lat)
###binding the results 
phillie_results_all <- rbind(phillie_results1, phillie_results2)
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/PA/phillie_polls")
saveRDS(phillie_results_all, "phillie_results2016dist_all.rds")
write.csv(phillie_results_all, "phillie_results2016dist_all.csv", row.names = FALSE)
