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

###set th ewd before hand, this code will save an rds file every time the nrow is perfectly divisible 
#by 1000
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



###test
wi_polls <- readRDS("F:/MEDSL/covid19/cleaned_wi2/wi_final_poll_datasp.Rdata")
nrow(wi_polls)
wi_voterfile <- readRDS("F:/MEDSL/covid19/cleaned_wi2/master_df_mil07082020.Rdata")
wi_voterfile <- wi_voterfile[,1:47]
wi_polls <- wi_polls@data
wi_polls <- wi_polls[,1:12]
wi_polls <- subset(wi_polls, County=="MILWAUKEE COUNTY")
###now let's do the test 
smple_vf <- wi_voterfile[1:100,]
test_obj <- poll_dist_fxn2(smple_vf,wi_polls,smple_vf$X,smple_vf$Y,wi_polls$Longitude,wi_polls$Latitude )
View(test_obj)
