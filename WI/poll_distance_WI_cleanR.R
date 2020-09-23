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

###note that this should be run for both polling places before and after. If the user already knows the matched polling places before and after,
#then simply run the data with the mapply(distGeo_mod,...) section 
wi_drop <- "C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis"
setwd(wi_drop)
list.files()
#mil_data <- readRDS("master_df_mil_opened07092020a.Rdata")
nrow(mil_data)
wi_bisg <- readRDS("wi_bisg_results.Rdata")
wi_polls <- readRDS("wi_final_poll_datasp.Rdata")
write.csv(wi_polls@data, "wisconsin_poll_places.csv",row.names = F)
####now let's get the information for Milwaukee, and then we will follow up with Madison 
mil_polls <- subset(wi_polls, County=="MILWAUKEE COUNTY")
summary(mil_polls$closed)
mil_polls <- mil_polls@data
mil_polls <- mil_polls[,c(1:7,11:12,21)]
###now let's get the results by Milwaukee voters 
mil_voters <- subset(wi_bisg, county=="079")
nrow(mil_voters)
head(wi_polls@data)
sort(unique(wi_polls$County))

getwd()
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/milwaukee")
mil_dist_results1 <- poll_dist_fxn3(mil_voters,mil_polls,mil_voters$X,mil_voters$Y,mil_polls$Longitude,
                                    mil_polls$Latitude)
####new requests 
mil_voters_part2 <- mil_voters[-c(1:872001),]
mil_voters_part2$latlong <- paste0(mil_voters_part2$Y, sep=" ", mil_voters_part2$X)
nrow(mil_voters_part2)
length(unique(mil_voters_part2$latlong))
mil_voters_part2 <- mil_voters_part2[!duplicated(mil_voters_part2$latlong), ]
##saving to part 2 folder 
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/milwaukee/part2")
mil_dist_results2 <- poll_dist_fxn3(mil_voters_part2,mil_polls,mil_voters_part2$X,mil_voters_part2$Y,
                                    mil_polls$Longitude,
                                    mil_polls$Latitude)
####now let's read in, then merge the dfs 
part1mil_data <- 
  readRDS("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/milwaukee/voter_poll_dyad_df_872001.rds")
head(part1mil_data)
part1mil_data$latlong <- paste0(part1mil_data$latitude_voter, sep=" ", part1mil_data$longitude_voter)
part1mil_data <- part1mil_data[!duplicated(part1mil_data$latlong), ]
nrow(part1mil_data)
part1mil_data <- subset(part1mil_data, select=-c(Voter.Reg.Number,surname,pred.whi,pred.bla,pred.his,pred.asi,
                                                 pred.oth))
mil_dist_results2 <- subset(mil_dist_results2, select=-c(Voter.Reg.Number,surname,pred.whi,
                                                         pred.bla,pred.his,pred.asi,
                                                 pred.oth))
mil_dist_results_all <- rbind(mil_dist_results2,part1mil_data)
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/milwaukee")
saveRDS(mil_dist_results_all, "mil_dist_results_all.rds")
write.dbf(mil_dist_results_all[1:1000,], "mil_dist_results_sample.dbf")
write.csv(mil_dist_results_all, "mil_dist_results_all.csv",row.names=FALSE)
###ok, now let's subset so closed are out 
mil_polls_opened <- subset(mil_polls, closed==0)
names(mil_voters)
mil_voters$latlong <- paste0(mil_voters$Y, sep=" ", mil_voters$X)
mil_voters_unique <- mil_voters[!duplicated(mil_voters$latlong), ]
mil_voters_unique <- subset(mil_voters_unique, select=-c(Voter.Reg.Number,surname,pred.whi,
                                                         pred.bla,pred.his,pred.asi,
                                                         pred.oth))
####good, can run now 
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/milwaukee/opened")
nrow(mil_voters_unique)
mil_dist_opened_results <- poll_dist_fxn3(mil_voters_unique,mil_polls_opened,mil_voters_unique$X,mil_voters_unique$Y,
                                          mil_polls_opened$Longitude,
                                          mil_polls_opened$Latitude)
saveRDS(mil_dist_opened_results, "mil_dist_results_open.rds")
write.csv(mil_dist_opened_results, "mil_dist_results_open.csv",row.names=F)


####we will do Madison here
madison_polls <- subset(wi_polls, County=="DANE COUNTY" )

madison_polls <- madison_polls@data
madison_polls <- madison_polls[,c(1:7,11:12,21)]
wi_bisg$latlong <- paste0(wi_bisg$Y, sep=" ", wi_bisg$X)
madison_voters <- subset(wi_bisg, county=="025")
madison_voters <- madison_voters[!duplicated(madison_voters$latlong), ]
head(madison_voters)
madison_voters <- subset(madison_voters, select=-c(Voter.Reg.Number,surname,pred.whi,
                                                   pred.bla,pred.his,pred.asi,
                                                   pred.oth))
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/madison/planned")
madison_dist_results1 <- poll_dist_fxn3(madison_voters,madison_polls,madison_voters$X,madison_voters$Y,
                                        madison_polls$Longitude,
                                        madison_polls$Latitude)
##let's save the data then 
saveRDS(madison_dist_results1, "madison_dist_results_planned.rds")
write.csv(madison_dist_results1, "madison_dist_results_planned.csv",row.names = F)

madison_polls_open <- subset(madison_polls, closed==0)
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/madison/open")
madison_dist_results2 <- poll_dist_fxn3(madison_voters,madison_polls_open,madison_voters$X,madison_voters$Y,
                                        madison_polls_open$Longitude,
                                        madison_polls_open$Latitude)
saveRDS(madison_dist_results2, "madison_dist_results_opened.rds")
write.csv(madison_dist_results2, "madison_dist_results_opened.csv",row.names = F)

#####Greenbay in Brown county here ; rest will be Waukesha, and Kenosha 
brown_polls <- subset(wi_polls, County=="BROWN COUNTY" )
brown_polls <- brown_polls@data
brown_polls <- brown_polls[,c(1:7,11:12,21)]
brown_voters <- subset(wi_bisg, county=="009")
brown_voters <- brown_voters[!duplicated(brown_voters$latlong), ]
head(brown_voters)
brown_voters <- subset(brown_voters, select=-c(Voter.Reg.Number,surname,pred.whi,
                                                   pred.bla,pred.his,pred.asi,
                                                   pred.oth))
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/brown/planned")
brown_dist_results1 <- poll_dist_fxn3(brown_voters,brown_polls,brown_voters$X,brown_voters$Y,
                                      brown_polls$Longitude,
                                      brown_polls$Latitude)
saveRDS(brown_dist_results1, "brown_dist_results_planned.rds")
write.csv(brown_dist_results1, "brown_dist_results_planned.csv",row.names = F)

#now, for brown open polls
brown_polls_open <- subset(brown_polls, closed==0)
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/brown/open")
brown_dist_results2 <- poll_dist_fxn3(brown_voters,brown_polls_open,brown_voters$X,brown_voters$Y,
                                      brown_polls_open$Longitude,
                                      brown_polls_open$Latitude)
saveRDS(brown_dist_results2, "brown_dist_results_open.rds")
write.csv(brown_dist_results2, "brown_dist_results_open.csv",row.names = F)

###ok, now waukesha 
#####Greenbay in Brown county here ; rest will be Waukesha, and Kenosha 
waukesha_polls <- subset(wi_polls, County=="WAUKESHA COUNTY" )
waukesha_polls <- waukesha_polls@data
waukesha_polls <- waukesha_polls[,c(1:7,11:12,21)]
waukesha_voters <- subset(wi_bisg, county=="133")
waukesha_voters <- waukesha_voters[!duplicated(waukesha_voters$latlong), ]
head(waukesha_voters)
waukesha_voters <- subset(waukesha_voters, select=-c(Voter.Reg.Number,surname,pred.whi,
                                               pred.bla,pred.his,pred.asi,
                                               pred.oth))
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/waukesha/planned")
waukesha_dist_results1 <- poll_dist_fxn3(waukesha_voters,waukesha_polls,waukesha_voters$X,waukesha_voters$Y,
                                      waukesha_polls$Longitude,
                                      waukesha_polls$Latitude)
saveRDS(waukesha_dist_results1, "waukesha_dist_results_plannned.rds")
write.csv(waukesha_dist_results1, "waukesha_dist_results_plannned.csv",row.names = F)

###now for opened 
waukesha_polls_open <- subset(waukesha_polls, closed==0)
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/waukesha/open")
waukesha_dist_results2 <- poll_dist_fxn3(waukesha_voters,waukesha_polls_open,waukesha_voters$X,waukesha_voters$Y,
                                         waukesha_polls_open$Longitude,
                                         waukesha_polls_open$Latitude)
saveRDS(waukesha_dist_results2, "waukesha_dist_results_open.rds")
write.csv(waukesha_dist_results2, "waukesha_dist_results_open.csv",row.names = F)

####Now Kenosha 
kenosha_polls <- subset(wi_polls, County=="KENOSHA COUNTY" )
kenosha_polls <- kenosha_polls@data
kenosha_polls <- kenosha_polls[,c(1:7,11:12,21)]
kenosha_voters <- subset(wi_bisg, county=="059")
kenosha_voters <- kenosha_voters[!duplicated(kenosha_voters$latlong), ]
head(kenosha_voters)
kenosha_voters <- subset(kenosha_voters, select=-c(Voter.Reg.Number,surname,pred.whi,
                                                     pred.bla,pred.his,pred.asi,
                                                     pred.oth))
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/kenosha/planned")
kenosha_dist_results1 <- poll_dist_fxn3(kenosha_voters,kenosha_polls,kenosha_voters$X,kenosha_voters$Y,
                                         kenosha_polls$Longitude,
                                         kenosha_polls$Latitude)
saveRDS(kenosha_dist_results1, "kenosha_dist_results_plannned.rds")
write.csv(kenosha_dist_results1, "kenosha_dist_results_plannned.csv",row.names = F)

###Now Kenosha open 
kenosha_polls_open <- subset(kenosha_polls, closed==0)
setwd("C:/Users/johna/Dropbox (Curiel Analytx)/Healthy_Elections/States/WI/distance_analysis/kenosha/open")
kenosha_dist_results2 <- poll_dist_fxn3(kenosha_voters,kenosha_polls_open,kenosha_voters$X,kenosha_voters$Y,
                                        kenosha_polls_open$Longitude,
                                        kenosha_polls_open$Latitude)
saveRDS(kenosha_dist_results2, "kenosha_dist_results_open.rds")
write.csv(kenosha_dist_results2, "kenosha_dist_results_open.csv",row.names = F)


sort(unique(wi_polls$County))
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


