library(stringr)
library(readr)
library(data.table)
library(dplyr)

#add the FVE files and the election map files into their own lists, both sorted by name
 election_map_files <- list.files(path = "~/Downloads/Statewide/pa_election_maps",full.names = TRUE)
 fve_files <- list.files(path = "~/Downloads/Statewide/pa_fve_files",full.names = TRUE)

#constant variables
keep <- data.frame(var = paste0("V",c(1,3,7,8,9,10,12,13,15,16,18,19,20,26,27,152)),
                   newvar = c("id","lastname","sex","dob","regdate","voterstat","pid","house_number","street","aptno","city","state","zip","lastelecdate","precinct","county"),
                   stringsAsFactors=F)

#list to place output from for loop
  pa_list <- list()

#for loop to extract and rename columns to match their respective elections in each county
 for(i in 1:67){
     vf <- fread(fve_files[i])
     ef <- read.delim(election_map_files[i],header = FALSE, stringsAsFactors = FALSE)
     
 #order elections by date – we will use this to name columns so they sort by order in the voter file later    
     ef$V3 <- tolower(ef$V3)
     ef <- ef[order(as.Date(ef$V4,format = "%m/%d/%Y")),]
     rownames(ef) <- NULL
     ef$year <- substr(ef$V4,9,10)
     ef$var_vote <- paste0("V",c(70+(ef$V2*2-1)))
     ef$var_voteparty <- paste0("V",c(70+(ef$V2*2)))
     ef$newvar_vote <- paste0("vote",ef$year,"_","-",rownames(ef),"-",ef$V4)
     ef$newvar_voteparty <- paste0("voteparty",ef$year,"_","-",rownames(ef),"-",ef$V4)
 
 #rename columns
     vf <- vf %>% select(c(keep$var,ef$var_vote,ef$var_voteparty))
     names(vf) <- c(keep$newvar,ef$newvar_vote,ef$newvar_voteparty)
  
 #grab the name of the first election column that is not blank for each voter — this is their first election
     vf$first_election <- apply(vf,1,function(x) names(which(which(!is.na(x) & x!="")>16))[1])
 
 #remove the rownumber from the column names and first_election value
     names(vf) <- c(str_remove(colnames(vf),"-[:digit:][:digit:]?-"))
     vf$first_election <- str_remove(vf$first_election,"-[:digit:][:digit:]?-")
  
 #drop all columns except for the constants, first election, and primary election data from 2008 onwards 
     keeps <- c("id","lastname","sex","dob","regdate","voterstat","pid","house_number","street","aptno","city","state","zip","lastelecdate","precinct","county","vote16_04/26/2016","vote20_06/02/2020","voteparty16_04/26/2016","voteparty20_06/02/2020","vote12_04/24/2012","vote08_04/22/2008","voteparty12_04/24/2012","voteparty08_04/22/2008","first_election")
     vf <- as.data.frame(vf)
     
     missing <- setdiff(keeps,names(vf[,names(vf) %in% keeps]))
     for(v in missing){
         vf[[v]] <- ""
     }
     
     vf <- vf[,keeps,drop = FALSE]
     pa_list[[i]] <- vf
 }
 
 #bind list into dataframe of voter files
 pa_voter_files <- do.call(rbind,pa_list)
 
 
