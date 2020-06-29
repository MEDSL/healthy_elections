####Script for getting to aggregated Florida table from voter files

# Unzip voter files in Dropbox (https://www.dropbox.com/sh/4uvbd0v88icod4r/AADG73sgULyqczeD87svgLW_a/States/FL/Voter%20file%20June%202020?dl=0&subfolder_nav_tracking=1)

#Make a list of files in the voter history and voter detail folders
june_voter_history_files <- list.files(path = "~/Downloads/20200609_VoterHistory", full.names = TRUE)
june_voter_detail_files <- list.files(path = "~/Downloads/20200609_VoterDetail", full.names = TRUE)

#Apply function to Voter Detail files to:
## 1. read each file in the voter detail folder
## 2. drop all columns that are not county, voter_id, birthdate, and party
## 3. rename columns

june_fl_voter_detail <- lapply(june_voter_detail_files, function(x) {
     t <- read.delim(x, quote = "", header = FALSE)
     keeps <- c("V1","V2","V21","V22","V24")
     t <- t[,keeps,drop = FALSE]
     names(t) <- c("county","voter_id","race","birthdate","party")
     return(t)
 })
 
#Create a larger dataframe of all the counties
june_fl_voter_detail <- do.call(rbind,june_fl_voter_detail)

#Apply function to Voter History files to:
## 1. read each file in the voter history folder
## 2. rename columns
## 3. drop all entries that are not for the 3/15/2016 or 3/17/2020 presidential preference primaries

june_fl_voter_history <- lapply(june_voter_history_files, function(x) {
     t <- read.delim(x, quote = "", header = FALSE)
     names(t) <- c("county","voter_id","election_date","election_type","history_code")
     t <- t[t$election_date == "03/15/2016" | t$election_date == "03/17/2020",]
     t <- t[t$election_type == "PPP",]
     return(t)
 })
 
#Create a larger dataframe of all the counties
june_fl_voter_history <- do.call(rbind,june_fl_voter_history)

#Merge new Voter History and Voter Detail dataframes on voter_id and county
june_fl_voter_files <- merge(june_fl_voter_history,june_fl_voter_detail,by = c("voter_id","county"))

#Create an age column based on election date and birthdate
june_fl_voter_files$age <- 
  round(as.numeric(difftime(as.Date(june_fl_voter_files[,3],format="%m/%d/%Y"),as.Date(june_fl_voter_files[,7],format="%m/%d/%Y"),unit="weeks"))/52.25)
  
#Create an age bucket column
june_fl_voter_files$age_bucket <- 
  cut(june_fl_voter_files[,9], breaks = c(17,29,44,59,Inf), labels = c("18-29","30-44","45-59","60+"))
  
#Aggregate voter file by county, party, race, age group, vote method, election type
aggregate_june_fl <- 
  aggregate(june_fl_voter_files$voter_id, by = list(june_fl_voter_files$county,june_fl_voter_files$election_date,june_fl_voter_files$history_code,june_fl_voter_files$election_type,june_fl_voter_files$race,june_fl_voter_files$party,june_fl_voter_files$age_bucket), FUN = length)

#Name the columns 
names(aggregate_june_fl) <- c("county","election_date","vote_mode","election_type","race","party","age_group","vote_count")

#Write to CSV — from here you can pivot the data in Excel/Sheets
write_csv(aggregate_june_fl, path = "~/Documents/FL_Voting_Data_MEDSL/aggregate_june_files.csv")

