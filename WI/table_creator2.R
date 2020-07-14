#########################33 Catalist Table Creator ########################33
library(extrafont)
library(showtext)
font_import("F:/MEDSL/healthy_elections/general/fonts/styrene_b_ttf", prompt = F)
font_import( prompt = F)
windowsFonts(A = windowsFont("StyreneB-Regular"))
windowsFonts(A = windowsFont("styrene_b"))
library(readxl)
library(tidyverse)
####################
setwd("F:/MEDSL/healthy_elections/WI")
polls_data <- readRDS("final_poll_dataframe_wwards.Rdata")
class(polls_data)
###Let's do by quartile 
quart_white_breaks <- quantile(polls_data$non_white_pct, seq(0,1,by=0.25))
quart_white_breaks
polls_data$white_quart_rank <- 1
polls_data$white_quart_rank[polls_data$non_white_pct >= quart_white_breaks[2] & polls_data$non_white_pct < quart_white_breaks[3]] <- 2
polls_data$white_quart_rank[polls_data$non_white_pct >= quart_white_breaks[3] & polls_data$non_white_pct < quart_white_breaks[4]] <- 3
polls_data$white_quart_rank[polls_data$non_white_pct >= quart_white_breaks[4] & polls_data$non_white_pct < quart_white_breaks[5]] <- 4
1- length(which(polls_data$closed==0 & polls_data$white_quart_rank==1))/length(which(polls_data$white_quart_rank==1))
1- length(which(polls_data$closed==0 & polls_data$white_quart_rank==2))/length(which(polls_data$white_quart_rank==2))
1- length(which(polls_data$closed==0 & polls_data$white_quart_rank==3))/length(which(polls_data$white_quart_rank==3))
1- length(which(polls_data$closed==0 & polls_data$white_quart_rank==4))/length(which(polls_data$white_quart_rank==4))
####let's do population density now 
quart_pop_breaks <- quantile(polls_data$log_pop_sqkm, seq(0,1,by=0.25))
quart_pop_breaks
polls_data$pop_quart_rank <- 1
polls_data$pop_quart_rank[polls_data$log_pop_sqkm >= quart_pop_breaks[2] & polls_data$log_pop_sqkm < quart_pop_breaks[3]] <- 2
polls_data$pop_quart_rank[polls_data$log_pop_sqkm >= quart_pop_breaks[3] & polls_data$log_pop_sqkm < quart_pop_breaks[4]] <- 3
polls_data$pop_quart_rank[polls_data$log_pop_sqkm >= quart_pop_breaks[4] & polls_data$log_pop_sqkm < quart_pop_breaks[5]] <- 4
(1- length(which(polls_data$closed==0 & polls_data$pop_quart_rank==1))/length(which(polls_data$pop_quart_rank==1)))*100
(1- length(which(polls_data$closed==0 & polls_data$pop_quart_rank==2))/length(which(polls_data$pop_quart_rank==2)))*100
(1- length(which(polls_data$closed==0 & polls_data$pop_quart_rank==3))/length(which(polls_data$pop_quart_rank==3)))*100
(1- length(which(polls_data$closed==0 & polls_data$pop_quart_rank==4))/length(which(polls_data$pop_quart_rank==4)))*100
####now let's do the Democratic distribution 
quart_dem_breaks <- quantile(polls_data$dem_pct18, seq(0,1,by=0.25))
quart_dem_breaks
polls_data$dem_quart_rank <- 1
polls_data$dem_quart_rank[polls_data$dem_pct18 >= quart_dem_breaks[2] & polls_data$dem_pct18 < quart_dem_breaks[3]] <- 2
polls_data$dem_quart_rank[polls_data$dem_pct18 >= quart_dem_breaks[3] & polls_data$dem_pct18 < quart_dem_breaks[4]] <- 3
polls_data$dem_quart_rank[polls_data$dem_pct18 >= quart_dem_breaks[4] & polls_data$dem_pct18 < quart_dem_breaks[5]] <- 4
(1- length(which(polls_data$closed==0 & polls_data$dem_quart_rank==1))/length(which(polls_data$dem_quart_rank==1)))*100
(1- length(which(polls_data$closed==0 & polls_data$dem_quart_rank==2))/length(which(polls_data$dem_quart_rank==2)))*100
(1- length(which(polls_data$closed==0 & polls_data$dem_quart_rank==3))/length(which(polls_data$dem_quart_rank==3)))*100
(1- length(which(polls_data$closed==0 & polls_data$dem_quart_rank==4))/length(which(polls_data$dem_quart_rank==4)))*100

####Let's get the data for the catalist here 
setwd("F:/MEDSL/healthy_elections/WI/catalist")
wi_cat_files <- list.files()
wi_cat_files2016 <- wi_cat_files[grepl("2016", wi_cat_files)]
wi_cat_files2020 <- wi_cat_files[grepl("2020", wi_cat_files)]
#####
names2016 <- c("precinct","ward","null","absentee","early","mail","polling","unknown","null2")
wi2016method <- read.csv("wi_2016_votemethod_vote1.csv", skip=3)
colnames(wi2016method) <- names2016
wi2016method$total <- rowSums(wi2016method[,c(4:7)],na.rm=T)
sum(wi2016method$total)
length(which(grepl("TOTAL", wi2016method$ward)))
View(wi2016method)
wi_cat_files2016
wi_cat_files2020

stargazer(catalist_data2 %>% select(),covariate.labels = c("Logged Donations","Weighted Donations",
                                                                                               "Weighted Donations per cap",
                                                                                               "Logged Donations per cap",
                                                                                               "Marginal-Majority",
                                                                                               "Leg. Control",
                                                                                               "Speaker Power",
                                                                                               "Governor Power (BPI)",
                                                                                               "Polarization",
                                                                                               "Leg. Seats Up",
                                                                                               "Leg. Prof.",
                                                                                               "Log GDP per cap.",
                                                                                               "GDP pct. chg.",
                                                                                               "Log Population",
                                                                                               "POTUS Approval"),
          summary = TRUE, omit.summary.stat = c("p25","p75"))
