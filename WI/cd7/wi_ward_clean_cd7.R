###### WI Ward CleanR ############3
##############################
library(extrafont)
library(showtext)
font_import("F:/MEDSL/healthy_elections/general/fonts/styrene_b_ttf", prompt = F)
font_import( prompt = F)
windowsFonts(A = windowsFont("StyreneB-Regular"))
windowsFonts(A = windowsFont("styrene_b"))

library(readxl)
library(tidyverse)
options(stringsAsFactors = FALSE)
setwd("F:/MEDSL/healthy_elections/WI/cd7")
wiward_files <- list.files()
wiward_files


temp_xl <- read_xlsx("cd7wards_results.xlsx")
nrow(temp_xl)
View(temp_xl)
colnames(temp_xl)[1:3] <- c("county","ward","total")
temp_xl <- temp_xl %>% fill(county)
temp_xl2 <- subset(temp_xl, select=c(county,ward,total,SCATTERING,`Tricia Zunker`,`Tom Tiffany`))
#making wide 
temp_xl2 <- gather(temp_xl2, candidate, votes, SCATTERING:`Tom Tiffany`)
temp_xl2$candidate <- str_to_upper(temp_xl2$candidate)
temp_xl2$ward <- str_to_upper(temp_xl2$ward)
temp_xl2$party <- ""
temp_xl2$party[temp_xl2$candidate=="TOM TIFFANY"] <- "REPUBLICAN"
temp_xl2$party[temp_xl2$candidate=="TRICIA ZUNKER"] <- "DEMOCRAT"
saveRDS(temp_xl2, "wi_cd7wards_long.Rdata")
###ok, now let's read in and create the time series plots 
wd_abs_reports <- "F:/MEDSL/healthy_elections/WI/abs_reports"
list.dirs(wd_abs_reports)
list.files(wd_abs_reports)
abs_names <- c("aug11primary","cd7")
wi_abs_dataframe <- data.frame(stringsAsFactors = FALSE)
for(j in 1:2){
  tem_wd <- paste0(wd_abs_reports,sep="/",abs_names[j])
  setwd(tem_wd)
  temp_files1 <- list.files()
  for(i in 1:length(temp_files1)){
    abs_temp <- read_xlsx(temp_files1[i])
    temp_date <- str_remove(temp_files1[i], ".xlsx")
    colnames(abs_temp)[1:4] <- c("county_name","abs_applications","ballots_sent","ballots_returned")
    abs_temp <- subset(abs_temp, county_name!="TOTAL")
    abs_temp$date <- temp_date
    abs_temp$election <- abs_names[j]
    if(nrow(wi_abs_dataframe)==0){
      wi_abs_dataframe <- abs_temp
    }else{
      wi_abs_dataframe <- rbind(wi_abs_dataframe,abs_temp)
    }
  }
}
saveRDS(wi_abs_dataframe, "wi_abs_county_timeseries.Rdata")
wi_abs_dataframe <- wi_abs_dataframe[order(wi_abs_dataframe$election, wi_abs_dataframe$county_name,wi_abs_dataframe$date),]
wi_abs_dataframe <- wi_abs_dataframe %>% group_by(election,county_name) %>% mutate(lag_applications=lag(abs_applications,default=0))
wi_abs_dataframe <- wi_abs_dataframe %>% group_by(election,county_name) %>% mutate(lag_sent=lag(ballots_sent,default=0))
wi_abs_dataframe <- wi_abs_dataframe %>% group_by(election,county_name) %>% mutate(lag_returned=lag(ballots_returned,default=0))
###new returns related fields 
wi_abs_dataframe$new_applications <- wi_abs_dataframe$abs_applications - wi_abs_dataframe$lag_applications
wi_abs_dataframe$new_sent_ballots <- wi_abs_dataframe$ballots_sent - wi_abs_dataframe$lag_sent
wi_abs_dataframe$new_returns <- wi_abs_dataframe$ballots_returned - wi_abs_dataframe$lag_returned
setwd("F:/MEDSL/healthy_elections/WI/abs_reports")
saveRDS(wi_abs_dataframe, "wi_abs_county_timeseries.Rdata")
wi_abs_dataframe <- readRDS("wi_abs_county_timeseries.Rdata")
###good, now let's create a statewide. THen we will create the shinyR app 
wi_stateabs_dataframe <- wi_abs_dataframe %>% group_by(election,date) %>% summarise(new_applications=sum(new_applications,na.rm=T),
                                                                                    new_sent_ballots=sum(new_sent_ballots,na.rm=T),
                                                                                    new_returns=sum(new_returns, na.rm=T))
###total cumulative 
wi_stateabs_dataframe_cum <- wi_abs_dataframe %>% group_by(election,date) %>% summarise(abs_applications=sum(abs_applications,na.rm=T),
                                                                                    ballots_sent=sum(ballots_sent,na.rm=T),
                                                                                    ballots_returned=sum(ballots_returned, na.rm=T))
#######
wi_stateabs_dataframe$Date <- as.Date(wi_stateabs_dataframe$date)
View(wi_stateabs_dataframe)
wi_stateabs_dataframe_cd7 <- subset(wi_stateabs_dataframe, election=="cd7")
wi_stateabs_dataframe_cd7 <- wi_stateabs_dataframe_cd7[-1,]
as.numeric(wi_stateabs_dataframe_cd7$Date)
(wi_stateabs_dataframe_cd7$Date)
###let's just create a long data set 
wi_stateabs_dataframe_cd7long <- gather(wi_stateabs_dataframe_cd7, type, total, new_applications:new_returns)
wi_stateabs_dataframe_cd7long$type2 <- as.factor(wi_stateabs_dataframe_cd7long$type)
###let's do the time series here 
library(ggplot2)
library(ggalt)
library(grid)
grob_prim <- grobTree(textGrob("Primary Period", x=0.15,  y=0.9, hjust=0,
                               gp=gpar(col="black", fontsize=12, fontface="bold")))
abs_cd7time_series <- ggplot(wi_stateabs_dataframe_cd7long) +
  geom_line( aes(x=Date,y=total,color=type2,linetype=type2),lwd=2) +
  annotate("rect", xmin = as.Date("2020-04-30"), xmax = as.Date("2020-05-12"),ymin=0,ymax=10000,fill="#948DE5",
                           alpha = .2)  + annotation_custom(grob_prim) + theme_minimal() + 
  scale_linetype_manual(values = c(1,3,5)) + scale_size_manual(values = c(2,2,2)) + 
  labs(color="Type",title= "Absentee ballots reported",y="Total")  + 
  guides(color = guide_legend(override.aes = list(linetype = c(1,3,5)),order=3 ),linetype=FALSE ) +
  scale_color_manual(labels=c("Applications","Sent Ballots", "Returns"),values = c("#156DD0","#C72654", "#C0BA79"),drop=F) +
  theme(title = element_text(size = rel(1.4), family="Styrene B"))
abs_cd7time_series  

ggsave("wi_cd7_abs_timeseries.jpg", plot = abs_cd7time_series, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
###we should also make a cumulative plot 
wi_stateabs_dataframe_cum_cd7 <- subset(wi_stateabs_dataframe_cum, election=="cd7")
wi_stateabs_dataframe_cum_cd7$Date <- as.Date(wi_stateabs_dataframe_cum_cd7$date)
head(wi_stateabs_dataframe_cum_cd7)
###let's just create a long data set 
wi_stateabs_dataframe_cum_cd7long <- gather(wi_stateabs_dataframe_cum_cd7, type, total, abs_applications:ballots_returned)
wi_stateabs_dataframe_cum_cd7long$type2 <- as.factor(wi_stateabs_dataframe_cum_cd7long$type)
##label for primary period 
grob_prim2 <- grobTree(textGrob("Pre-primary \nperiod", x=0.3,  y=0.3, hjust=0,
                               gp=gpar(col="black", fontsize=12, fontface="bold")))
abs_cd7time_series_cum <- ggplot(wi_stateabs_dataframe_cum_cd7long) +
  geom_line( aes(x=Date,y=total,color=type2,linetype=type2),lwd=2) +
  annotate("rect", xmin = as.Date("2020-04-29"), xmax = as.Date("2020-05-12"),ymin=0,ymax=120000,fill="#948DE5",
           alpha = .2)  + annotation_custom(grob_prim2) + theme_minimal() + 
  scale_linetype_manual(values = c(1,3,5)) + scale_size_manual(values = c(2,2,2)) + 
  labs(color="Type",title= "Absentee ballots reported",y="Total")  + 
  guides(color = guide_legend(override.aes = list(linetype = c(1,3,5)),order=3 ),linetype=FALSE ) +
  scale_color_manual(labels=c("Applications","Sent Ballots", "Returns"),values = c("#156DD0","#C72654", "#C0BA79"),drop=F) +
  theme(title = element_text(size = rel(1.4), family="Styrene B"))
abs_cd7time_series_cum  
ggsave("wi_cd7_abs_timeseries_cum.jpg", plot = abs_cd7time_series_cum, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
###let's read in the data here for the new abs file 
options(stringsAsFactors = FALSE)
wicd7abs_file <- read.csv("F:/voterfile/wi/absentee_file_20200817.csv")
wicd7abs_file$full_addr <- paste0(wicd7abs_file$address1, sep=" ", wicd7abs_file$address2)
wicd7abs_file <- wicd7abs_file[,c(1:4,6:8,21:25,27:30,33:37)]
###now let's read in the August data 
###we should also make a cumulative plot 
wi_stateabs_dataframe_cum_aug11 <- subset(wi_stateabs_dataframe_cum, election=="aug11primary")
wi_stateabs_dataframe_cum_aug11$Date <- as.Date(wi_stateabs_dataframe_cum_aug11$date)
head(wi_stateabs_dataframe_cum_aug11)
###let's just create a long data set 
wi_stateabs_dataframe_cum_aug11long <- gather(wi_stateabs_dataframe_cum_aug11, type, total, abs_applications:ballots_returned)
wi_stateabs_dataframe_cum_aug11long$type2 <- as.factor(wi_stateabs_dataframe_cum_aug11long$type)
##label for primary period 
grob_prim2 <- grobTree(textGrob("Pre-primary \nperiod", x=0.3,  y=0.9, hjust=0,
                                gp=gpar(col="black", fontsize=12, fontface="bold")))
abs_aug11time_series_cum <- ggplot(wi_stateabs_dataframe_cum_aug11long) +
  geom_line( aes(x=Date,y=total,color=type2,linetype=type2),lwd=2) +
  annotate("rect", xmin = as.Date("2020-07-06"), xmax = as.Date("2020-08-11"),ymin=0,ymax=1000000,fill="#948DE5",
           alpha = .2)  + annotation_custom(grob_prim2) + theme_minimal() + 
  scale_linetype_manual(values = c(1,3,5)) + scale_size_manual(values = c(2,2,2)) + 
  labs(color="Type",title= "Absentee ballots reported",y="Total")  + 
  guides(color = guide_legend(override.aes = list(linetype = c(1,3,5)),order=3 ),linetype=FALSE ) +
  scale_color_manual(labels=c("Applications","Sent Ballots", "Returns"),values = c("#156DD0","#C72654", "#C0BA79"),drop=F) +
  theme(title = element_text(size = rel(1.4), family="Styrene B"))
abs_aug11time_series_cum  
ggsave("wi_aug11_abs_timeseries_cum.jpg", plot = abs_aug11time_series_cum, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
##########333 Let's looks at the voterfile and absentee data here : 
wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
wi_vf_absaug11 <- read.csv("absentee_file_20200817.csv")
wi_vf_cd7 <- read.csv("F:/voterfile/wi/wi_abs_export.csv")
wi_vf_cd7$voterregnumber <- str_pad(wi_vf_cd7$voterregnumber, width=10,pad="0",side="left")
sort(unique(wi_vf_cd7$electionname))
wi_vf_cd7 <- subset(wi_vf_cd7, electionname=="2020 Special Election Representative in Congress District 7")
nrow(wi_vf_cd7) #111,127
wi_vf_cd7$dum = 1
wi_vf_cd7 <- wi_vf_cd7 %>% group_by(voterregnumber) %>% mutate(dup_voter = sum(dum))
summary(wi_vf_cd7$dup_voter)
wi_vf_cd7dup <- subset(wi_vf_cd7, dup_voter > 1)
length(unique(wi_vf_cd7dup$voterregnumber))##1224 ; need to reduce it to this 
wi_vf_cd7dup$returned <- 0
wi_vf_cd7dup$returned[wi_vf_cd7dup$ballotstatusreason=="Returned"] <- 1
wi_vf_cd7dup <- wi_vf_cd7dup %>% group_by(voterregnumber) %>% mutate(returned_global=sum(returned))
wi_vf_cd7dup <- subset(wi_vf_cd7dup, (returned == 1 & returned_global == 1) | (returned==0 & returned_global==0) )
wi_vf_cd7dup <- wi_vf_cd7dup %>% group_by(voterregnumber) %>% mutate(dup_voter2 = sum(dum))
wi_vf_cd7dup2 <- subset(wi_vf_cd7dup, dup_voter2>1)
wi_vf_cd7dup <- subset(wi_vf_cd7dup, dup_voter2==1)
ncol(wi_vf_cd7)
wi_vf_cd7dup2 <- subset(wi_vf_cd7dup2, ballotstatusreason!="")
wi_vf_cd7dup <- rbind(wi_vf_cd7dup,wi_vf_cd7dup2)
wi_vf_cd7dup <- wi_vf_cd7dup[,c(1:39)]
wi_vf_cd7 <- subset(wi_vf_cd7, dup_voter == 1)
wi_vf_cd7 <- rbind(wi_vf_cd7, wi_vf_cd7dup)
saveRDS(wi_vf_cd7, "wi_cd7absfile.rds" )
#### now let's figure out how the results compare 
table(wi_vf_cd7$ballotstatusreason) # 92375 returned 
table(wi_vf_cd7$ballotdeliverymethod) # 8966 in person 
wi_vf_cd7_ret <- subset(wi_vf_cd7, ballotstatusreason== "Returned")
table(wi_vf_cd7_ret$ballotdeliverymethod)
###let's now look at the times 
View(wi_vf_cd7)


(nrow(wi_vf_cd7_ret)-8944)/191720
8944/191720
(191720 - nrow(wi_vf_cd7_ret))/191720
wi_vf_cd7_ret$voterregnumber <- str_pad(wi_vf_cd7_ret$voterregnumber,width=10,side="left",pad="0")
###what do the results look like for the entire voter file ? 
wi_vf_raw <- readRDS("wi_voterfile_cleaned.Rdata")
wi_vf_raw$Voter.Reg.Number <- str_pad(wi_vf_raw$Voter.Reg.Number, width=10,side="left",pad="0")
wi_vf_rawcd7 <- subset(wi_vf_raw, Congressional=="Congressional - District 7")
###will now look at the times for rthe abs file 
as.numeric(as.Date("01/03/2020",tryFormats = c("%m/%d/%y")))  - as.numeric(as.Date("01/02/2020",tryFormats = c("%m/%d/%y"))) ##good, this works

wi_vf_cd7$num_dateballotsent <- as.numeric(as.Date(wi_vf_cd7$dateballotsent,tryFormats = c("%m/%d/%y")))
wi_vf_cd7$dateballotreturned[wi_vf_cd7$dateballotreturned==""] <- NA
wi_vf_cd7$num_dateballotreturned <- as.numeric(as.Date(wi_vf_cd7$dateballotreturned,tryFormats = c("%m/%d/%y")))

str(wi_vf_cd7$dateballotsent)
str(wi_vf_cd7$dateballotreturned)
wi_vf_cd7$time_gap <- wi_vf_cd7$num_dateballotreturned - wi_vf_cd7$num_dateballotsent
summary(wi_vf_cd7$time_gap)#looks like the dates are mixed for these, so I'll just go ahead and flip. 
wi_vf_cd7$time_gap[wi_vf_cd7$time_gap < 0 & is.na(wi_vf_cd7$time_gap)==F] <- 
  (wi_vf_cd7$num_dateballotsent - wi_vf_cd7$num_dateballotreturned)[wi_vf_cd7$time_gap < 0 & is.na(wi_vf_cd7$time_gap)==F]
##well, that works better. Let's create a histograme then? What's the deal with the 392 days? 
wi_vf_cd7$time_gap[wi_vf_cd7$time_gap==392] <- 26
summary(wi_vf_cd7$time_gap)#appears to work now 
###let's create a geom histogram then
grob_ret <- grobTree(textGrob("Accepted", x=0.7,  y=0.8, hjust=0,
                                gp=gpar(col=medsl_blues[5], fontsize=12, fontface="bold")))
grob_notret <- grobTree(textGrob("Not returned/rejected", x=0.7,  y=0.7, hjust=0,
                              gp=gpar(col=medsl_red[5], fontsize=12, fontface="bold")))
time_gap_hist <- ggplot() +
  geom_histogram(data=subset(wi_vf_cd7,ballotstatusreason== "Returned"),aes(x=time_gap,y = ..density..), fill=medsl_blues[5],alpha=0.6) +
  geom_histogram(data=subset(wi_vf_cd7,ballotstatusreason!= "Returned"),aes(x=time_gap,y = ..density..), fill=medsl_red[5],alpha=0.6) + 
  labs(color="Type",title= "Absentee ballot return times",y="Density", x="Days until ballot return")  + 
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + annotation_custom(grob_ret) + annotation_custom(grob_notret)
ggsave("cd7abs_return_times.png", plot = time_gap_hist, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)  
####get stats on returns and ballots 
wi_vf_cd7$success_count <- 0
wi_vf_cd7$success_count[wi_vf_cd7$ballotreasontype=="" & wi_vf_cd7$ballotstatusreason== "Returned"] <- 1
length(which(wi_vf_cd7$ballotstatusreason== "Returned"))
length(which(wi_vf_cd7$ballotreasontype!=""))
table(wi_vf_cd7$ballotreasontype)
92375/nrow(wi_vf_cd7)
summary(wi_vf_cd7$success_count)
table(wi_vf_cd7$ballotstatusreason)
(10468+526)/length(which(wi_vf_cd7$ballotstatusreason!="Returned"))

unique(wi_vf_cd7$ballotreasontype)


?ggsave
time_gap_hist

View(wi_vf_cd7) 

###let's read in BISG 
wi_bisg <- readRDS("wi_bisg_results.Rdata")
wi_bisg$Voter.Reg.Number <- str_pad(wi_bisg$Voter.Reg.Number, width=10,pad="0",side="left")
wi_vf_rawcd7 <- merge(wi_vf_rawcd7, wi_bisg, by="Voter.Reg.Number", all.x=T,all.y=F)
wi_vf_rawcd7_2018 <- subset(wi_vf_rawcd7, November2018!="")
colSums(wi_vf_rawcd7_2018[,103:107],na.rm=T)
wi_vf_rawcd7_2018abs <- subset(wi_vf_rawcd7_2018, November2018=="Absentee")
wi_vf_rawcd7_2018ed <- subset(wi_vf_rawcd7_2018, November2018=="At Polls")
colSums(wi_vf_rawcd7_2018abs[,103:107],na.rm=T)/colSums(wi_vf_rawcd7_2018[,103:107],na.rm=T)
colSums(wi_vf_rawcd7_2018ed[,103:107],na.rm=T)/colSums(wi_vf_rawcd7_2018[,103:107],na.rm=T)

table()

table(wi_vf_rawcd7$November2018)
table(wi_vf_rawcd7$April2020)

279840/(47768  + 279840 ) # 2018 gen 
82714/(112813 +   82714)
wi_vf_raw2020 <- subset(wi_vf_raw, May2020!="" | April2020 != "" )
wi_vf_raw2020$voted_april <- 0
wi_vf_raw2020$voted_april[wi_vf_raw2020$April2020!=""] <- 1
wi_vf_raw2020$voted_may <- 0
wi_vf_raw2020$voted_may[wi_vf_raw2020$May2020!=""] <- 1
wi_vf_raw2020 <- subset(wi_vf_raw2020, Congressional=="Congressional - District 7")

table(wi_vf_raw2020$voted_may, wi_vf_raw2020$voted_april)
sum(wi_vf_raw2020$voted_april) # 195527
sum(wi_vf_raw2020$voted_may)  # 177890 ; this seems to be off compared to official 191,720
191720 - 177890 # off by 13830 ; odd. Well, let's look at rates 
table(wi_vf_raw2020$April2020)/(112813  +  82714)
###let's reada in the data for april 2020 abs 
april2020abs <- readRDS("wi_abs_file_bisg08072020.Rdata")
april2020abs_ret <- subset(april2020abs,not_returned==0 )
april2020abs_ret$voterregnumber <- str_pad(april2020abs_ret$voterregnumber,width=10,side="left",pad="0")
wi_vf_cd7_dum <- subset(wi_vf_cd7_ret, select = c(voterregnumber))
wi_vf_cd7_dum$cd7 <- 1
april2020abs_ret <- merge(april2020abs_ret, wi_vf_cd7_dum, by="voterregnumber")
april2020abs_ret <- subset(april2020abs_ret, cd7==1)
nrow(april2020abs_ret)

names(april2020abs_ret)
table(april2020abs$not_returned)
View(wi_vf_raw)


###will now drop the data 


######################################################################



medsl_heat <- c("#8D2115","#FF715A","#EBD600","#ADCC18","#37C256") #red to green 
quantile(wi_county_shp$gop_pct_chg.x, seq(0,1,by=0.05))
####plotting the results here 
jpeg("wi_cty_pct_chgplot_man.jpeg", res=300, height = 6, width = 10, units = "in")
par(mfrow=(c(1,2)))
dem_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_manual_dem, weight_mod = 4.1, size_correct = F,
                        title = "Democratic 2020/2016 Turnout"  )
op <- par(family = "StyreneB-Regular")
#par(op)
legend("bottomleft", fill=medsl_purples,
       legend = c("< -25%" , "-25 to -5%", "-5 to 0%", "0 to 10%", "10%+"), title="Dem. % Chg.",
       bty="n", horiz=FALSE, cex=0.7)
gop_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_manual_gop, weight_mod = 4.1, size_correct = F,
                        title = "Republican 2020/2016 Turnout")
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_purples,
       legend = c("< -25%" , "-25 to -5%", "-5 to 0%", "0 to 10%", "10%+"), title="GOP % Chg.",
       bty="n", horiz=FALSE, cex=0.7)
dev.off()
mil_county <- subset(polls_all2c, County=="MILWAUKEE COUNTY")
sum(mil_county$closed)
nrow(mil_county)
###turnout:
sum(wi_county_shp$vote2020dem) # 1850130
sum(wi_county_shp$vote2020gop) # 1260396 
1260396 + 1850130
sum(wi_county_shp$vote2016dem) # 2015200
sum(wi_county_shp$vote2016gop) # 2211888
sum(wi_county_shp$vote2016dem) + sum(wi_county_shp$vote2016gop)
plot(wi_county_shp)
plot(dem_carto)
text(dem_carto, wi_county_shp$CNTY_NAME)
getJenksBreaks(wi_county_shp$dem_pct_chg, 5)
#' carto_plot(state_obj2,state_obj2$total_pop,state_obj2$color,weight_mod = 7, size_correct = TRUE)
#' #add legend after as appropriate 
#' legend("bottomleft", fill=medsl_purples,legend = c("< 59.22%", "59.22 -< 71.05%", "71.05 -< 84.67%", "84.67 -< 91.59%", "91.59% +"), 
#'       title=" ",
#'       bty="n", horiz=FALSE, cex=0.8, ncol=1)

?map_breaks_calc

###polling place analysis here 
nrow(polls_all2c)
sum(polls_all2c$closed)
View(polls_all2c)

wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
wi_vf <- read.csv("wi_voterfile.csv", header = T, skip=1)
names(wi_vf)
head(wi_vf)[1:21]
###let's create an address file 
wi_vf$full_addr <- paste0(wi_vf$Address1, sep=", ", wi_vf$Address2)
###we will now want to subset the data 
setwd(getwd())
wi_vf <- wi_vf[,c(1:4,8:9,17:26,40:118,120)]
saveRDS(wi_vf, "wi_voterfile_cleaned.Rdata")
wi_vf <- readRDS("wi_voterfile_cleaned.Rdata")
wi_vf_addr <- subset(wi_vf, select=c(full_addr, County))
wi_vf_addr <- wi_vf_addr[!duplicated(wi_vf$full_addr), ]
write.dbf(wi_vf_addr, "wi_vf_addrs.dbf")
####let's read in the geocoded voterfile 
wi_geo <- read.csv("F:/voterfile/wi/wi_addr_geo.csv") 
length(which(wi_geo$StName=="")) # 203,434 are based on postal imputation 
length(which(wi_geo$X==0))/nrow(wi_geo) # 5591 need to be further searched. 
nongeocoded_wi <- subset(wi_geo, X==0)
nrow(nongeocoded_wi)
nongeocoded_wi <- subset(nongeocoded_wi, select=c(ObjectID, full_addr, County))
write.csv(nongeocoded_wi, "nongeocoded_wi.csv")
####now we will read in the data for the google geocoded data 
wi_google_addrs <- read.csv("F:/voterfile/wi/google_geocode_wi.csv")
wi_non_google_addrs <- read.csv("F:/voterfile/wi/missing_geocoded_wi.csv")
##we will want to merge by addr, then subset the data 
head(wi_non_google_addrs)
head(wi_google_addrs)
wi_google_addrs <- subset(wi_google_addrs, select=-c(X, address))
wi_geo <- merge(wi_geo, wi_google_addrs, by="full_addr",all.x=T)
nrow(wi_geo)
names(wi_geo)
sum(is.na(wi_geo$X))


wi_geo$X[wi_geo$X==0] <- wi_geo$lon[wi_geo$X==0]
wi_geo$Y[wi_geo$Y==0] <- wi_geo$lat[wi_geo$Y==0]
wi_geo <- subset(wi_geo, select = -c(County.x, ObjectID.y, County.y, lon , lat))
nrow(wi_geo)
wi_vf2 <- merge(wi_vf, wi_geo,  by="full_addr",all.x=T)
sum(is.na(wi_vf2$X)) # 365 missing 
wi_vf2 <- subset(wi_vf2, is.na(X)==F)
###now it is time to read in the CBG data 
library(wru)
cbgs <- readOGR("F:/MEDSL/healthy_elections/general/outside_data/cbg_shps", "CB2010")
census.wi <- get_census_data(key = "b85306550d1fd788ddc045abfa6acf6ba7110abc",
                             state = c("WI"), age = FALSE, sex = FALSE)
cbgs <- subset(cbgs, STATE_FIPS=="55")
cbgs<- spTransform(cbgs, CRS=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
wi_vf_coords1 <- subset(wi_vf2, select=c(X,Y))
sum(is.na(wi_vf_coords1$X))
wi_vf_spdf <- SpatialPointsDataFrame(coords = wi_vf_coords1, data = wi_vf2,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
wi_vf_spdf$county <- over(wi_vf_spdf, cbgs)$STCOFIPS
wi_vf_spdf$tract <- over(wi_vf_spdf, cbgs)$TRACT

colnames(wi_vf_spdf@data)[colnames(wi_vf_spdf@data)=="LastName"] <- "surname"
wi_vf2 <- wi_vf_spdf@data
wi_vf2$state <-"WI"
wi_vf2$county <- substr(wi_vf2$county,3,5)
sort(unique(wi_vf2$county))
head(wi_vf2$county)
plot(wi_vf_spdf)
sum(is.na(wi_vf2$tract))
sum(is.na(wi_vf2$county))


head(wi_vf2$tract)
wi_vf2 <- subset(wi_vf2, is.na(tract)==F)
names(wi_vf2)
wi_vf2$state <-"WI"

wi_vf2 <- subset(wi_vf2, select = c(Voter.Reg.Number,surname,X,Y,county,tract,state))
wi_vf2 <- predict_race(voter.file = wi_vf2, census.geo = "tract", census.data = census.wi,
                       age = FALSE, sex = FALSE)
saveRDS(wi_vf2,"wi_bisg_results.Rdata")
write.csv(wi_vf2,"wi_bisg_results.csv")
###we should read in here
wi_vf2 <- readRDS("wi_bisg_results.Rdata")

########will now do the histogram
library(ggplot2)
poll_density <- ggplot(polls_all2c_df, aes(x=non_white_pct)) +
  geom_density(data=subset(polls_all2c_df, closed==1), aes(color="#156DD0")) +
  geom_density(data=subset(polls_all2c_df, closed==0), aes(color="#C0BA79")) 
poll_density <- poll_density +  scale_color_discrete(labels=c("Closed", "Opened")) + theme_minimal() +  
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Non-white %") + ylab("Proportion")  +guides(size=FALSE) +
  labs(color="Polling \nPlace")
setwd("F:/MEDSL/healthy_elections/WI")
ggsave("poll_density_race.jpg",poll_density, height = 6, width = 9, units = "in", dpi=600)
###let's now do the results by population 
poll_density_pop <- ggplot(polls_all2c_df, aes(x=log_pop_sqkm)) +
  geom_density(data=subset(polls_all2c_df, closed==1), aes(color="#156DD0")) +
  geom_density(data=subset(polls_all2c_df, closed==0), aes(color="#C0BA79")) 
poll_density_pop <- poll_density_pop +  scale_color_discrete(labels=c("Closed", "Opened")) + theme_minimal() +  
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Logged Pop. per sqkm") + ylab("Proportion")  +guides(size=FALSE) +
  labs(color="Polling \nPlace")
poll_density_pop
setwd("F:/MEDSL/healthy_elections/WI")
ggsave("poll_density_pop.jpg",poll_density_pop, height = 6, width = 9, units = "in", dpi=600)
##############results by party ############3
poll_density_dem <- ggplot(polls_all2c_df, aes(x=dem_pct18)) +
  geom_density(data=subset(polls_all2c_df, closed==1), aes(color="#156DD0")) +
  geom_density(data=subset(polls_all2c_df, closed==0), aes(color="#C0BA79")) 
poll_density_dem <- poll_density_dem +  scale_color_discrete(labels=c("Closed", "Opened")) + theme_minimal() +  
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Democratic %") + ylab("Proportion")  +guides(size=FALSE) +
  labs(color="Polling \nPlace")
poll_density_dem
setwd("F:/MEDSL/healthy_elections/WI")
ggsave("poll_density_dem.jpg",poll_density_dem, height = 6, width = 9, units = "in", dpi=600)


opened <- subset(polls_all2c_df, closed==0)
closed <- subset(polls_all2c_df, closed==1)

quantile(closed$dem_pct18, seq(0,1,by=0.05))
quantile(opened$dem_pct18, seq(0,1,by=0.01))

###ok, now I'll need to get the data by polling place and such 
wi_vf_spdf_mil <- subset(wi_vf_spdf, County=="Milwaukee County" | County=="Milwaukee County Supervisory District 14")
nrow(wi_vf_spdf_mil) # 1,296,383 
wi_vf_spdf_mil <- subset(wi_vf_spdf_mil, select=c( full_addr, Voter.Reg.Number, surname,FirstName,Address1, Address2, ZipCode,                   
                                                    Jurisdiction, DistrictCombo,Ward, Congressional,             
                                                    State.Senate, State.Assembly, County, Voter.Status,Voter.Status.Reason,ApplicationDate,
                                                   ApplicationSource,  IsPermanentAbsentee,  Voter.Type, May2020, April2020,
                                                   February2020, April2019, February2019, November2018, October2018,August2018, June2018,
                                                   May2018,    April2018,      February2018,   January2018, December2017,   April2017,
                                                   February2017,   November2016,August2016,     April2016,      February2016, Loc_name,
                                                   Score, StName, X,Y,county, tract))
wi_vf_spdf_mil <- subset(wi_vf_spdf_mil, Voter.Status!="Inactive" )
nrow(wi_vf_spdf_mil) # 514502


sort(unique(wi_vf_spdf_mil$Voter.Status))
#####now let's find the nearest polling place. We will want to subset 

names(polls_all2c_df)
polls_all2c_df_sub <- subset(polls_all2c_df, select = c(Longitude, Latitude,PollingPlaceAddress,PollingPlaceName,County,closed))
polls_all2c_df_sub <- subset(polls_all2c_df_sub, County=="MILWAUKEE COUNTY")
master_df_mil <- data.frame(stringsAsFactors = FALSE)
for(i in 1:nrow(wi_vf_spdf_mil)){
  temp_cbg <- wi_vf_spdf_mil[i,]
  temp_cbg$long <- coordinates(temp_cbg)[1]
  temp_cbg$lat <- coordinates(temp_cbg)[2]
  temp_cbg <- temp_cbg@data
  temp_merge <- merge(temp_cbg,polls_all2c_df_sub,by=NULL)
  temp_merge$euc_distance <- NA
  for(u in 1:nrow(temp_merge)){
    temp_merge$euc_distance[u] <- distGeo(c(temp_merge$long[u],temp_merge$lat[u]), 
                                          c(temp_merge$Longitude[u], temp_merge$Latitude[u]))
    
  }
  temp_sub <- temp_merge %>% slice(which.min(euc_distance))
  if(nrow(master_df_mil)==0){
    master_df_mil <- temp_sub
  }else{
    master_df_mil <- rbind(master_df_mil, temp_sub)
  }
}
saveRDS(master_df_mil, "master_df_mil07082020.Rdata")
setwd("F:/MEDSL/covid19/cleaned_wi2")
master_df_mil <- readRDS("master_df_mil07082020.Rdata")
nrow(master_df_mil)

polls_all2c_df <- polls_all2c@data
###now we will do teh opened data 
polls_all2c_df_sub <- subset(polls_all2c_df, select = c(Longitude, Latitude,PollingPlaceAddress,PollingPlaceName,closed,County))
polls_all2c_df_sub_opened <- subset(polls_all2c_df_sub, closed==0 & County=="MILWAUKEE COUNTY")
master_df_mil_opened <- data.frame(stringsAsFactors = FALSE)
for(i in nrow(master_df_mil_opened):nrow(wi_vf_spdf_mil)){
  svMisc::progress((i/nrow(wi_vf_spdf_mil))*100)
  temp_cbg <- wi_vf_spdf_mil[i,]
  temp_cbg$long <- coordinates(temp_cbg)[1]
  temp_cbg$lat <- coordinates(temp_cbg)[2]
  temp_cbg <- temp_cbg@data
  temp_merge <- merge(temp_cbg,polls_all2c_df_sub,by=NULL)
  temp_merge$euc_distance <- NA
  for(u in 1:nrow(temp_merge)){
    temp_merge$euc_distance[u] <- distGeo(c(temp_merge$long[u],temp_merge$lat[u]), 
                                          c(temp_merge$Longitude[u], temp_merge$Latitude[u]))
    
  }
  temp_sub <- temp_merge %>% slice(which.min(euc_distance))
  if(nrow(master_df_mil_opened)==0){
    master_df_mil_opened <- temp_sub
  }else{
    master_df_mil_opened <- rbind(master_df_mil_opened, temp_sub)
  }
}
nrow(master_df_mil_opened) # 25861 as of 2:04 PM ; 122763 as of 9:01 AM on 7/9/2020
saveRDS(master_df_mil_opened, "master_df_mil_opened07092020a.Rdata")
master_df_mil_opened <- readRDS("master_df_mil_opened07082020a.Rdata")
###finding distance of nearest polling places 
poll_dist_mat <- data.frame(stringsAsFactors = F)
for(i in 1:nrow(polls_all2c_df_sub)){
  svMisc::progress((i/nrow(polls_all2c_df_sub))*100)
  temp_poll <- polls_all2c_df_sub[i,]
  temp_merge <- merge(temp_poll,polls_all2c_df_sub_opened,by=NULL,suffixes=c("_before","_after"))
  for(u in 1:length(temp_merge)){
    temp_merge$poll_distance[u] <- distGeo(c(temp_merge$Longitude_before[u],temp_merge$Latitude_before[u]), 
                                          c(temp_merge$Longitude_after[u], temp_merge$Latitude_after[u]))
    
  }
  temp_sub_poll <- temp_merge %>% slice_min(poll_distance, n = 3)
  if(nrow(poll_dist_mat)==0){
    poll_dist_mat <- temp_sub_poll
  }else{
    poll_dist_mat <- rbind(poll_dist_mat, temp_sub_poll)
  }
}
###ok, so now we will want to run a quick loop, finding out the distance between three points and such 
nrow(master_df_mil_opened) # 122763
master_df_mil_opened_merge <- merge(master_df_mil_opened, poll_dist_mat, by.x=c("Longitude","Latitude","PollingPlaceAddress"),
                                    by.y=c("Longitude_before","Latitude_before","PollingPlaceAddress_before"))
master_df_mil_opened_merge_affected  <- subset(master_df_mil_opened_merge, closed==1)
master_df_mil_opened_merge_not_affected <- subset(master_df_mil_opened_merge, closed==0)
nrow(master_df_mil_opened_merge_affected)
nrow(master_df_mil_opened_merge_not_affected) #need to get unique values 
######################
distGeo_mod <- function(lon1,lat1,lon2,lat2){
  distGeo(c(lon1,lat1),c(lon2,lat2))
  #return(value)
}
distGeo_mod(df_sub[1,1],df_sub[1,2],df_sub[1,3],df_sub[1,4])
df_sub <- subset(master_df_mil_opened_merge_affected, select=c(Longitude,Latitude,Longitude_after,Latitude_after))
master_df_mil_opened_merge_affected$new_distance2 <- mapply(distGeo_mod, df_sub[,1],df_sub[,2],df_sub[,3],df_sub[,4] )
master_df_mil_opened_merge_affected$new_distance2 <- sapply(df_sub,
                                                            function(w,x,y,z) distGeo_mod(w,x,y,z))
summary(master_df_mil_opened_merge_affected$new_distance2)
master_df_mil_opened_merge_affected2 <- master_df_mil_opened_merge_affected %>% group_by(Voter.Reg.Number) %>% slice(which.min(new_distance2))
nrow(master_df_mil_opened_merge_affected2)
master_df_mil_opened_merge_not_affected2 <- 
  master_df_mil_opened_merge_not_affected[!duplicated(master_df_mil_opened_merge_not_affected$Voter.Reg.Number),]
###now grabbing the fields of interest so as to merge on 
master_df_mil_opened_merge_not_affected2 <- subset(master_df_mil_opened_merge_not_affected2, select=c(Voter.Reg.Number,euc_distance))
master_df_mil_opened_merge_not_affected2$new_distance2 <- master_df_mil_opened_merge_not_affected2$euc_distance
master_df_mil_opened_merge_affected2 <- subset(master_df_mil_opened_merge_affected2, select=c(Voter.Reg.Number,euc_distance,new_distance2))
summary(master_df_mil_opened_merge_affected2$new_distance2)
summary(master_df_mil_opened_merge_affected2$euc_distance)
master_df_mil_opened_merge2 <- rbind(master_df_mil_opened_merge_affected2,master_df_mil_opened_merge_not_affected2)
master_df_mil_opened_merge2$dist_change <- (master_df_mil_opened_merge2$new_distance2 - master_df_mil_opened_merge2$euc_distance)/1000
master_df_mil_opened_merge2$dist_change[master_df_mil_opened_merge2$dist_change <0] <- 0
summary(master_df_mil_opened_merge2$dist_change)
##excellent! Let's merge now
master_df_mil_opened2 <- merge(master_df_mil_opened, master_df_mil_opened_merge2,by="Voter.Reg.Number")
###now we will find out voting history dummies 
saveRDS(master_df_mil_opened2, "milwaukee_analysis_df07092020.Rdata")
master_df_mil_opened2$voted2016prim <- 0
master_df_mil_opened2$voted2016prim[master_df_mil_opened2$April2016!=""] <- 1
master_df_mil_opened2$voted2018ge <- 0
master_df_mil_opened2$voted2018ge[master_df_mil_opened2$November2018!=""] <- 1
###ok, lets merge on the bisg results 
wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
wi_bisg <- readRDS("wi_bisg_results.Rdata")
names(wi_bisg)
wi_bisg <- subset(wi_bisg, select = c(Voter.Reg.Number,pred.whi))
master_df_mil_opened2 <-merge(master_df_mil_opened2, wi_bisg,by="Voter.Reg.Number")
nrow(master_df_mil_opened2)###good 
master_df_mil_opened2$voted2020all <- 0
master_df_mil_opened2$voted2020all[master_df_mil_opened2$April2020!=""] <- 1
master_df_mil_opened2$voted2020abs <- 0
master_df_mil_opened2$voted2020abs[master_df_mil_opened2$April2020=="Absentee"] <- 1
master_df_mil_opened2$voted2020ip <- 0
master_df_mil_opened2$voted2020ip[master_df_mil_opened2$April2020=="At Polls"] <- 1
###ok, good. Now we should be able to run the analysis, though we will first most likely want to create a spatial df 
master_df_mil_opened2_coords1 <- subset(master_df_mil_opened2, select=c(X,Y))
sum(is.na(wi_vf_coords1$X))
master_df_mil_opened2_spdf <- SpatialPointsDataFrame(coords = master_df_mil_opened2_coords1, data = master_df_mil_opened2,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
###test with a normal logit 
master_df_mil_opened2$dum=1
master_df_mil_opened2 <- master_df_mil_opened2 %>% group_by(PollingPlaceAddress) %>% mutate(poll_n=sum(dum,na.rm=T))
master_df_mil_opened2 <- subset(master_df_mil_opened2, poll_n >= 30)
library(lme4)
test_nonspace_logit <- glmer(voted2020all ~ pred.whi + closed + dist_change  + voted2016prim + voted2018ge + 
                               (1|PollingPlaceAddress),
                           data=master_df_mil_opened2, family = binomial(link = "logit") )
summary(test_nonspace_logit)
test_nonspace_logit_fe <- glm(voted2020all ~ pred.whi + closed + dist_change  + voted2016prim + voted2018ge + 
                               as.factor(PollingPlaceAddress),
                             data=master_df_mil_opened2, family = binomial(link = "logit") )
summary(test_nonspace_logit_fe)

##check if we can exclude na 
library(functional)
test_vcovlog <- vcov(test_nonspace_logit_fe)
dim(test_vcovlog)
test_vcovlog <- test_vcovlog[,-165]
test_vcovlog <- test_vcovlog[-165,]
dim(test_vcovlog)# dropped the row and column of na vals 
sum(is.na(test_vcovlog))
test_ceof_log <- coef(test_nonspace_logit_fe)
test_ceof_log <- test_ceof_log[-length(test_ceof_log)]
###now should be able to do mvrnorm

sum(is.na(test_ceof_log))
test_nonspace_probit <- glmer(voted2020all ~ pred.whi + closed + dist_change + voted2016prim + voted2018ge + (1|Ward),
                           data=master_df_mil_opened2, family = binomial(link = "probit") )
summary(test_nonspace_probit)
test_nonspace_probit_fe <- glm(voted2020all ~ pred.whi + closed + dist_change + voted2016prim + voted2018ge + as.factor(PollingPlaceAddress),
                              data=master_df_mil_opened2, family = binomial(link = "probit") )
summary(test_nonspace_probit_fe)
summary(master_df_mil_opened2$dist_change)
write.csv(master_df_mil_opened2, "master_df_mil_opened2geo_distance_df.csv",row.names = F)
###let's read in the abs file
wi_abs_file <- read.csv("F:/voterfile/wi/Absentee_file_20200702/wi_abs_file.csv")
sort(unique(wi_abs_file$applicationsource))

### will now run the mvrnorm here: 
library(MASS)
set.seed(1337)
summary(master_df_mil_opened2$dist_change)
sim.betas_vote <- mvrnorm(10000,mu=test_ceof_log,Sigma = test_vcovlog)
dist_seq <- seq(0,12.6,by=0.1)
distan_mat_pred_w <- as.matrix(cbind(1,1,1,dist_seq,0,1,1))
distan_mat_pred_nw <- as.matrix(cbind(1,0,1,dist_seq,0,1,1))

sim.betas_vote <- sim.betas_vote[,c(1:6,133)]
xb2_dist_logit_w <- distan_mat_pred_w %*% t(sim.betas_vote)
xb2_dist_logit_w <- inv.logit(xb2_dist_logit_w)
xb2_dist_logit_colw <- apply(xb2_dist_logit_w, 1, quantile, probs=c(0.025,.5,0.975))
xb2_dist_logit_nw <- distan_mat_pred_nw %*% t(sim.betas_vote)
xb2_dist_logit_nw <- inv.logit(xb2_dist_logit_nw)
xb2_dist_logit_colnw <- apply(xb2_dist_logit_nw, 1, quantile, probs=c(0.025,.5,0.975))

xb2_dist_logit_colw
xb2_dist_logit_colnw

###now let's create an awesome plot 
setwd("F:/MEDSL/covid19/cleaned_wi2")
jpeg("distance_vote_plot.jpg", res=600, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
plot(dist_seq,xb2_dist_logit_colw[2,], type="l",lty=1, col="#3791FF",ylab="Prob. of Voting",ylim=c(0,1),lwd=2,
     xlab="Change in Polling Place Dist. (km)", main="Effect of Polling Place Distance on Voting", cex.lab=1.2)
lines(dist_seq,xb2_dist_logit_colw[1,], lty=4, col="#C72654")
lines(dist_seq,xb2_dist_logit_colw[3,],lty=4, col="#C72654")
###now for black 
lines(dist_seq,xb2_dist_logit_colnw[2,], lty=1, col="#C0BA79",lwd=2)
lines(dist_seq,xb2_dist_logit_colnw[1,], lty=5, col="#FF6878")
lines(dist_seq,xb2_dist_logit_colnw[3,], lty=5, col="#FF6878")
###legend
legend("topright", 
       c("White","Non-white","95% CI White","95% CI Non-white"), lty=c(1,1,4,5),col=c("#3791FF","#C0BA79", "#C72654","#FF6878"),
       bty="n", horiz=FALSE, cex=0.7)
rug(master_df_mil_opened2$dist_change, lwd=0.01)
dev.off()

white_pred_df <- as.data.frame(cbind(dist_seq,xb2_dist_logit_colw[2,],xb2_dist_logit_colnw[2,]))

#xb2_nonwhite <- nonwhite2stage_mat %*% t(sim.betas_turn_sub)
#xb2_nonwhite_col <- apply(xb2_nonwhite, 1, quantile, probs=c(0.025,.5,.975))
###performing matrix algebra for hlit as well 

###good. Now we should be able to do the spatial probit 
library(spdep)
library(spatialprobit)
Sy8_nb <- knn2nb(knearneigh(master_df_mil_opened2_spdf, k = 5), row.names = master_df_mil_opened2_spdf$Voter.Reg.Number)
nb2_normal <- nb2mat(Sy8_nb, style="W", zero.policy = TRUE)
lw2 <- nb2listw(Sy8_nb, zero.policy = T)
##weight mat 
library(raster)

nb_sparse <- as(nb2_normal, "sparseMatrix")
poll_sarprob2 <- sarprobit(closed ~ log_pop_sqkm  +  non_white_pct  +  dem_pct18 + dem_pct2   + 
                             ,data=polls_all2c,W=nb_sparse)



names(master_df_mil_opened2)




sum(is.na(master_df_mil_opened_merge_affected$new_distance)==F)
nrow(master_df_mil_opened_merge_affected)
nrow(master_df_mil_opened_merge)


summary(master_df_mil_opened$euc_distance)
summary(master_df_mil$euc_distance)
###merging data by voter id 
master_df_mil_opened_sub <- subset(master_df_mil_opened, select=c(Voter.Reg.Number,euc_distance))
master_df_mil_merged <- merge(master_df_mil, master_df_mil_opened_sub, by="Voter.Reg.Number",suffixes=c("_before","_after"))
str(master_df_mil_merged)

###let's look at val for euc distance 
master_df_mil_merged$dist_diff <- master_df_mil_merged$euc_distance_before - master_df_mil_merged$euc_distance_after
summary(master_df_mil_merged$dist_diff)
master_df_mil_merged$euc_distancebefore <- master_df_mil_merged$euc_distancebefore/1000
master_df_mil_merged$euc_distanceafter <- master_df_mil_merged$euc_distanceafter/1000

summary(master_df_mil_merged$euc_distanceafter)
summary(master_df_mil_merged$euc_distancebefore)

###reading in what I think is final data 
poll_test_df <- readRDS("F:/MEDSL/covid19/cleaned_wi2/wi_final_poll_datasp.Rdata")
nrow(poll_test_df)
