###### WI Ward CleanR ############3
##############################
library(extrafont)
library(showtext)
font_import("F:/MEDSL/healthy_elections/general/fonts/styrene_b_ttf", prompt = F)
font_import( prompt = F)
windowsFonts(A = windowsFont("StyreneB-Regular"))
windowsFonts(A = windowsFont("styrene_b"))
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
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
  scale_color_manual(labels=c("Applications","Returns", "Sent Ballots"),values = c("#156DD0","#C72654", "#C0BA79"),drop=F) +
  theme(title = element_text(size = rel(1.4), family="Styrene B"))
abs_cd7time_series_cum  
ggsave("wi_cd7_abs_timeseries_cum.jpg", plot = abs_cd7time_series_cum, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
###let's read in the data here for the new abs file 
options(stringsAsFactors = FALSE)
wicd7abs_file <- read.csv("F:/voterfile/wi/absentee_file_20200817.csv")
wicd7abs_file$full_addr <- paste0(wicd7abs_file$address1, sep=" ", wicd7abs_file$address2)
wicd7abs_file <- wicd7abs_file[,c(1:4,6:8,21:25,27:30,33:37)]
length(which(wicd7abs_file$ballotstatusreason=="Returned" & wicd7abs_file$ballotdeliverymethod=="Mail"))
(453/187873)*100
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
  scale_color_manual(labels=c("Applications","Returns", "Sent Ballots"),values = c("#156DD0","#C72654", "#C0BA79"),drop=F) +
  theme(title = element_text(size = rel(1.4), family="Styrene B"))
abs_aug11time_series_cum  
ggsave("wi_aug11_abs_timeseries_cum.jpg", plot = abs_aug11time_series_cum, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
##########333 Let's looks at the voterfile and absentee data here : 
wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
wi_vf_absaug11 <- read.csv("absentee_file_20200817.csv")
wi_vf_absaug11 <- subset(wi_vf_absaug11, electionname=="2020 Partisan Primary")
nrow(wi_vf_absaug11) # 346416
####let's get the ward info read in data 
aug11files <- list.files("F:/MEDSL/healthy_elections/WI/aug11data")
aug11files <- aug11files[3:4]
wi_congress_df <- data.frame(stringsAsFactors = F)
for(i in 1:2){
  setwd("F:/MEDSL/healthy_elections/WI/aug11data")
  temp_offices <- read_xlsx(aug11files[i])
  num_offices <- nrow(temp_offices)
  for(j in 1:num_offices){
    temp_name_office <- temp_offices[j,2]
    temp_name_office <- as.character(temp_name_office)
    temp_xl1 <- read_xlsx(aug11files[i],sheet=j+1, skip=10)
    if(colnames(temp_xl1)[1]!="...1"){
      temp_xl1 <- read_xlsx(aug11files[i],sheet=j+1, skip=9)
    }
    temp_xl1 <- temp_xl1[,1:3]
    colnames(temp_xl1)[1:3] <- c("county","ward","total_votes")
    temp_xl1$ward <- str_to_upper(temp_xl1$ward)
    temp_xl1 <- temp_xl1 %>% fill(county)
    temp_xl1 <- subset(temp_xl1, ward != "COUNTY TOTALS:")
    temp_xl1$ward_pos <- str_locate(temp_xl1$ward, "WARD")
    temp_xl1$town <- substr(temp_xl1$ward,1,temp_xl1$ward_pos-2)
    temp_xl1 <- subset(temp_xl1, select=-c(ward_pos))
    temp_xl1$office <- "US HOUSE"
    temp_xl1$race <- temp_name_office
    temp_xl1$race <- str_to_upper(temp_xl1$race)
    temp_xl1$party_pos <- str_locate(temp_xl1$race, "-")
    temp_xl1$party <- substr(temp_xl1$race,temp_xl1$party_pos+2, nchar(temp_xl1$race) )
    temp_xl1 <- subset(temp_xl1, select=-c(party_pos))
    temp_xl1$district <- gsub("[^0-9.-]", "", temp_name_office)
    temp_xl1$file <- aug11files[i]
    if(nrow(wi_congress_df)==0){
      wi_congress_df <- temp_xl1
    }else{
      wi_congress_df <- rbind(wi_congress_df, temp_xl1)
    }
  }
}
###let's save now
wi_congress_df$year <- 2018
wi_congress_df$year[str_detect(wi_congress_df$file,"2020")] <- 2020
wi_congress_df$county <- str_remove(wi_congress_df$county, " COUNTY")
sort(unique(wi_congress_df$county))
###turnout? 
sum(wi_congress_df$total_votes[wi_congress_df$year==2018])
sum(wi_congress_df$total_votes[wi_congress_df$year==2020])

sum(wi_congress_df$total_votes,na.rm=T)
# 1745417

saveRDS(wi_congress_df, "wi_congress_primary.rds")
wi_congress_county <- wi_congress_df %>% group_by(county,party,file) %>% summarise(total_votes=sum(total_votes,na.rm=T))
#wi_congress_county$district <- substr(wi_congress_county$district,1,1)
###let's assign dates 
wi_congress_county$year <- 2018
wi_congress_county$year[str_detect(wi_congress_county$file,"2020")] <- 2020
###good. Now let's do a comparison 
wi_congress_county <- subset(wi_congress_county, party=="DEMOCRATIC" | party=="REPUBLICAN")
wi_dems <- subset(wi_congress_county, party=="DEMOCRATIC")
wi_gops <- subset(wi_congress_county, party=="REPUBLICAN")
wi_dems2018 <- subset(wi_dems, year==2018)
wi_dems <- subset(wi_dems, year==2020)
(866949-295559-50404)/866949
wi_dems2018 <- subset(wi_dems2018, select=c(county, total_votes))
colnames(wi_dems2018)[2] <- "dem2018"
wi_dems <- merge(wi_dems, wi_dems2018, by="county")
colnames(wi_dems)[colnames(wi_dems)=="total_votes"] <- "dem2020"
wi_dems <- subset(wi_dems, select=-c(year))
wi_dems$dem_pct_change <- ((wi_dems$dem2020-wi_dems$dem2018)/wi_dems$dem2018)*100
wi_dems <- subset(wi_dems, select=-c(file))
####let's do the comparison for GOP now 
wi_gops2018 <- subset(wi_gops, year==2018)
wi_gops <- subset(wi_gops, year==2020)
wi_gops2018 <- subset(wi_gops2018, select=c(county, total_votes))
colnames(wi_gops2018)[2] <- "gop2018"
wi_gops <- merge(wi_gops, wi_gops2018, by="county")
colnames(wi_gops)[colnames(wi_gops)=="total_votes"] <- "gop2020"
wi_gops <- subset(wi_gops, select=-c(year,file))
wi_gops$gop_pct_change <- ((wi_gops$gop2020-wi_gops$gop2018)/wi_gops$gop2018)*100
wi_gops <- subset(wi_gops, select=-c(party))
wi_dems <- subset(wi_dems,select=-c(party))
###ok, let's now read in the wi counties data 
wi_county_shp <- readOGR("F:/MEDSL/healthy_elections/WI/shpfiles", "wi_counties_diss")
wi_county_shp$CNTY_NAME <- str_to_upper(wi_county_shp$CNTY_NAME)
wi_county_shp$CNTY_NAME[wi_county_shp$CNTY_NAME=="SAINT CROIX"] <- "ST. CROIX"
wi_county_shp <- merge(wi_county_shp, wi_dems, by.x="CNTY_NAME",by.y="county")
wi_county_shp <- merge(wi_county_shp, wi_gops, by.x="CNTY_NAME",by.y="county")
###let's look at quantiles now 
quantile(wi_county_shp$dem_pct_change,seq(0,1,by=0.05),na.rm=T)
quantile(wi_county_shp$gop_pct_change,seq(0,1,by=0.05),na.rm=T)
# -50, -25, 0, 25, 50,
medsl_blues <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")
wi_county_shp$color_dem <- "lightgray"
wi_county_shp$color_dem[wi_county_shp$dem_pct_change <= -25] <- medsl_blues[1]
wi_county_shp$color_dem[wi_county_shp$dem_pct_change > -25 & wi_county_shp$dem_pct_change <= 0 ] <- medsl_blues[2]
wi_county_shp$color_dem[wi_county_shp$dem_pct_change > 0 & wi_county_shp$dem_pct_change <= 25 ] <- medsl_blues[3]
wi_county_shp$color_dem[wi_county_shp$dem_pct_change > 25 & wi_county_shp$dem_pct_change <= 50 ] <- medsl_blues[4]
wi_county_shp$color_dem[wi_county_shp$dem_pct_change > 50  ] <- medsl_blues[5]
####now gop 
wi_county_shp$color_gop <- "lightgray"
wi_county_shp$color_gop[wi_county_shp$gop_pct_change <= -25] <- medsl_blues[1]
wi_county_shp$color_gop[wi_county_shp$gop_pct_change > -25 & wi_county_shp$gop_pct_change <= -0 ] <- medsl_blues[2]
wi_county_shp$color_gop[wi_county_shp$gop_pct_change > 0 & wi_county_shp$gop_pct_change <= 25 ] <- medsl_blues[3]
wi_county_shp$color_gop[wi_county_shp$gop_pct_change > 25 & wi_county_shp$gop_pct_change <= 50 ] <- medsl_blues[4]
wi_county_shp$color_gop[wi_county_shp$gop_pct_change > 50 ] <- medsl_blues[5]
##filling na vals 
wi_county_shp$log_dem <- log(wi_county_shp$dem2020)
wi_county_shp$log_dem[is.na(wi_county_shp$dem2020)==T] <- 0.1
wi_county_shp$log_gop <- log(wi_county_shp$gop2020)
wi_county_shp$log_gop[is.na(wi_county_shp$gop2020)==T] <- 0.1
###now lets do a carto plot
jpeg("wi_congress_dem_pct_chg.jpeg", res=400, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
par(mar = c(1, 1, 1, 1))
dem_carto <- carto_plot(wi_county_shp, wi_county_shp$log_dem, wi_county_shp$color_dem, weight_mod = 4.1, size_correct = F,
                        title = "Democratic 2020/2018 Turnout"  )
op <- par(family = "StyreneB-Regular")
#par(op)
legend("bottomleft", fill=medsl_blues,
       legend = c("< -25%" , "-25 to 0%", "0 to 25%", "25 to 50%", "50%+"), title="Dem. % Chg.",
       bty="n", horiz=FALSE, cex=1.1, xpd = TRUE)
dev.off()

jpeg("wi_congress_gop_pct_chg.jpeg", res=400, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
par(mar = c(1, 1, 1, 1))
gop_carto <- carto_plot(wi_county_shp, wi_county_shp$log_gop, wi_county_shp$color_gop, weight_mod = 4.1, size_correct = F,
                        title = "Democratic 2020/2018 Turnout"  )
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_blues,
       legend = c("< -25%" , "-25 to 0%", "0 to 25%", "25 to 50%", "50%+"), title="GOP % Chg.",
       bty="n", horiz=FALSE, cex=1.1, xpd = TRUE)
dev.off()
####skipping scatterplot; moving onto returns for aug 11 





sum(wi_county_shp$dem2020,na.rm=T)
sum(wi_county_shp$dem2018,na.rm=T)
sum(wi_county_shp$dem2020,na.rm=T) - sum(wi_county_shp$dem2018,na.rm=T)
sum(wi_county_shp$gop2020,na.rm=T)
sum(wi_county_shp$gop2018,na.rm=T)
sum(wi_county_shp$gop2020,na.rm=T) - sum(wi_county_shp$gop2018,na.rm=T)

##### I'm satisfied with this. Now let's figure out the rates of absentee voting and such in the abs file 
#line 163 where loaded
wi_vf_absaug11$rejected_ballot <- 0
wi_vf_absaug11$rejected_ballot[wi_vf_absaug11$ballotreasontype!=""] <- 1
#now for returned 
wi_vf_absaug11$early_inperson <- 0
wi_vf_absaug11$early_inperson[wi_vf_absaug11$ballotdeliverymethod=="Voted In Person"] <- 1
wi_vf_absaug11$returned_ballot <- 0
wi_vf_absaug11$returned_ballot[wi_vf_absaug11$ballotstatusreason=="Returned"] <- 1
###now we should be able to figure out lengths and such 
length(which(wi_vf_absaug11$early_inperson==1 & wi_vf_absaug11$rejected_ballot==0)) # 50404
length(which(wi_vf_absaug11$early_inperson==0 & wi_vf_absaug11$rejected_ballot==0)) # 295559
##now figure out denom 
length(which(wi_vf_absaug11$early_inperson==0 & wi_vf_absaug11$returned_ballot==0))/nrow()
table(wi_vf_absaug11$ballotstatusreason)
239040/(nrow(wi_vf_absaug11)-50404)
###lets get rej rate
table(wi_vf_absaug11$ballotstatusreason) # 239040 returned 
length(which(wi_vf_absaug11$ballotreasontype!=""))
(453/239040)*100

####let's map these data as well 
wi_vf_absaug11$success_counted <- 0
wi_vf_absaug11$success_counted[wi_vf_absaug11$early_inperson == 0 & wi_vf_absaug11$ballotstatusreason=="Returned" & 
                                 wi_vf_absaug11$ballotreasontype==""] <- 1
###let's do a summarize 
aug11sums <- wi_vf_absaug11 %>% group_by(county) %>% summarise(mail_counted = sum(success_counted,na.rm=T),
                                                               returned_ballot= sum(returned_ballot,na.rm=T),
                                                               rejected_ballot=sum(rejected_ballot,na.rm=T),
                                                               early_inperson=sum(early_inperson,na.rm=T))
aug11sums$county <- str_to_upper(aug11sums$county)
aug11sums$county <- str_remove_all(aug11sums$county, " COUNTY")
#wi_county_shp <- wi_county_shp[,1:11]
wi_county_shp <- merge(wi_county_shp,aug11sums,by.x="CNTY_NAME",by.y="county" )
wi_county_shp$twoparty2020vote <- wi_county_shp$dem2020 + wi_county_shp$gop2020
wi_county_shp$mail2020pct <- (wi_county_shp$mail_counted/wi_county_shp$twoparty2020vote)*100
wi_county_shp$early2020pct <- (wi_county_shp$early_inperson/wi_county_shp$twoparty2020vote)*100

summary(wi_county_shp$mail2020pct)
quantile(wi_county_shp$mail2020pct, seq(0,1,by=0.05))
#10,15,20,25,25+
summary(wi_county_shp$early2020pct)
quantile(wi_county_shp$early2020pct, seq(0,1,by=0.05))
#2.5, 5, 7.5, 10, 10+
medsl_purple <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")

###let's do the mail map now

wi_county_shp$mail_color <- medsl_purple[1]
wi_county_shp$mail_color[wi_county_shp$mail2020pct > 10 & wi_county_shp$mail2020pct <= 15] <- medsl_purple[2]
wi_county_shp$mail_color[wi_county_shp$mail2020pct > 15 & wi_county_shp$mail2020pct <= 20] <- medsl_purple[3]
wi_county_shp$mail_color[wi_county_shp$mail2020pct > 20 & wi_county_shp$mail2020pct <= 25] <- medsl_purple[4]
wi_county_shp$mail_color[wi_county_shp$mail2020pct > 25 ] <- medsl_purple[5]
#early 
wi_county_shp$early_color <- medsl_blues[1]
wi_county_shp$early_color[wi_county_shp$early2020pct > 2.5 & wi_county_shp$early2020pct <= 5] <- medsl_blues[2]
wi_county_shp$early_color[wi_county_shp$early2020pct > 5 & wi_county_shp$early2020pct <= 7.5] <- medsl_blues[3]
wi_county_shp$early_color[wi_county_shp$early2020pct > 7.5 & wi_county_shp$early2020pct <= 10] <- medsl_blues[4]
wi_county_shp$early_color[wi_county_shp$early2020pct > 10 ] <- medsl_blues[5]
###now let's produce the maps 

jpeg("wi_aug11vbm_map.jpeg", res=400, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
par(mar = c(1, 1, 1, 1))
dem_carto <- carto_plot(wi_county_shp, log(wi_county_shp$mail_counted), wi_county_shp$mail_color, weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
#par(op)
legend("bottomleft", fill=medsl_purple,
       legend = c("< 10%" , "10 to 15%", "15 to 20%", "20 to 25%", "25%+"), title="VBM %",
       bty="n", horiz=FALSE, cex=1.1, xpd = TRUE)
dev.off()
###now early 
wi_county_shp$log_early <- log(wi_county_shp$early_inperson)
wi_county_shp$log_early[wi_county_shp$log_early < 0] <- 0.1
jpeg("wi_aug11early_map.jpeg", res=400, height = 6, width = 7, units = "in")
par(mfrow=(c(1,1)))
par(mar = c(1, 1, 1, 1))
dem_carto <- carto_plot(wi_county_shp, wi_county_shp$log_early, wi_county_shp$early_color, weight_mod = 4.1, size_correct = F,
                        title = ""  )
op <- par(family = "StyreneB-Regular")
#par(op)
legend("bottomleft", fill=medsl_blues,
       legend = c("< 2.5%" , "2.5 to 5%", "5 to 7.5%", "7.5 to 10%", "10%+"), title="Early %",
       bty="n", horiz=FALSE, cex=1.1, xpd = TRUE)
dev.off()



View(wi_county_shp)
###The ward data is severely missing 



View(aug11sums)


### turnout on ED 
865210 - 295559 - 50404
519247/865210 # 0.6001399 for ED 
295559/865210 # 0.3416038 for mail
50404/865210 # 0.05825638 for early 


table(wi_vf_absaug11$ballotstatusreason)
View(wi_vf_absaug11)
239040/(nrow(wi_vf_absaug11))
length(unique(wi_vf_absaug11$voterregnumber))
###ok, let's see if there is data from the previous primaries 
setwd(wi_vf_wd)
#wi_vf_absold <- read.csv("absentee_file_20200817.csv")
#sort(unique(wi_vf_absold$electionname))#only for 2020 races 
#wi_vf_absaug11 <- subset(wi_vf_absaug11, electionname=="2020 Partisan Primary")
#nrow(wi_vf_absaug11) # 346416



################################################
sort(unique(wi_vf_absaug11$electionname))
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
wi_vf_cd7 <- readRDS("wi_cd7absfile.rds")
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
medsl_red <- c("#EDD0CB","#F59181","#F6573E","#CD3C2B","#8D2115")

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
time_gap_hist <- time_gap_hist + theme_minimal()
time_gap_hist
ggsave("cd7abs_return_times.png", plot = time_gap_hist, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)  
###looking at total rejected 
length(which(wi_vf_cd7$ballotreasontype!=""))
table(wi_vf_cd7$ballotstatusreason) # 92375 returned 
(795/92375)*100

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
###let's find the average time for success v not success here 
success_return=subset(wi_vf_cd7,ballotstatusreason== "Returned")
failed_return=subset(wi_vf_cd7,ballotstatusreason!= "Returned")
###now quants 
quantile(success_return$time_gap, seq(0,1,by=0.05),na.rm=T)
quantile(failed_return$time_gap, seq(0,1,by=0.05),na.rm=T)


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


###let's create an address file 

