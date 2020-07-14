######################################################################################
##################### WI Voterfile Cleanr and MappR #################################
######################################################################################
library(extrafont)
library(showtext)
font_import("F:/MEDSL/healthy_elections/general/fonts/styrene_b_ttf", prompt = F)
font_import( prompt = F)
windowsFonts(A = windowsFont("StyreneB-Regular"))
windowsFonts(A = windowsFont("styrene_b"))
library(readxl)
library(tidyverse)
library(foreign)
library(dplyr)
library(rgdal)
library(ggplot2)
options(stringsAsFactors = FALSE)
setwd("F:/MEDSL/healthy_elections/WI")
###################################################
wi_vf_wd <- "F:/voterfile/wi"
setwd(wi_vf_wd)
wi_abs_file <- read.csv("F:/voterfile/wi/wi_abs_export.csv")
wi_vf <- readRDS("wi_voterfile_cleaned.Rdata")
names(wi_abs_file) ##good, looks like it was read in successfully 
###good. Now we should be able to merge on the data 
length(unique(wi_abs_file$voterregnumber)) # 1272206
length(which(wi_abs_file$electionname=="2020 Partisan Primary"))
length(which(wi_abs_file$electionname=="2020 Spring Election and Presidential Preference Vote"))
wi_abs_slimmed <- subset(wi_abs_file, electionname=="2020 Spring Election and Presidential Preference Vote" )
wi_abs_slimmed <- subset(wi_abs_slimmed, select=c(voterregnumber,ballotdeliverymethod,dateballotsent,dateballotreturned))
wi_vf2 <- merge(wi_vf, wi_abs_slimmed,by.x="Voter.Reg.Number" , by.y="voterregnumber",all.x=T)
nrow(wi_vf2)-nrow(wi_vf) # 9129 extra 
length(unique(wi_vf2$Voter.Reg.Number))
wi_vf2 <- wi_vf2[!duplicated(wi_vf2$Voter.Reg.Number), ]
######now let's get rid of some of the older vote cols, those before 2016
wi_vf2 <- wi_vf2[,-c(43:95)]
###now let's check the voting methods 
table(wi_vf2$April2020)
1169256/(1169256+436248) #good, the numbers add up
table(wi_vf2$ballotdeliverymethod)
table(wi_abs_slimmed$ballotdeliverymethod)
###seems strange that not all of the records were matched. Ah well. 
nrow(wi_abs_slimmed) - 5673
####let's create a time series plot
wi_abs_slimmed <- subset(wi_abs_slimmed, dateballotsent!="" )
wi_abs_slimmed$dum=1
wi_abs_slimmed$return_dum <- 1
wi_abs_slimmed$return_dum[wi_abs_slimmed$dateballotreturned==""] <- 0
ts_wi_ballots <- wi_abs_slimmed %>% group_by(dateballotsent) %>% summarise(total_sent=sum(dum,na.rm=T), total_return=sum(return_dum,na.rm=T))
ts_wi_ballots$DATE <- as.Date(ts_wi_ballots$dateballotsent, tryFormats = c("%m/%d/%Y")) #good, this works 
ts_wi_ballots2 <- ts_wi_ballots[1:73, ] 
ts_wi_ballots2 <- ts_wi_ballots2[35:73, ]
###ballot return plot 
ballot_return_ts_plot <- ggplot(ts_wi_ballots2) +
  geom_line(aes(x=DATE,y=total_sent,col="#3791FF"),size=1.4) +
  geom_line(aes(x=DATE,y=total_return,col="#C0BA79"),size=1.4)
ballot_return_ts_plot <- ballot_return_ts_plot + theme_minimal() +   
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Date") + ylab("Number of ballots") + 
  scale_color_discrete(labels=c("Total sent","Total returned")) + labs(color="") + guides(size=NULL)
ballot_return_ts_plot # the three spikes occur on: 3/18, 3/25, 3/30
ggsave("wi_ballot_ts_plot.png", plot = ballot_return_ts_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 400)
###we should now also merge on the data for race estimates 
wi_bisg <- readRDS("wi_bisg_results.Rdata")
names(wi_bisg)
wi_vf2 <- merge(wi_vf2, wi_bisg, by="Voter.Reg.Number",all.x=T)
nrow(wi_vf2)
rm(wi_vf)
####ok, now let's find the abs ballots and such cast by county and all of that . Subset such that April 2020 is filled 
wi_vf2 <- subset(wi_vf2, April2020!="")
wi_vf2$dum=1
wi_vf_collapsed <- wi_vf2 %>% group_by(April2020,County) %>% summarise(total_ballots=sum(dum),white_sum=sum(pred.whi,na.rm=T)) 
wi_vf_collapsed$CNTY_NAME <- str_to_upper(wi_vf_collapsed$County)
wi_vf_collapsed$CNTY_NAME <- str_remove(wi_vf_collapsed$CNTY_NAME, " COUNTY")
wi_vf_collapsed$nonwhite_sum <- wi_vf_collapsed$total_ballots - wi_vf_collapsed$white_sum
###totals for race 
wi_vf_collapsed <- wi_vf_collapsed %>% group_by(County) %>% mutate(white_cty_pop=sum(white_sum,na.rm=T))
wi_vf_collapsed <- wi_vf_collapsed %>% group_by(County) %>% mutate(nonwhite_cty_pop=sum(nonwhite_sum,na.rm=T))

wi_vf_collapsed$white_pct <- (wi_vf_collapsed$white_sum/wi_vf_collapsed$total_ballots)*100
wi_vf_collapsed <- wi_vf_collapsed %>% group_by(County) %>% mutate(county_total=sum(total_ballots,na.rm=T))
wi_vf_collapsed$mode_pct <- (wi_vf_collapsed$total_ballots/wi_vf_collapsed$county_total)*100
wi_vf_collapsed$white_mode_pct <- (wi_vf_collapsed$white_sum/wi_vf_collapsed$county_total)*100
wi_vf_collapsed <- subset(wi_vf_collapsed, County!="")
wi_vf_collapsed_abs <- subset(wi_vf_collapsed, April2020=="Absentee")
wi_vf_collapsed_ip <- subset(wi_vf_collapsed, April2020!="Absentee")
wi_vf_collapsed_ip <- subset(wi_vf_collapsed_ip, select=c(County,total_ballots, white_sum,white_pct,white_mode_pct,mode_pct))
colnames(wi_vf_collapsed_ip) <- c("County","ip_ballots","white_ip","white_ip_pct","white_ip2pct","ip_pct")
wi_vf_collapsed_wide <- merge(wi_vf_collapsed_abs,wi_vf_collapsed_ip,by="County")
wi_vf_collapsed_wide$CNTY_NAME[wi_vf_collapsed_wide$CNTY_NAME=="ST. CROIX"] <- "SAINT CROIX"
sort(unique(wi_vf_collapsed_wide$CNTY_NAME))
#### good. Now let's get in the data by race and such 
wi_county_shp <- readOGR("F:/MEDSL/healthy_elections/WI/shpfiles", "wi_counties_diss")
wi_county_shp$CNTY_NAME <- str_to_upper(wi_county_shp$CNTY_NAME)
wi_county_shp <- merge(wi_county_shp,wi_vf_collapsed_wide, by="CNTY_NAME" )
###let's assign colors now
library(medslcleanR2)
medsl_purple <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")
quantile(wi_county_shp$mode_pct,seq(0,1,by=0.25))
wi_county_shp$color_abs <- medsl_purple[1]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 50 & wi_county_shp$mode_pct <60 ] <- medsl_purple[2]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 60 & wi_county_shp$mode_pct <70 ] <- medsl_purple[3]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 70 & wi_county_shp$mode_pct <80 ] <- medsl_purple[4]
wi_county_shp$color_abs[wi_county_shp$mode_pct >= 80  ] <- medsl_purple[5]
wi_county_shp$nonwhite_vbmsum <- wi_county_shp$total_ballots - wi_county_shp$white_sum
wi_county_shp <- wi_county_shp %>% group_by(County) %>% mutate(nonwhite_cty_pop=sum(nonwhite_vbmsum))
View(wi_county_shp)
quantile(wi_vf_collapsed_abs$nonwhite_pct, seq(0,1,0.05))

setwd("F:/MEDSL/healthy_elections/WI")
jpeg("wi_cty_abs_map.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
wi_abs_carto <- carto_plot(wi_county_shp, log(wi_county_shp$county_total), wi_county_shp$color_abs, weight_mod=4, size_correct=F)
legend("bottomleft", fill=medsl_purple,
       legend = c("< 50%" , "50 - 60%", "60 - 70%", "70 - 80%", "80%+"), title="VBM %.",
       bty="n", horiz=FALSE, cex=0.9)
dev.off()

View(wi_vf_collapsed)


head(wi_abs_file)
