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
setwd("F:/MEDSL/healthy_elections/WI/ward2020data")
wiward_files <- list.files()
wiward_files <- wiward_files[2:4]
wi2020 <- data.frame()
for(i in 1:length(wiward_files)){
  temp_xl <- read_xlsx(wiward_files[i])
  colnames(temp_xl)[1:3] <- c("county","ward","total")
  temp_xl <- temp_xl %>% fill(county)
  temp_xl2 <- subset(temp_xl, select=c(county,ward,total,SCATTERING))
  temp_xl2$file <- str_remove_all(wiward_files[i], ".xlsx")
  if(nrow(wi2020)==0){
    wi2020 <- temp_xl2
  }else{
    wi2020 <- rbind(wi2020, temp_xl2)
  }
}
wi2020$ward <- str_to_upper(wi2020$ward)
wi2020$office <- "SUPREME COURT"
wi2020$office[wi2020$file=="gop_pprimary_wards"] <- "PRESIDENT"
wi2020$office[wi2020$file=="dem_pprimary_wards"] <- "PRESIDENT"
wi2020$party[wi2020$file=="gop_pprimary_wards"] <- "REPUBLICAN"
wi2020$party[wi2020$file=="dem_pprimary_wards"] <- "DEMOCRAT"
####
setwd("F:/MEDSL/healthy_elections/WI/ward2016data")
wiward2016files <- list.files()
wiward2016files <- wiward2016files[c(1,3)]
wi2016 <- data.frame()
for(i in 1:length(wiward2016files)){
  temp_xl <- read_xlsx(wiward2016files[i])
  colnames(temp_xl)[1:3] <- c("county","ward","total")
  temp_xl <- temp_xl %>% fill(county)
  temp_xl2 <- subset(temp_xl, select=c(county,ward,total,SCATTERING))
  temp_xl2$file <- str_remove_all(wiward2016files[i], ".xlsx")
  if(nrow(wi2016)==0){
    wi2016 <- temp_xl2
  }else{
    wi2016 <- rbind(wi2016, temp_xl2)
  }
}
wi2016$ward <- str_to_upper(wi2016$ward)
wi2016$office <- "PRESIDENT"
wi2016$party <- "REPUBLICAN"
wi2016$party[wi2016$file=="dem_pprimary2016ward"] <- "DEMOCRAT"
###will now drop the data 
wi2020$total_dum <- 0
wi2020$county <- str_to_upper(wi2020$county)
wi2020$total_dum[str_detect(wi2020$county,"TOTAL" )] <- 1
wi2020$total_dum[str_detect(wi2020$ward,"TOTAL" )] <- 1
wi2020 <- subset(wi2020, total_dum==0)
####
wi2016$total_dum <- 0
wi2016$county <- str_to_upper(wi2016$county)
wi2016$total_dum[str_detect(wi2016$county,"TOTAL" )] <- 1
wi2016$total_dum[str_detect(wi2016$ward,"TOTAL" )] <- 1
wi2016 <- subset(wi2016, total_dum==0)
sum(wi2020$total)
####
wi_ward_all <- merge(wi2020,wi2016,by=c("county","ward","office","party"),all.x=F,all.y=F)
nrow(wi_ward_all)
nrow(wi2016)
nrow(wi2020)
###well, got the scatter plot working. Let's look at the 
wi_ward_all <- wi_ward_all %>% group_by(county, ward, office) %>% summarise(total2020 = sum(total.x,na.rm=T), total2016 = sum(total.y,na.rm=T))
wi_ward_all <- subset(wi_ward_all, county!="Office Totals:")
wi_ward_all <- subset(wi_ward_all, ward!="COUNTY TOTALS:")
wi_ward_all$ward_pos <- sapply(wi_ward_all$ward, function(x) str_locate(x, "WARD")[1])
wi_ward_all$muni <- substr(wi_ward_all$ward, 1, wi_ward_all$ward_pos-2)
sort(unique(wi_ward_all$muni)) #good, it worked 
############### scatter plot 
####example plot  
library(ggplot2)
plot_scatter1 <- ggplot(wi_ward_all, aes(x=total2016,y=total2020,size=total2016,color=factor(county))) + 
  geom_point(aes(alpha=0.3)) + 
  ##subsetting for party and mode, and then also specifying color ; same below
  geom_point(aes(alpha=0.3))
plot_scatter1 <- plot_scatter1 + guides(size=FALSE,alpha=FALSE,color=FALSE) #getting rid of extra legend for size and shading (alpha)
#given that the legend left 
#refers to color, I am telling it to change the color lab to "party", and to make labels of the party names. Note, if I were to keep the other
#legends, then I would replace scale_color_discrete with scale_alpha_discrete, etc. 
plot_scatter1 <- plot_scatter1 + theme_minimal() +  
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Total Turnout 2016") + ylab("Total Turnout 2020") 
#changing the axis names 
coef(lm(total2020 ~ total2016, data = wi_ward_all))
plot_scatter1 <- plot_scatter1 + geom_abline(intercept = 0, slope = 1, colour="black",lwd=0.7,lty=2, show.legend =TRUE) + 
  geom_abline(intercept = 5.0474452, slope = 0.5429014, colour="blue",lwd=1,lty=2, show.legend =TRUE) 
plot_scatter1  
###let's now do it for Milwaukee only 
mil_wards <- subset(wi_ward_all, county=="MILWAUKEE")

###plotting results 
plot_scatter_mil <- ggplot(mil_wards, aes(x=total2016,y=total2020,size=total2016)) + 
  geom_point(data = subset(mil_wards, muni=="CITY OF MILWAUKEE"), aes(alpha=0.3, color="#156DD0")) + 
  ##subsetting for party and mode, and then also specifying color ; same below
  geom_point(data = subset(mil_wards, muni!="CITY OF MILWAUKEE"), aes(alpha=0.3, color="#8D2115"))
plot_scatter_mil <- plot_scatter_mil + guides(size=FALSE,alpha=FALSE) + labs(color="Municipality") + 
  scale_color_discrete(labels=c("City of Milwaukee", "Other"))
#getting rid of extra legend for size and shading (alpha)
#given that the legend left 
#refers to color, I am telling it to change the color lab to "party", and to make labels of the party names. Note, if I were to keep the other
#legends, then I would replace scale_color_discrete with scale_alpha_discrete, etc. 
plot_scatter_mil <- plot_scatter_mil + theme_minimal() +  
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Total Turnout 2016") + ylab("Total Turnout 2020") 
#changing the axis names 
coef(lm(total2020 ~ total2016, data = mil_wards))
plot_scatter_mil <- plot_scatter_mil + geom_abline(intercept = 0, slope = 1, colour="black",lwd=0.7,lty=2, show.legend =TRUE) + 
  geom_abline(intercept = -97.9360010, slope = 0.4638166, colour="blue",lwd=1,lty=2, show.legend =TRUE) 
plot_scatter_mil

####will now run the plots with the catalist data 
setwd("F:/MEDSL/healthy_elections/WI")
wi_catalist <- read.csv("returns_catalist_cleaned.csv")
wi_county_shp <- readOGR("F:/MEDSL/healthy_elections/WI/shpfiles", "wi_counties_diss")
wi_county_shp$CNTY_NAME <- str_to_upper(wi_county_shp$CNTY_NAME)
wi2020cty <- wi2020 %>% group_by(county,office,party) %>% summarise(votes=sum(total,na.rm=T))
wi2020cty_dem <- subset(wi2020cty, party=="DEMOCRAT" & office=="PRESIDENT")
wi2020cty_gop <- subset(wi2020cty, party=="REPUBLICAN" & office=="PRESIDENT")
###
wi2016cty <- wi2016 %>% group_by(county,office,party) %>% summarise(votes=sum(total,na.rm=T))
wi2016cty_dem <- subset(wi2016cty, party=="DEMOCRAT" & office=="PRESIDENT")
wi2016cty_gop <- subset(wi2016cty, party=="REPUBLICAN" & office=="PRESIDENT")
##merge
wi_dems <- merge(wi2020cty_dem,wi2016cty_dem,by="county")
wi_dems <- subset(wi_dems, county!="Office Totals:")
nrow(wi2016cty_dem)
wi_gop <- merge(wi2020cty_gop,wi2016cty_gop,by="county")
wi_gop <- subset(wi_gop, county!="Office Totals:")
#####now let's get the data by county pct change 
wi_dems$pct_change <- ((wi_dems$votes.x-wi_dems$votes.y)/(wi_dems$votes.y))*100
wi_gop$pct_change <- ((wi_gop$votes.x-wi_gop$votes.y)/(wi_gop$votes.y))*100
wi_dems <- subset(wi_dems, select = c(county,votes.x,votes.y,pct_change))
wi_gop <- subset(wi_gop, select = c(county,votes.x,votes.y,pct_change))
colnames(wi_dems)[2:4] <- c("vote2020dem","vote2016dem","dem_pct_chg")
colnames(wi_gop)[2:4] <- c("vote2020gop","vote2016gop","gop_pct_chg")
wi_county_shp <- merge(wi_county_shp,wi_dems, by.x="CNTY_NAME", by.y="county" )
wi_county_shp <- merge(wi_county_shp,wi_gop, by.x="CNTY_NAME", by.y="county" )
####now let's run our awesome dot plot pkg 
wi_county_shp
library(medslcleanR2)
wi_county_shp <- map_breaks_calc_wi(wi_county_shp, wi_county_shp$dem_pct_chg, color_vec = "heat_rev" )
#wi_county_shp <- wi_county_shp[, -c(13:14)]
colnames(wi_county_shp@data)[names(wi_county_shp@data)=="color"] <- "color_dem_heat2"
wi_county_shp <- map_breaks_calc_wi(wi_county_shp, wi_county_shp$gop_pct_chg, color_vec = "heat_rev" )
colnames(wi_county_shp@data)[names(wi_county_shp@data)=="color"] <- "color_gop_heat2"
###good. Now let's run the plot 
#readin acs data as well, and collapse by county 
acs_data <- read.csv("F:/MEDSL/healthy_elections/general/outside_data/county_acs_demos.csv")
acs_wi <- subset(acs_data, Geo_STUSAB=="wi")
acs_wi$county <- str_remove(acs_wi$Geo_NAME, " County")
acs_wi$county <- str_to_upper(acs_wi$county)
acs_wi_cty <- acs_wi %>% group_by(county, Geo_FIPS) %>% summarise(total_pop=sum(total_pop,na.rm=T))
wi_county_shp <- merge(wi_county_shp, acs_wi_cty, by.x="CNTY_NAME",by.y="county")
wi_county_shp$log_pop <- log(wi_county_shp$total_pop)
round(getJenksBreaks(wi_county_shp$dem_pct_chg, 4),2)
round(getJenksBreaks(wi_county_shp$gop_pct_chg, 4),2)

dem_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_dem_heat2, weight_mod = 4.6, size_correct = F  )

gop_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_gop_heat2, weight_mod = 4.6, size_correct = F  )

#####Let's just manually assign colors 
wi_county_shp$color_manual_dem <- "#8D2115"
wi_county_shp$color_manual_dem[wi_county_shp$dem_pct_chg.x >= -25 & wi_county_shp$dem_pct_chg.x < -5] <- "#FF715A"
wi_county_shp$color_manual_dem[wi_county_shp$dem_pct_chg.x >= -5 & wi_county_shp$dem_pct_chg.x < 0] <- "#EBD600"
wi_county_shp$color_manual_dem[wi_county_shp$dem_pct_chg.x >= 0 & wi_county_shp$dem_pct_chg.x < 10] <- "#ADCC18"
wi_county_shp$color_manual_dem[wi_county_shp$dem_pct_chg.x >= 10 ] <- "#37C256"
###GOP 
wi_county_shp$color_manual_gop <- "#8D2115"
wi_county_shp$color_manual_gop[wi_county_shp$gop_pct_chg.x >= -40 & wi_county_shp$gop_pct_chg.x < -30] <- "#FF715A"
wi_county_shp$color_manual_gop[wi_county_shp$gop_pct_chg.x >= -30 & wi_county_shp$gop_pct_chg.x < -20] <- "#EBD600"
wi_county_shp$color_manual_gop[wi_county_shp$gop_pct_chg.x >= -20 & wi_county_shp$gop_pct_chg.x < 10] <- "#ADCC18"
wi_county_shp$color_manual_gop[wi_county_shp$gop_pct_chg.x >= 10 ] <- "#37C256"

dem_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_manual_dem, weight_mod = 4.1, size_correct = F  )
gop_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_manual_gop, weight_mod = 4.1, size_correct = F  )

medsl_pal <- c("#8D2115","#FF715A","#EBD600","#ADCC18","#37C256") #red to green 
quantile(wi_county_shp$gop_pct_chg.x, seq(0,1,by=0.05))
####plotting the results here 
jpeg("wi_cty_pct_chgplot_man.jpeg", res=300, height = 6, width = 10, units = "in")
par(mfrow=(c(1,2)))
dem_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_manual_dem, weight_mod = 4.1, size_correct = F,
                        title = "Democratic 2020/2016 Turnout"  )
op <- par(family = "StyreneB-Regular")
#par(op)
legend("bottomleft", fill=medsl_pal,
       legend = c("< -25%" , "-25 -< -5%", "-5 -< 0%", "0 -< 10%", "10%+"), title="Dem. % Chg.",
       bty="n", horiz=FALSE, cex=0.7)
gop_carto <- carto_plot(wi_county_shp, wi_county_shp$log_pop, wi_county_shp$color_manual_gop, weight_mod = 4.1, size_correct = F,
                        title = "Republican 2020/2016 Turnout")
op <- par(family = "StyreneB-Regular")
legend("bottomleft", fill=medsl_pal,
       legend = c("< -40%" , "-40 -< -30%", "-30 -< -20%", "-20 -< 0%", "0%+"), title="GOP % Chg.",
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
polls_all2c_df_sub <- subset(polls_all2c_df, select = c(Longitude, Latitude,PollingPlaceAddress,PollingPlaceName))
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
###now we will do teh opened data 
polls_all2c_df_sub <- subset(polls_all2c_df, select = c(Longitude, Latitude,PollingPlaceAddress,PollingPlaceName,closed))
polls_all2c_df_sub_opened <- subset(polls_all2c_df_sub, closed==0)
master_df_mil_opened <- data.frame(stringsAsFactors = FALSE)
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

saveRDS(master_df, "cbg_closest_hospital_greatlakes.Rdata")
write.dbf(master_df, "cbg_closest_hospital_greatlakes.dbf")
master_df <- readRDS("cbg_closest_hospital_greatlakes.Rdata")