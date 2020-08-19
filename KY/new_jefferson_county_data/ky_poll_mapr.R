######################################################################
############### Jefferson County Mapping #############################
#######################################################################
library(sp)
library(foreign)
library(rgdal)
library(dplyr)
library(ggplot2)
library(geosphere)
jeff_polls <- readOGR("F:/voterfile/ky/locations", "Jefferson_County_KY_Voting_Locations")
jeff_polls@coords
jeff_polls$longitude <- jeff_polls@coords[,1]
jeff_polls$latitude <- jeff_polls@coords[,2]
View(jeff_polls)

jeff_polls <- jeff_polls@data
jeff_precs <- readOGR("F:/voterfile/ky/precincts","Jefferson_County_KY_Voting_Precincts")

## coordinates for 2020 : 38.022564, -84.504965
plot(jeff_precs)
points(dist_jeff$new_poll_long[1],dist_jeff$new_poll_lat[1],cex=5)
###good. Now we will want to 1) merge the precinct data onto the shpfile, then 2) find distances
setwd("F:/voterfile/ky")
list.files()
jeff2020 <- read_xlsx("pres_primary_dem_2020.xlsx")
jeff2016 <- read_xlsx("pres_primary_dem_2016.xlsx")
float2016 <- jeff2016[624:630,]
jeff2016 <- jeff2016[1:623,]
##let's see if this merges correclty 
jeff_all <- cbind(jeff2016,jeff2020)
colnames(jeff_all)[8] <- "precincts2020"
jeff_all$match_prec <- 0
jeff_all$match_prec[jeff_all$precinct==jeff_all$precincts2020] <- 1
jeff_all$precincts2020 <- substr(jeff_all$precincts2020,1,4)
summary(jeff_all$match_prec) #92% match; let's see why others don't ; looks like everything is good. Let's see if it merges to the shp
nrow(jeff_precs)#excellent, appears standardized 
#####test merge 
jeff_precs <- merge(jeff_precs, jeff_all, by.x="PRECINCT", by.y="precincts2020")
sum(is.na(jeff_precs$tvotes20))#good, it works 
###let's find diff in votes now 
jeff_precs$vote_change <- jeff_precs$tvotes20 - jeff_precs$tvotes16
summary(jeff_precs$vote_change)
###ok, let's now find the diff 
#dist2016jeff <- poll_dist_fxn1(jeff_precs,jeff_polls)#odd, lets just get the data  frame
#class(dist2016jeff)
#dist2016jeff <- dist2016jeff[,c(1:5,7:9,15:16,33,35:37,49)]
#jeff_polls <- jeff_polls@data
#dist_jeff <- dist_jeff[,1:15]
dist_jeff <- poll_dist_fxn2(dist2016jeff,jeff_polls,dist2016jeff$long,dist2016jeff$lat,jeff_polls$longitude,jeff_polls$latitude)
dist_jeff$dist_change <- (dist_jeff$distance2020-dist_jeff$euc_distance)/1000
dist_jeff$new_poll_long <- -85.741452
dist_jeff$new_poll_lat <- 38.199089
dist_jeff$distance2020B <- mapply(distGeo_mod, dist_jeff$long, dist_jeff$lat, dist_jeff$new_poll_long, dist_jeff$new_poll_lat )
dist_jeff$distance2020miles <- dist_jeff$distance2020B*0.000621371
dist_jeff$distance2016miles <- dist_jeff$euc_distance*0.000621371
dist_jeff$poll_dist_change_miles <- dist_jeff$distance2020miles - dist_jeff$distance2016miles
names(dist_jeff)
colnames(dist_jeff)[colnames(dist_jeff)=="longitude"] <- "longitude2016poll"
colnames(dist_jeff)[colnames(dist_jeff)=="latitude"] <- "latitude2016poll"
colnames(dist_jeff)[colnames(dist_jeff)=="long"] <- "longitude_prec_cent"
colnames(dist_jeff)[colnames(dist_jeff)=="lat"] <- "latitude_prec_cent"
dist_jeff <- subset(dist_jeff, select=-c(distance2020B,euc_distance,dist_change))
write.csv(dist_jeff, "jefferson_county_polling_distance_change_df.csv",row.names = FALSE)
dist_jeff <- read.csv( "jefferson_county_polling_distance_change_df.csv")
###will now read in the data 
setwd("F:/voterfile/ky")
jeff_int <- read.dbf("jeff_prec_cbg_int.dbf")
#View(jeff_int)
jeff_int <- subset(jeff_int, FID_jeffer > -1)
jeff_int <- subset(jeff_int, is.na(FIPS)==F)

nrow(jeff_int)
###lets find the prec_cbg overlap and cbg_prec overlap 
jeff_int$prec_cbg_overlap <- jeff_int$area_int/jeff_int$area_prec
summary(jeff_int$prec_cbg_overlap)
jeff_int$cbg_prec_overlap <- jeff_int$area_int/jeff_int$cbg_area
summary(jeff_int$cbg_prec_overlap) #not exactly great, but screw it, let's use it when collapsing down 
jeff_int$pop_wt <- jeff_int$POP2010*jeff_int$cbg_prec_overlap
jeff_int$white_wt <- jeff_int$WHITE*jeff_int$cbg_prec_overlap
####now let's do age 
jeff_int$over65_wt <- (jeff_int$AGE_65_74+jeff_int$AGE_75_84+jeff_int$AGE_85_UP)*jeff_int$cbg_prec_overlap
###let's add on the data from social explorer on transportation 
setwd("F:/voterfile/ky")
soc_tranport <- read.csv("jeff_soc_trans.csv")
#         A09005_003:      Public Transportation (Includes Taxicab) 
#             A09005_002:      Car, Truck, or Van
#A09005_009:         Drove Alone
# A09005_010:         Carpooled
#            A10054B_001:   Renter-Occupied Housing Units:
# A10054B_002:      No Vehicle Available
soc_tranport <- soc_tranport[,-c(2:55)]
names(soc_tranport)
length(which(soc_tranport$SE_A09005_001==soc_tranport$SE_A09001_001))#equal 
summary(soc_tranport$SE_A09005_003/soc_tranport$SE_A09005_001)
summary(soc_tranport$SE_A09005_002/soc_tranport$SE_A09005_001)

soc_tranport <- subset(soc_tranport, select=c(Geo_FIPS,SE_A09005_001,SE_A09005_002,SE_A09005_003))
colnames(soc_tranport)[2:4] <- c("transport_pop","drive_pop","public_transit")
###one thing we can do is see whether the 2 fields are greater than the base 
soc_tranport2 <- soc_tranport
soc_tranport2$dirve_and_mass <- soc_tranport2$drive_pop + soc_tranport2$public_transit
length(which(soc_tranport2$dirve_and_mass > soc_tranport2$transport_pop))
nrow(soc_tranport2)
jeff_int <- merge(jeff_int,soc_tranport, by.x="FIPS",by.y="Geo_FIPS" )
###let's create weights 
jeff_int$transport_pop_wt <- jeff_int$transport_pop*jeff_int$cbg_prec_overlap
jeff_int$public_transit_wt <- jeff_int$public_transit*jeff_int$cbg_prec_overlap
jeff_int$drive_pop_wt <- jeff_int$drive_pop*jeff_int$cbg_prec_overlap
#jeff_int <- jeff_int[,-c(81:105)]
summary(jeff_int$drive_pop_wt)
summary(jeff_int$public_transit_wt)
summary(jeff_int$transport_pop_wt)


nrow(jeff_int)
names(jeff_int)
#jeff_int <- jeff_int[,-c(78:84)]
###now let's summarize 
jeff_prec_collapse <- jeff_int %>% group_by(PRECINCT) %>% summarise(total_pop_sum = sum(pop_wt), white_sum=sum(white_wt),
                                                                    over65sum=sum(over65_wt), 
                                                                    transport_pop = sum(transport_pop_wt),
                                                                    public_transit=sum(public_transit_wt),
                                                                    drive_pop = sum(drive_pop_wt))
View(jeff_prec_collapse)
jeff_prec_collapse$white_pct <- (jeff_prec_collapse$white_sum/jeff_prec_collapse$total_pop_sum)*100
jeff_prec_collapse$nonwhite_pct <- 100 - jeff_prec_collapse$white_pct
jeff_prec_collapse$over65pct <- (jeff_prec_collapse$over65sum/jeff_prec_collapse$total_pop_sum)*100
jeff_prec_collapse$PRECINCT <- substr(jeff_prec_collapse$PRECINCT, 1,4)
jeff_prec_collapse$public_transit_pct <- (jeff_prec_collapse$public_transit/jeff_prec_collapse$transport_pop)*100
jeff_prec_collapse$drive_pct <- (jeff_prec_collapse$drive_pop/jeff_prec_collapse$transport_pop)*100

summary(jeff_prec_collapse$public_transit_pct)
summary(jeff_prec_collapse$drive_pct)

nrow(dist_jeff)
dist_jeff2 <- merge(dist_jeff, jeff_prec_collapse, by.x="PRECINCT.x",by.y="PRECINCT")
sum(is.na(dist_jeff2$tvotes16)==T)
dist_jeff2$tvotes16B <- dist_jeff2$tvotes16
dist_jeff2$tvotes16B[dist_jeff2$tvotes16==0] <- 1
dist_jeff2$turnout_pct_chg <- ((dist_jeff2$tvotes20 - dist_jeff2$tvotes16)/dist_jeff2$tvotes16B)*100
summary(dist_jeff2$turnout_pct_chg)
##basic lm 
basic_lm <- lm(turnout_pct_chg ~ poll_dist_change_miles + nonwhite_pct + over65pct ,data=dist_jeff2)
summary(basic_lm)
basic_lm2 <- lm(turnout_pct_chg ~ poll_dist_change_miles + nonwhite_pct + 
                  poll_dist_change_miles*nonwhite_pct + over65pct + 
                  poll_dist_change_miles*over65pct,data=dist_jeff2)
summary(basic_lm2)
###fe model
dist_jeff2$dist_chgXage65over <- dist_jeff2$poll_dist_change_miles*dist_jeff2$over65pct
dist_jeff2$dist_chgXnonwhite <- dist_jeff2$poll_dist_change_miles*dist_jeff2$nonwhite_pct
dist_jeff2$dist_chg_squared <- dist_jeff2$poll_dist_change_miles^2
basic_lm2fe <- lm(turnout_pct_chg ~ poll_dist_change_miles + dist_chg_squared + nonwhite_pct + 
                    dist_chgXnonwhite + over65pct + 
                    dist_chgXage65over + as.factor(VOTE_LOC) ,data=dist_jeff2)
summary(basic_lm2fe)
###re read in map 
jeff_precs <- readOGR("F:/voterfile/ky/precincts","Jefferson_County_KY_Voting_Precincts")
names(jeff_precs@data)
jeff_precs$PRECINCT <- substr(jeff_precs$PRECINCT, 1,4)
jeff_precs <- subset(jeff_precs, select=c(PRECINCT))
jeff_precs <- merge(jeff_precs,dist_jeff2, by.x="PRECINCT", by.y="PRECINCT.x" )
sum(is.na(jeff_precs$LEGISDIST))#good, successful merge 
###now let's create the maps 
medsl_blues <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")

quantile(jeff_precs$nonwhite_pct, seq(0,1,by=0.05))# under 20, 20 - 40, 40 -50, 50 70, 70+ 
jeff_precs$color_nonwhite <- medsl_blues[1]
jeff_precs$color_nonwhite[jeff_precs$nonwhite_pct > 20 & jeff_precs$nonwhite_pct <= 40] <- medsl_blues[2]
jeff_precs$color_nonwhite[jeff_precs$nonwhite_pct > 40 & jeff_precs$nonwhite_pct <= 50] <- medsl_blues[3]
jeff_precs$color_nonwhite[jeff_precs$nonwhite_pct > 50 & jeff_precs$nonwhite_pct <= 70] <- medsl_blues[4]
jeff_precs$color_nonwhite[jeff_precs$nonwhite_pct > 70] <- medsl_blues[5]
###ok, let's do the plot now 
jpeg("jefferson_race.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
ky_race_carto <- carto_plot(jeff_precs, log(jeff_precs$total_pop_sum - jeff_precs$white_sum), 
                            jeff_precs$color_nonwhite, weight_mod=1.5, size_correct=F)
points(-85.741452,38.199089, pch=10, cex=6, lwd=6, col="red")
legend("topleft", fill=medsl_blues,
       legend = c("< 20%" , "20 - 40%", "40 - 50%", "50 - 70%", "70%+"), title="Non-white %",
       bty="n", horiz=FALSE, cex=0.9)
legend("bottomright", pch=10,col="red",
       legend = c("Voting center"), title="",
       bty="n", horiz=FALSE, cex=1.5)
dev.off()
##normal choro 
jpeg("jefferson_race_normal.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
plot(jeff_precs, col=jeff_precs$color_nonwhite)
points(-85.741452,38.199089, pch=10, cex=5, lwd=4, col="red")
legend("topleft", fill=medsl_blues,
       legend = c("< 20%" , "20 - 40%", "40 - 50%", "50 - 70%", "70%+"), title="Non-white %",
       bty="n", horiz=FALSE, cex=0.9)
legend("bottomright", pch=10,col="red",
       legend = c("Voting center"), title="",
       bty="n", horiz=FALSE, cex=1.5)
dev.off()
###now finally age 
medsl_purple <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")

quantile(jeff_precs$over65pct,seq(0,1,by=0.05))
jeff_precs$color65 <- medsl_purple[1]
jeff_precs$color65[jeff_precs$over65pct > 10 & jeff_precs$over65pct <= 20] <- medsl_purple[2]
jeff_precs$color65[jeff_precs$over65pct > 20 & jeff_precs$over65pct <= 30] <- medsl_purple[3]
jeff_precs$color65[jeff_precs$over65pct > 30 & jeff_precs$over65pct <= 40] <- medsl_purple[4]
jeff_precs$color65[jeff_precs$over65pct > 40] <- medsl_purple[5]
###now will do both maps 
jpeg("jefferson65over.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
ky_race_carto <- carto_plot(jeff_precs, log(jeff_precs$over65sum), 
                            jeff_precs$color65, weight_mod=1.5, size_correct=F)
points(-85.741452,38.199089, pch=10, cex=6, lwd=6, col="red")
legend("topleft", fill=medsl_purple,
       legend = c("< 10%" , "10 - 20%", "20 - 30%", "30 - 40%", "40%+"), title="Over 65 %",
       bty="n", horiz=FALSE, cex=0.9)
legend("bottomright", pch=10,col="red",
       legend = c("Voting center"), title="",
       bty="n", horiz=FALSE, cex=1.5)
dev.off()
##onrmal choro
jpeg("jefferson65over_normal.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
plot(jeff_precs, col=jeff_precs$color65)
points(-85.741452,38.199089, pch=10, cex=5, lwd=4, col="red")
legend("topleft", fill=medsl_purple,
       legend = c("< 10%" , "10 - 20%", "20 - 30%", "30 - 40%", "40%+"), title="Over 65 %",
       bty="n", horiz=FALSE, cex=0.9)
legend("bottomright", pch=10,col="red",
       legend = c("Voting center"), title="",
       bty="n", horiz=FALSE, cex=1.5)
dev.off()
############# now create maps by public transit 
round(quantile(jeff_precs$public_transit_pct,seq(0,1,0.05)),2)
medsl_red <- c("#EDD0CB","#F59181","#F6573E","#CD3C2B","#8D2115")

jeff_precs$color_pt <- medsl_red[1]
jeff_precs$color_pt[jeff_precs$public_transit_pct > 0 & jeff_precs$public_transit_pct <= 5] <- medsl_red[2]
jeff_precs$color_pt[jeff_precs$public_transit_pct > 5 & jeff_precs$public_transit_pct <= 10] <- medsl_red[3]
jeff_precs$color_pt[jeff_precs$public_transit_pct > 10 & jeff_precs$public_transit_pct <= 20] <- medsl_red[4]
jeff_precs$color_pt[jeff_precs$public_transit_pct > 20] <- medsl_red[5]
###now we will create normal plot 
jpeg("jefferson_public_transit.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
plot(jeff_precs, col=jeff_precs$color_pt)
points(-85.741452,38.199089, pch=10, cex=5, lwd=4, col="blue")
legend("topleft", fill=medsl_red,
       legend = c("0%" , "0 - 5%", "5 - 10%", "10 - 20%", "20%+"), title="Public Transit %",
       bty="n", horiz=FALSE, cex=0.9)
legend("bottomright", pch=10,col="blue",
       legend = c("Voting center"), title="",
       bty="n", horiz=FALSE, cex=1.5)
dev.off()
###do the same for the drive 
jeff_precs$color_drive <- medsl_red[1]
jeff_precs$color_drive[jeff_precs$drive_pct > 60 & jeff_precs$drive_pct <= 70] <- medsl_red[2]
jeff_precs$color_drive[jeff_precs$drive_pct > 70 & jeff_precs$drive_pct <= 80] <- medsl_red[3]
jeff_precs$color_drive[jeff_precs$drive_pct > 80 & jeff_precs$drive_pct <= 90] <- medsl_red[4]
jeff_precs$color_drive[jeff_precs$drive_pct > 90] <- medsl_red[5]
###now we will create normal plot 
jpeg("jefferson_drive.jpeg", res=500, height = 6, width = 9, units = "in")
par(mfrow=(c(1,1)))
plot(jeff_precs, col=jeff_precs$color_drive)
points(-85.741452,38.199089, pch=10, cex=5, lwd=4, col="blue")
legend("topleft", fill=medsl_red,
       legend = c("< 60%" , "60 - 70%", "70 - 80%", "80 - 90%", "90%+"), title="Drive %",
       bty="n", horiz=FALSE, cex=0.9)
legend("bottomright", pch=10,col="blue",
       legend = c("Voting center"), title="",
       bty="n", horiz=FALSE, cex=1.5)
dev.off()

###turnout plot 
quantile(jeff_precs$turnout_pct_chg,seq(0,1,0.05))
jeff_precs$color_turnout <- medsl_red[1]
jeff_precs$color_turnout[jeff_precs$turnout_pct_chg > 60 & jeff_precs$turnout_pct_chg <= 70] <- medsl_red[2]
jeff_precs$color_turnout[jeff_precs$turnout_pct_chg > 70 & jeff_precs$turnout_pct_chg <= 80] <- medsl_red[3]
jeff_precs$color_turnout[jeff_precs$turnout_pct_chg > 80 & jeff_precs$turnout_pct_chg <= 90] <- medsl_red[4]
jeff_precs$color_turnout[jeff_precs$turnout_pct_chg > 90] <- medsl_red[5]
###let's create the ints for transport mode 
jeff_precs$public_transitXdist <- jeff_precs$public_transit_pct*jeff_precs$poll_dist_change_miles
jeff_precs$driveXdist <- jeff_precs$drive_pct*jeff_precs$poll_dist_change_miles


###basic linear model 
basic_lm2fe <- lm(turnout_pct_chg ~ poll_dist_change_miles  + nonwhite_pct + 
                    dist_chgXnonwhite + over65pct + 
                    dist_chgXage65over +public_transit_pct + as.factor(VOTE_LOC) ,data=jeff_precs@data)
summary(basic_lm2fe)
stargazer(basic_lm2fe)
library(stargazer)
stargazer::stargazer(basic_lm2fe, type="html",out = "jefferson_lmfe.html",
                     covariate.labels = c("Poll Distance Change (mi)", "Non-white %", "Dist X non-white %", 
                                                                                 "Over 65 %", "Dist X over 65 %", "Public Transit %"),
          add.lines = list(c("Fixed effects", "Yes")),
          column.labels = " ", omit="VOTE_LOC", intercept.bottom = TRUE, omit.stat=c("LL","ser","f"))


####now we will want to do a spatial linear model 
###good. Now we should be able to do the spatial probit 
library(spdep)
#library(spatialprobit)
nb2 <- poly2nb(jeff_precs)
nb2[[21]] <- sort(as.integer(c(nb2[[21]], 38)))
nb2[[38]] <- sort(as.integer(c(21, nb2[[38]])))
#par(mai=c(0,0,0,0))
lw2 <- nb2listw(nb2, zero.policy = TRUE)
###spatial model for jefferston
spatial_jefferson1 <- errorsarlm(turnout_pct_chg ~ poll_dist_change_miles +over65pct + nonwhite_pct +
                                   drive_pct + public_transit_pct, data=jeff_precs, lw2, tol.solve=1.0e-30, zero.policy = TRUE)
summary(spatial_jefferson1)

###exporting the data 
write.csv(jeff_precs@data, "jefferson_county_master_data2.csv",row.names = FALSE)


###let's read in the data for NC 
#nc_addrs <- read.csv("F:/voterfile/nc/nc_addrs_geocoded.csv")
#rm(nc_addrs)
#head(nc_addrs)
#nrow(nc_addrs)
