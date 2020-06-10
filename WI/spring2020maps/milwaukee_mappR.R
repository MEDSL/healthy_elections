#######################################################################################
################## MEDSL Milwaukee Mapper of Polling Places ########################
######################################################################################
############By John A. Curiel, jcuriel.mit@edu,  5/20/2020############################
###################################################################################
library(rgdal)
library(gtools)
library(geosphere)
library(spdep)
library(spatialprobit)
library(dplyr)
library(stringr)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
library(extrafont)#these allow you to read in the non-default fonts 
library(showtext)
font_import("F:/MEDSL/blogs/styrene_b")  #specify font path here 
windowsFonts(A = windowsFont("styrene_b"))
options(stringsAsFactors = F)
#F:/MEDSL/covid19/cleaned_wi2
wd <- "" #directory name
setwd(wd) #set directory here 
# link to website: https://elections.wi.gov/node/6524
######################reading in data 
##### Step 1: read in the different polling place data 
polls_after <- read.csv("pollingplaces04042020.csv")
polls_after <- subset(polls_after, select=-c(Phone,ElectionName))
polls_after$obs = 1
nrow(polls_after)
polls_after2 <- polls_after %>% group_by(County,Muni,PollingPlaceAddress,Longitude,Latitude) %>% dplyr::summarise(wards_served=sum(obs))
colnames(polls_after2)[4:5] <- c("longitude","latitude")
##the above is a data set with unique addresses. Let's move on and get the data for what was supposed to be open 
polls_before <- read.csv("primary_polling_list2020.csv")
polls_before$multi_wards <- 0
polls_before$multi_wards[str_detect(polls_before$ReportingUnit, "Wards")] <- 1
nrow(polls_before)
nrow(polls_after2)
####now let's subset these data to Milwaukee, then go onto map
polls_before_mil <- subset(polls_before, County=="MILWAUKEE COUNTY" )
polls_after_mil <- subset(polls_after2, County=="MILWAUKEE COUNTY" )
###reaed in the jurisdiction map 
wi_wards <- readOGR(wd, "20122020_Election_Data_with_2018_Wards")
##making points into a spatial dataset
polls_before_mil <- subset(polls_before_mil, is.na(polls_before_mil$Longitude)==F)
polls_before_milcoor <- subset(polls_before_mil, select = c(Longitude,Latitude))
polls_before_mil <- SpatialPointsDataFrame(coords = polls_before_milcoor, data = polls_before_mil,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
polls_after_mil <- subset(polls_after_mil, is.na(polls_after_mil$longitude)==F)
polls_after_milcoor <- subset(polls_after_mil, select = c(longitude,latitude))
polls_after_mil <- SpatialPointsDataFrame(coords = polls_after_milcoor, data = polls_after_mil,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
###now let's set the colors 
###now let's set the colors 
polls_after_mil$color <- "#C72654"
polls_after_mil$color[polls_after_mil$Muni=="CITY OF MILWAUKEE"] <- "#156DD0"
polls_before_mil$color <- "#C72654"
polls_before_mil$color[polls_before_mil$Muni=="CITY OF MILWAUKEE"] <- "#156DD0"
###subsetting the ward data 
wards_mil <- subset(wi_wards,CNTY_NAME=="MILWAUKEE" )
wards_mil$color1 = "white"
wards_mil$color1[wards_mil$MCD_NAME=="MILWAUKEE"] = "#C0BA79"

#########actual map 
jpeg("wi_milwauke_pollingplaces2.jpeg", res=600, height = 12, width = 18, units = "in")
par(mfrow=c(1,2))
plot(wards_mil, main="Scheduled Polling Places for 2020 Spring Election", col=wards_mil$color1 )
plot(polls_before_mil, col=polls_before_mil$color, add=T, pch=19,cex=1.5 )
legend("bottomleft", fill=c("#C72654","#156DD0","#C0BA79"),
       legend = c("Suburban", "City of Milwaukee", "Milwaukee wards"), 
       title="",
       bty="n", horiz=FALSE, cex=1.1, ncol=3)
###2020 plots 
plot(wards_mil, main="Opened Polling Places for 2020 Spring Election",col=wards_mil$color1)
plot(polls_after_mil, col=polls_after_mil$color, add=T, pch=19,cex=1.5 )
#works, though let's go with Cam method 
op <- par(family = "Styrene B")
legend("bottomleft", fill=c("#C72654","#156DD0","#C0BA79"),
       legend = c("Suburban", "City of Milwaukee", "Milwaukee wards"), 
       title="",
       bty="n", horiz=FALSE, cex=1.1, ncol=3)
dev.off()

##note: By using this script, you implicitly agree to support hedgehog welfare programs within your nation, state, or locality. This in no 
#way grants permission to harass hedgehogs, however, who tend to be solitary animals that at max bond with one person. 
