######################################################################################
################# Georiga Mapper ############################################
library(maps)
library(rgdal)
##############################3
setwd("F:/MEDSL/healthy_elections/GA/reportdata")
list.files()
ga_cty_stats <- read.csv("countylevelstats.csv")
cty_cents <- readRDS("counties_centroids.rds")


health_breaks <- getJenksBreaks(variable,5)
sub_covid$color[sub_covid$variable==0] <- shades[1]
sub_covid$color[sub_covid$variable>health_breaks[1] & sub_covid$variable <= health_breaks[2] ] <- shades[2]
sub_covid$color[sub_covid$variable>health_breaks[2] & sub_covid$variable <= health_breaks[3] ] <- shades[3]
sub_covid$color[sub_covid$variable>health_breaks[3] & sub_covid$variable <= health_breaks[4] ] <- shades[4]
sub_covid$color[sub_covid$variable>health_breaks[4] ] <- shades[5]
###creating legend title 
legend.title <- paste0(legend.title, sep=" ", week1, " of 2020")
#print(health_breaks)
###creating legend text 
# add a legend
legend.text <- c(paste0(round(health_breaks[1], 0)), 
                 paste0("1", sep=" -< ", round(health_breaks[2], 0)),
                 paste0(round(health_breaks[2],0),sep=" -< ", round(health_breaks[3],0)),
                 paste0(round(health_breaks[3],0),sep=" -< ", round(health_breaks[4],0)),
                 paste0(round(health_breaks[4],0), "+"))

##merge data 
###now merge on the data 
spdf2 <- merge(spdf, sub_covid, by.x="GEOID",by.y="fips",all.x=T,all.y=F)

#cents_coords <- SpatialPointsDataFrame(coords = spat_coords1, data = spdf,
#                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
max_x <- max(spdf2$V1,na.rm=TRUE) 
min_x <- min(spdf2$V1,na.rm=TRUE) 
max_y <- max(spdf2$V2,na.rm=TRUE) 
min_y <- min(spdf2$V2,na.rm=TRUE) 
##mapping results here
#map('county', state_choice , fill = TRUE, col = "white",ylim=c(min_y,max_y),mar=c(0,0,0,0))
maps::map('county', state_choice , fill = TRUE, col = "white", ylim=c(min_y-2,max_y+2),mar=c(2,2,2,2) )
points(spdf2$V1,spdf2$V2,pch=21,bg=spdf2$color,cex=spdf2$weight)
#legend("bottom", legend = legend.text, 
#     fill = shades, title = "Per 100k",cex=0.6,bty = "n",ncol=2,xpd=TRUE)
legend(colpos, legend = legend.text, 
       fill = shades, title = "Per 100k",cex=0.6,bty = "n",ncol=colnums,xpd = TRUE)