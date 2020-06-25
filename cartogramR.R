#################################################################################################
############## script for cartogram #########################
library(rgdal)
library(sp)
library(BAMMtools) # for jenks breaks 
wd<- "" # set state working directory
#wd_shp <- "F:/MEDSL/healthy_elections" # set working directory for the shp file for counties 
wd_shp <- ""
setwd(wd_shp)
unzip("counties.zip") #unzipping the shpfile data in the general healthy elections github repository 
state_obj <- readOGR(wd_shp, "counties") # reading in the state shpfile 
state_obj <- subset(state_obj, STATEFP=="") #subset by fip code, which you should have memorized for beer trivia purposes 
###read in the pop data from the acs 
acs_counties <- read.csv("county_acs_demos.csv")
acs_counties$Geo_FIPS <- str_pad(acs_counties$Geo_FIPS, width = 5,pad="0",side="left")
state_obj <- merge(state_obj, acs_counties, by.x="GEOID",by.y="Geo_FIPS")
View(state_obj)
#####breaks assigner
medsl_blues <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")
map_breaks_calc <- function(map_obj,field,color_vec=medsl_blues){
  state_breaks <- getJenksBreaks(field, 5)
  state_breaks <- round(state_breaks, 2)
  map_obj$color <- medsl_blues[1]
  map_obj$color[field > state_breaks[1] & field <= state_breaks[2]] <- medsl_blues[2]
  map_obj$color[field > state_breaks[2] & field <= state_breaks[3]] <- medsl_blues[3]
  map_obj$color[field > state_breaks[3] & field <= state_breaks[4]] <- medsl_blues[4]
  map_obj$color[field > state_breaks[4]] <- medsl_blues[5]
  return(map_obj)
}
#########################################################
state_obj2 <- map_breaks_calc(state_obj, state_obj$white_pct)

#state_obj <- subset(state_obj, STATEFP=="39")
carto_plot <- function(spat_obj, pop_vec, color_vec, weight_mod = 5, title=""){
  spat_obj <- spTransform(spat_obj, CRS=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spat_coords <- as.data.frame(coordinates(spat_obj))
  spat_coords <- cbind(spat_coords, pop_vec)
  colnames(spat_coords)[3] <- "pop_field"
  norm1 <- as.data.frame(apply(as.data.frame(spat_coords$pop_field), 
                               2, function(x) (x - min(x))/(max(x) - min(x))))
  norm1 <- norm1*weight_mod
  spat_coords <- cbind(spat_coords,norm1)
  colnames(spat_coords)[4] <- "weight"
  spat_coords <- cbind(spat_coords, color_vec)
  colnames(spat_coords)[5] <- "color"
  spat_coords1 <- subset(spat_coords, select=c(V1,V2))
  cents_coords <- SpatialPointsDataFrame(coords = spat_coords1, data = spat_coords,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  plot(spat_obj)
  points(cents_coords$V1,cents_coords$V2, add=TRUE,pch=21,bg=cents_coords$color,cex=cents_coords$weight)
}
carto_plot(state_obj2,state_obj2$total_pop,state_obj2$color)


