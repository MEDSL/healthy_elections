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
map_breaks_calc <- function(map_obj,field,color_vec="blues"){
  ###assigning colors here; defaults to blue as it is my favority color 
  if(color_vec=="blues"){
    medsl_pal <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")
  }else if(color_vec=="purples"){
    medsl_pal <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")
  }else if(color_vec=="reds"){
    medsl_pal <- c("#EDD0CB","#F59181","#F6573E","#CD3C2B","#8D2115")
  }else if(color_vec=="heat"){
    medsl_pal <- c("#37C256","#ADCC18","#EBD600","#FF715A","#8D2115") #green to red
  }else if(color_vec=="heat_rev"){
    medsl_pal <- c("#8D2115","#FF715A","#EBD600","#ADCC18","#37C256") #red to green 
  }else{
    medsl_pal <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")
  }
  ###e
  state_breaks <- round(getJenksBreaks(field, 5),2) #finding breaks 
  map_obj$color <- medsl_pal[1] #setting colors based upon the field of interest 
  map_obj$color[field > state_breaks[1] & field <= state_breaks[2]] <- medsl_pal[2]
  map_obj$color[field > state_breaks[2] & field <= state_breaks[3]] <- medsl_pal[3]
  map_obj$color[field > state_breaks[3] & field <= state_breaks[4]] <- medsl_pal[4]
  map_obj$color[field > state_breaks[4]] <- medsl_pal[5]
  return(map_obj)
}
#########################################################
state_obj2 <- map_breaks_calc(state_obj, state_obj$white_pct,"purples")

#state_obj <- subset(state_obj, STATEFP=="39")
carto_plot <- function(spat_obj, pop_vec, color_vec, weight_mod = 5, title="",size_correct=FALSE){
  spat_obj <- spTransform(spat_obj, CRS=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spat_coords <- as.data.frame(coordinates(spat_obj))
  spat_coords <- cbind(spat_coords, pop_vec)
  colnames(spat_coords)[3] <- "pop_field"
  norm1 <- as.data.frame(apply(as.data.frame(spat_coords$pop_field), 
                               2, function(x) (x - min(x))/(max(x) - min(x))))
  norm1 <- norm1*weight_mod
  spat_coords <- cbind(spat_coords,norm1)
  colnames(spat_coords)[4] <- "weight"
  spat_coords$weight[spat_coords$weight==0] <- weight_mod*1e-04
  spat_coords <- cbind(spat_coords, color_vec)
  colnames(spat_coords)[5] <- "color"
  quant_weights <- quantile(spat_coords$weight, seq(0,1,by=0.05))
  spat_coords$weight2 <- 0.05
  spat_coords$weight2[spat_coords$weight >= quant_weights[2] & spat_coords$weight < quant_weights[5] ] <- 0.1
  spat_coords$weight2[spat_coords$weight >= quant_weights[5] & spat_coords$weight < quant_weights[7] ] <- 0.15
  spat_coords$weight2[spat_coords$weight >= quant_weights[7] & spat_coords$weight < quant_weights[9] ] <- 0.2
  spat_coords$weight2[spat_coords$weight >= quant_weights[9] & spat_coords$weight < quant_weights[11] ] <- 0.25
  spat_coords$weight2[spat_coords$weight >= quant_weights[11] & spat_coords$weight < quant_weights[13] ] <- 0.3
  spat_coords$weight2[spat_coords$weight >= quant_weights[13] & spat_coords$weight < quant_weights[15] ] <- 0.35
  spat_coords$weight2[spat_coords$weight >= quant_weights[15] & spat_coords$weight < quant_weights[17] ] <- 0.4
  spat_coords$weight2[spat_coords$weight >= quant_weights[17] & spat_coords$weight < quant_weights[19] ] <- 0.45
  spat_coords$weight2[spat_coords$weight >= quant_weights[19]] <- 0.5
  spat_coords$weight2 <- spat_coords$weight2*weight_mod
  
  spat_coords1 <- subset(spat_coords, select=c(V1,V2))
  cents_coords <- SpatialPointsDataFrame(coords = spat_coords1, data = spat_coords,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  plot(spat_obj)
  if(size_correct==TRUE){
    points(cents_coords$V1,cents_coords$V2, add=TRUE,pch=21,bg=cents_coords$color,cex=cents_coords$weight2)
  }else{
    points(cents_coords$V1,cents_coords$V2, add=TRUE,pch=21,bg=cents_coords$color,cex=cents_coords$weight)
  }
}
round(getJenksBreaks(state_obj2$white_pct,5),2) # grabbing the values for the breaks 
 
carto_plot(state_obj2,state_obj2$total_pop,state_obj2$color,weight_mod = 7, size_correct = TRUE)
##add legend after as appropriate 
legend("bottomleft", fill=medsl_blues,
       legend = c("< 59.22%", "59.22 -< 71.05%", "71.05 -< 84.67%", "84.67 -< 91.59%", "91.59% +"), 
       title=" ",
       bty="n", horiz=FALSE, cex=0.8, ncol=1)


