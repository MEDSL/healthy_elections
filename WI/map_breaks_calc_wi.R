#' Create a choropleth color scheme for maps based upon jenks breaks
#' 
#' This function takes a dataframe (presumably a spatial object data frame) and assigns medsl themed colors based upon natural 5 jenks breaks. 
#' These colors can then be carried over into a choropleth map, or a population weighted dot map, which is part of the carto_plot function.  
#' @param map_obj The dataframe/spatial object with a dataframe that needs color assignment
#' @param field The dataframe column/vector that will provide the basis for the choropleth map
#' @param color_vec The MEDSL color scheme that will be used for the choropleth map. This argument can be one of five options: "blues",
#' "purples", "reds", "heat" (green to red), and "heat_rev" (red to green). Note that the blue, red, and purple spectrums each go from 
#' light to dark. If no option is entered, then it defaults to blues.  
#' @return The dataframe with the new color scheme column. The column is named "color" 
#' }
#' 
#' @export
#' @examples 
#' state_obj <- readOGR(wd_shp, "counties") # reading in the state shpfile 
#' state_obj <- subset(state_obj, STATEFP=="39") #subset by fip code, which you should have memorized for beer trivia purposes 
#' ##read in the pop data from the acs 
#' acs_counties <- read.csv("county_acs_demos.csv")
#' acs_counties$Geo_FIPS <- str_pad(acs_counties$Geo_FIPS, width = 5,pad="0",side="left")
#' state_obj <- merge(state_obj, acs_counties, by.x="GEOID",by.y="Geo_FIPS")
#' state_obj <- map_breaks_calc(state_obj, state_obj$white_pct,"purples")
#' 
map_breaks_calc_wi <- function(map_obj,field,color_vec="blues"){
  list.of.packages <- c("BAMMtools","rgdal","sp")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(rlang)
  for(pkg in (list.of.packages)){
    eval(bquote(library(.(pkg))))
  }
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
  state_breaks <- round(getJenksBreaks(field, 4),2) #finding breaks 
  map_obj$color <- medsl_pal[1] #setting colors based upon the field of interest 
  map_obj$color[field > state_breaks[1] & field <= state_breaks[2]] <- medsl_pal[2]
  map_obj$color[field > state_breaks[2] & field <= state_breaks[3]] <- medsl_pal[3]
  map_obj$color[field > state_breaks[3] & field <= state_breaks[4]] <- medsl_pal[4]
  map_obj$color[field > state_breaks[4]] <- medsl_pal[5]
  return(map_obj)
}
