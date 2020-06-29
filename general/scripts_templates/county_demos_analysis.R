####################################################################################
####################### Script for County Scatterplots #############################
library(dplyr)
library(ggplot2)
######################################################################################
wd_global <- readline(prompt = "Enter file path for working directory: ")
# set working directory to overall healthy elections folder 
# F:\MEDSL\healthy_elections
setwd(wd_global) 
options(stringsAsFactors = FALSE)
###now read in the data
county_acs <- read.csv("county_acs_demos.csv")

county_acs_sub <- subset(county_acs, Geo_STUSAB=="nc" ) #insert lower case abbreviation

###right here I'm reading in NC precicnt data ; change and save as appropriate for your data 
test_data <- read.csv("2016-nc-precincts-primary.csv")
test_data <- subset(test_data, office=="US HOUSE")
test_data <- subset(test_data, mode!="total")
test_data <- test_data %>% group_by(county_name,county_fips, party_simplified,mode) %>% summarise(votes=sum(votes,na.rm=T))
test_data <- test_data %>% group_by(county_name,county_fips, party_simplified) %>% mutate(total_votes=sum(votes,na.rm=T))
test_data$mode_pct = (test_data$votes/test_data$total_votes)*100
head(test_data)
###merge on county data next 
nrow(test_data)
test_data <- merge(test_data, county_acs_sub, by.x="county_fips",by.y="Geo_FIPS",all.x=T)
###medsl colors link: http://www.mit.edu/~medsl/brand/charts/index.html


####example plot  
plot_scatter1 <- ggplot(test_data, aes(x=white_pct,y=mode_pct,size=total_votes)) + #specifying x axis, yaxis, and weight for point size
  geom_point(data = subset(test_data, party_simplified == "REPUBLICAN" & mode=="absentee"), aes(color="#C72654",alpha=0.3)) + 
  ##subsetting for party and mode, and then also specifying color ; same below
  geom_point(data = subset(test_data, party_simplified == "DEMOCRAT" &  mode=="absentee"), aes(color="#3791FF",alpha=0.3))
plot_scatter1 <- plot_scatter1 + guides(size=FALSE,alpha=FALSE) #getting rid of extra legend for size and shading (alpha)
plot_scatter1 <- plot_scatter1 + labs(color="Party") + scale_color_discrete(labels=c("Republican","Democrat")) #given that the legend left 
#refers to color, I am telling it to change the color lab to "party", and to make labels of the party names. Note, if I were to keep the other
#legends, then I would replace scale_color_discrete with scale_alpha_discrete, etc. 
plot_scatter1 <- plot_scatter1 + theme_minimal() +  
  theme(title = element_text(size = rel(1.4), family="Styrene B")) + xlab("Demographic Var %") + ylab("Turnout %") #changing the axis names 
plot_scatter1

ggsave("example_plot.png", plot = plot_scatter1, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
