###############################################################################################
############### MEDSL Healthy Elections COVID Rate CleanR #################################
##########################################################################################
library(ggplot2)
library(ggalt)
library(grid)
library(dplyr)
library(stringr)
library(BAMMtools) #pkg to create nat jenks breaks 
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
library(extrafont)#these allow you to read in the non-default fonts 
library(showtext)
font_import("F:/MEDSL/blogs/styrene_b")  #specify font path here 
windowsFonts(A = windowsFont("styrene_b"))
options(stringsAsFactors = F)
wd <- "" # set working directory 
setwd(wd)
covid_cases <- read.csv("F:/MEDSL/healthy_elections/general/outside_data/us-counties-nyt.csv") # read in the COVID data from the NYT 
#covid_cases <- readRDS("F:/MEDSL/healthy_elections/general/outside_data/covid_county_ts.rds")
covid_cases <- subset(covid_cases, state=="Texas") ###subset the data to equal the state of interest 
#The fields are : date, county (title case), state (title case), fips (st_fips+county_fips), cases, and deaths.
#note that the deaths are excess deaths above the distribution of deaths given a model that the NYT has  


#### the covid data set is formatted as %y-%m-%d , i.e. 2020-04-21 . Can subset via the following example 
#covid_primary_date <- subset(covid_cases, date=="2020-04-03")

covid_primary_date <- subset(covid_cases, date=="") # set the date to the day of the primary to run a scatterplot of some sort , then merge onto
#the elections data of interest. 

#path to medsl colors: http://www.mit.edu/~medsl/brand/charts/index.html ; use this to choose colors of interest 

####general non-grouped plot 
ts_covid_all <- covid_cases %>% group_by(date) %>% dplyr::summarize(total_cases = sum(cases,na.rm=T),total_deaths=sum(deaths,na.rm=T))
ts_covid_all$Date <- as.Date(ts_covid_all$date)
ts_covid_all <- ts_covid_all %>% mutate(lag_cases=lag(total_cases,default=0))
ts_covid_all$new_cases <- ts_covid_all$total_cases - ts_covid_all$lag_cases
summary(ts_covid_all$new_cases)
ts_covid_all$new_cases[ts_covid_all$new_cases<0] <- 0

sort(unique(ts_covid_all$date))
#exe
as.numeric(sort(unique(ts_covid_all$Date))[39])# soc distance start order
as.numeric(sort(unique(ts_covid_all$Date))[113]) # soc distance end
as.numeric(sort(unique(ts_covid_all$Date))[21])#primary date 1 
as.numeric(sort(unique(ts_covid_all$Date))[154])# primary date 2 

###new plot with rectangle 
cases_plot_all <- ggplot(ts_covid_all, aes(x = Date, y = new_cases)) + 
  geom_line(color="#156DD0", size=1)+  
  annotate("rect", xmin = as.Date("2020-03-21"), xmax = as.Date("2020-06-03"),ymin=0,ymax=15000,fill="#948DE5",
           alpha = .2)+ 
  geom_vline(aes(xintercept=18324, color="#C0BA79" ), linetype=5, show.legend = F,lwd=1.4) + 
  geom_vline(aes(xintercept=18457, color="#F6573E" ), linetype=2, show.legend = F,lwd=1.4)

#
cases_plot_all
grob_start <- grobTree(textGrob("Social \nDistancing \nDuration", x=0.4,  y=0.8, hjust=0,
                                gp=gpar(col="black", fontsize=12, fontface="bold")))
grob_prim <- grobTree(textGrob("Primary", x=0.15,  y=0.4, hjust=0,
                               gp=gpar(col="black", fontsize=12, fontface="bold")))
grob2weeks <-   grobTree(textGrob("Primary \nRunoff", x=0.7,  y=0.8, hjust=0,
                                  gp=gpar(col="black", fontsize=12, fontface="bold")))
cases_plot_all <- cases_plot_all +  annotation_custom(grob_start) +  annotation_custom(grob_prim) + annotation_custom(grob2weeks) +
  labs( title= "COVID-19 Cases", y="New Cases") + theme_minimal()
cases_plot_all <- cases_plot_all + theme(title = element_text(size = rel(1.4), family="Styrene B")) #example plot of new cases 
cases_plot_all

ggsave("covid_tx_plot_example.jpg", plot = cases_plot_all, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)




###for the covid time series plot, you will want to collapse by date, and someother variable of interest. For example, you might plot by 
#set breaks of absentee votes pcts, poll closures, etc, to show how these vary by given election variable  

ts_covidA <- covid_cases %>% group_by(date,var) %>% dplyr::summarize(total_cases = sum(cases,na.rm=T),total_deaths=sum(deaths,na.rm=T))
ts_covidA$Date <- as.Date(ts_covidA$date)
sort(unique(ts_covidA$date)) #use this to figure out what the positions of the dates are. After this, you can then add x intercepts for relevant 
#primary dates 

medsl_purple <- c("#4E4A81","#635E99","#7D76C7","#948DE5","#B1AAFB","#CCC8FC","#DDDBFB")
###this is a cases plot, and the main thing that you will want to change is the variable, which can vary by group, or just sum it overall 
cases_plot <- ggplot(ts_covidA, aes(x = Date, y = total_cases, group=var)) + 
  geom_line(aes(color=var,linetype=var ), size=1)+  
  geom_vline(aes(xintercept=as.numeric(sort(unique(ts_covid$Date))[64]) ), linetype=4, show.legend = F) + 
  geom_vline(aes(xintercept=as.numeric(sort(unique(ts_covid$Date))[77]) ), linetype=5, show.legend = F) + 
  geom_vline(aes(xintercept=as.numeric(sort(unique(ts_covid$Date))[38]) ), linetype=2, show.legend = F)
cases_plot <- cases_plot +  scale_color_manual(values = c("grey10", "grey30","grey50","grey70", "grey80")) # set colors here for the legend; 
#can change as appropriate to medsl colors. Right now, it is set to grey scale at 5 values/cats; might just instead use the other medsl colors,
#i.e. medsl purple 
###these are used to add labels for the x intercepts for dates. Note that you will likely need to play around with these a bit in order to 
#position them, using the x and y options 
grob_start <- grobTree(textGrob("Public VBM \nNotice", x=0.2,  y=0.6, hjust=0,
                                gp=gpar(col="Black", fontsize=12, fontface="bold")))
grob_prim <- grobTree(textGrob("Primary", x=0.5,  y=0.7, hjust=0,
                               gp=gpar(col="Black", fontsize=12, fontface="bold")))
grob2weeks <-   grobTree(textGrob("2 Weeks \npost-primary", x=0.6,  y=0.8, hjust=0,
                                  gp=gpar(col="Black", fontsize=12, fontface="bold")))
cases_plot <- cases_plot +  annotation_custom(grob_start) +  annotation_custom(grob_prim) + annotation_custom(grob2weeks) +
  labs( title= "COVID-19 Cases", y="Total Cases") + theme_minimal()

#####the following is the code for the plot for WI, 
setwd("F:/MEDSL/healthy_elections/WI")
wi_abs <- read.csv("abs_county_data_spring.csv", stringsAsFactors = F)
wi_abs$unreturned_pct <- ((wi_abs$abs_transmitted - wi_abs$abs_returned)/wi_abs$abs_transmitted)*100
summary(wi_abs$unreturned_pct)
abs_breaks <- getJenksBreaks(wi_abs$unreturned_pct, 6)
abs_breaks # now we have ordinal categories 
wi_abs$abs_cat <- 1
wi_abs$abs_cat[wi_abs$unreturned_pct >= abs_breaks[2] & wi_abs$unreturned_pct < abs_breaks[3] ] <- 2
wi_abs$abs_cat[wi_abs$unreturned_pct >= abs_breaks[3] & wi_abs$unreturned_pct < abs_breaks[4] ] <- 3
wi_abs$abs_cat[wi_abs$unreturned_pct >= abs_breaks[4] & wi_abs$unreturned_pct < abs_breaks[5] ] <- 4
wi_abs$abs_cat[wi_abs$unreturned_pct >= abs_breaks[5]  ] <- 5
###now we will want to merge the data
wi_abs$county <- str_remove_all(wi_abs$county, " COUNTY")
covid_cases$county <- str_to_upper(covid_cases$county)
covid_casesB <- merge(covid_cases, wi_abs, by="county",all.x=T)
###now with these categories are established, we will collapse by them. 

ts_covidA <- covid_casesB %>% group_by(date,abs_cat) %>% dplyr::summarize(total_cases = sum(cases,na.rm=T),total_deaths=sum(deaths,na.rm=T))
ts_covidA$Date <- as.Date(ts_covidA$date)
sort(unique(ts_covidA$date)) #use this to figure out what the positions of the dates are. After this, you can then add x intercepts for relevant 
#primary dates 

medsl_purple <- c("#4E4A81","#635E99","#7D76C7","#948DE5","#B1AAFB","#CCC8FC","#DDDBFB")
ts_covidA$abs_unreturned <- factor(ts_covidA$abs_cat, levels=c(1,2,3,4,5),labels=c("Minimal","Some","Moderate","Many","Most"))

###this is a cases plot, and the main thing that you will want to change is the variable, which can vary by group, or just sum it overall 
cases_plot <- ggplot(ts_covidA, aes(x = Date, y = total_cases, group=abs_unreturned)) + 
  geom_line(aes(color=abs_unreturned,linetype=abs_unreturned ), size=1)+  
  geom_vline(aes(xintercept=as.numeric(sort(unique(ts_covid$Date))[64]) ), linetype=4, show.legend = F) + 
  geom_vline(aes(xintercept=as.numeric(sort(unique(ts_covid$Date))[77]) ), linetype=5, show.legend = F) + 
  geom_vline(aes(xintercept=as.numeric(sort(unique(ts_covid$Date))[38]) ), linetype=2, show.legend = F) 
cases_plot = cases_plot + scale_color_manual(values = c("#DDDBFB", "#CCC8FC","#948DE5","#7D76C7", "#4E4A81") )
grob_start <- grobTree(textGrob("Public VBM \nNotice", x=0.2,  y=0.6, hjust=0,
                                gp=gpar(col="Black", fontsize=12, fontface="bold")))
grob_prim <- grobTree(textGrob("Primary", x=0.5,  y=0.7, hjust=0,
                               gp=gpar(col="Black", fontsize=12, fontface="bold")))
grob2weeks <-   grobTree(textGrob("2 Weeks \npost-primary", x=0.6,  y=0.8, hjust=0,
                                  gp=gpar(col="Black", fontsize=12, fontface="bold")))
cases_plot <- cases_plot +  annotation_custom(grob_start) +  annotation_custom(grob_prim) + annotation_custom(grob2weeks) +
  labs( title= "COVID-19 Cases", y="Total Cases", linetype ="Abs. Unreturned", color="Abs. Unreturned") + theme_minimal()
cases_plot = cases_plot  + theme(title = element_text(size = rel(1.1), family="Styrene B"))
cases_plot

###exporting the plots
covid_grid <- ggarrange(cases_plot, cases_plot_all,
                        ncol = 2, nrow = 1)
setwd(wd)
ggsave("covid_wi_plot.jpg", plot = covid_grid, scale = 1,
       width = 24, height = 12, units = c("in"), dpi = 600)
