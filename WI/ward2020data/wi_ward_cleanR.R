###### WI Ward CleanR ############3
##############################
library(readxl)
library(tidyverse)
options(stringsAsFactors = FALSE)
setwd("F:/MEDSL/healthy_elections/WI/ward2020data")
wiward_files <- list.files()
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
wi2016$party[wi2016$file=="dem_pprimary_wards"] <- "DEMOCRAT"
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
