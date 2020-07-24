### maps######

setwd("~/Downloads/GitHub-Tutorial-master/healthy_elections/ME")
library(tidyverse)
library(osrm)
library(sf)
library(jsonlite)
library(httr)

View(all_counted)


maine_shape<- st_read("/Users/jessesmac/Downloads/Maine_Boundaries_Town_and_Townships_Polygon_Dissolved-shp/Maine_Boundaries_Town_and_Townships_Polygon_Dissolved.shp")
maine_shape$TOWN<- toupper(as.character(maine_shape$TOWN))
all_counted$TOWN<- as.character(all_counted$MUNICIPALITY)
maine_shape<- merge(maine_shape, all_counted, by = "TOWN", all.x = T)
shapefile_df <- fortify(maine_shape)
sort(unique(maine_shape$TOWN))
sort(unique(all_counted$TOWN))


all_counted$TOWN


map<- ggplot(data = shapefile_df) +
  geom_sf(aes(fill = perc_rejected20))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")# +ggtitle("Rejected Absentee Votes by Town")
map<- map + theme_minimal() + ggtitle("2020 Maine Absentee  Ballot \n Rejection Rate")  +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
map <- map + guides(fill=guide_legend(title="Rejection Rate 2020"))
map


map2<- ggplot(data = shapefile_df) +
  geom_sf(aes(fill = perc_rejected18))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")# +ggtitle("Rejected Absentee Votes by Town")
map<- map + theme_minimal() + ggtitle("2018 Maine Absentee  \n Ballot Rejection Rate")  +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
map2 <- map + guides(fill=guide_legend(title="Rejection Rate 2018"))
map2




maine_shape$TOWN<- toupper(maine_shape$TOWN)


length(which(maine_shape$TOWN %in% ME_mailfile$MUNICIPALITY))






######################


install.packages("gtools")
library(gtools)
setwd("/Users/jessesmac/Downloads/GitHub-Tutorial-master/healthy_elections/ME/absentee_files")


file <- "/Users/jessesmac/Downloads/GitHub-Tutorial-master/healthy_elections/ME/maine_presapproval.csv"

ME_mailfile <- read.csv(file, header=TRUE)
ME_mailfile$race<-"2020_primary"
View(ME_mailfile)
length(which(ME_mailfile$ACC.OR.REJ == "REJ"))
unique(ME_mailfile$REJRSN)


ME_mailfile$rejnum<- ifelse(as.character(ME_mailfile$ACC.OR.REJ) == "REJ", 1, 0)
ME_mailfile$sbv<- ifelse(ME_mailfile$REJRSN == "SBV", 1, 0)


length(which(ME_mailfile$REJRSN == "BND"))

sum(ME_mailfile$rejnum)


me_rejdates<- ME_mailfile %>% 
  group_by(REQDATE) %>% 
  summarize(rejected_num = sum(rejnum))
#Total Number Of Records: 58734
#Total Requested: 58734
#Total Issued: 58687
##Total Returned: 53496
#Total Rejected: 5935

View(me_rejdates)
me_rejdates$date<- lubridate::mdy(as.character(me_rejdates$REQDATE))

table(ME_mailfile$REJRSN)

ex_plot <- ggplot(data=me_rejdates, aes(x=date, y=rejected_num, group=1)) + #..ncount.. is the actual command, and should not be changed, as this is what keeps the 
  #y axis as a proportion instead of a count 
  geom_vline(xintercept = as.Date(c('2020-03-03'), format="%Y-%m-%d"))+
  geom_line()+
  theme_minimal() 
ex_plot <- ex_plot  + xlab("Date") + theme(title = element_text(size = rel(1.4), family="Styrene B")) 
ex_plot+ggtitle("Rejected Votes by Date") +
  scale_x_date(limits = as.Date(c('2019-09-13', '2020-03-06'), format="%Y-%m-%d"))+
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20), axis.title.y=element_blank())

ME_mailfile$requested<- 1

######## now look at where they were rejected ######

me_municipality<- ME_mailfile %>% 
  group_by(MUNICIPALITY) %>% 
  summarize(rejected_num = sum(rejnum), 
            per_reject = sum(rejnum) / sum(requested))


##### now: make files for all elections. add together. 
mail_files<- list.files("/Users/jessesmac/Downloads/GitHub-Tutorial-master/healthy_elections/ME/absentee_files/")

for(i in 1:length(mail_files)){
  a<- read.table(mail_files[i], sep = "|", header = T)
  print(mail_files[i])
  a$race<-  substr(mail_files[i],1,nchar(mail_files[i])-4) 
  ME_mailfile <- smartbind(ME_mailfile, a)
}
rm(a)
unique()
length(unique(ME_mailfile$VOTER.ID))/nrow(ME_mailfile)
ME_mailfile$r<- 1

t<- ME_mailfile %>% 
  group_by(VOTER.ID) %>% 
  summarise(s = sum(r))
View(t)



##### compare 2016 primary, those one?
#### number of absentee ballots between years 
#### number of rejected ballots between eyars, by municipality

View(p_20)
p_18<- read.table("2018_primary.txt", sep = "|", header = T)
p_20<- read.csv(file, header = T)
p_20$cast <- 1

p_18$cast <- 1

twenty_counted<- p_20 %>% 

  group_by(MUNICIPALITY) %>% 
  summarise(cast20 = sum(cast))
View(eighteen_counted)


eighteen_counted<- p_18 %>% 

  group_by(MUNICIPALITY) %>% 
  summarise(cast18 = sum(cast))

all_counted<- 
  merge(twenty_counted, eighteen_counted, by = "MUNICIPALITY", all = T)
View(all_counted)


all_counted[is.na(all_counted)] <- 0




ex_plot<- ggplot(all_counted, aes(x=cast18, y=cast20)) + geom_point() + xlim(0, 4500) + ylim(0,4500) +  theme_minimal() 
ex_plot <- ex_plot  +  ylab("2020 Absentee Votes") + xlab("2018 Absentee Votes") + ylab("2018 Absentee Votes")+theme(title = element_text(size = rel(1.4), family="Styrene B")) +  geom_abline(intercept = 0, slope = 1)
ex_plot+ggtitle("Absentee Votes by Town") + geom_smooth(method = lm, se = FALSE) +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20))



twenty_uncounted<- p_20 %>% 
  filter(ACC.OR.REJ == "REJ") %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(rejected20 = sum(cast))


eighteen_uncounted<- p_18 %>% 
  filter(ACC.OR.REJ == "REJ") %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(rejected18 = sum(cast))

all_counted2<- 
  merge(twenty_uncounted, eighteen_uncounted, by = "MUNICIPALITY", all = T)
all_counted<- merge(all_counted, all_counted2, by = "MUNICIPALITY", all = T)
all_counted[is.na(all_counted)] <- 0

View(all_counted)
ex_plot2<- ggplot(all_counted, aes(x=rejected18, y=rejected20)) + geom_point() + xlim(0, 100) + ylim(0,500) +  theme_minimal() 
ex_plot2 <- ex_plot2  +  ylab("2020 Rejected Votes") + xlab("2018 Rejected Votes") +theme(title = element_text(size = rel(1.4), family="Styrene B")) +  geom_abline(intercept = 0, slope = 1)
ex_plot2 +ggtitle("Rejected Absentee Votes by Town") + geom_smooth(method = lm, se = FALSE) +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20))

#### towns that were lower in 2020 than 2018:
all_counted$MUNICIPALITY[all_counted$rejected20 < all_counted$rejected18 ]


### where fewwer were cast in 2018 than 2020
all_counted$MUNICIPALITY[all_counted$cast20 < all_counted$cast18]

#######


###### maine_democrat: 


dem_18<- p_18[p_18$P == "D",]
dem_20<- p_20[p_20$P == "D",]


dem_20$cast <- 1

dem_18$cast <- 1

twenty_counted_dem<- dem_20 %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(cast20_dem = sum(cast))
View(eighteen_counted)


eighteen_counted_dem<- dem_18 %>% 
  filter(ACC.OR.REJ == "ACC") %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(cast18_dem = sum(cast))

all_counted_dem<- 
  merge(twenty_counted_dem, eighteen_counted_dem, by = "MUNICIPALITY", all = T)

twenty_uncounted_dem<- dem_20 %>% 
  filter(ACC.OR.REJ == "REJ") %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(rejected20_dem = sum(cast))


eighteen_uncounted_dem<- dem_18 %>% 
  filter(ACC.OR.REJ == "REJ") %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(rejected18_dem = sum(cast))

all_counted2_dem<- 
  merge(twenty_uncounted_dem, eighteen_uncounted_dem, by = "MUNICIPALITY", all = T)
all_counted_dem<- merge(all_counted_dem, all_counted2_dem, by = "MUNICIPALITY", all = T)
all_counted_dem[is.na(all_counted_dem)] <- 0
View(all_counted_dem)


all_counted<- merge(all_counted_dem, all_counted, by = "MUNICIPALITY", all = T)
all_counted[is.na(all_counted)] <- 0
View(all_counted)

#### plot for just dems; cast
ex_plot2<- ggplot(all_counted, aes(x=rejected18_dem, y=rejected20_dem)) + geom_point()  + theme_minimal() 
ex_plot2 <- ex_plot2  +  ylab("2020 Rejected Votes") + xlab("2018 Rejected Votes") +theme(title = element_text(size = rel(1.4), family="Styrene B")) +  geom_abline(intercept = 0, slope = 1)
ex_plot2 +geom_point(aes( size=cast20))+ggtitle("Rejected Absentee Votes by Town") + geom_smooth(method = lm, se = FALSE) +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20))


View(all_counted)
##### this works. 

#### percents
all_counted$perc_rejected18<- all_counted$rejected18/all_counted$cast18
all_counted$perc_rejected20<- all_counted$rejected20/all_counted$cast20

all_counted$perc_rejected18[is.nan(all_counted$perc_rejected18)] <- 0
all_counted$perc_rejected20[is.nan(all_counted$perc_rejected20)] <- 0


all_counted$perc_rejected20[which(!is.finite(all_counted$perc_rejected20))] <- 0
all_counted$perc_rejected18[which(!is.finite(all_counted$perc_rejected18))] <- 0


ex_plot2<- ggplot(all_counted, aes(x=perc_rejected18, y=perc_rejected20)) + ylim(0,1) +theme_minimal() 
ex_plot2 <- ex_plot2  +  ylab("2020 Rejected Votes") + xlab("2018 Rejected Votes") +theme(title = element_text(size = rel(1.4), family="Styrene B")) +  geom_abline(intercept = 0, slope = 1)
ex_plot2<- ex_plot2 +geom_point(aes(size=cast20))+ggtitle("Percent Rejected Absentee Votes by Town") + geom_smooth(method = lm, se = FALSE) +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20))+ guides(size=FALSE,alpha=FALSE) + labs(color="Municipality")
ex_plot2

##### make mapes

ex_plot2<- ggplot(all_counted, aes(x=perc_rejected18, y=perc_rejected20)) + geom_point()  + ylim(0,1) + theme_minimal() 
ex_plot2 <- ex_plot2  +  ylab("2020 Rejected Percent") + xlab("2018 Rejected Percent") +theme(title = element_text(size = rel(1.4), family="Styrene B")) +  geom_abline(intercept = 0, slope = 1)
ex_plot2 +geom_point(aes( size=cast20))+ggtitle("Rejected Absentee Votes by Town") + geom_smooth(method = lm, se = FALSE) +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20)) + guides(size=FALSE,alpha=FALSE) + labs(size= "Votes Cast")

colnames(all_counted)

a<- all_counted[order(-all_counted$cast20),]


a<- a[c(1:10),]

write.csv(a, "top_tentowns.csv")


#### why ballots were rejected 

View(p_20)
summary(p_20$REJRSN)

g<- p_20 %>%
  group_by(MUNICIPALITY) %>% 
  count(REJRSN, sort = TRUE) 

g<- g[g$REJRSN == "BND",]
g<- g[order(-g$n),]
View(g)

write.csv(g[c(1:10),], "g.csv")


wide_DF <- g %>% spread(key=REJRSN, value=n)
wide_DF[is.na(wide_DF)]<- 0

ncol(wide_DF)


wide_df<- merge(wide_DF, all_counted, by = "MUNICIPALITY")
View(wide_df)
colnames(wide_df)
wide_df<- wide_df %>%  select(MUNICIPALITY, BND, ANC, BRU, DBR, ENS, NEN, OTH, RAD, SBV, SNM, VIP, WNC, rejected20 )
wide_df<- wide_df[order(-wide_df$rejected20),]
wide_df<- wide_df[c(1:10),]
write.csv(wide_df, "wide.csv")



View(wide_df)

##### see how many new voters there were
## first; subset by 2020, before 2020
### find how many were in before, not in before
### make tables comparing rejetion rates of new, old voters. 


primary2020<- ME_mailfile[ME_mailfile$race == "2020_primary",]
priorto_2020<- ME_mailfile[!ME_mailfile$race == "2020_primary",]

primary2020$previousvoter<- ifelse(primary2020$VOTER.ID %in% priorto_2020$VOTER.ID, 1, 0)
sum(primary2020$previousvoter)
nrow(primary2020)

newvoters<- length(unique(primary2020$VOTER.ID[primary2020$previousvoter == 0]))
oldvoters<- length(unique(primary2020$VOTER.ID[primary2020$previousvoter == 1]))
newvoters/ (newvoters+oldvoters)

nrow(primary2020[primary2020$ACC.OR.REJ == "REJ" & primary2020$previousvoter == 1,]) / newvoters
nrow(primary2020[primary2020$ACC.OR.REJ == "REJ" & primary2020$previousvoter == 0,]) / oldvoters

nrow(primary2020[primary2020$ACC.OR.REJ == "REJ" & primary2020$previousvoter == 1,])+ nrow(primary2020[primary2020$ACC.OR.REJ == "REJ" & primary2020$previousvoter == 0,])

View(all_counted)



