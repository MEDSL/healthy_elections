
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


p_18<- read.table("2018_primary.txt", sep = "|", header = T)
p_20<- read.csv(file, header = T)
p_20$cast <- 1

p_18$cast <- 1

twenty_counted<- p_20 %>% 
  filter(ACC.OR.REJ == "ACC") %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(cast20 = sum(cast))
View(eighteen_counted)


eighteen_counted<- p_18 %>% 
  filter(ACC.OR.REJ == "ACC") %>% 
  group_by(MUNICIPALITY) %>% 
  summarise(cast18 = sum(cast))

all_counted<- 
  merge(twenty_counted, eighteen_counted, by = "MUNICIPALITY", all = T)
View(all_counted)


all_counted[is.na(all_counted)] <- 0




ex_plot<- ggplot(all_counted, aes(x=cast18, y=cast20)) + geom_point() + xlim(0, 4500) + ylim(0,4500) +  theme_minimal() 
ex_plot <- ex_plot  +  ylab("2020 Absentee Votes") + xlab("2018 Absentee Votes") + ylab("2018 Absentee Votes")+theme(title = element_text(size = rel(1.4), family="Styrene B")) +  geom_abline(intercept = 0, slope = 1)
ex_plot+ggtitle("Absentee Votes by Town") + geom_smooth(method = lm, se = FALSE) +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20), axis.title.y=element_blank())

