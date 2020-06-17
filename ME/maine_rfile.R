file <- "/Users/jessesmac/Downloads/GitHub-Tutorial-master/healthy_elections/ME/maine_presapproval.csv"

ME_mailfile <- read.csv(file, header=TRUE)
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

