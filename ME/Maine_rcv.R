#### RCV VOTE TABULUATION   ###
setwd("/Users/jessesmac/Downloads/rcv")
a<- list.files("/Users/jessesmac/Downloads/rcv")
y<- data.frame(stringsAsFactors = F)
for(i in 1:length(a)){
  print(i)
  c<- read.csv(a[i])
  y<- rbind(y, c)
}
length(unique(b2$TOWN))

length(unique(y$Precinct))
b <- y
b$pref_voter_id<- 1:nrow(b)
colnames(b)
b<- dplyr::select(b, Precinct, pref_voter_id, REP.Rep..to.Congress.1st.Choice.District.2, REP.Rep..to.Congress.2nd.Choice.District.2,REP.Rep..to.Congress.3rd.Choice.District.2 )

b$REP.Rep..to.Congress.1st.Choice.District.2[b$REP.Rep..to.Congress.1st.Choice.District.2 == "undervote"]<- NA
b$REP.Rep..to.Congress.2nd.Choice.District.2[b$REP.Rep..to.Congress.2nd.Choice.District.2 == "undervote"]<- NA
b$REP.Rep..to.Congress.3rd.Choice.District.2[b$REP.Rep..to.Congress.3rd.Choice.District.2 == "undervote"]<- NA
b$REP.Rep..to.Congress.1st.Choice.District.2[b$REP.Rep..to.Congress.1st.Choice.District.2 == "overvote"]<- NA
b$REP.Rep..to.Congress.2nd.Choice.District.2[b$REP.Rep..to.Congress.2nd.Choice.District.2 == "overvote"]<- NA
b$REP.Rep..to.Congress.3rd.Choice.District.2[b$REP.Rep..to.Congress.3rd.Choice.District.2 == "overvote"]<- NA


names(b)[names(b) == "REP.Rep..to.Congress.1st.Choice.District.2"] <- "choice1"
names(b)[names(b) == "REP.Rep..to.Congress.2nd.Choice.District.2"] <- "choice2"
names(b)[names(b) == "REP.Rep..to.Congress.3rd.Choice.District.2"] <- "choice3"


colnames(b)

b <- b %>% select(pref_voter_id, choice1, choice2, choice3)

write.csv(b, "scale_ballots.csv")


### recode the column names to numbers; 
View(votes_long2)
votes_long2 <- b %>%
  gather(vote_rank, candidate, choice1:choice3 ) %>%
  mutate(vote_rank=substring(vote_rank, 7)) %>%
  group_by(pref_voter_id)

votes_long2$contest<- "ME2R"

colnames(votes_long2)

View(votes_long2)

View(votes_long)

library(devtools)
install_url("https://cran.r-project.org/src/contrib/Archive/rcv/rcv_0.2.0.tar.gz")

###### here it is: 


results<- rcv_tally(votes_long2, rcvcontest = "ME2R") 
xtable::xtable(results)

#### rewrite approval mechanism 


approve_pct<- function(image, results){
library(dplyr)
g<- image
g$approved<- ifelse(g$candidate == results$candidate[1], 1, 0)
g$approved[is.na(g$approved)] <- 0
g<- g %>% group_by(pref_voter_id) %>% 
  summarise(approved = sum(approved))
gmean<- round((mean(g$approved)*100) ,digits=2)
gmean<- paste0(gmean, "% Approval Rate")
return(gmean)
}


approve_pct(image = votes_long2, results = results)



knitr::kable(results)
### works pretty well 
d3_7 <- make_d3list(results = results)
networkD3::sankeyNetwork(Links = d3_7$values, Nodes = d3_7$names,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "candidate", units = "voters",
                         fontSize = 12, nodeWidth = 20)


### map time 


b$Precinct<- as.character(b$Precinct)
b$TOWN <- toupper(b$Precinct)
library(stringr)
b$TOWN<- str_remove(b$TOWN, " W1")
b$TOWN<- str_remove(b$TOWN, " W2")
b$TOWN<- str_remove(b$TOWN, " W3")
b$TOWN<- str_remove(b$TOWN, " W4")
b$TOWN<- str_remove(b$TOWN, " ALL")


(which(t %!in% maine_shape$TOWN))
'%!in%' <- function(x,y)!('%in%'(x,y))


t<- unique(b2$TOWN)
t2<- t[which(t %!in% maine_shape$TOWN)]



g<- votes_long2
g$approved<- ifelse(g$candidate == results$candidate[1], 1, 0)
g$approved[is.na(g$approved)] <- 0
g<- g %>% group_by(pref_voter_id) %>% 
  summarise(approved = sum(approved))
g$approved<- ifelse(g$approved >1, 1,g$approved )

b$approved<- g$approved
g$approved

b2<- b %>% group_by(TOWN) %>% 
  summarise(approval = mean(approved))



n<- c("CONNOR", 
      "FLETCHER'S LANDING TWP", 
      "GREENFIELD TWP", 
      "BENEDICTA TWP", 
      "SILVER RIDGE TWP",
      "BIG TWENTY TWP", 
      "T15 R15 WELS", 
      "ARGYLE TWP", 
      "FREEMAN", 
      "SALEM")

m<- c(b2$approval[b2$TOWN == "CARIBOU/CONNOR"][1], 
      b2$approval[b2$TOWN == "ELLSWORTH/FLETCHER'S LANDING"][1], 
      b2$approval[b2$TOWN == "MILFORD/GREENFIELD TWP"][1],
      b2$approval[b2$TOWN == "SHERMAN/BENEDICTA TWP/SILVER RIDGE TWP"][1],
      b2$approval[b2$TOWN == "SHERMAN/BENEDICTA TWP/SILVER RIDGE TWP"][1],
      b2$approval[b2$TOWN == "FORT KENT/BIG TWENTY TWP/T15R15 WELS"][1],
      b2$approval[b2$TOWN == "FORT KENT/BIG TWENTY TWP/T15R15 WELS"][1],
      b2$approval[b2$TOWN == "OLD TOWN/ARGYLE TWP"][1],
      b2$approval[b2$TOWN == "STRONG/FREEMAN/SALEM"][1] , 
      b2$approval[b2$TOWN == "STRONG/FREEMAN/SALEM"][1])
u<- cbind(n, m)
b2<- b

b2[b2$TOWN == "CARIBOU/CONNOR"]

b2$TOWN[b2$TOWN == "CARIBOU/CONNOR"]<- "CARIBOU"
b2$TOWN[b2$TOWN == "ELLSWORTH/FLETCHER'S LANDING"] <-  "ELLSWORTH"
b2$TOWN[b2$TOWN == "MILFORD/GREENFIELD TWP"] <- "MILFORD"
b2$TOWN[b2$TOWN == "SHERMAN/BENEDICTA TWP/SILVER RIDGE TWP"] <- "SHERMAN"
b2$TOWN[b2$TOWN == "FORT KENT/BIG TWENTY TWP/T15R15 WELS"] <- "FORT KENT"
b2$TOWN[b2$TOWN == "OLD TOWN/ARGYLE TWP"] <- "OLD TOWN"
b2$TOWN[b2$TOWN == "STRONG/FREEMAN/SALEM"] <-   "STRONG"
b2$TOWN[b2$TOWN == "CROSS LAKE"] <-   "CROSS LAKE TWP"
### need to handle each by collapsing; then, create new rows for the ones you already have, 
## merge to map, make map. 

b2<- as.data.frame(b2)
sort(unique(maine_shape$TOWN))

maine_shape<- st_read("/Users/jessesmac/Downloads/Maine_Boundaries_Town_and_Townships_Polygon_Dissolved-shp/Maine_Boundaries_Town_and_Townships_Polygon_Dissolved.shp")
maine_shape$TOWN<- toupper(as.character(maine_shape$TOWN))

### left to do here: rename missing precincts to correct ones.  aggregate. merge to map. 
maine_shape<- merge(maine_shape, b2, by = "TOWN", all.x = T)
shapefile_df <- fortify(maine_shape)


maine_shape$approval
map<- ggplot(data = shapefile_df) +
  geom_sf(aes(fill = approval))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")# +ggtitle("Rejected Absentee Votes by Town")
map<- map + theme_minimal() + ggtitle("Dale Crafts \n Approval Rate by Town")  +
  theme(plot.title=element_text(family="Styrene B", face="bold", size=20), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
map <- map + guides(fill=guide_legend(title="Approval Rate"))
map


###### audit audit audit #######

results<- rcv_tally(votes_long2) ### correct tally 


b$Precinct<- as.character(b$Precinct)
b$Precinct[is.na(b$Precinct)] <- "UOCAVA"
precinct<- unique(as.character(b$Precinct))

correct_winner<- c()
winner_name<- c()

c<- b


#### audit 
start_time <- Sys.time()
for(i in 28:length(precinct)){
  print(paste0((i/length(precinct)* 100), "% Complete"))
  c<- b[!b$Precinct == precinct[i],]
  d <- c %>% select(pref_voter_id, choice1, choice2, choice3)
  
  votes_long3 <- d %>%
    gather(vote_rank, candidate, choice1:choice3 ) %>%
    mutate(vote_rank=substring(vote_rank, 7)) %>%
    group_by(pref_voter_id)
  
  res_test<- rcv_tally(votes_long3) 
  correct_winner[i]<- ifelse(res_test$candidate[1] == results$candidate[1], 1, 0)
  winner_name[i]<- res_test$candidate[1]
}
end_time <- Sys.time()
end_time - start_time

i
results<- rcv_tally(votes_long2) 
results$candidate[1]



g<- cbind(correct_winner, winner_name)

View(g)
