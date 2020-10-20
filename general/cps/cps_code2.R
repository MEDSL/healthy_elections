install.packages("remotes")
remotes::install_github("Reed-EVIC/cpsvote")
library(cpsvote)
library(tidyverse)
library(janitor)
library(srvyr)
library(extrafont)
library(showtext)
font_import("C:/Users/johna/OneDrive/Documents/GitHub/healthy_elections/general/fonts/styrene_b_ttf")
windowsFonts(A = windowsFont("StyreneB-Regular"))
# https://github.com/Reed-EVIC/cpsvote - link to get the repo
medsl_brands <- c("#3791FF","#59CBF5","#C0BA79","#F6573E","#156DD0","#C72654","#FF6878")
theme_medsl3gen <- theme(title = element_text(size = rel(1.2), family="Styrene B"),
                         plot.caption = element_text(hjust=0))
## Load Data Files

#For reasons we haven't tracked down, you can load all years of data *except* for 2018. First, I will download the 1994-2016 CPS and load
#them into memory (I already have the data downloaded). 

#Notes:
#1. For the time being, we only read a random subset of all of the CPS data. We can turn that off if users
#request. 
#2. We are unable to read the 2018 data file as of the time of this writing (=the data are available and codes are 
#ready, we simply have an file pattern match error). 



# WARNING: THIS NEXT COMMAND DOWNLOADS A LOT OF DATA
#cpsvote::cps_download_data(year = seq(1994,2018,2))
cps <- cpsvote::cps_read(year = seq(1996, 2018, 2))
names(cps)
###see that the coding is confusing here and not readable. This will be corrected later with the 
#cps_label cmd 
sort(unique(cps$VRS_VOTEMODE_2004toPRESENT))
sort(unique(cps$VRS_VOTEMETHOD_1996to2002))
###applying labels 
cps <- cps_label(cps)
nrow(cps)
###let's code race better 
sort(unique(cps$RACEb))
cps$RACEb <- as.character(cps$RACE)
cps$RACE2 <- "OTHER"
cps$RACE2[str_detect(cps$RACEb, "WHITE")] <- "WHITE"
cps$RACE2[str_detect(cps$RACEb, "BLACK")] <- "BLACK"
sort(unique(cps$RACE2))


########################## Get the states set up 
#when creating the cps recoded object, basically everything becomes NA. I beleve that this is likely due to the 
#cps_recode_vote cmd, or cps label. Let's try 
#####let's try again with the vote mode ; will need to deal with the fact that the variable names are different. 
#Need to account for VRS_VOTE_METHOD being different 
cps_recoded <- cps %>% 
  cps_recode_vote() %>%    # Create two new vote turnout variables to correspond to CPS and Hur-Achen coding0
  mutate(
    census_region = case_when(
      STATE %in% c("ME", "NH", "VT", "MA", "CT", "RI", 
                   "NY", "PA", "NJ") ~ "Northeast",
      STATE %in% c("ME", "DE", "WV", "DC", "VA", "NC", "SC", "GA", "FL",
                   "KY", "TN", "MS", "AL", 
                   "OK", "AR", "LA", "TX") ~ "South",
      STATE %in% c("WI", "MI", "IL", "IN", "OH", 
                   "ND", "MN", "SD", "IA", "NE", "MO", "KS") ~ "Midwest",
      STATE %in% c("MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM", 
                   "WA", "OR", "CA", "AK", "HI") ~ "West"),
    census_division = case_when(
      STATE %in% c("ME", "NH", "VT", "MA", "CT", "RI") ~ "New England", 
      STATE %in% c("NY", "PA", "NJ") ~ "Middle Atlantic",
      STATE %in% c("ME", "DE", "WV", "DC", "VA", "NC", "SC", "GA", "FL") ~ "South Atlantic",
      STATE %in% c("KY", "TN", "MS", "AL") ~ "East South Central", 
      STATE %in% c("OK", "AR", "LA", "TX") ~ "West South Central",
      STATE %in% c("WI", "MI", "IL", "IN", "OH") ~ "East North Central", 
      STATE %in% c("ND", "MN", "SD", "IA", "NE", "MO", "KS") ~ "West North Central",
      STATE %in% c("MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM") ~ "Mountain", 
      STATE %in% c("WA", "OR", "CA", "AK", "HI") ~ "Pacific"),
    vote_mode = case_when(
      VRS_VOTEMODE_2004toPRESENT == "IN PERSON" & VRS_VOTEWHEN_2004toPRESENT == "ON ELECTION DAY" ~ "Election Day",
      VRS_VOTEMETHOD_1996to2002 == "IN PERSON ON ELECTION DAY"  ~ "Election Day",
      
      VRS_VOTEMODE_2004toPRESENT == "IN PERSON" & VRS_VOTEWHEN_2004toPRESENT == "BEFORE ELECTION DAY" ~ "Early In Person",
      VRS_VOTEMETHOD_1996to2002 == "IN PERSON BEFORE ELECTION DAY"  ~ "Early In Person",
      
      VRS_VOTEMODE_2004toPRESENT == "BY MAIL" ~ "Vote By Mail",
      VRS_VOTEMETHOD_1996to2002 == "VOTED BY MAIL (ABSENTEE)" ~ "Vote By Mail"
      
    )
  )

cps_recoded %>%
  filter(YEAR == 2016 & !is.na(VRS_REG) & STATE=="WI" & VRS_REG!="REFUSED" & VRS_REG!="NO RESPONSE") %>%
  cps_recode_vote() %>%    # Create two new vote turnout variables to correspond to CPS and Hur-Achen coding
  tabyl(VRS_REG)
###cps normal 
cps %>% 
  filter(!is.na(VRS_REG) & STATE=="WI" & VRS_REG!="REFUSED" & VRS_REG!="NO RESPONSE" & RACE2=="BLACK") %>%
  tabyl(VRS_REG, YEAR) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns() 


vccps_recoded %>% 
  filter(!is.na(VRS_REG) & STATE=="WI" & VRS_REG!="REFUSED" & VRS_REG!="NO RESPONSE" & RACE2=="BLACK") %>%
  tabyl(VRS_REG, YEAR) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns() 
cps_recoded %>% 
  filter(!is.na(VRS_REG) & STATE=="WI" & VRS_REG!="REFUSED" & VRS_REG!="NO RESPONSE" & RACE2=="WHITE") %>%
  tabyl(VRS_REG, YEAR) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns() 


cps_weight <- as_survey_design(cps_recoded, weights = WEIGHT)
class(cps_weight)
#####ggplot of the results of vote mode 
voterreg_fig <- cps_weight %>%
  filter(YEAR > 1994 & !is.na(VRS_REG) & STATE=="WI" & VRS_REG!="REFUSED" & VRS_REG!="NO RESPONSE") %>%
  group_by(YEAR, VRS_REG) %>%
  summarize(value = survey_mean(na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = value, col = VRS_REG, group = VRS_REG)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = YEAR, y = value, color = VRS_REG), size = 2) +
  scale_x_continuous(breaks = seq(1996, 2018, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Registered voters in Wisconsin over time", 
       caption = "Source: Voting and Registration Supplement Current Population Survey \n 
       https://www.census.gov/topics/public-sector/voting.html \n
       Created using the cpsvote R package: Paul Gronke and Jay Lee, Early Voting Information Center Reed College. \n
       https://github.com/Reed-EVIC/cpsvote",
       color = "Mode of Voting",
       y = "%",
       x = "Year") +
  theme_minimal() +theme_medsl3gen 
voterreg_fig
# 
cps_weight_black <- subset(cps_weight, RACE2=="BLACK")
#####let's try to get the results by race 
black_voterreg_fig <- cps_weight_black %>%
  filter(YEAR > 1994 & !is.na(VRS_REG) & STATE=="WI" & VRS_REG!="REFUSED" & VRS_REG!="NO RESPONSE" ) %>%
  group_by(YEAR, VRS_REG) %>%
  summarize(value = survey_mean(na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = value, col = VRS_REG, group = VRS_REG)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = YEAR, y = value, color = VRS_REG), size = 2) +
  scale_x_continuous(breaks = seq(1996, 2018, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Registered Black voters in Wisconsin over time", 
       caption = "Source: Voting and Registration Supplement Current Population Survey \n 
       https://www.census.gov/topics/public-sector/voting.html \n
       Created using the cpsvote R package: Paul Gronke and Jay Lee, Early Voting Information Center Reed College. \n
       https://github.com/Reed-EVIC/cpsvote",
       color = "Mode of Voting",
       y = "Electorate %",
       x = "Year") +
  theme_minimal() +theme_medsl3gen 
black_voterreg_fig

####white voters 
cps_weight_white <- subset(cps_weight, RACE2=="WHITE")
#####let's try to get the results by race 
white_voterreg_fig <- cps_weight_white %>%
  filter(YEAR > 1994 & !is.na(VRS_REG) & STATE=="WI" & VRS_REG!="REFUSED" & VRS_REG!="NO RESPONSE" ) %>%
  group_by(YEAR, VRS_REG) %>%
  summarize(value = survey_mean(na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = value, col = VRS_REG, group = VRS_REG)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = YEAR, y = value, color = VRS_REG), size = 2) +
  scale_x_continuous(breaks = seq(1996, 2018, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Registered White voters in Wisconsin over time", 
       caption = "Source: Voting and Registration Supplement Current Population Survey \n 
       https://www.census.gov/topics/public-sector/voting.html \n
       Created using the cpsvote R package: Paul Gronke and Jay Lee, Early Voting Information Center Reed College. \n
       https://github.com/Reed-EVIC/cpsvote",
       color = "Mode of Voting",
       y = "%",
       x = "Year") +
  theme_minimal() +theme_medsl3gen 
white_voterreg_fig



ggsave("mail_fig1.png",plot=mailfig1,scale=1,width=9,height=6,units=c("in"),dpi=600)
## Recoding Vote Turnout
# , breaks=c(0,15,30,45,60,75,90)
#We know that CPS has an unusual method for coding turnout. Let's compare how the CPS codes turnout and the method recommended by Hur and Achen.


####let's check WI results 
dim(cps_weight)

###let'sdo the registered voters 
mailfig1 <- cps_weight %>%
  filter(YEAR > 1994 & !is.na(vote_mode)) %>%
  group_by(YEAR, vote_mode) %>%
  summarize(value = survey_mean(na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = value, col = vote_mode, group = vote_mode)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = YEAR, y = value, color = vote_mode), size = 2) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 4)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Ballots cast by modes of voting over time", 
       caption = "Source: Voting and Registration Supplement Current Population Survey \n 
       https://www.census.gov/topics/public-sector/voting.html \n
       Created using the cpsvote R package: Paul Gronke and Jay Lee, Early Voting Information Center Reed College. \n
       https://github.com/Reed-EVIC/cpsvote",
       color = "Mode of Voting",
       y = "Electorate %",
       x = "Year") +
  theme_minimal() +theme_medsl3gen 


# Graph 2: Rate of Early In Person Voting By Year and By Region
cps_weight %>%
  filter(YEAR > 1994) %>%
  mutate(earlyinperson = case_when(
    vote_mode == "Early In Person" ~ 1,
    vote_mode == "Election Day" |
      vote_mode == "Vote By Mail" ~ 0)
  ) %>%
  group_by(YEAR, census_region) %>%
  summarize(value = survey_mean(earlyinperson, na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = value, col = census_region, group = census_region)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = YEAR, y = value, color = census_region), size = 2) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Regional Use of Early In Person Voting, 1996 - 2016", 
       subtitle = "Source: Current Population Survey, Voting and Registration Supplement",
       color = "Region") +
  ylab("") + xlab("") + 
  theme(plot.title = element_text(size = 20, family = "Times"),
        legend.position = c(.15,.8), legend.background = element_rect(),  
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) 
 


