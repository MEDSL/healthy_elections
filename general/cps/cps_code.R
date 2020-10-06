install.packages("remotes")
remotes::install_github("Reed-EVIC/cpsvote")
library(cpsvote)
library(tidyverse)
library(janitor)
library(srvyr)

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
cps <- cpsvote::cps_read(year = seq(1994, 2016, 2))


## Basic Data Descriptives 

#We will start by checking some basic data descriptives for voting 

cps %>% 
  tabyl(VRS_VOTE, YEAR)

# Let's add nice percentages

cps %>% 
  tabyl(VRS_VOTE, YEAR) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns() 



## Recoding Vote Turnout

#We know that CPS has an unusual method for coding turnout. Let's compare how the CPS codes turnout and the method recommended by Hur and Achen.


cps %>%
  filter(YEAR == 2016) %>%
  cps_label() %>%          # Convert columns with factor labels
  cps_recode_vote() %>%    # Create two new vote turnout variables to correspond to CPS and Hur-Achen coding
  tabyl(VRS_VOTE, cps_turnout)

cps %>%
  filter(YEAR == 2016) %>%
  cps_label() %>%          # Convert columns with factor labels
  cps_recode_vote() %>%    # Create two new vote turnout variables to correspond to CPS and Hur-Achen coding
  tabyl(VRS_VOTE, achenhur_turnout)

cps %>%
  filter(YEAR == 2016) %>%
  cps_label() %>%          # Convert columns with factor labels
  cps_recode_vote() %>%    # Create two new vote turnout variables to correspond to CPS and Hur-Achen coding
  tabyl(cps_turnout, achenhur_turnout)


## Voting Mode By Year With Appropriate Weights

#The CPS requires that we use proper survey weights. Below, I demonstrate how to use the `srvyr` package to identify
#the survey design, weight data properly, and produce turnout by mode of balloting. 

#There are some glitches below, most notably, Voting mode appears to be available only from 2002 forward. 


cps_recoded <- cps %>% 
  cps_label() %>%          # Convert columns with factor labels
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
      VRS_VOTE_MAIL == "IN PERSON" & VRS_VOTE_EDAY == "ON ELECTION DAY" ~ "Election Day",
      VRS_VOTE_MAIL == "IN PERSON" & VRS_VOTE_EDAY == "BEFORE ELECTION DAY" ~ "Early In Person",
      VRS_VOTE_MAIL == "BY MAIL" ~ "Vote By Mail"
    )
  )

#
# Set up the survey design. Weights must be used for the CPS
#
cps_weight <- as_survey_design(cps_recoded, weights = WEIGHT)

cps_weight %>%
  filter(YEAR > 1994 & !is.na(vote_mode)) %>%
  group_by(YEAR, vote_mode) %>%
  summarize(value = survey_mean(na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = value, col = vote_mode, group = vote_mode)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = YEAR, y = value, color = vote_mode), size = 2) +
  scale_x_continuous(breaks = seq(1996, 2016, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The Growth of Early Voting, 1996 - 2016", 
       subtitle = "Source: Current Population Survey, Voting and Registration Supplement",
       color = "Mode of Voting",
       y = "",
       x = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, family = "Times", face = "bold.italic", colour = "red"),
        legend.position = c(.9, 1), 
        legend.background = element_rect(), 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) 

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
  theme(plot.title = element_text(size = 20, family = "Times", face = "bold.italic", colour = "red"),
        legend.position = c(.15,.8), legend.background = element_rect(),  
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) 

