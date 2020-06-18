####################################################################################
####################### Script for County Scatterplots #############################
######################################################################################
wd_global <- readline(prompt = "Enter file path for working directory: ")
# set working directory to overall healthy elections folder 
# F:/MEDSL/healthy_elections
setwd(wd_global) 
###now read in the data
county_acs <- read.csv("county2018acs.csv")
county_acs <- county_acs[,-c(6:55)]
names(county_acs)
head(county_acs)
county_acs <- subset(county_acs, select=-c(SE_A02002_001,SE_A04001_001))
colnames(county_acs)[6:59] <- c("total_pop","pop_ppsm","area_miles","male_pop","male5under","male5to9pop","male10to14pop",
                                "male15to17pop","male18to24pop","male25to34pop","male35to44pop","male45to54pop",
                                "male55to64pop","male65to74pop","male75to84pop","male85over_pop",
                                "female_pop","female5under","female5to9pop","female10to14pop","female15to17pop","female18to24pop",
                                "female25to34pop","female35to44pop","female45to54pop","female55to64pop","female65to74pop",
                                "female75to84pop","female85over_pop","not_hispanic_pop","white_alone_pop","black_alone_pop",
                                "amer_indian_alone_pop","asian_alone_pop","pac_island_alone_pop","other_race_alone",
                                "races2plus_alone","hispanic_pop","white_hisp","black_hisp","amer_ind_hisp","asian_hisp",
                                "pac_island_hisp_pop","other_race_hisp","races2plus_hisp","pop25over","less_than_hs",
                                "high_school","some_college","college_degree","masters_degree","prof_degree","phd_degree",
                                "gini_index") 
##getting pcts; age 
county_acs$male_pct <- (county_acs$male_pop/county_acs$total_pop)*100
county_acs$female_pct <- (county_acs$female_pop/county_acs$total_pop)*100
county_acs$age18underpct <- (rowSums(county_acs[,10:13,23:26])/county_acs$total_pop)*100
county_acs$age18to24pct <- ((county_acs$male18to24pop+county_acs$female18to24pop)/county_acs$total_pop)*100
county_acs$age25to34pct <- ((county_acs$male25to34pop+county_acs$female25to34pop)/county_acs$total_pop)*100
county_acs$age35to44pct <- ((county_acs$male35to44pop+county_acs$female35to44pop)/county_acs$total_pop)*100
county_acs$age45to54pct <- ((county_acs$male45to54pop+county_acs$female45to54pop)/county_acs$total_pop)*100
county_acs$age55to64pct <- ((county_acs$male55to64pop+county_acs$female55to64pop)/county_acs$total_pop)*100
county_acs$age65to74pct <- ((county_acs$male65to74pop+county_acs$female65to74pop)/county_acs$total_pop)*100
county_acs$age75to84pct <- ((county_acs$male75to84pop+county_acs$female75to84pop)/county_acs$total_pop)*100
county_acs$age85over_pct <- ((county_acs$male85over_pop+county_acs$female85over_pop)/county_acs$total_pop)*100
###race 
county_acs$white_pct <- (county_acs$white_alone_pop/county_acs$total_pop)*100
county_acs$black_pct <- ((county_acs$black_alone_pop+county_acs$black_hisp)/county_acs$total_pop)*100
county_acs$asianpi_pct <- ((county_acs$asian_alone_pop+county_acs$asian_hisp+county_acs$pac_island_alone_pop+county_acs$pac_island_hisp_pop)/
                             county_acs$total_pop)*100
county_acs$hispanic_pct <- ((county_acs$hispanic_pop)/county_acs$total_pop)*100
county_acs$other_pct <- (rowSums(county_acs[,c(38,41:42,46,49:40)])/county_acs$total_pop)*100
#####now educ
county_acs$less_than_hs_pct <- (county_acs$less_than_hs/county_acs$pop25over)*100
county_acs$high_school_pct <- (county_acs$high_school/county_acs$pop25over)*100
county_acs$some_college_pct <- (county_acs$some_college/county_acs$pop25over)*100
county_acs$college_degree_pct <- (county_acs$college_degree/county_acs$pop25over)*100
county_acs$masters_degree_pct <- (county_acs$masters_degree/county_acs$pop25over)*100
county_acs$prof_degree_pct <- (county_acs$prof_degree/county_acs$pop25over)*100
county_acs$phd_degree_pct <- (county_acs$phd_degree/county_acs$pop25over)*100
####subset the data 
county_acs2 <- county_acs[,-c(35:50)]
names(county_acs2)
write.csv(county_acs2, "county_acs_demos.csv",row.names = FALSE)
saveRDS(county_acs2, "county_acs_demos.Rdata")


View(county_acs)
