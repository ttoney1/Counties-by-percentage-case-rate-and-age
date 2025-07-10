#Program:  Make county map for 7-day case rate by vax status for 0-17 years 
#Propose alternate maps of bins using Natural Breaks or other methods 


#Link to COVID data tracker for how maps should look:
#https://covid.cdc.gov/covid-data-tracker/#vaccination-case-rate

#LOAD PACKAGES 

library(classInt) # Jenks breaks
library(rstudioapi) #to use dirname
library(readr) #read_csv
library(usmap) #generates maps, but can also use ggplot2
library(tigris) #load R built-in shape files - used for PR only
#un-comment below to cache census data instead of re-downloading if error
# options(tigris_use_cache = TRUE)
library(plyr) #rbind.fill
library(xlsx)
library(data.table)
library(janitor)
library(tidyverse)
library(readxl)
library(ggplot2)
library(gdata) #cbindX 
library(tidyr) #%>%
library(scales) #view color palette 
library(RColorBrewer)
library(wesanderson)# specific color palette
library(viridis) #most robust color palette to date
library(dplyr) #group_by()'
library(pals) #comprehensive pallets 
library(wesanderson)#specific color palette
library(sf)  #shape files - ggplot maps
library(ggpattern) #shape files 
library(psych) #describe function 
library(devtools)
library(maps)
library(mapdata)
library(ggsflabel) 
library(biscale) #bivariate map
library(cowplot) #ggdraw and theme_nothing() 
library(grid) #grid.draw

# SET DIRECTORIES

#Must have already synced SharePoint 'Data On-Call Team' folder to local drive

#Set working directory 
current.dir <- dirname(getActiveDocumentContext()$path)
setwd(current.dir)
getwd() #display

#Set census data file path
user <- Sys.getenv("USERNAME")
census.path <- paste0("C:/Users/", user, "/CDC/Data, Analytics, Visualization Task Force - Data On-Call Team/3. Data On Call Data Files/")

#current date vector
today.date<-Sys.Date()
# convert from default format to no dashes
today.date<-format(today.date,"%Y%m%d")
#today.date<-format(today.date,"%Y%m%d","%b%d%Y")

#####################################################
#     IMPORT DATA AND TRANSFORM COLUMNS
          #- must rename fips and state for map
#vaccine
suppressWarnings(
vax_df<- read_csv("raw data/Tiberius co_29JUL_04AUG_080521.csv")%>%
rename(fips = anly_recip_address_county) %>%
  mutate(fips = as.integer(fips)) %>%
  mutate(vax.age_0_17 = SUM_of_age_0_4_years_old + SUM_of_age_5_17_years_old)%>%
  subset(!(is.na(fips))) %>%
  select(-COUNT)
)

#case line 
case_df <- read_csv(
  "raw data/County_Pediatric_cases_29JUL_04AUG_080521.csv")%>%
  rename(fips=county_fips_code, State_Abb = state, case_7_day= COUNT) %>%
  mutate(fips = as.integer(fips))

#census COUNTY data
#census county data does not have Puerto Rico by age. Drop PR from analysis! 
census <- read.delim(paste0(
  census.path,"2019 Vintage Population/County/CENSUS.txt"),
  header=TRUE, sep=" ")%>%
  rename(fips=FIPS) %>%
  mutate(fips = as.integer(fips)) 

#load PR County shape file 
pr.co <- counties("Puerto Rico")%>%
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))

#load PR State shape file 
pr.STATE <- states()%>% 
  subset(NAME=="Puerto Rico")

###############################################################

# CUT CENSUS AGE GROUP AND AGGREGATE BY COUNTY

# census$ageCat.0_17<-NULL
census<-add_column(census, ageCat.0_17 = NA, .after = "Age")
census$ageCat.0_17<-ifelse(census$Age %in% 0:17,"0-17 Years", 
                ifelse(census$Age > 17,"Over 17 Years", ""))

#Get sum /aggregate population by age group
census.agg.1<-aggregate(pop ~ ageCat.0_17  + fips, census, sum)
#subset
census.agg.1 <- census.agg.1 %>%
  subset(ageCat.0_17 %in% "0-17 Years")


################################################################
#     MERGE  DATA

#merge census and vax. Keeping only census rows. Census does not have PR!
#rm(bi.map.df)
bi.map.df<-merge(x = census.agg.1,
    y = vax_df, by.x = "fips",  by.y = "fips",all.x=TRUE)

#merge census, vaccine, case
bi.map.df<-merge(x = bi.map.df,
     y = case_df, by.x = "fips",  by.y = "fips",all.x=TRUE)
#Remove rows with possible duplicate fips
bi.map.df<-bi.map.df[!duplicated(bi.map.df[ , c("fips")]),]
              
#Make Puerto Rico data - separate 
#census data does not have PR.  So no analysis on PR.
#rm(pr.bi.map.df)
pr.bi.map.df<-left_join(pr.co, bi.map.df, by=c("fips"="fips"))
#check counts
table(pr.bi.map.df$case_7_day, useNA = "ifany")
table(pr.bi.map.df$vax.age_0_17, useNA = "ifany")

#relocate
bi.map.df<-bi.map.df %>% relocate("State_Abb", .after= "pop") %>%
  relocate("res_county", .after= "State_Abb")

###################################################################

# CALCULATE COUNTY CASE RATE PER 100K

bi.map.df<-bi.map.df%>%
  add_column(case_7_day_per_100K = NA,.after = "case_7_day")%>%
  mutate(case_7_day_per_100K = (case_7_day / pop)*100000)

###############################################################

# CALCULATE FULLY VACCINATED PERCENTAGE OF COUNTY AGE GROUP 

bi.map.df<-bi.map.df %>%
  add_column(vax.pct = NA,.after = "vax.age_0_17") %>%
  mutate(vax.pct = (vax.age_0_17 / pop) *100)
#bi.map.df$vax.pct<-NULL

###############################################################

# CREATE BINS  - JENKS NATURAL BREAKS :

jenks.brks_vax <- classIntervals(bi.map.df$vax.pct,4, style='jenks')
jenks.brks_vax
#count number of fips with vaccine %
addmargins(table(bi.map.df$vax.pct, useNA = "ifany"))
#insert into data frame
#bi.map.df$vax.pct.jenks<-NULL
bi.map.df$vax.pct.jenks <-cut(bi.map.df$vax.pct, 
      jenks.brks_vax$brks, #labels = FALSE, 
      include.lowest=TRUE)
#relocate
bi.map.df<-bi.map.df %>% relocate("vax.pct", .before= "vax.pct.jenks") 

#check the number of counties that have vaccinations for 0-17 years
addmargins(table(bi.map.df$vax.pct>0, useNA = "ifany"))
# Only 678 counties of 3,142 have vaccinations for age 0-17 years
#Note. There are 3,243 counties and county equivalents in U.S. 

##############################################################

#CREATE BINS BASED ON QUARTILE FOR PROPOSED ALTERNATE MAP
#  -- 75% of values as the 75th percentile and so on 

#Include 0 in levels
vax.sub.1<-cbind.data.frame(vax.pct=c(0))

#Subset numbers to cut

vax.sub <-bi.map.df %>% subset(vax.pct > 0)
# insert zero level for vaccine pct
vax.sub <- rbind.fill(vax.sub, vax.sub.1) 
vax.sub <-vax.sub %>%
  mutate(vax.pct.quarts= cut(vax.pct, breaks=c(quantile(vax.pct,
  probs = seq(0, 1, by = 0.25), na.rm = TRUE)), 
  include.lowest=TRUE))

#view first level -- make sure includes zero
levels(vax.sub$vax.pct.quarts)[1]
#counts
table(vax.sub$vax.pct.quarts)

#merge in quartile bins 
bi.map.df<-merge(x = bi.map.df,
   y = vax.sub[c("vax.pct.quarts", "vax.pct")], 
   by.x = "vax.pct",  by.y = "vax.pct",all.x=TRUE)

#############################################################

# CREATE BI-VARIATE COLUMN

#case rate level
bi.map.df$case.level <-
  ifelse(bi.map.df$case_7_day_per_100K >=100,4,
  ifelse(bi.map.df$case_7_day_per_100K >=50, 3,     
  ifelse(bi.map.df$case_7_day_per_100K >=10,2,
  ifelse(bi.map.df$case_7_day_per_100K < 10, 1, 
                              NA))))
#vax pct jenk level label
bi.map.df$vax.pct.jenks.level <-
  ifelse(bi.map.df$vax.pct.jenks == levels(bi.map.df$vax.pct.jenks)[4],4,
  ifelse(bi.map.df$vax.pct.jenks == levels(bi.map.df$vax.pct.jenks)[3], 3,     
  ifelse(bi.map.df$vax.pct.jenks == levels(bi.map.df$vax.pct.jenks)[2],2,
  ifelse(bi.map.df$vax.pct.jenks == levels(bi.map.df$vax.pct.jenks)[1], 1, 

                                      NA))))

#cross-tab for tables 
addmargins(table(bi.map.df$vax.pct.jenks,bi.map.df$case.level, useNA = "ifany"))

#Alternate vax pct level label - quartiles 
bi.map.df$vax.pct.quarts.level <-
  ifelse(bi.map.df$vax.pct.quarts == levels(bi.map.df$vax.pct.quarts)[4],4,
  ifelse(bi.map.df$vax.pct.quarts == levels(bi.map.df$vax.pct.quarts)[3], 3,     
  ifelse(bi.map.df$vax.pct.quarts == levels(bi.map.df$vax.pct.quarts)[2],2,
  ifelse(bi.map.df$vax.pct.quarts == levels(bi.map.df$vax.pct.quarts)[1], 1, 
                              NA))))


#cross-tab for tables 
addmargins(table(bi.map.df$vax.pct.quarts, bi.map.df$case.level, useNA = "ifany"))

#bi-variate column of all possible combos (case * vax) -- jenks:

bi.map.df$vax.case.bins.jenks<-
  paste(bi.map.df$vax.pct.jenks.level, bi.map.df$case.level, sep="_")
#check values
table(bi.map.df$vax.case.bins.jenks)
#Replace NA  with NA
bi.map.df<-bi.map.df %>%
  mutate(vax.case.bins.jenks = gsub("NA", NA, vax.case.bins.jenks)) 

#-----------------------------------------------------------

#Alternate bi-variate column of all possible combos (case * vax) -- quarts:

bi.map.df$vax.case.bins.quarts<-
  paste(bi.map.df$vax.pct.quarts.level, bi.map.df$case.level, sep="_")
#check values
table(bi.map.df$vax.case.bins.quarts)
#Replace NA  with NA
bi.map.df<-bi.map.df %>%
  mutate(vax.case.bins.quarts = gsub("NA", NA, vax.case.bins.quarts)) 
##########################################################

#             MAKE MAPS

states <- plot_usmap("states", exclude="PR") #make state borders

#county map
  plot_usmap(data= bi.map.df,
             #values = "vax.case.bins.jenks",
             values = "vax.case.bins.quarts",
             labels = FALSE, 
         color="grey"
  ) +
  scale_fill_manual(
    na.value="grey53",
    values=c(
      '1_1'="#E8E8E8",
      '1_2'="#E5B1BE",
      '1_3'="#E37590",
      '1_4'="#DE1648",
      '2_1'="#B4E0D5",
      '2_2'="#B4B1BE",
      '2_3'="#B47590",
      '2_4'="#B41648",
      '3_1'="#7AD7C0",
      '3_2'="#7AB1BE",
      '3_3'="#7A7590",
      '3_4'="#7A1648",
      '4_1'="#31CCA5",
      '4_2'="#31B1A5",
      '4_3'="#317590",
      '4_4'="#311648"),               
    name="Vaccine and Case Rate Level\n(No. of Counties)", 
   # labels=c()
  ) +
  theme_nothing() +
#combine maps to make borders
    geom_polygon(data=states[[1]], 
                 aes(x=x, y=y, group=group),  color = "black", size=0.8, 
                 fill = "transparent")
  
###################################################################
  #               EXPORT MAPS
  
  #Update file name based on map
  #             
  ggsave( paste0(
    "maps/vax_case _map_0_17_years_vax_quarts_",
    #today.date,".png"),width=12, height=7, units="in", bg = "transparent")#standard
  today.date,".png"),width=7, height=6, units="in", bg = "transparent")
  
  





  
