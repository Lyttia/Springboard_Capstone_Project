# Capstone Project Data Wrangling Script 

# I will be using an R interface to weather underground's API. 
# In order to use this library you must obtain an API key. Prior to ____, 
# a free key was available. The free key limits requests to 10 per minute. 
# If you are grabbing weather for a large date range using history_range 
# then by default limit = 10 will limit the calls to a maximum of 10 
# per minute. This package has functions that follow the online api.

# 0-Install & Load packages=================================================

install.packages("tidyverse", dependencies = TRUE)
install.packages("devtools")
install.packages("stringi", dependencies = TRUE)
library(tidyverse)
library(devtools)
devtools::install_github("ALShum/rwunderground", force = TRUE)
library(rwunderground)

# Set API key

rwunderground::set_api_key("337b40a1234484ca")
WUNDERGROUNDID = '337b40a1234484ca'

# 1-Read and view original data============================================= 

# property data original csv is too large to upload to github. 
# access the original csv file from [Realtor.com](https://www.realtor.com/research/data)

property_original <- read_csv("RDC_InventoryCoreMetrics_Zip_Hist2.csv")
property <- tbl_df(property_original)
str(property)
head(property)
data.frame(head(property))

crimes_original <- read_csv("Phoenix Crime Data 7.2.18.csv")
crimes <- tbl_df(crimes_original)
str(crimes)
head(crimes)
data.frame(head(crimes))

# Call rwundergound API Package to create PHX weather history df

zip_code = (as.character(unique(crimes$ZIP)))
location_zips = sapply(zip_code, set_location)
getdata <- function(location_zip, date_start = "20151101", 
                    date_end = "20151102"){
                    df  = history_range(location_zip,date_start,date_end) 
                    return (df)
                    }
weather_df_combined = do.call(rbind, lapply(location_zips, getdata))
weather_df_combined <- tbl_df(weather_df_combined)
View(weather_df_combined)

# Why DOES this NOT WORK?: 
# weatheroriginalTEST <- history_range(sapply(zip_code, set_location), 
                                     #date_start = "20151101", 
                                     #date_end = "20151102") #TEST DATES
# Error in is.url(url) : length(url) == 1 is not TRUE

# 2-Clean Variables/Column Names/Observations================================

weather <- weather_df_combined %>%
  select('date', 'temp', 'cond') %>%
  separate("date", c("date", "time"), sep = " ") %>%
  separate("date", c("year", "month", "day"), sep = "-") %>%
  separate("time", c("hour"), sep = ":") %>% #discarded minutes for join
  rownames_to_column("zipcode")
# adding row names to use for join
weather$zipcode <- substr(weather$zipcode, 1, 5)
# substring row names to use for join
View(weather)

# some reason this didn't work below:
# weather <- separate(weather, "zipcode", c("zipcode", "callcount"), sep = ".")

property <- property %>% 
  filter(ZipName == "Phoenix, AZ" & Month >= "2015-10-01") %>%
  select(Month, zipcode = ZipCode, median_value = "Median Listing Price", 
         total_listed = "Total Listing Count") %>%
  separate("Month", c("year", "month", "day"), sep = "-") %>% 
  select(-"day")
View(property)

crimes <- crimes %>%
  select(-"OCCURRED TO") %>% 
  separate("OCCURRED ON", c("start_date", "start_time"), sep = "  ") %>%
  separate("start_date", c("month", "day", "year"), sep = "/") %>%
  separate("start_time", c("hour", "minute"), sep = ":") %>% 
  rename(category = "UCR CRIME CATEGORY", block = "100 BLOCK ADDR", 
         zipcode = "ZIP", premise = "PREMISE TYPE")
crimes$zipcode <- as.character(crimes$zipcode)
# change zipcode to chr string for join
View(crimes)

# 3- Join dataframes=======================================================

crimes2 <- left_join(crimes, property)
View(crimes2)
# missing all property data from 6/2018 onward.
# there is no way to know future property data for prediction model.

crimes3 <- left_join(crimes2, weather)
View(crimes3)

which(is.na(crimes3))
sum(is.na(crimes3))
colSums(is.na(crimes3))

unique(crimes3$category)
category <- crimes3$category

# Create Dummy Variables

vehicle_theft <- ifelse(category == "MOTOR VEHICLE THEFT", 1, 0) 
rape <- ifelse(category == "RAPE", 1, 0)
larceny_theft <- ifelse(category == "LARCENY-THEFT", 1, 0)
drug_offense <- ifelse(category == "DRUG OFFENSE", 1, 0)
burglary <- ifelse(category == "BURGLARY", 1, 0)
aggravated_assault <- ifelse(category == "AGGRAVATED ASSAULT", 1, 0)
homicide <- ifelse(category == "MURDER AND NON-NEGLIGENT MANSLAUGHTER", 1, 0)
robbery <- ifelse(category == "ROBBERY", 1, 0)
arson <- ifelse(category == "ARSON", 1, 0)

premise <- crimes3$premise

residential_count <- length(which(premise == "SINGLE FAMILY HOUSE")) 
residential_count2 <- length(which(premise == "SINGLE FAMILY HOUSING")) 
residential_count3 <- length(which(premise == "FENCED RESIDENTIAL YARD"))
tot_res_count <- residential_count + residential_count2 + residential_count3 
tot_res_count

school_count <- length(which(premise == "SCHOOL/COLLEGE/CHILD CARE"))
school_count2 <- length(which(premise == "CHILD CARE / DAY CARE"))
school_count3 <- length(which(premise == "SCHOOL-COLLEGE/UNIVERSITY")) 
school_count4 <- length(which(premise == "SCHOOL-ELEMENTARY/SECONDARY"))
tot_school_count <- school_count + school_count2 + school_count3 + 
  school_count4
tot_school_count

apartment_count <- length(which(premise == "APARTMENT"))
apartment_count

condotownhouse_count <- length(which(premise == "CONDO / TOWNHOUSE"))
condotownhouse_count

hotelmotel_count <- length(which(premise == "HOTEL / MOTEL"))
hotelmotel_count

#single_family_residence <- ifelse(premise == "SINGLE FAMILY HOUSE"|
                               "SINGLE FAMILY HOUSING"|
                               "FENCED RESIDENTIAL YARD", 1, 0) 

#school <- ifelse(premise == "SCHOOL/COLLEGE/CHILD CARE"|
                   "CHILD CARE / DAY CARE"| "SCHOOL-COLLEGE/UNIVERSITY"|
                   "SCHOOL-ELEMENTARY/SECONDARY", 1, 0) 

apartment <- ifelse(premise == "APARTMENT", 1, 0)

condo_townhouse <- ifelse(premise == "CONDO / TOWNHOUSE", 1, 0)

# crimes3 <- add_column(crimes3, )

# crimes3 %>% group_by(zipcode) %>% summarise()

# missing all weather data after 11/2/15 (These are test dates, to be
# replaced with actual dates)
# why are there missing values for weather if zipcodes were used fm crime df

# Analysis Ideas:
# 
# Next Steps:
# Create dummy variables for premise? (there are 95 unique values)