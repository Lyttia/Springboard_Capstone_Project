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
# missing all weather data after 11/2/15 (These are test dates, to be
# replaced with actual dates)
# why are there missing values for weather if zipcodes were used fm crime df

# Analysis Ideas:
# crimes %>% group_by(zipcode) %>% summarise(count)
# Next Steps:
# Possibly create dummy variables for premise and category?