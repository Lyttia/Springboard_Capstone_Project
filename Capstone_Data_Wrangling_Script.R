# Capstone Project Data Wrangling Script 

# I will be using an R interface to weather underground's API. 
# In order to use this library you must obtain an API key. Prior to ____, 
# a free key was available. The free key limits requests to 10 per minute. 
# If you are grabbing weather for a large date range using history_range 
# then by default limit = 10 will limit the calls to a maximum of 10 
# per minute. This package has functions that follow the online api.

# 0-Install & Load packages=================================================

install.packages("tidyverse")
install.packages("devtools")
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

propvals_original <- read_csv("RDC_InventoryCoreMetrics_Zip_Hist.csv")
propvals <- tbl_df(propvals_original)
propvals
str(propvals)
head(propvals)
data.frame(head(propvals))

crimes_original <- read_csv("Phoenix Crime Data 7.2.18.csv")
crimes <- tbl_df(crimes_original)
crimes
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
View(weather_df_combined)
weather_df_combined <- tbl_df(weather_df_combined)

# Why DOES this NOT WORK?: 
# weatheroriginalTEST <- history_range(sapply(zip_code, set_location), 
#                                     date_start = "20151101", 
#                                     date_end = "20151102") TEST DATES
# view(weatheroriginalTEST)

# 2-Clean Variables/Column Names/Observations================================

propvals <- propvals[ which(propvals$ZipName == "Phoenix, AZ" & 
                            propvals$Month >= "2015-10-01"), ]  
propvals <- propvals %>% 
  select('Month', 'ZipCode', 'Median Listing Price', 'Total Listing Count') 

View(propvals)

# separate date and time in crimes df
crimes <- separate(crimes, 2, c("start_date", "start_time"), sep = " ")
crimes <- separate(crimes, 4, c("end_date", "end_time"), sep = " ")
View(crimes)

# Next Steps:
# Propvals df: Rename- month, year, zip_code, median_value, total_listing  
# Crimes df: Remove- 100 Block Addr Crime, Separate- dates col to month, year, day
# Possibly create dummy variables for premise type and crime type (ucr crime category)