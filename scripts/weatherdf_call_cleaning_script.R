#Rwunderground Call/ Cleaning Script

# I am using an R interface to weather underground's API. 
# In order to use this library you must obtain an API key. In the past, 
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
devtools::install_github("ALShum/rwunderground", force = TRUE, 
                         dependencies = TRUE)
library(rwunderground)

# Set API key

rwunderground::set_api_key("337b40a1234484ca")
WUNDERGROUNDID = '337b40a1234484ca'

# Call rwundergound API Package to create PHX weather history df

now <- Sys.time()
zip_code = as.character(unique(crimes$ZIP))
View(zip_code)
location_zips = sapply(zip_code, set_location)
View(location_zips)

getdata <- function(location_zip, date_start = "20151101", 
                    date_end = "20151102"){
                    df = history_range(location_zip, date_start, date_end, 
                                       limit = 10) 
                    return (df)
                    }

weather_df_combined = do.call(rbind, lapply(location_zips, getdata))
weather_df_combined <- tbl_df(weather_df_combined)
View(weather_df_combined)

getdata2 <- function(location_zip, date_start = "20151103", 
                    date_end = "20151104"){
                    df  = history_range(location_zip, date_start, date_end, 
                                        limit = 10, Sys.sleep(60))
                    if (now == 3:30:00) {Sys.sleep(14400)}
                    else(return (df))
                    }

weather_df_combined2 = do.call(rbind, lapply(location_zips, getdata2))
weather_df_combined2 <- tbl_df(weather_df_combined2)
View(weather_df_combined2)

getdata3 <- function(location_zip, date_start = "20151105", 
                    date_end = "20151107"){
                    df  = history_range(location_zip, date_start, date_end, 
                                        limit = 10)
                    return (df)
                    }

weather_df_combined3 = do.call(rbind, lapply(location_zips, getdata3))
weather_df_combined3 <- tbl_df(weather_df_combined3)
View(weather_df_combined3)

getdata4 <- function(location_zip, date_start = "20151108", 
                    date_end = "20151110"){
                    df  = history_range(location_zip, date_start, date_end, 
                                        limit = 10)
                    return (df)
                    }

weather_df_combined4 = do.call(rbind, lapply(location_zips, getdata4))
weather_df_combined4 <- tbl_df(weather_df_combined4)
View(weather_df_combined4)

getdata5 <- function(location_zip, date_start = "20151111", 
                    date_end = "20151115"){
                    df  = history_range(location_zip, date_start, date_end, 
                                        limit = 10)
                    return (df)
                    }

weather_df_combined5 = do.call(rbind, lapply(location_zips, getdata5))
weather_df_combined5 <- tbl_df(weather_df_combined5)
View(weather_df_combined5)


getdata6 <- function(location_zip, date_start = "20151116", 
                    date_end = "20151130"){
                    df  = history_range(location_zip, date_start, date_end, 
                                        limit = 10)
                    return (df)
                    }

weather_df_combined6 = do.call(rbind, lapply(location_zips, getdata6))
weather_df_combined6 <- tbl_df(weather_df_combined6)
View(weather_df_combined6)
#getdata6 stopped at index 27(zipcode: 85016) on 11/24

weather_df_combined6.5 = do.call(rbind, lapply(location_zips[27:102], 
                                               getdata6))
weather_df_combined6.5 <- tbl_df(weather_df_combined6.5)
View(weather_df_combined6.5)

# Join Weather dataframes
weather_dfs_combined <- join(...)
View(weather_dfs_combined)

# 2-Clean Variables/Column Names/Observations===============================

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

