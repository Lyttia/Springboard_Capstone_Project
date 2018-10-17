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
devtools::install_github("ALShum/rwunderground", force = TRUE, 
                         dependencies = TRUE)
library(tidyverse)
library(devtools)
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
                                        limit = 10)
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

getdata7 <- function(location_zip, date_start = "20151201", 
                     date_end = "20151215"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined7 = do.call(rbind, lapply(location_zips, getdata7))
weather_df_combined7 <- tbl_df(weather_df_combined7)
View(weather_df_combined7)

getdata7.5 <- function(location_zip, date_start = "20151216", 
                     date_end = "20151231"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 6)
  return (df)
}

weather_df_combined7.5 = do.call(rbind, lapply(location_zips, getdata7.5))
weather_df_combined7.5 <- tbl_df(weather_df_combined7.5)
View(weather_df_combined7.5)

#getdata7.5 stopped at index 38 (85021.121) on 12/20

weather_df_combined7.75 = do.call(rbind, lapply(location_zips[39:102], getdata7.5))
weather_df_combined7.75 <- tbl_df(weather_df_combined7.75)
View(weather_df_combined7.75)

getdata8 <- function(location_zip, date_start = "20160101", 
                       date_end = "20160115"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined8 = do.call(rbind, lapply(location_zips, getdata8))
weather_df_combined8 <- tbl_df(weather_df_combined8)
View(weather_df_combined8)

getdata8.5 <- function(location_zip, date_start = "20160116", 
                     date_end = "20160131"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}
#---------------------->>>>>>>>>> START HERE <<<<<<<<<<--------------------------------

weather_df_combined8.5 = do.call(rbind, lapply(location_zips, getdata8.5))
# stopped at index (85250)

weather_df_combined8.5 <- tbl_df(weather_df_combined8.5)
View(weather_df_combined8.5)

getdata9 <- function(location_zip, date_start = "20160201", 
                     date_end = "20160215"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined9 = do.call(rbind, lapply(location_zips, getdata9))
weather_df_combined9 <- tbl_df(weather_df_combined9)
#stopped at index

View(weather_df_combined9)

getdata9.5 <- function(location_zip, date_start = "20160216", 
                     date_end = "20160229"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined9.5 = do.call(rbind, lapply(location_zips, getdata9.5))
#stopped at index (85250) 
weather_df_combined9.5 <- tbl_df(weather_df_combined9.5)
View(weather_df_combined9.5)

getdata10 <- function(location_zip, date_start = "20160301", 
                     date_end = "20160315"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined10 = do.call(rbind, lapply(location_zips, getdata10))
weather_df_combined10 <- tbl_df(weather_df_combined10)
View(weather_df_combined10)

getdata10.5 <- function(location_zip, date_start = "20160316", 
                      date_end = "20160331"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined10.5 = do.call(rbind, lapply(location_zips, getdata10.5))
weather_df_combined10.5 <- tbl_df(weather_df_combined10.5)
View(weather_df_combined10.5)

getdata11 <- function(location_zip, date_start = "20160401", 
                      date_end = "20160415"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined11 = do.call(rbind, lapply(location_zips, getdata11))
weather_df_combined11 <- tbl_df(weather_df_combined11)
View(weather_df_combined11)

getdata11.5 <- function(location_zip, date_start = "20160416", 
                      date_end = "20160430"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined11.5 = do.call(rbind, lapply(location_zips, getdata11.5))
weather_df_combined11.5 <- tbl_df(weather_df_combined11.5)
View(weather_df_combined11.5)

View(location_zips)

getdata12 <- function(location_zip, date_start = "20160501", 
                      date_end = "20160515"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined12 = do.call(rbind, lapply(location_zips, getdata12))
weather_df_combined12 <- tbl_df(weather_df_combined12)
View(weather_df_combined12)

getdata12.5 <- function(location_zip, date_start = "20160516", 
                      date_end = "20160530"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined12.5 = do.call(rbind, lapply(location_zips, getdata12.5))
weather_df_combined12.5 <- tbl_df(weather_df_combined12.5)
View(weather_df_combined12.5)

getdata13 <- function(location_zip, date_start = "20160601", 
                      date_end = "20160615"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined13 = do.call(rbind, lapply(location_zips, getdata13))
weather_df_combined13 <- tbl_df(weather_df_combined13)
View(weather_df_combined13)

getdata13.5 <- function(location_zip, date_start = "20160616", 
                      date_end = "20160630"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined13.5 = do.call(rbind, lapply(location_zips, getdata13.5))
weather_df_combined13.5 <- tbl_df(weather_df_combined13.5)
View(weather_df_combined13.5)

getdata14 <- function(location_zip, date_start = "20160701", 
                      date_end = "20160715"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined14 = do.call(rbind, lapply(location_zips, getdata14))
weather_df_combined14 <- tbl_df(weather_df_combined14)
View(weather_df_combined14)

getdata14.5 <- function(location_zip, date_start = "20160716", 
                      date_end = "20160731"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined14.5 = do.call(rbind, lapply(location_zips, getdata14.5))
weather_df_combined14.5 <- tbl_df(weather_df_combined14.5)
View(weather_df_combined14.5)

getdata15 <- function(location_zip, date_start = "20160801", 
                      date_end = "20160815"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined15 = do.call(rbind, lapply(location_zips, getdata15))
weather_df_combined15 <- tbl_df(weather_df_combined15)
View(weather_df_combined15)

getdata16 <- function(location_zip, date_start = "20160901", 
                      date_end = "20160915"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined16 = do.call(rbind, lapply(location_zips, getdata16))
weather_df_combined16 <- tbl_df(weather_df_combined16)
View(weather_df_combined16)

getdata17 <- function(location_zip, date_start = "20161001", 
                      date_end = "20161015"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined17 = do.call(rbind, lapply(location_zips, getdata17))
weather_df_combined17 <- tbl_df(weather_df_combined17)
View(weather_df_combined17)

getdata18 <- function(location_zip, date_start = "20161101", 
                      date_end = "20161115"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined18 = do.call(rbind, lapply(location_zips, getdata18))
weather_df_combined18 <- tbl_df(weather_df_combined18)
View(weather_df_combined18)

getdata19 <- function(location_zip, date_start = "20161201", 
                      date_end = "20161215"){
  df  = history_range(location_zip, date_start, date_end, 
                      limit = 5)
  return (df)
}

weather_df_combined19 = do.call(rbind, lapply(location_zips, getdata19))
weather_df_combined19 <- tbl_df(weather_df_combined19)
View(weather_df_combined19)



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

