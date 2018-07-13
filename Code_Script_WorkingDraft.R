# This is the Capstone Project Data Wrangling Script File. 

#I will be using an R interface to weather underground's API. In order to use this 
#library you must obtain an API key. Prior to ____, a free key was available. The free key 
#limits requests to 10 per minute. If you are grabbing weather for a large date range 
#using history_range then by default limit = 10 will limit the calls to a maximum of 
#10 per minute. This package has functions that follow the online api.

#0) Install & Load packages
library(tidyverse)
install.packages("devtools")
library(devtools)
devtools::install_github("ALShum/rwunderground", force = TRUE)
library(rwunderground)

#0) Set Variables
rwunderground::set_api_key("337b40a1234484ca")
WUNDERGROUNDID = '337b40a1234484ca'

#1) Read and View in original data (csv files)
PropVars_original <- read_csv("RDC_InventoryCoreMetrics_Zip_Hist.csv")
View(PropVars_original)

Crime_df_original <- read_csv("Phoenix Crime Data 7.2.18.csv")
View(Crime_df_original)

Crime_df_refine <- separate(Crime_df_original, 2, c("Date Occurred On", "Time Occurred On"), sep = " ")
Crime_df_refine <- separate(Crime_df_refine, 4, c("Date Occurred To", "Time Occurred To"), sep = " ")

View(Crime_df_refine)

#) Trying to make a vector/list of the UNIQUE values in the Crime ZIP column to use as set_location zipcode values.
ZIPCODE <- as.list(c(Crime_df_refine$ZIP))
ZIPCODE

# sapply(ZIPCODE, set_location)????

#1a) Call Wundergound API Package to create PHX weather history df
WVars_originalTEST <- history_range(sapply(ZIPCODE, set_location), date_start = 
                                      "20151101", date_end = "20151102") #TEST DATES#
View(WVars_originalTEST)

#1b) Clean Variables/Column Names/Observations 
PropVars_refine <- PropVars_original[ which (PropVars_original$ZipName == "Phoenix, AZ" & 
                                               PropVars_original$Month >= 2015-11-01), ] 

# Edit date format, convert column to different date format before trying code 44 

PropVars_refine <- PropVars_refine %>% select('ZipName', 'Month', 'ZipCode', 'Median Listing Price', 
                                              'Avg Listing Price', 'Total Listing Count') 

View(PropVars_refine)