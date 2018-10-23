# Property Data Cleaning Script

# It is not necessary to use this script if you will 
# be using the property1.csv file
# available in the data_files folder of this repo. 
# This script should be used if you will be obtaining
# the raw data csv file from the realtor.com website. 

# 0-Install & Load packages=================================================

install.packages("tidyverse", dependencies = TRUE)
install.packages("devtools")
install.packages("stringi", dependencies = TRUE)
library(tidyverse)
library(devtools)

# 1-Read and view original data============================================= 

# property data original csv is too large to upload to github. 
# access the original csv file from [Realtor.com]
# (https://www.realtor.com/research/data)

property_original <- "RDC_InventoryCoreMetrics_Zip_Hist2.csv"
property.5 <- read_csv(property_original)
property0 <- tbl_df(property.5)

# 2-Clean Variables/Column Names/Observations===============================

property1 <- property0 %>% 
  filter(Month >= "2015-10-01") %>%
  select(zipname = ZipName, Month, zipcode = ZipCode, median_value = 
           "Median Listing Price", total_listed = "Total Listing Count") %>%
  separate("Month", c("year", "month", "day"), sep = "-") %>% 
  select(-"day")

write_csv(property1, "property1.csv")

# 3- Remove Variables from working memory===================================
rm(list = ls(pattern = "^property"))
gc()



