# Property Data Cleaning Script

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

property_original <- read_csv("RDC_InventoryCoreMetrics_Zip_Hist2.csv")
property0 <- tbl_df(property_original)

# 2-Clean Variables/Column Names/Observations===============================

property1 <- property0 %>% 
  filter(Month >= "2015-10-01") %>%
  select(zipname = ZipName, Month, zipcode = ZipCode, median_value = 
           "Median Listing Price", total_listed = "Total Listing Count") %>%
  separate("Month", c("year", "month", "day"), sep = "-") %>% 
  select(-"day")

View(property1)

write_csv(property1, "property1.csv")




