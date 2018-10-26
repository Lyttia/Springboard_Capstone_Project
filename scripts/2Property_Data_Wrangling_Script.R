# Property Data Cleaning Script

# It is not necessary to use this script if you will 
# be using the property1.csv file
# available in the data_files folder of this repo. 
# This script should be used if you will be obtaining
# the raw data csv file from the realtor.com website. 

# 1-Read and view original data============================================= 

# property data original csv is too large to upload to github. 
# access the original csv file from [Realtor.com]
# (https://www.realtor.com/research/data)

property_original <- file.choose()
property_original <- read_csv(property_original)
property <- tbl_df(property_original)

# 2-Clean Variables/Column Names/Observations===============================

property <- property %>% 
  filter(Month >= "2015-10-01") %>%
  select(Month, zipcode = ZipCode, median_value = 
           "Median Listing Price", total_listed = "Total Listing Count") %>%
  separate("Month", c("year", "month", "day"), sep = "-") %>% 
  select(-"day")

# change year to factor for join
property$year <- as_factor(property$year)

# change month to factor for join
property$month <- as_factor(property$month)

# change total listed for analysis
property$total_listed <- as.integer(property$total_listed)

# change median value for analysis
property$median_value <- as.numeric(property$median_value)

# filter for phx zipcodes
property <- property %>% filter(str_detect(zipcode, "^850"))

property$zipcode <- as.integer(property$zipcode)

# check for missing values
colSums(is.na(property))

# Calculate the mean of the listing column and use that value to populate the missing values
list_mean <- mean(property$total_listed, na.rm = TRUE)

property$total_listed[c(which(is.na(property$total_listed)))] <- list_mean
str(property)

# save cleaned up property file
write_csv(property, "property_tidy.csv")
