# Crime Data Cleaning Script 

# 1-Read and view original data============================================= 

# The file.choose() function allows you to choose the file from your local 
# directory where you saved the file after downloading it from this repo 
# or the Phoenix Open Data website
crimes_original <- file.choose()
crimes_original <- read_csv(crimes_original)
crimes <- tbl_df(crimes_original)

# 2-Clean Variables/Column Names/Observations===============================

crimes <- crimes %>%
  select(-"OCCURRED TO", -"INC NUMBER", -"100 BLOCK ADDR") %>% 
  separate("OCCURRED ON", c("start_date", "start_time"), sep = "  ") %>%
  separate("start_date", c("month", "day", "year"), sep = "/") %>%
  separate("start_time", c("hour", "minute"), sep = ":") %>% 
  rename(category = "UCR CRIME CATEGORY", zipcode = "ZIP", premise = "PREMISE TYPE") %>% 
  select(-"minute")

# add recoded months variable
crimes$monthRC <- factor(x = crimes$month, 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul" ,"Aug" ,"Sep" , "Oct", "Nov", "Dec"),
                       ordered = T)

# Check for missing variables===============================================

colSums(is.na(crimes))

# fill na dates by order of date/time reported
crimes <- crimes %>% fill(month, day, year, hour, monthRC)

# see missing zipcodes
crimes_missing_zip <- crimes %>% filter(is.na(zipcode))

# There were only 3 missing zipcodes, with no pattern, or method of identification. 
# Since there is no way to figure these out, I will remove the 3 obs w/o zipcodes. 
crimes <- crimes %>% filter(!is.na(zipcode))

# see missing premise 
# There are 923 crimes missing premise, there may be reason in this. 
# I think it would be best to keep these obs, and mark premise as "unknown". 
crimes_missing_pre <- crimes %>% filter(is.na(premise))

# Find na indices in premise and assign to variable
premise.na <- c(which(is.na(crimes$premise)))

# replace missing values with "unknown"
# random forest may be able to find a pattern with this. 
crimes$premise[premise.na] <- "unknown"

# check that all columns are filled
colSums(is.na(crimes))

# In the end, I decided to only use zip codes for the city of Phoenix, since the data for 
# these zip codes are likely to more accurately represent the actual crime distribution. 

crimes <- crimes %>% filter(str_detect(zipcode, "^850")) %>% 
  mutate_if(is.character, as.factor)
str(crimes)

# save cleaned up file
write_csv(crimes, "crimes_tidy.csv")
