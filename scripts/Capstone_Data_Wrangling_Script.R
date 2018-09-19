# Crime Data Cleaning Script 

# 0-Install & Load packages=================================================

install.packages("tidyverse", dependencies = TRUE)
install.packages("devtools")
install.packages("stringi", dependencies = TRUE)
library(tidyverse)
library(devtools)

# 1-Read and view original data============================================= 

crimes_original <- read_csv("Phoenix Crime Data 7.2.18.csv")
crimes <- tbl_df(crimes_original)

# 2-Clean Variables/Column Names/Observations===============================

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

# fill na dates by order of date/time reported
crimes1 <- crimes %>% fill(month, day, year, hour)
View(crimes1)
# to see crimes missing dates:
crimes_missing_dates <- crimes %>% filter(is.na(month))

crimes1$monthRC <- factor(x = crimes1$month, 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul" ,"Aug" ,"Sep" , "Oct", "Nov", "Dec"),
                       ordered = T)
View(crimes1)

# 3- Join dataframes=======================================================

crimes2 <- left_join(crimes1, property1)
#Joining, by = c("month", "year", "zipcode")
View(crimes2)

crimes3 <- left_join(crimes2, weather)
#Joining, by = c("month", "day", "year", "hour", "zipcode")
View(crimes3)

# 5- Create Dummy Variables================================================
unique(crimes3$category)
category <- crimes3$category

category <- factor(category)
category <- reorder(category, X = category, FUN = length)

vehicle_theft <- ifelse(category == "MOTOR VEHICLE THEFT", 1, 0) 
rape <- ifelse(category == "RAPE", 1, 0)
larceny_theft <- ifelse(category == "LARCENY-THEFT", 1, 0)
drug_offense <- ifelse(category == "DRUG OFFENSE", 1, 0)
burglary <- ifelse(category == "BURGLARY", 1, 0)
aggravated_assault <- ifelse(category == "AGGRAVATED ASSAULT", 1, 0)
homicide <- ifelse(category == "MURDER AND NON-NEGLIGENT MANSLAUGHTER", 1, 0)
robbery <- ifelse(category == "ROBBERY", 1, 0)
arson <- ifelse(category == "ARSON", 1, 0)

crimes3 <- add_column(crimes3, vehicle_theft, rape, larceny_theft, 
                      drug_offense, burglary, aggravated_assault, homicide, 
                      robbery, arson)

violent_crimes <- ifelse(rape == 1| robbery == 1| homicide == 1|
                           aggravated_assault == 1| arson == 1, 1, 0)

nonviolent_crimes <- ifelse(vehicle_theft == 1| larceny_theft == 1| 
                              drug_offense == 1| burglary == 1, 1, 0)

crimes3 <- add_column(crimes3, violent_crimes, nonviolent_crimes)

crime.type <- ifelse(violent_crimes == 1, "violent", "nonviolent")

crimes3 <- add_column(crimes3, crime.type)
View(crimes3)


###UNDER CONSTRUCTION###

premise <- crimes3$premise
unique(premise) %>% length()
length(unique(premise))
unique_premise <- unique(premise)
length(unique_premise)
unique_premise
count(premise)

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

combine_premise <- function(x) {
  if(x == "SINGLE FAMILY HOUSE") {
    return("single_family_residence")
    } else if (x == "SINGLE FAMILY HOUSING") {
      return("single_family_residence")
    } else if (x == "FENCED RESIDENTIAL YARD") {
      return("single_family_residence")
    } else if(x == "SCHOOL/COLLEGE/CHILD CARE") {
      return("schools_childcare")
    } else if(x == "CHILD CARE / DAY CARE") {  
      return("schools_childcare")    
    } else if(x == "SCHOOL-COLLEGE/UNIVERSITY") {
      return("schools_childcare")    
    } else if(x == "SCHOOL-ELEMENTARY/SECONDARY") {
      return("schools_childcare")
    } 
}

premise_categories <- sapply(premise, combine_premise)

# Add premise_categories column to df

crimes3 <- add_column(crimes3, premise_categories)  

# Create premise_categories dummy variables 

single_family_residence <- ifelse(premise_categories == 
                                    "single_family_residence", 1, 0)

schools_childcare <- ifelse(premise_categories == "schools_childcare", 1, 0) 

apartment <- ifelse(premise_categories == "APARTMENT", 1, 0)

condo_townhouse <- ifelse(premise_categories == "CONDO / TOWNHOUSE", 1, 0)

# Add premise_categories dummy variables to df

crimes3 <- add_column(crimes3, single_family_residence, schools_childcare, 
                      apartment, condo_townhouse,... )  

###END CONSTRUCTION###

# 4- Exploring Missing Data=================================================
# I might be missing property values for reasons other than missing zipcodes in property data
# Zipcode may be present, but can't match by month&year(date)

nopropval <- crimes3 %>% filter(is.na(median_value))

View(nopropval)

replace_zipcodes <- nopropval %>% filter(!is.na(month)) %>% distinct(zipcode)

replace_zipcodes # 6 zipcodes missing, includes NA

fill_dates <- nopropval %>% filter(is.na(month))

View(fill_dates) # filled dates, now = 0

missingzipcount <- nopropval %>% filter(!is.na(month)) %>% count(zipcode)

View(missingzipcount) #missing 85034 downtown zipcode with 3,511 crimes

contained_zipcodes <- crimes3 %>% filter(!is.na(median_value)) %>% 
  distinct(zipcode) 
View(contained_zipcodes) #96 zipcodes

all_zipcodes <- crimes3 %>% distinct(zipcode) 
all_zipcodes #102 zipcodes

# data without na zipcodes
crimes4 <- crimes3 %>% filter(!is.na(zipcode))

# data with missing zipcodes, exclude this data fm analysis, use crimes4
crimes5 <- crimes3 %>% filter(is.na(zipcode))

View(crimes5)

write_csv(crimes4, "crimes4.csv")
View(crimes4)

