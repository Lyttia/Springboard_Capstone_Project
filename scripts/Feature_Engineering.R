# Feature Engineering

# Make sure you have run the 0Install/Load packages at the start of each new session

# load data crimes_complete.csv from local machine or data file folder in my github repo
crimes_joined <- file.choose()
crimes_joined <- read_csv(crimes_joined)
View(crimes_joined)
colSums(is.na(crimes_joined))

# 5- Create Crime Category Dummy Variables =======================================================

category <- factor(crimes_joined$category)

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

crimes_joined <- add_column(crimes_joined, vehicle_theft, rape, larceny_theft, 
                      drug_offense, burglary, aggravated_assault, homicide, 
                      robbery, arson)

violent_crimes <- ifelse(rape == 1| robbery == 1| homicide == 1|
                           aggravated_assault == 1| arson == 1, 1, 0)

nonviolent_crimes <- ifelse(vehicle_theft == 1| larceny_theft == 1| 
                              drug_offense == 1| burglary == 1, 1, 0)

crimes_joined <- add_column(crimes_joined, violent_crimes, nonviolent_crimes)

crime.type <- ifelse(violent_crimes == 1, "violent", "nonviolent")

crimes_joined <- add_column(crimes_joined, crime.type)

crimes_joined <- crimes_joined %>% 
  mutate_if(is.character, as.factor)

View(crimes_joined)

colSums(is.na(crimes_joined))

str(crimes_joined)

write_csv(crimes_joined, "tempcrimesfortestingmodel.csv")

# 6- Create Premise Categories & Dummy Variables ===============================================

# store premise variable
premise <- crimes_joined$premise

# stores unique premise var to count how many premise types are represented in the data
unique_premise <- unique(premise)
length(unique_premise)
unique_premise

#counts each premise type occurance to plot
crime_count_by_premise <- crimes_joined %>% count(premise) %>% arrange(-n)
#write_csv(crime_count_by_premise, "premise_count.csv")
head(crime_count_by_premise)

g <- ggplot(crimes_joined, aes(premise, decreasing = TRUE))
g + geom_bar(aes(fill=category), width = 0.5) + 
  theme(axis.text.x= element_text(angle = 65, vjust= 0.6)) + 
  labs(title= "Premise Frequency Bar Chart")
ggsave("Premise Frequency Bar Chart.png")

#make table to order for ordered bar plot
premise_table <- table(crimes_joined$premise)
ordered_premise <- order(premise_table, decreasing = TRUE)
head(ordered_premise)
barplot(premise_table[ordered_premise])

# all of these premise types are incomprehensible
# combine premise types since many are redundant and 
# Random Forest can only handle 53 or less categories


###UNDER CONSTRUCTION###
str(premise)
levels(premise)

#EXAMPLE:
#df$col_01 <- +(grepl("dog|cat|rat", df$col_one))

crimes_joined$house <- +(grepl("SINGLE FAMILY HOUSE|SINGLE FAMILY HOUSING|
                               FENCED RESIDENTIAL YARD", crimes_joined$premise))
#crimes_joined$edu_facility <- +(grepl("SCHOOL/COLLEGE/CHILD CARE|CHILD CARE / DAY CARE|
                             SCHOOL-COLLEGE/UNIVERSITY|SCHOOL-ELEMENTARY/SECONDARY|, 
                             crimes_joined$premise))
crimes_joined$edu_facility <- +(grepl("SCHOOL|CHILD", crimes_joined$premise))

View(crimes_joined)

apartment = c("APARTMENT")
condotownhouse = c("CONDO / TOWNHOUSE")
hotelmotel = c("HOTEL / MOTEL")
  
premise_categories <- sapply(premise, combine_premise)

# Add premise_categories column to df

crimes <- add_column(crimes, premise_categories)  

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

gc()
