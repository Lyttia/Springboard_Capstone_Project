# Feature Engineering

# load data crimes_joined.csv from local machine or data file folder in github 

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

# 6- Create Premise Categories & Dummy Variables ===============================================

# store premise variable

premise <- crimes_joined$premise
str(premise)
levels(premise)

# all of these premise types are incomprehensible
# combine premise types since many are redundant and 
# Random Forest can only handle 53 or less categories

# stores unique premise var to count how many premise types are represented in the data

unique_premise <- unique(premise)
length(unique_premise)
unique_premise

#counts each premise type occurance to analyze for combination

crime_count_by_premise <- crimes_joined %>% count(premise) %>% 
  arrange(-n)

#write_csv(crime_count_by_premise, "premise_count.csv")

# Create dummy variables

crimes_joined$singl_fam_home <- +(grepl("FAMILY|RESIDENTIAL",
                                        crimes_joined$premise))

crimes_joined$edu_facility <- +(grepl("SCHOOL|CHILD|UNIVERSITY",
                                      crimes_joined$premise))

crimes_joined$unknown <- +(grepl("UNKNOWN|unknown|OTHER",
                                 crimes_joined$premise))

crimes_joined$public_trans <- +(grepl("TRAIN|RAIL|PARK AND RIDE|DOCK|BUS|",
                                      crimes_joined$premise))

crimes_joined$medical_facility <- +(grepl("MEDICAL|NURSING|HOSPITAL",
                                          crimes_joined$premise))

crimes_joined$natl_envnmt <- +(grepl("DESERT|MOUNTAIN|FIELD|RIVER",
                                     crimes_joined$premise))

# Recode premise type levels
# library(car)

crimes_joined$premise <- recode(crimes_joined$premise, 
"c('07A STOREROOM/SHED (RESIDENTIAL)','CARPORT','DRIVEWAY',
'FENCED RESIDENTIAL YARD','GARAGE / CARPORT','GARAGE',
'SINGLE FAMILY HOUSING','SINGLE FAMILY HOUSE','EASEMENT')=
'single family home';
c('UNKNOWN','unknown','OTHER','FOJ - PREMISE UNKNOWN')=
'unknown';
c('SCHOOL-COLLEGE/UNIVERSITY','SCHOOL-ELEMENTARY/SECONDARY',
'SCHOOL-OTHER','SCHOOL/COLLEGE/CHILD CARE','CHILD CARE / DAY CARE')=
'edu_facility';
c('HOSPITAL','HOSPITIAL / NURSING CARE','NURSING CARE',
'MEDICAL OFFICE')='medical_facility';
c('BUS','BUS / LIGHT RAIL','BUS / RAIL STATION','BUS FACILITY',
'BUS STATION','BUS STOP','DOCK/WHARF/FREIGHT/MODAL TERMINAL',
'LIGHT RAIL','LIGHT RAIL FACILITY','LIGHT RAIL PLATFORM',
'LIGHT RAIL TRAIN','PARK AND RIDE', 'RAIL STATION','RAIL STOP',
'TRAIN STATION')='public_trans';
c('RIVER BOTTOM','OPEN SPACE / DESERT','MOUNTAIN AREA','FIELD/WOODS')=
'natl_envnmt';
c('THEATRE','DRIVE-IN MOVIE')='movies';
c('STREET / ROADWAY / ALLEY / SIDEWALK','STREET / ROADWAY / ALLEY SIDEWALK')=
'strt_road_alley_sdwlk';c('PARKING LOT','PARKING GARAGE')='parking';
c('LOAN / FINANCE COMPANY','BANK / SAVINGS / CREDIT UNION','ATM SEPARATE FROM BANK')=
'financial_facilities';c('RESTAURANT','FAST FOOD STORE')='food_service'")

levels(crimes_joined$premise)

write_csv(crimes_joined, "tempcrimesfortestingmodel.csv")
