# Joining Data

# 3- Join dataframes=======================================================

crimes_joined <- left_join(crimes, property)
# Joining, by = c("month", "year", "zipcode")
# warning coercing year, month, to character vector
# both were factors in original data, with the same number of levels, changing back to factors
crimes_joined$year <- as.factor(crimes_joined$year)
crimes_joined$month <- as.factor(crimes_joined$month)

# 4- Exploring Missing Data=================================================

colSums(is.na(crimes_joined))

nopropval_zipcount <- crimes_joined %>% filter(is.na(median_value)) %>% count(zipcode)

nolist_zipcount <- crimes_joined %>% filter(is.na(total_listed)) %>% count(zipcode)

# 1 zipcode missing:
# 85034 downtown: 3,511 crimes
# keep zipcode, fill property value and total listings as mean
# Random Forest might be able to find pattern in this

crimes_joined$median_value[c(which(is.na(crimes_joined$median_value)))] <- mean(crimes_joined$median_value, na.rm = TRUE)
crimes_joined$total_listed[c(which(is.na(crimes_joined$total_listed)))] <- mean(crimes_joined$total_listed, na.rm = TRUE)

colSums(is.na(crimes_joined))
str(crimes_joined)

# save data file for feature engineering

write_csv(crimes_joined, "crimes_joined.csv")
