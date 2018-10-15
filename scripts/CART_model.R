# CART script

str(crimes4)

library(caTools)

set.seed(10)

split = sample.split(crimes4$violent_crimes, SplitRatio = 0.7)
Train = subset(crimes4, split == TRUE)
Test = subset(crimes4, split == FALSE)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

Crimes4Tree = rpart(violent_crimes ~ month + hour + zipcode + median_value + total_listed, data = Train, method ="class", control = rpart.control(minsplit = 2, minbucket = 1))
prp(Crimes4Tree)

