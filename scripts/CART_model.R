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

Crimes4Tree = rpart(violent_crimes ~ month + hour + median_value + total_listed, data = Train, method ="class", control = rpart.control(minbucket = 25))
prp(Crimes4Tree)

PredictCART = predict(Crimes4Tree, newdata = Test, type = "class")
# this uses a threshold of 0.5 which would be like a baseline model, majority outcome

# create confusion matrix for validation
table(Test$violent_crimes, PredictCART)
# PredictCART
#     0     1
# 0 43907     0
# 1  7038     0

(43907+0)/(43907+0+7038+0)
# 0.861851
