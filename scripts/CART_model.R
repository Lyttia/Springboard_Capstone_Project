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

# create confusion matrix for validation, predicting accuracy at threshold of 0.5
table(Test$violent_crimes, PredictCART)
# PredictCART
#     0     1
# 0 43907     0
# 1  7038     0

(43907+0)/(43907+0+7038+0)
# 0.861851

# Make ROC Curve
library(ROCR)
PredictROC = predict(Crimes4Tree, newdata = Test)
PredictROC
#             0         1
#[1,] 0.8618491 0.1381509
#[2,] 0.8618491 0.1381509
#[3,] 0.8618491 0.1381509
#[4,] 0.8618491 0.1381509
#[5,] 0.8618491 0.1381509
#[6,] 0.8618491 0.1381509
#[7,] 0.8618491 0.1381509
#[8,] 0.8618491 0.1381509
#[9,] 0.8618491 0.1381509
#[10,] 0.8618491 0.1381509

# the output is two columns for each test set observation (0 or 1) 
# the first column is labeled 0 this is the percentage of training 
# set data in the same subset as that testing observation that had 
# tested obs output 0  
# second column is the probability that 
# use second column when thresholding

pred = prediction(PredictROC[,2], Test$violent_crimes)
perf = performance(pred, "tpr", "fpr")
plot(perf)
# plots ROC curve
