# CART script

crimest <- file.choose()
crimest <- read_csv(crimest)
str(crimest)
# remove from modeling data: year(only 3 years, not enough year data), 
# block(factor w 28937levels, not precise, not giving any beneficial info), 
# zipcode(as.int but actually a factor w/ over 53 levels, max allowed by randomForest), 
  # Keep: hour(factor24), category(factor9), day(factor31), monthRC
  # Reduce categories/levels: premise,
  # convert to integer: total_listed
  # convert to numeric: median_value
# convert to factor: year, zipcode(would have over 53 levels...), binary variables(listed as.numeric)

treedata <- crimest %>% 
  select(-"year", -"zipcode") %>% 
  mutate_if(is.character, as.factor)
str(treedata)

set.seed(10)

split = sample.split(treedata$violent_crimes, SplitRatio = 0.7)
Train = subset(treedata, split == TRUE)
Test = subset(treedata, split == FALSE)

CrimesTree = rpart(violent_crimes ~ month + hour + median_value + premise + total_listed, data = Train, method ="class", control = rpart.control(minbucket = 25))
prp(CrimesTree)

PredictCART = predict(CrimesTree, newdata = Test, type = "class")
# this uses a threshold of 0.5 which would be like a baseline model, majority outcome

# create confusion matrix for validation, predicting accuracy at threshold of 0.5
table(Test$violent_crimes, PredictCART)
# PredictCART
#     0     1
# 0 41126     0
# 1  6669     0

(41126+0)/(41126+0+6669+0)
# 0.8604666

# Make ROC Curve
library(ROCR)
PredictROC = predict(CrimesTree, newdata = Test)
PredictROC
#             0         1
# [1,] 0.8604605 0.1395395
# [2,] 0.8604605 0.1395395
# [3,] 0.8604605 0.1395395
# [4,] 0.8604605 0.1395395
# [5,] 0.8604605 0.1395395
# [6,] 0.8604605 0.1395395
# [7,] 0.8604605 0.1395395
# [8,] 0.8604605 0.1395395
# [9,] 0.8604605 0.1395395
# [10,] 0.8604605 0.1395395

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

# Try Random Forest

# outcome variable must be set as factor, since there is no method arg in random forest for classification problems
Train$violent_crimes = as.factor(Train$violent_crimes)
Test$violent_crimes = as.factor(Test$violent_crimes)
str(Test)

# Random Forest model
CrimeForest = randomForest(violent_crimes ~ month + hour + median_value + total_listed, data = Train, nodesize = 25, ntree=200)
PredictForest = predict(CrimeForest, newdata = Test)
table(Test$violent_crimes, PredictForest)
PredictForest
#     0     1
# 0 41099    27
# 1  6658    11

(41099+11)/(41099+27+6658+11)
# 0.8601318

#Not ready yet:
#CrimeForest = randomForest(violent_crimes ~ month + hour + median_value + total_listed + premise, data = Train, nodesize = 25, ntree=200)
#WARNING: Can not handle categorical predictors with more than 53 categories.