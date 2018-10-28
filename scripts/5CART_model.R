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

CrimesTree = rpart(violent_crimes ~ month + day + hour + premise + monthRC + 
                   median_value + total_listed + singl_fam_home + edu_facility +
                   unknown + public_trans + medical_facility + natl_envnmt,
                   data = Train, method ="class", control = rpart.control(minbucket = 25))
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
CrimeForest = randomForest(violent_crimes ~ month + day + hour + premise + monthRC + 
                             median_value + total_listed + singl_fam_home + edu_facility +
                             unknown + public_trans + medical_facility + natl_envnmt, data = Train, nodesize = 25, ntree=200)

print(CrimeForest)
importance(CrimeForest)
varImpPlot(CrimeForest)

PredictForest = predict(CrimeForest, newdata = Test)
table(Test$violent_crimes, PredictForest)
# PredictForest
#     0     1
# 0 41125    1
# 1  6668    1

(41125+1)/(41125+1+6668+1)
# 0.8604666

library(caret)
library(e1071)

#define cross validation exp, make sure we are using the best complexity parameter value
fitControl= trainControl(method="cv", number=10)
cartGrid = expand.grid(.cp=(1:50)*0.01)
train(violent_crimes ~ month + day + hour + premise + monthRC + 
        median_value + total_listed + singl_fam_home + edu_facility +
        unknown + public_trans + medical_facility + natl_envnmt, data=Train, method="rpart", trControl=fitControl, tuneGrid=cartGrid)

CrimesTreeCV = rpart(violent_crimes ~ month + day + hour + premise + monthRC + 
                       median_value + total_listed + singl_fam_home + edu_facility +
                       unknown + public_trans + medical_facility + natl_envnmt,method = "class", data = Train, control = rpart.control(cp = 0.5))
PredictCV = predict(CrimesTreeCV, newdata = Test, type = "class")
table(Test$violent_crimes, PredictCV)
#   PredictCV
#        0     1
#  0 41126     0
#  1  6669     0

41126/(41126+6669)
# [1] 0.8604666

# cp stayed the same, and accuracy did not change 

# display indicator importance
# make dataframe from importance() output
feat_imp_df <- importance(CrimeForest) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

# plot dataframe
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: CrimeForest"
  )