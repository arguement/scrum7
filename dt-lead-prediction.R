### DT for predicting loan prospect
# Created by Nathaniel Christie: May 24, 2020

library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)

### LOADING the Data & Formating
rm(list=ls()) # clear environment
bank <- read.csv("BankLoan-Cleaned Dataset.csv")
str(bank)
summary(bank)

# target value: lead
bank$lead = as.factor(bank$lead)

# other factor vals
bank$qualified = as.factor(bank$qualified)
bank$contacted = as.factor(bank$contacted)
bank$won = as.factor(bank$won)

bank$date = as.Date(bank$date)

### SPLIT data into TRAIN and TEST - Stratified Sampline
set.seed(123)
dset = sample.split(Y=bank$lead, SplitRatio = 0.70)
trainData = bank[dset,]
testData = bank[!dset,]

### BUILD THE MODEL - Fit a DT using Training Data

# This model has only one node as the answer: a class of 1: pred acc is 68.8%
DTModel1 <- rpart(lead ~ education+age+marital+job+deposit+balance,
                  data = trainData,
                  parms = list(split="information gain"),
                  control = rpart.control(minsplit = 100, maxdepth = 4))

# more developed tree with more decisions: pred acc is 66.1%
DTModel1 <- rpart(lead ~ education+age+marital+job+deposit+balance,
                  data = trainData,
                  parms = list(split="information gain"),
                  control = rpart.control(minsplit = 10, maxdepth = 4))

# more developed tree with more decisions: pred acc is 67.7%
DTModel1 <- rpart(lead ~ education+age+marital+job+deposit+balance,
                  data = trainData,
                  parms = list(split="information gain"),
                  control = rpart.control(minsplit = 5, maxdepth = 6))

# more developed tree with more decisions: pred acc is 67.7%
DTModel1 <- rpart(lead ~ education+age+marital+job+deposit+balance,
                  data = trainData,
                  parms = list(split="information gain"),
                  control = rpart.control(minsplit = 5, maxdepth = 8))

# ignore education & job: pred accuracy is 68.1%
DTModel1 <- rpart(lead ~ age+marital+deposit+balance,
                  data = trainData,
                  parms = list(split="information gain"),
                  control = rpart.control(minsplit = 10, maxdepth = 4))

rpart.plot(DTModel1, type=3, extra=101, fallen.leaves = F, cex = 0.7)
rpart.plot(DTModel1, type=3, extra=104, fallen.leaves = F, cex = 0.7)

### Perform predictions and Test Accuracy
actualTest = testData$lead
predTest <- predict(DTModel1, testData, type = "class")
probTest <- predict(DTModel1, testData, type = "prob")

# View(predTest, "Class Predictions")

# Accuracy:
tl = table(predictions = predTest, actual = actualTest)
print(tl)

acc = sum(diag(tl)/sum(tl))
print(acc)

### ROC & Area Under the Curve - Sensitivity
ROC = roc(actualTest, probTest[,2],
          smoothed = TRUE,
          # arguments for ci
          # ci=TRUE, ci.alpha=0.9, stratified=FALSE,
          # arguments for plot
          plot=TRUE, auc.polygon=TRUE, max.auc.polygon=T, grid=F,
          print.auc=TRUE, show.thres=T)

plot(ROC, col="blue")


AUC = auc(ROC)
AUC
