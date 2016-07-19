

credit <- read.csv("Decision Tree Data (Credit).csv")

## TUNING MODEL
library(caret)
set.seed(300)

m <- train(default ~ ., data = credit, method = "C5.0")
m
#decision tree with 20 trials is the best model and 73 percent is the a realistic estimate of future performance
#kappa is .31

table(p, credit$default)

head(predict(m, credit))
head(predict(m, credit, type = "prob"))

#Customizing the tuning process
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "oneSE")
grid <- expand.grid(.model = "tree",.trials = c(1, 5, 10, 15, 20, 25, 30, 35),.winnow = "FALSE")
grid

set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",metric = "Kappa",trControl = ctrl,tuneGrid = grid)
m

# 1 Trial model is selected as best model with kappa = .31 


## BAGGING 
#install.packages("ipred")
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25) #default value of 25 decision trees
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)
#Model fitted training data extremely well with kappa = .361

#Analyzing model's future performance with 10-fold CV (Bagged decision tree model)
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",trControl = ctrl)

#Analyzing bagged SVM model

#Creating a bagging control object

#bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred,aggregate = svmBag$aggregate)
#set.seed(300)
#svmbag <- train(default ~ ., data = credit,"bag", trControl = ctrl, bagControl = bagctrl)
#svmbag

#Working with the problem 
predfunct<-function (object, x)
{
  if (is.character(lev(object))) {
    out <- predict(object, as.matrix(x), type = "probabilities")
    colnames(out) <- lev(object)
    rownames(out) <- NULL
  }
  else out <- predict(object, as.matrix(x))[, 1]
  out
}

bagctrl <- bagControl(fit = svmBag$fit, predict = predfunct,aggregate = svmBag$aggregate)

library(kernlab)
set.seed(300)
svmbag <- train(default ~ ., data = credit,"bag", trControl = ctrl, bagControl = bagctrl)
svmbag

#kappa statistic is .32; bagged SVM model performs worse than the bagged decision tree model

## BOOSTING 

library(adabag)
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)

#Making predictions
p_adaboost <- predict(m_adaboost, credit)

head(p_adaboost$class)

#Confusion matrix
p_adaboost$confusion #100 percent accuracy maybe the over fitting. 

#Model with 10-fold cv
adaboost_cv <- boosting.cv(default ~ ., data = credit)

adaboost_cv$confusion

install.packages("vcd")
library(vcd)

Kappa(adaboost_cv$confusion)

#The kappa statistic 0.331


## Random Forests

library(randomForest)

#Training random forests
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

#Evaluating random forest performance
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#Results for decision tree 
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",metric = "Kappa", trControl = ctrl,tuneGrid = grid_rf)
m_rf

#Results for boosted C5.0 model 
grid_c50 <- expand.grid(.model = "tree",.trials = c(10, 20, 30, 40),.winnow = "FALSE")
set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",metric = "Kappa", trControl = ctrl,tuneGrid = grid_c50)
m_c50

#mtry16 and bagged tree model with same kappa statistic (.3618), are two final models with highest kappa statistics. 
#Accuracy of bagged tree model is 74.7 percent
#Whereas accuracy of mtry16 is 75.6 

