credit <- read.csv("Decision Tree Data (Credit).csv")
str(credit)

#install.packages("ipred")
library(ipred)

set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25) #default value of 25 decision trees

credit_pred <- predict(mybag, credit)

table(credit_pred, credit$default)
#Model fitted training data extremely well

#Analyzing model's future performance with 10-fold CV (Bagged decision tree model)
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",trControl = ctrl)
#The kappa statistic is 0.361 


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
