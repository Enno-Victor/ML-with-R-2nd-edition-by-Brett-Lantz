credit <- read.csv("Decision Tree Data (Credit).csv")

#Simple Tuned Model 
library(caret)
set.seed(300)

m <- train(default ~ ., data = credit, method = "C5.0")
m
#decision tree with 20 trials is the best model and 73 percent is the a realistic estimate of future performance

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


#kappa is .338