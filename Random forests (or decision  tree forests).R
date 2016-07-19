credit <- read.csv("Decision Tree Data (Credit).csv")

#install.packages("randomForest")
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
