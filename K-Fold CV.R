credit <- read.csv("Decision Tree Data (Credit).csv")

library(caret)
library(C50)
#install.packages("irr")
library(irr)


set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)

mean(unlist(cv_results))

# The kappa statistic (.28) is fairly low, corresponding to "fair" on the interpretation scale




 