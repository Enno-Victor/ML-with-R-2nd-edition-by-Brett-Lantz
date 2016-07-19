
credit <- read.csv("Decision Tree Data (Credit).csv")
str(credit)

#Analyzing categorical checking and savings balance using table() output, deemed to be important predictors of loan default status.
table(credit$checking_balance)
table(credit$savings_balance)

#Analyzing some critical numeric features 
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

#Data preperation; Random training and test datasets 
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
#We should have atleast 30% of defaulted loans in each datasets, as there were 300 defaults in default variable.

#Training model on data 
#install.packages("C50")
library(C50)

credit_model <- C5.0(credit_train[-17], credit_train$default)
#17th column (default) excluded from training dataframe. 

credit_model

summary(credit_model)

#The error rate is higher (14.8%) with training dataset.

#Evaluating model performance (Checking performace of model on test dataset)
credit_pred <- predict(credit_model, credit_test)

library(gmodels)
CrossTable(credit_test$default, credit_pred,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual default', 'predicted default'))

#Accuracy of the model 
(59+14)/(59+14+19+8)
#Model Error rate 
1-(59+14)/(59+14+19+8)

#The results are even worse with test dataset with 27 percent error.Also, 42% (14/33) of actual loan defaults got correctly predited


#Improving model performance 
#Adaptive boosting
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,trials = 10)
credit_boost10

summary(credit_boost10)

#Accuracy of the model using boosting 
(629+237)/(629+237+4+30)
#Error rate
1-(629+237)/(629+237+4+30)

#Error rate is significantly less now 3.8%

#Checking on Test dataset
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual default', 'predicted default'))

#Accuracy of the model
(62+20)/(62+20+5+13)
#Error Rate 
1-(62+20)/(62+20+5+13)

#Total error rate reduced from 27 percent prior to boosting down to 18 percent in the boosted model. 
#61% (20/33) of actual loan defaults got correctly predited

#Decreasing False Negatives using cost matrix 
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,dimnames = matrix_dimensions)
error_cost
#False negative has higher cost associated to it (4) than false positive (1)

credit_cost <- C5.0(credit_train[-17], credit_train$default,costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual default', 'predicted default'))

#Accuracy of the model
(37+26)/(37+26+30+7)
#Error Rate of the model 
1-(37+26)/(37+26+30+7)

#The error rate is high (37%) than boosted model (18%),  but false negatives decreased; Previous models predicted 61 and 42% 
#actual defaults correctly, this model predicted 79% (26/33) correctly. 