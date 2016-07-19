credit <- read.csv("Decision Tree Data (Credit).csv")

#install.packages("adabag")
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



