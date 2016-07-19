##1 Exploring and preparing the data

insurance <- read.csv("Linear Regression Data.csv", stringsAsFactors = TRUE)
str(insurance)

#Checking dependent variable
summary(insurance$expenses)

#mean value is greater than the median, this implies that the distribution of insurance expenses is right-skewed. 
#Confirming this visually using a histogram:
hist(insurance$expenses)

#Exploring relationships among features – the correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

#Visualizing relationships among features – the scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])

#install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

##2 Training a model on the data
ins_model <- lm(expenses ~ age + children + bmi + sex +smoker + region, data = insurance)
 # or 
ins_model <- lm(expenses ~ ., data = insurance)

##3 Evaluating model performance
summary(ins_model)

#Improving model performance
#Model specification – adding non-linear relationships
insurance$age2 <- insurance$age^2

#Transformation – converting a numeric variable to a binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

#Model specification – adding interaction effects
#expenses ~ bmi30 + smokeryes + bmi30:smokeryes

#Improved regression model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +bmi30*smoker + region, data = insurance)

summary(ins_model2)
