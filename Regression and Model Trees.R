##1 Exploring and preparing the data

wine <- read.csv("Regression and Model Trees Data.csv")
str(wine)

#Checking distribution of outcome variable 
hist(wine$quality)

#Even though trees are fairly robust with messy data, It is prudent to Check data (e.g outliers or other data related issues)
summary(wine)
boxplot(wine$density)
boxplot.stats(wine$density)
#According to the book example; moving forward assuming data is reliable. 

#Partitioning theh data (75/25)
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]


##2 Training a model on the data
#install.packages("rpart")
library(rpart)

m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart

#Detailed Summary of m.part
summary(m.rpart)

#Visualizing decision trees
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,type = 3, extra = 101)

##3 Evaluating model performance 
p.rpart <- predict(m.rpart, wine_test)

summary(p.rpart) #The predictions falling on a much narrower range than the true values
summary(wine_test$quality) #Model is not correctly identifying the extreme cases
cor(p.rpart, wine_test$quality) 
#A correlation of 0.54 is certainly acceptable.However, the correlation only measures how strongly the predictions 
#are related to the true value; it is not a measure of how far off the predictions were from the true values.

#Measuring performance with the mean absolute error
MAE <- function(actual, predicted) {mean(abs(actual - predicted))}
MAE(p.rpart, wine_test$quality)


#The mean quality rating in the training data is as follows:
mean(wine_train$quality)
MAE(5.87, wine_test$quality) #predicted the value 5.87 for every wine sample

#Regression tree (MAE = 0.59) comes closer on average to the true quality score than the imputed mean (MAE = 0.67), but not by much

##4 Improving model performance
#Using M5' algorithm (M5-prime)
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p

summary(m.m5p)

p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)
#The model has slightly reduced the mean absolute error