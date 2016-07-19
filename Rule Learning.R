mushrooms <- read.csv("Rule Learning Data.csv", stringsAsFactors = TRUE)

#1 Exploring and preparing data 

str(mushrooms)

#Dropping incorrectly coded dataset from dataframe mushrooms
mushrooms$veil_type <- NULL

#Looking into type variable
table(mushrooms$type)
round(prop.table(table(mushrooms$type)) * 100, digits = 1)

#2 Training a model on the data  
#install.packages("RWeka")
library(RWeka)

#Using the type ~ . formula, allowing first OneR() rule learner to consider all the possible features in the 
#mushroom data while constructing its rules to predict type:

mushroom_1R <- OneR(type ~ ., data = mushrooms)

mushroom_1R

#Evaluating model performance
summary(mushroom_1R)

#Model Accuracy
(4208+3796)/(4208+3796+120)
#Error Rate 
1-(4208+3796)/(4208+3796+120)

#In the table, 1R classifier did not classify any edible mushrooms as poisonous, it did classify 120 poisonous mushrooms as edible.

#Improving performance model 
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip

#Using RIPPER Algorithm, nine rules got developed which identified edibiity of each mushroom correctly. 
