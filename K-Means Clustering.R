##Exploring and preparing the data
teens <- read.csv("K-Means Clustering Data.csv")
str(teens)
View(teens)

table(teens$gender)

#Adding NA values
table(teens$gender, useNA = "ifany")

summary(teens$age)


teens$age <- ifelse(teens$age >= 13 & teens$age < 20,teens$age, NA)

summary(teens$age)

#Data preparation – dummy coding missing values
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

#Checking constructed dummy variables with original dummy variable
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

#The number of 1 values for teens$female and teens$no_gender matches the number of F and NA values

##Data preparation – Imputing (IMPUTATION) the missing values
mean(teens$age)
mean(teens$age, na.rm = TRUE)
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
summary(teens$age)

##Training a model on the data
library(stats)

#Selecting 36 features representing interests 
interests <- teens[5:40] 

#Standardization usinh z-score
interests_z <- as.data.frame(lapply(interests, scale))

set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

#Evaluating model performance
teen_clusters$size

teen_clusters$centers

#Improving model performance
teens$cluster <- teen_clusters$cluster

teens[1:5, c("cluster", "gender", "age", "friends")]

aggregate(data = teens, age ~ cluster, mean)

aggregate(data = teens, female ~ cluster, mean)

aggregate(data = teens, friends ~ cluster, mean)

