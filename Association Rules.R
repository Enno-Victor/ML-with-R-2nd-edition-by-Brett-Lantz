
## Exploring and preparing the data
groceries <- read.csv("Association Rules Data.csv", head = FALSE)
str(groceries) 
View(groceries)

##Data preparation – creating a sparse matrix for transaction data
#install.packages("arules")
library(arules)

#Reading data into sparse matrix grocery
groceries <- read.transactions("Association Rules Data.csv", sep = ",")
summary(groceries)

#Looking at first five transactions
inspect(groceries[1:5])

#Looking for proportion for first three items in sparse matrix 
itemFrequency(groceries[, 1:3])

## Visualizing item support – item frequency plots
itemFrequencyPlot(groceries, support = 0.1)

itemFrequencyPlot(groceries, topN = 20)

##Visualizing the transaction data – plotting the sparse matrix
image(groceries[1:5])

#matrix diagram with 100 rows and 169 columns
image(sample(groceries, 100))

##Training a model on the data
apriori(groceries) # Using default seetings; support = 0.1 and confidence = 0.8

#Adding support and confidence parameters for reasona number of association rules
groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

groceryrules

##Evaluating model performance
summary(groceryrules)

inspect(groceryrules[1:3])

## Improving model performance 

inspect(sort(groceryrules, by = "lift")[1:5]) # Best five rules according to lift statistic

berryrules <- subset(groceryrules, items %in% "berries") # subsetting association rules
inspect(berryrules)

write(groceryrules, file = "groceryrules.csv",sep = ",", quote = TRUE, row.names = FALSE)


