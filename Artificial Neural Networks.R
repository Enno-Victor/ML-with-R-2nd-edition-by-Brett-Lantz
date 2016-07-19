##1Exploring and preparing the data

concrete <- read.csv("Artificial Neural Networks Data.csv")
str(concrete)

#Normalzing data 
normalize <- function(x) {return((x - min(x)) / (max(x) - min(x)))}
concrete_norm <- as.data.frame(lapply(concrete, normalize))

#Checking if function worked 
summary(concrete_norm$strength)
summary(concrete$strength)

#Partitioning the data (75/25)
set.seed(12345)
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

##Training a model on the data
#install.packages("neuralnet") 
library(neuralnet) 

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)
plot(concrete_model)

## Evaluating model performance
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength)
#With Correlation .806 our model is doing a fairly good job, even with only a single hidden node

##Improving model performance 
set.seed(12345)
concrete_model2 <- neuralnet(strength ~ cement + slag +ash + water + superplastic +coarseagg + fineagg + age, data = concrete_train, hidden = 5)
plot(concrete_model2)

#Error reduced from 5.08 in the previous model to 1.63 in this model

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

#Correlation increased with 5 nodes, .92. It is better than previous results of .80. 
# The model reported mean correlation of .885. 
