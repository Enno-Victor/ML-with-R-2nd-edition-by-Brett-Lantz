wbcd <- read.csv("k-NN Data.csv", stringsAsFactors = FALSE)
str(wbcd)

#randomizing order row-wise
set.seed(6852)       
gp = runif(nrow(wbcd))         
wbcd = wbcd[order(gp),]     
head(wbcd$diagnosis) 

#Dropping id column 
wbcd <- wbcd[-1]

#Looking at target feature/variable
table(wbcd$diagnosis)

#Coding target feature variable as factor 
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))

#prop.table() output
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Transformation- Normalizing numeric data
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

#Data preparation – creating training and test datasets
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#Training model on the data
#install.packages("class")
library(class)


wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)
#k=21; square root of 469 instances of training data

#Evaluating Model 

#install.packages("gmodels") 
library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)
#Accuracy of the model
(59+35)/(59+35+5+1)
#Error Rate in the model
1-(59+35)/(59+35+5+1)

#Results: Total table values = 100
#59 out of 60 Benign values classified correctly as Benign 
#35 out of 40 Malignant values classified correctly as Malignant

#Improving model performance 
#transformation- z-score standardization 
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq = FALSE)
#Accuracy of the model
(60+32)/(60+32+8)
#Error Rate in the model
1-(60+32)/(60+32+8)


#Unfortunately, in the following table, the results of our new transformation show a slight decrease in accuracy. 
#The instances where we had correctly classified 94 percent of examples previously, we classified only 92 percent 
#correctly this time. Making matters worse, we did not do better at classifying the dangerous false negatives.


#Testing alternative values of k on normalized training and test datasets, using k=1,5,11,15,21,27

#The 11-NN approach is more efficient, avoiding more false negatives and false positives than any other value of k.  



