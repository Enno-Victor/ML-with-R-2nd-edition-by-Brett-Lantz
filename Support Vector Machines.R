##Exploring and preparing the data

letters <- read.csv("Support Vector Machines Data.csv")
str(letters)

#Partitioning Data (80/20)
#Dataset is already rendomized (Slate and Frey, 1991)
set.seed(12345)
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

#Training a model on the data
#install.packages("kernlab") 
library(kernlab)

letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot") #Using Linear Kernel 

letter_classifier

##Evaluating model performance
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

#Comparing the predicted letter to the true letter in the testing dataset
table(letter_predictions, letters_test$letter)

#Checking if model's predicted letter matches with the actual letter in the test dataset
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))
vanilla <- sum(agreement==TRUE)

#Accuracy of 84 percent 

##Improving model performance
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot") #Using Gausian RBF kernel 

letter_predictions_rbf <- predict(letter_classifier_rbf,letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter

table(agreement_rbf)

prop.table(table(agreement_rbf))

rbf <- sum(agreement_rbf==TRUE)

#Changing Kernel resulted in accuracy of character recognition from 84 percent to 93 percent. Checking classifier with 


#####Different types of kernels in kernlab package####
# rbfdot Radial Basis kernel function "Gaussian
# polydot Polynomial kernel functio
# vanilladot Linear kernel functio
# tanhdot Hyperbolic tangent kernel function
# laplacedot Laplacian kernel function
# besseldot Bessel kernel function
# anovadot ANOVA RBF kernel function 
# splinedot Spline kernel
# stringdot String kernel3

set.seed(12345)
letter_classifier_bessel <- ksvm(letter ~ ., data = letters_train,  kernel = "besseldot") #Using Bessel kernel 

letter_predictions_bessel <- predict(letter_classifier_bessel,letters_test)

agreement_bessel<- letter_predictions_bessel == letters_test$letter

table(agreement_bessel)

prop.table(table(agreement_bessel))

bessel <- sum(agreement_bessel==TRUE)

#Accuracy of 71%

set.seed(12345)
letter_classifier_laplace <- ksvm(letter ~ ., data = letters_train, kernel = "laplace") #Using Laplace kernel 

letter_predictions_laplace <- predict(letter_classifier_laplace,letters_test)

agreement_laplace <- letter_predictions_laplace == letters_test$letter

table(agreement_laplace)

prop.table(table(agreement_laplace))

laplace <- sum(agreement_laplace==TRUE)

#Accuracy of 89%


#Plotting Classification Accuracy for four diffrent kernels 
names <- c("Linear Kernel Function","Radial Basis Function","Bessel Function","Laplace Function") 
values <- c(vanilla, rbf, bessel,laplace) 
df <- data.frame(names,values) 
barplot(df$values , names.arg=df$names,width=1,cex.names=0.5,main=paste("Classification Accuracy"))


### Increasing the cost of constraints parameter C (Lantz, B. 2015)
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, C=10, kernel = "rbfdot") #Using Gausian RBF kernel 

letter_predictions_rbf <- predict(letter_classifier_rbf,letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter

table(agreement_rbf)

prop.table(table(agreement_rbf))

#The accuracy increased to 97% by increasing cost of constraints parameter C to 10

