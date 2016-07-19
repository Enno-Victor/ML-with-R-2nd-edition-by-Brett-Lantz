sms_raw <- read.csv("Naive Bayes Data.csv", stringsAsFactors = FALSE)
str(sms_raw)

##Step 1; Exploring and preparing the data
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

#Step 1.1; Data preparation – cleaning and standardizing text data
#install.packages("tm") 
library(tm)

#Processing text data; creating Corpus using VCorpus
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)

#Standardizing the messages
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
as.character(sms_corpus[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

#install.packages("SnowballC")
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)


#Checking the Standardization
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

#Step 1.2;Data preparation-splitting text documents into words
#Creating DTM sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,removeNumbers = TRUE,stopwords = TRUE,removePunctuation = TRUE,stemming = TRUE))

sms_dtm
sms_dtm2

#Step 1.3; Data preperation- creating training and test datasets 
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
#Both the training data and test data contain about 13 percent spam.

#Step 1.4;Visualizing the text data- Word Clouds 
#install.packages("wordcloud")
library(wordcloud)

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#Step 1.5;Data preperation- creating indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

#Filtering DTM to include only the terms appearing in a specified vector
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#Conversion to categorical (To Yes/No), as Naive Bayes classifier is trained on data with categorical features
convert_counts <- function(x) {x <- ifelse(x > 0, "Yes", "No")}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,convert_counts)


#Step 2 – Training a model on the data
#install.packages("e1071")
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)


#Step3; Evaluating model performance 
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)

CrossTable(sms_test_pred, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))

#Accuracy of the model
(1201+153)/(1201+153+30+6)
#Error rate 
1-(1201+153)/(1201+153+30+6)

#Total 36 out of 1390 SMS messages were classified incorrectly. 

#Checking predictions probabilities
sms_test_prob <- predict(sms_classifier, sms_test, type = "raw")
head(sms_test_prob)

#comparing actual values, predicted values and predictied probabilities 
sms_results <- data.frame(actual_type = sms_test_labels, predict_type = sms_test_pred, prob_spam = sms_test_prob[,2], prob_ham = sms_test_prob[,1])
head(sms_results)

#Looking into a case where prob_spam > 0.40 & prob_spam < 0.60
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))

#Cases where the model was wrong
head(subset(sms_results, actual_type != predict_type))

#Other measures of performace (kappa statistic, sensitivity and specifity, precision and recall and F-measure) ***
install.packages("caret")
library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

#ROC Curve
#install.packages("ROCR")
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam,labels = sms_results$actual_type)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter",col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

#Step4; Improving model performance 
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn = c('predicted', 'actual'))

#The false negative results decreased from 6 to five and false positive results decreased from 30 to 28

#Visualizing comparative performance
sms_results <- cbind(sms_results, predict_type2 = sms_test_pred2)
confusionMatrix(sms_results$predict_type2,sms_results$actual_type,positive = "spam")

predicted_prob <- predict(sms_classifier2, sms_test, type = "raw")
sms_results <- cbind(sms_results, prob_spam2 = predicted_prob[,2])

pred <- prediction(predictions = sms_results$prob_spam, labels = sms_results$actual_type)
pred2 <- prediction(predictions = sms_results$prob_spam2, labels = sms_results$actual_type)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")

plot(perf, main = "ROC curves for SMS spam filter",col = "blue", lwd = 2)
plot(perf2, col = "red", lwd = 1, add = TRUE)
abline(a = 0, b = 1, lwd = 1, lty = 2)

perf.auc2 <- performance(pred2, measure = "auc")
unlist(perf.auc2@y.values)

#The AUC value for second classifier is higher, which shows second classifier with laplace classifier shows better 
#performance in genral

