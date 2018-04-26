#LOADING DATA
readfile <- read.table(file="house-votes.txt", na.strings=c("?", "NA"), sep=",", header = FALSE)
str(readfile)

#REMOVING NULL VALUES
colSums(is.na(readfile))

removeValues <- ifelse(count(readfile, "V2")[1,2] > count(readfile, "V2")[2,2], "n", "y")
readfile$V2 <- as.factor(ifelse(is.na(readfile$V2), removeValues, as.character(readfile$V2)))

removeValues <- ifelse(count(readfile, "V3")[1,2] > count(readfile, "V3")[2,2], "n", "y")
readfile$V3 <- as.factor(ifelse(is.na(readfile$V3), removeValues, as.character(readfile$V3)))

removeValues <- ifelse(count(readfile, "V4")[1,2] > count(readfile, "V4")[2,2], "n", "y")
readfile$V4 <- as.factor(ifelse(is.na(readfile$V4), removeValues, as.character(readfile$V4)))

removeValues <- ifelse(count(readfile, "V5")[1,2] > count(readfile, "V5")[2,2], "n", "y")
readfile$V5 <- as.factor(ifelse(is.na(readfile$V5), removeValues, as.character(readfile$V5)))

removeValues <- ifelse(count(readfile, "V6")[1,2] > count(readfile, "V6")[2,2], "n", "y")
readfile$V6 <- as.factor(ifelse(is.na(readfile$V6), removeValues, as.character(readfile$V6)))

removeValues <- ifelse(count(readfile, "V7")[1,2] > count(readfile, "V7")[2,2], "n", "y")
readfile$V7 <- as.factor(ifelse(is.na(readfile$V7), removeValues, as.character(readfile$V7)))

removeValues <- ifelse(count(readfile, "V8")[1,2] > count(readfile, "V8")[2,2], "n", "y")
readfile$V8 <- as.factor(ifelse(is.na(readfile$V8), removeValues, as.character(readfile$V8)))

removeValues <- ifelse(count(readfile, "V9")[1,2] > count(readfile, "V9")[2,2], "n", "y")
readfile$V9 <- as.factor(ifelse(is.na(readfile$V9), removeValues, as.character(readfile$V9)))

removeValues <- ifelse(count(readfile, "V10")[1,2] > count(readfile, "V10")[2,2], "n", "y")
readfile$V10 <- as.factor(ifelse(is.na(readfile$V10), removeValues, as.character(readfile$V10)))

removeValues <- ifelse(count(readfile, "V11")[1,2] > count(readfile, "V11")[2,2], "n", "y")
readfile$V11 <- as.factor(ifelse(is.na(readfile$V11), removeValues, as.character(readfile$V11)))

removeValues <- ifelse(count(readfile, "V12")[1,2] > count(readfile, "V12")[2,2], "n", "y")
readfile$V12 <- as.factor(ifelse(is.na(readfile$V12), removeValues, as.character(readfile$V12)))

removeValues <- ifelse(count(readfile, "V13")[1,2] > count(readfile, "V13")[2,2], "n", "y")
readfile$V13 <- as.factor(ifelse(is.na(readfile$V13), removeValues, as.character(readfile$V13)))

removeValues <- ifelse(count(readfile, "V14")[1,2] > count(readfile, "V14")[2,2], "n", "y")
readfile$V14 <- as.factor(ifelse(is.na(readfile$V14), removeValues, as.character(readfile$V14)))

removeValues <- ifelse(count(readfile, "V15")[1,2] > count(readfile, "V15")[2,2], "n", "y")
readfile$V15 <- as.factor(ifelse(is.na(readfile$V15), removeValues, as.character(readfile$V15)))

removeValues <- ifelse(count(readfile, "V16")[1,2] > count(readfile, "V16")[2,2], "n", "y")
readfile$V16 <- as.factor(ifelse(is.na(readfile$V16), removeValues, as.character(readfile$V16)))

removeValues <- ifelse(count(readfile, "V17")[1,2] > count(readfile, "V17")[2,2], "n", "y")
readfile$V17 <- as.factor(ifelse(is.na(readfile$V17), removeValues, as.character(readfile$V17)))

# Implementing Decision Tree
install.packages("C50")
library(C50)
c5model <- C5.0(readfile[-1], readfile$V1)
set.seed(97)
sampledata <- sample(435, 345)
#test and train data
c5_train <- readfile[sampledata,]
credit_test <- readfile[-sampledata, ]
c5_predicted <- predict(c5model, c5_test)
library(gmodels) 
CrossTable(c5_test$V1, c5_predicted, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
c5_pred_probability <- predict(c5model, c5_test, type ="prob")
head(c5_pred_probability)

# Implementing Naive Bayes
install.packages("tm")
library(tm)
library(SnowballC)
readfileNB <- readfile
readfileNB$V1 <- factor(readfileNB$V1)
#test and train data
readfileNB_train <- readfileNB[1:345, ]
readfileNB_test <- readfileNB[346:435, ]
#test and train labels
readfileNB_train_labels <- readfileNB[1:335, ]$V1 // readfileNB_train_labels <- readfileNB[1:345, ]$V1
readfileNB_test_labels <- readfileNB[336:435, ]$V1// readfileNB_test_labels <- readfileNB[346:435, ]$V1
install.packages("e1071")
library(e1071)
readfileNB_classifier <- naiveBayes(readfileNB_train, readfileNB_train_labels)
readfileNB_test_pred <- predict(readfileNB_classifier, readfileNB_test)
readfileNB_test_prob <- predict(readfileNB_classifier, readfileNB_test, type = "raw")
CrossTable(readfileNB_test_pred, readfileNB_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
head(readfileNB_test_prob)


#CROSS-VALIDATION
install.packages("caret")
library(caret)
folds <- createFolds(readfile$V1, k = 10)
str(folds)
readfile01_test <- readfile[folds$Fold01, ]
readfile01_train <- readfile[-folds$Fold01, ]
install.packages("irr")
library(irr)

#cross-validation Desicion Tree
cross_validation_Decision_Tree <- lapply(folds, function(x) {
  readfile_train <- readfile[-x, ]
  readfile_test <- readfile[x, ]
  readfile_model <- C5.0(V1 ~ ., data = readfile_train)
  readfile_pred <- predict(readfile_model, readfile_test)
  readfile_actual <- readfile_test$V1
  kappa <- kappa2(data.frame(readfile_actual, readfile_pred))$value
  return(kappa)
   })
str(cross_validation_Decision_Tree)
mean(unlist(cv_resultsDT)) // mean(unlist(cross_validation_Decision_Tree))

#cross-validation Naive Bayes
cross_validation_Naive_Bayes <- lapply(folds, function(x) {
  readfile_train <- readfile[-x, ]
  readfile_test <- readfile[x, ]
  readfile_model <- naiveBayes(V1 ~ ., data = readfile_train)
  readfile_pred <- predict(readfile_model, readfile_test)
  readfile_actual <- readfile_test$V1
  kappa <- kappa2(data.frame(readfile_actual, readfile_pred))$value
  return(kappa)
})
str(cross_validation_Naive_Bayes)
mean(unlist(cross_validation_Naive_Bayes))


#AUTOMATED PARAMETER TUNING
#Decision tree
set.seed(1)
APT_DT<- train(V1 ~ ., data = readfile, method = "C5.0")
APT_DT
APT_DT$finalModel
plot(APT_DT)
p <- predict(APT_DT, readfile)
table(p, readfile$V1) 
head(predict(APT_DT, readfile, type = "prob"))

#Naive Bayes
set.seed(1)
APT_NB<- train(V1 ~ ., data = readfile, method = "nb")
APT_NB
APT_NB$finalModel
plot(APT_NB)
p <- predict(APT_NB, readfile)
table(p, readfile$V1) 
head(predict(APT_NB, readfile, type = "prob"))


#Ensemble Learning - Bagging
install.packages("ipred")
library(ipred)
install.packages("kernlab")
library(kernlab)

#Decision Tree
str(EL_Bagging_DT)
bagctrl <- bagControl(fit = EL_Bagging_DT$fit,predict = EL_Bagging_DT$pred,aggregate = EL_Bagging_DT$aggregate) 
set.seed(123)
ctrl <-trainControl(method = "cv", number = 10)
EL_Bagging_DT <- train(V1 ~ ., data = readfile , trControl = ctrl, bagControl = bagctrl)
EL_Bagging_DT

#Naive Bayes
str(EL_Bagging_NB)
bagctrl <- bagControl(fit = EL_Bagging_NB$fit,predict = EL_Bagging_NB$pred,aggregate = EL_Bagging_NB$aggregate) 
set.seed(1)
ctrl <-trainControl(method = "cv", number = 10)
EL_Bagging_NB <- train(V1 ~ ., data = readfile, "bag",trControl = ctrl, bagControl = bagctrl)
EL_Bagging_NB
