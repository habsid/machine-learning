# Bayesian classifiers are a special case of MAP estimation,The Naive Bayes classifier is a special case of Bayes classifier that assumes class-conditional independence,
#load library
library("tm") #FOR TEXT PROCESSING
library("SnowballC")
library("wordcloud")
library(NLP)
library(e1071) 
library('plyr')

#set the directory to home
setwd("C:/Users/HABEEBA SIDDIQUI/Documents/machile learning/Github/sms_spam/spam_data")

#train is the folder which is present in home directory and spam is inside train
file_path_trainspam = file.path(".","train","spam")

#we set reader to readPlain as we dont know the format of the file
train_spam_Corpus <-VCorpus(DirSource(file_path_trainspam), readerControl = list(reader = readPlain ))
#sms_corpus <- VCorpus(VectorSource(sms_raw$text))

file_path_trainham = file.path(".","train","ham")
train_ham_Corpus <-VCorpus(DirSource(file_path_trainham), readerControl = list(reader = readPlain ))

file_path_testspam = file.path(".","test","spam")
test_spam_Corpus <-VCorpus(DirSource(file_path_testspam), readerControl = list(reader = readPlain ))

file_path_testham = file.path(".","test","ham")
test_ham_Corpus <-VCorpus(DirSource(file_path_testham), readerControl = list(reader = readPlain ))

#merge the train datasets
train_data <- c( train_spam_Corpus, train_ham_Corpus)
inspect(train_data[5])
as.character(train_data[5])

#merge the test datasets
test_data <- c(test_spam_Corpus, test_ham_Corpus)
inspect(train_data[5])
as.character(train_data[5])

#cleaning the data
corpus_train_clean <- tm_map(train_data,content_transformer(tolower))
corpus_test_clean <- tm_map(test_data,content_transformer(tolower))

#cleaning train spam ham only
corpus_train_spam_clean <- tm_map(train_spam_Corpus, content_transformer(tolower))
corpus_train_ham_clean <- tm_map(train_ham_Corpus, content_transformer(tolower))

#remove numbers
corpus_train_clean <- tm_map(corpus_train_clean, removeNumbers)
corpus_test_clean <- tm_map(corpus_test_clean, removeNumbers)
corpus_train_spam_clean <- tm_map(corpus_train_spam_clean, removeNumbers)
corpus_train_ham_clean <- tm_map(corpus_train_ham_clean, removeNumbers)

#remove stopwords
corpus_train_clean <- tm_map(corpus_train_clean, removeWords, stopwords())
corpus_test_clean <- tm_map(corpus_test_clean, removeWords, stopwords())
corpus_train_spam_clean <- tm_map(corpus_train_spam_clean, removeWords, stopwords())
corpus_train_ham_clean <- tm_map(corpus_train_ham_clean, removeWords, stopwords())

#replacing all punctation marks with whitespaces
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ",x)
}

#replace punctuations
corpus_train_clean <- tm_map(corpus_train_clean, replacePunctuation)
corpus_test_clean <- tm_map(corpus_test_clean, replacePunctuation)
corpus_train_spam_clean <- tm_map(corpus_train_spam_clean, replacePunctuation)
corpus_train_ham_clean <- tm_map(corpus_train_ham_clean, replacePunctuation)

#implement stemming of words for text standardization
corpus_train_clean <- tm_map(corpus_train_clean, stemDocument)
corpus_test_clean <- tm_map(corpus_test_clean, stemDocument)
corpus_train_spam_clean <- tm_map(corpus_train_spam_clean, stemDocument)
corpus_train_ham_clean <- tm_map(corpus_train_ham_clean, stemDocument)

#remove ADDITIONAL white spaces
corpus_train_clean <- tm_map(corpus_train_clean, stripWhitespace)
corpus_test_clean <- tm_map(corpus_test_clean, stripWhitespace)
corpus_train_spam_clean <- tm_map(corpus_train_spam_clean, stripWhitespace)
corpus_train_ham_clean <- tm_map(corpus_train_ham_clean, stripWhitespace)

#convert to plain document
corpus_train_clean <- tm_map(corpus_train_clean, PlainTextDocument)
corpus_test_clean <- tm_map(corpus_test_clean, PlainTextDocument)
corpus_train_spam_clean <- tm_map(corpus_train_spam_clean, PlainTextDocument)
corpus_train_ham_clean <- tm_map(corpus_train_ham_clean, PlainTextDocument)

#creating wordcloud of words having frequenct more than 50
wordcloud(corpus_train_spam_clean, min.freq = 50)
wordcloud(corpus_train_ham_clean, min.freq = 50)

###creating DocumentTermMatix to determine frequency of terms that occur in the documents.
corpus_train_dtm <- DocumentTermMatrix(corpus_train_clean)
corpus_test_dtm <- DocumentTermMatrix(corpus_test_clean)
dtm_train<-corpus_train_dtm[,]
dtm_test<-corpus_test_dtm[,]


sms_freq_words <- findFreqTerms(dtm_train, 5)
sms_dtm_freq_train<- dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- dtm_test[ , sms_freq_words] 

#writing a function to change the data to categorical format
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

#apply the function
train_final <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
test_final <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)
nrow(train_final)
nrow(test_final)

#creating train labels Using rep function
train_spam_val<- rep("SPAM",length(1:123))
train_ham_val<- rep("HAM",length(1:340))
train_labels <-as.factor(c(train_spam_val,train_ham_val))


#creating test labels Using rep function
test_spam_val<- rep("SPAM",length(1:130)) 
test_ham_val<- rep("HAM",length(1:348))
test_labels <-as.factor(c(test_spam_val,test_ham_val))


#Implement Naive Bayes classification
classifier1 <- naiveBayes(train_final, train_labels)
test_pred <- predict(classifier1, test_final)

library(gmodels)
CrossTable(test_pred, test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

#Improving Model Performance with Laplacian coerrection
#Implement Naive Bayes classification
classifier2 <- naiveBayes(train_final, train_labels, laplace = 1)
test_pred2 <- predict(classifier2, test_final)

library(gmodels)
CrossTable(test_pred2, test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))
