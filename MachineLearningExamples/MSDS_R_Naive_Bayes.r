###Project: Naive Bayes   

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

###Use Naive Bayes to create a SMS spam filter
###Data set SMSSpamCollection.txt from http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection

#Load packages

library("tm")
library ("SnowballC")
library("wordcloud")  
library("RColorBrewer")
library("e1071")
library("gmodels")
library("ggplot2") 

###Load SMS SPAM COLLECTION 
###https://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection 

#Load Data into a table 
raw.data <- read.table("SMSSpamCollection.txt", header = FALSE, sep="\t", quote="", stringsAsFactors = FALSE)

#Add column names
names(raw.data) <- c("Label", "Text")

#Check the structure and content of the data table
summary(raw.data)
table(raw.data$Label)

#Change Label type from char to factor
raw.data$Label <- factor(raw.data$Label)

#Randomizse data using sample() command
set.seed(2018)
raw.data <- raw.data[sample(nrow(raw.data)),]
View(raw.data) #Check data 

#Create corpus using VCorpus() from the tm package

corpus.data <- VCorpus(VectorSource(raw.data$Text))

#Print the corpus
print(corpus.data)

#Look at the first two messages 
inspect(corpus.data[1:2])
lapply(corpus.data[1:2], as.character)

###Text transformations

#Change to lower case
corpus.clean <- tm_map(corpus.data, content_transformer(tolower))

#Check results
#original message
as.character(corpus.data[[1]])
#processed message
as.character(corpus.clean[[1]])

#Remove numbers
corpus.clean <- tm_map(corpus.clean, removeNumbers)

#Remove stop words
corpus.clean <- tm_map(corpus.clean, removeWords, stopwords())

#Remove punctuation
corpus.clean<- tm_map(corpus.clean, removePunctuation)

#Stemming
corpus.clean <- tm_map(corpus.clean, stemDocument)

#Remove extra white spaces
corpus.clean <- tm_map(corpus.clean, stripWhitespace)

#Check results by comparing first five messages before and after processing
lapply(corpus.data[1:5], as.character)
lapply(corpus.clean[1:5], as.character)

###Tokenization  -  Document-Term-Matrix (DTM)

#Create DTM
dtm.all <-DocumentTermMatrix(corpus.clean)

#Check results
dtm.all

###Split data into the training and testing sets
dtm.train <- dtm.all[1:4459, ] #80% training data
dtm.test <- dtm.all[4460:5574, ] #20% testing data

#Create vectors with labels for each row
labels.train <- raw.data[1:4459,]$Label
labels.test <- raw.data[4460:5574,]$Label

#Compare proportions between spam and ham in testing and training data
prop.table(table(labels.train))
prop.table(table(labels.test))


#Top 50 words for the combine data set 
wordcloud(corpus.clean, min.freq = 50, random.order = FALSE)#####Test analysis


#Top five words for all messages
wordcloud(corpus.clean, max.words = 5, random.order = FALSE)

#create a subset of spam messages
spam <-subset(raw.data, Label == "spam")
ham <- subset(raw.data, Label == "ham")

#Create word clouds with the 5 most frequent words for spam and ham separately
wordcloud(spam$Text, max.words = 5, colors=brewer.pal(8, "Dark2"))
wordcloud(ham$Text, max.words = 5, colors=brewer.pal(8, "Dark2"))

#Find the most frequent terms in all messages
findFreqTerms(dtm.all, lowfreq = 200)

#Create a list of frequncy words
freq <-colSums(as.matrix(dtm.all))
#Sort in descending order
ord <- order(freq, decreasing = TRUE)
#Display the most frequently occuring words
freq[head(ord, 20)]
#Show the least frequently occuring words
freq[tail(ord, 20)]

####Model

###Creating indicators for frequent words
train_freq_words <- findFreqTerms(dtm.train, 5)
str(train_freq_words)

#Filter DTM to keep only the words in train_freq_words

dtm.train.freq <- dtm.train[ , train_freq_words]
dtm.test.freq <- dtm.test[ , train_freq_words]

#Function to convert word counts to categorical 
convert_counts <- function(x) {
  x <- ifelse (x >0, "Yes", "No")
}

#Convert the training data set
dtm.train.model <- apply(dtm.train.freq, MARGIN = 2, convert_counts)

#convert the testing data set
dtm.test.model <- apply(dtm.test.freq, MARGIN=2, convert_counts)

#Fit the model
nb_model <- naiveBayes(dtm.train.model, labels.train)

#Use the model for predictions
test_prediction <- predict(nb_model, newdata = dtm.test.model)

#Evaluate model performance
CrossTable(test_prediction, labels.test, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c("predicted", "actual"))
confusionMatrix(test_prediction)

#Adjusting the model 
#Add Laplace estimator

nb_model2 <- naiveBayes(dtm.train.model, labels.train, laplace = 1)

#Use the second model for predictions
test_prediction2 <- predict(nb_model2, newdata = dtm.test.model)

#Evaluate performance for model 2
CrossTable(test_prediction2, labels.test, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c("predicted", "actual"))

