###  Validity and Reliability 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

###         The following spam filter code was previousely used  in a             ###       
###               separate machine learning project                               ### 
###   It is reprodiced here for the sole purpose of demonstrating various         ###
###                 performance measures  - see below                             ### 
###                                                                               ###  
###          Using Supervised Learning for Spam Detection                         ###
###   Based on A Gentle Introduction to Data Classification with R                ###
###   https://blog.paperspace.com/intro-to-datascience/                           ###

###   Data exploration steps and visualizations were omitted from the code        ###


###Load packages

#for statistical analysis of textual data
library(quanteda)
library(caret)
#for calculating kappa statistic
library(vcd)

###Load SMS SPAM COLLECTION 
###https://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection 

#Load Data into a table 
raw.data <- read.table("SMSSpamCollection.txt", header = FALSE, sep="\t", quote="", stringsAsFactors = FALSE)

#Add column names
names(raw.data) <- c("Label", "Text")

#Check the structure and content of the data table
summary(raw.data)
table(raw.data$Label)

#Randomizse data using sample() command
set.seed(1912)
raw.data <- raw.data[sample(nrow(raw.data)),]
View(raw.data) #Check data 

###Construct corpus object
sms.corpus <- corpus(raw.data$Text) #Construct corpus using text field
docvars(sms.corpus) <- raw.data$Label #Attach Label field as a document variable using docvars() command

####Pre-processing sms data, wieght word counts
sms.dfm <- dfm(sms.corpus, tolower = TRUE) #convert to lower case
sms.dfm <-dfm_trim(sms.dfm, min_docfreq = 3)
sms.dfm <- dfm_weight(sms.dfm)

#Split the data set into traing and testing subsets
#80% training and 20% testing data 
#since the initial set was randomised we can just split
#observations [1:4459] and [4460:5574]
sms.raw.train <-raw.data[1:4459,]#Training part of the raw dataset
sms.raw.test <-raw.data[4460:nrow(raw.data),] #Testing part of the raw data

#Split DFM into training and testing parts
sms.dfm.train <- sms.dfm[1:4459,] #training part DFM
sms.dfm.test <- sms.dfm[4460:nrow(raw.data),] #Testing part DFM

#Use a Naive Bayes classification model to classify the text messages
#use textmodel_NB command from quanteda package

###Fit the model
#Use trainging DFM and a vector of associated labels - sms.raw.train$Label
sms.classifier <- textmodel_nb(sms.dfm.train, sms.raw.train$Label)

###Use sms.classifier to generate predictions on test data
sms.predictions <-predict(sms.classifier, newdata = sms.dfm.test)
#Display prediction results vs. actuual labels
table1 <- table(sms.predictions, sms.raw.test$Label)
table1

######                        ######   
###### Performance evaluation ######
######                        ######

#Using vcd package to calculate Kappa statistic
Kappa(table1)

#Confusion Matrix using caret package
confusionMatrix(table1)

#Sensitivity using caret package
sensitivity(table1) #Recall (Sensitivity, true positive rate)

#Precision using caret package
precision(table1) #Precision, positive predictive value

#Specificity using caret package
specificity(table1)

#F-measure using caret
F_meas(table1)

