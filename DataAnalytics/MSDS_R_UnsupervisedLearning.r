# Unsupervised Learning 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory


#######################################################################
######Using Supervised Learning for Spam Detection######################
######Based on A Gentle Introduction to Data Classification with R######
######https://blog.paperspace.com/intro-to-datascience/#################
#######################################################################


###Load packages

#for statistical analysis of textual data
install.packages("quanteda")

library(quanteda)


###Load SMS SPAM COLLECTION 
###https://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection 

#Load Data into a table 
raw.data <- read.table("SMSSpamCollection", header = FALSE, sep="\t", quote="", stringsAsFactors = FALSE)

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

####Pre-processing sms data, wieght word counts
sms.dfm <- dfm(sms.corpus, tolower = TRUE) #convert to lower case
sms.dfm <-dfm_trim(sms.dfm, min_docfreq = 3)
sms.dfm <- dfm_weight(sms.dfm)


###Perform K-means clustering

sms.cluster <- kmeans(sms.dfm, 2) # Chose 2 clusters( for spam and ham)
sms.cluster$size # COutput the number of objects in each cluster

