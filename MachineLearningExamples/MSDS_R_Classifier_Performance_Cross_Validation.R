###  Classifier Performance and Cross Validation

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Age prediction for abalone shells based on phisical measurements
#Abalone dataset https://archive.ics.uci.edu/ml/datasets/Abalone

#Load packages
library(caret)
library(e1071)
library(rpart)

#Load data
abalone <- read.table("abalone.data", header = FALSE, sep = ",")

#Check 
View(abalone) #Preview data
str(abalone) #Check internal structure of the data frame

#Add column names
names(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")
str(abalone) #Check results

###EDA

head(abalone) #First few rows
summary(abalone) #summary stats for all variables

sum(is.na(abalone)) #Missing values?

#Boxplots for all numerical variables in the dataset
par(mfrow=c(2,4))
for (i in 2:9){
  boxplot(abalone[i], main=i)
}

#Data Pre-Processing
#Encoding sex variable
table(abalone$sex)

abalone$female <- ifelse(abalone$sex == "F", 1, 0)
abalone$infant <- ifelse(abalone$sex == "I", 1, 0)

#Check results
table(abalone$female)
table(abalone$infant)
str(abalone)
#delete sex column
abalone <- abalone[,-1]
#check results
str(abalone)

#Z-score standartization for columns with measurement data
abalone_z <- as.data.frame(lapply(abalone[, 1:7], scale))
head(abalone_z)
str(abalone_z)

#attach rings, female and infant columns
abalone_fullz <- cbind(abalone_z, abalone$rings, abalone$female, abalone$infant)
names(abalone_fullz)[8:10] <- c("rings", "female", "infant")
head(abalone_fullz)
str(abalone_fullz) 

#add age column instead of rings
abalone_fullz$age <- abalone_fullz$rings + 1.5
#assign abalone data to the new object
abalone_new <-abalone_fullz
#aggregate into 3 groups
abalone_new$age <- cut(abalone_new$age, breaks = c(0,7,12,100), labels = c("young", "adult", "old"))
abalone_new$age <-as.factor(abalone_new$age)
#remove rings column
abalone_new <- subset(abalone_new, select = -rings)

#check results
#abalone_new - full df ready to fit models
str(abalone_new)
head(abalone_new)

#Split the dataset into 80% training and  20% testing data
set.seed(123)
split = 0.08
trainIndex <- createDataPartition(abalone_new$age, p=split, list = FALSE)
abalone_test<- abalone_new[trainIndex, ]
abalone_train <- abalone_new[-trainIndex, ]

#check results
str(abalone_train)
str(abalone_test)

#Test dataset without the age column
abalone_testnoage <- abalone_test[-10]
str(abalone_testnoage)

########Train the models########

####Naive Bayes
set.seed(123)
nb_model <- naiveBayes(age ~ ., data=abalone_train)

#Generate predictions
nb_prediction <- predict(nb_model,newdata = abalone_testnoage)

#generate classification table
nb_table1 <- table(nb_prediction, abalone_test$age)
nb_table1

#Evaluate model performance
confusionMatrix(nb_table1)


###Build classification tree model
set.seed(234)
abalone_rp <- rpart(age ~ ., data = abalone_train)

abalone_rp #node details
printcp(abalone_rp) #complexity
plotcp(abalone_rp) #Plot cost complexity parameters

#Examine the model
summary(abalone_rp)

#Visualize the tree
#using the uniform tree presentation
par(mfrow=c(1,1))
plot(abalone_rp, uniform = TRUE, branch = 0.6, margin = 0.1)
text(abalone_rp, all=TRUE, use.n=TRUE)

###Evaluate predictive performance
#Generate predictions
predict_rp1 <- predict(abalone_rp, abalone_test, type="class")

table(abalone_test$age, predict_rp1)

#generate confusion matrix using caret package
confusionMatrix(table(predict_rp1, abalone_test$age))

###Cross Validation#####

train_control <- trainControl(method="cv", number =10)

#cross-validation with Naive Bayes
set.seed(345)
nb_modelCV <- train(age ~ ., data = abalone_train, method = "nb",  trControl=train_control)

nb_modelCV

#Generate predictions
nb_predictionCV <- predict(nb_modelCV,newdata = abalone_testnoage)

#generate classification table
nb_tableCV <- table(nb_predictionCV, abalone_test$age)
nb_tableCV

#Evaluate model performance
confusionMatrix(nb_tableCV)


###Cross Validation with rpart
set.seed(456)
rpr_modelCV <- train(age~ ., data =abalone_train, method = "rpart", trControl = train_control)
rpr_modelCV

#generate predictions
rpr_predictionCV <-predict(rpr_modelCV, newdata = abalone_testnoage)
#generate classification table
rpr_tableCV <- table(rpr_predictionCV, abalone_test$age)
rpr_tableCV

#Evaluate model performance
confusionMatrix(rpr_tableCV)


  

  


