###Assignment:  Decision trees and random forest

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory


#Use decision trees and random forest to predict wine quality from its chemical properties
#Dataset red wine from https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv 

#Load packages

library(rpart)
library (caret)
library(randomForest)


#Load data from the CSV file
redwine <- read.csv2("winequality-red.csv", header = TRUE, sep= ";", quote= "", stringsAsFactors= FALSE)

#Check 
View(redwine) #Preview data
str(redwine) #Check internal structure of the data frame

###Change column names to easier to read
#Delete X. in the beginning of column names
names(redwine) <- gsub(pattern = "X.", replacement = "", names(redwine))
#Delete the period in the end of column names
names(redwine) <- gsub(pattern = ".$", replacement = "", names(redwine))
#Check results
str(redwine)

###Data type conversions
#Convert the response variable to factor
redwine$quality <- as.factor(redwine$quality)
#Convert all other columns to numeric
redwine[1:11] <- lapply(redwine[1:11], as.numeric)
#Check results
str(redwine)

###Summary statistics for all variables in the dataset
summary(redwine)

#Group wine according to quality ratings
#3 and 4 - low
#5 and 6 - midium
#7 and 8 high

redwine$quality <- as.factor(with(redwine, ifelse(quality%in%c("3","4"), "low", (ifelse(quality%in%c("5", "6"), "medium", "high"))))) 
summary(redwine)
str(redwine)

#Prepare training and testing datasets (80% training, 20% testing)

set.seed(15)
ind <- sample.int(n=nrow(redwine), size=floor(0.8*nrow(redwine)), replace = FALSE)
redwine_train <-  redwine[ind, ] #training set
redwine_test <- redwine[ -ind, ] #testing set

#Check resulting datasets
dim(redwine_train)
dim(redwine_test)

#Compare proportions between low, medium and high quality wines in testing and training data
prop.table(table(redwine_train$quality))
prop.table(table(redwine_test$quality))

#Build classification tree model
winequality_rp <- rpart(quality ~ ., data = redwine_train)

#Retrieve node details
winequality_rp

#Examine complexity
printcp(winequality_rp)

#Plot cost complexity parameters
plotcp(winequality_rp)

#Examine the model
summary(winequality_rp)

#Visualize the three
#using the uniform tree presentation
plot(winequality_rp, uniform = TRUE, branch = 0.6, margin = 0.1)
text(winequality_rp, all=TRUE, use.n=TRUE)

###Evaluate the model predictive performance
#Generate predictions
predictions <- predict(winequality_rp, redwine_test, type="class")

table(redwine_test$quality, predictions)

#generate confusion matrix using caret package
confusionMatrix(table(predictions, redwine_test$quality))

###Pruning the tree
#Find minimum cross-validation error
min(winequality_rp$cptable[ ,"xerror"])

#Find the record with the minimum cross-validation error
which.min(winequality_rp$cptable[ ,"xerror"])

#Get the cost complexity parameter of the record with min cross-validation error
winequality_rp.cp <- winequality_rp$cptable[6, "CP"] 

#Prune the tree
winequality_prune <- prune(winequality_rp, cp=winequality_rp.cp)

#Examine the pruned model
summary(winequality_prune)

#Visualize the prunned model
#using the uniform tree presentation
plot(winequality_prune, uniform = TRUE, branch = 0.6, margin = 0.1)
text(winequality_prune, all=TRUE, use.n=TRUE)

#Generate classification table
predictions <- predict(winequality_prune, redwine_test, type = "class")
table(redwine_test$quality, predictions)

#Grenrate confusion matrix
confusionMatrix(table(predictions, redwine_test$quality))

#######Random Forest##############
winequality_rf <- randomForest(quality ~ ., data = redwine_train, importance = T)

#display model details                           
winequality_rf 

#Make predictions
predictions_rf <- predict(winequality_rf, redwine_test)

#Classification table
table(predictions_rf, redwine_test$quality)

#Confusion matrix
confusionMatrix(table(predictions_rf, redwine_test$quality))

#Plot the mean square error of the forest object
plot(winequality_rf)

#Examine importance of each attribute
importance(winequality_rf)

#Plot varaible importance
varImpPlot(winequality_rf)

#calculate margins and plot the margin cumulative distribution
margins_rf <- margin(winequality_rf, redwine_train)
plot(margins_rf)

#Visualize margin distribution
hist(margins_rf, main="Margins of Random Forest for Red Wine dataset")

#Visualize margins by class
boxplot(margins_rf ~ redwine_train$quality, main = "Margins of Random Forest for Red Wine dataset by class")







