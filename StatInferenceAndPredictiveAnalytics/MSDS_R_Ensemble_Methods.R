### Assignment:   Pima Indians Diabetes Dataset              

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

###Load packages
library(mlbench)
library(caret)
library(adabag)
library(fastAdaboost)


####Load data

data(PimaIndiansDiabetes)
#Save under a different name to keep separate from the original data file
diabetes <- PimaIndiansDiabetes

###EDA

str(diabetes) #df structure
colnames(diabetes)[9] <- "class" #change name for the target variable to class
summary(diabetes) #summary for all variables
head(diabetes) #First few rows of data
sum(is.na(diabetes)) #Missing values?
#Boxplots for all numerical variables in the dataset
par(mfrow=c(2,3))
for (i in 1:8){
  boxplot(diabetes[i], main=i)
}


#check proortions for the target variable
prop.table(table(diabetes$class))

###Split the dataset into training and testing data
set.seed(123)
ind <- sample(2,nrow(diabetes),replace=TRUE,prob=c(0.7,0.3))
train_set <- diabetes[ind==1,]
test_set <- diabetes[ind==2,]

#check proportions in the training and testing sets
prop.table(table(train_set$class))
prop.table(table(test_set$class))

###Bagging using Adabag

#create the bagging ensemble  with 20 iterations
diabetes_bagging <- bagging(class~ ., data=train_set, mfinal=20)
#variable importance
diabetes_bagging$importance
#evaluate the model on the test set using predict()
diabetes_predict.baggig <- predict(diabetes_bagging, newdata = test_set)
#classification table using predicted results
diabetes_predict.baggig$confusion
#cross-tabulation of observed and predicted classes
diabetes_predict.bagcfm <- confusionMatrix(table(test_set$class, diabetes_predict.baggig$class))
#Display results
diabetes_predict.bagcfm
#average error of bagging results
diabetes_predict.baggig$error


###10-fold cross validation using bagging
diabetes_baggingcv <- bagging.cv(class ~., v=10, data = train_set, mfinal = 20)
#results classification matrix from the cross-validated results
diabetes_baggingcv$confusion
#confusion matrix
confusionMatrix(diabetes_baggingcv$confusion)
#estimation error from the cross-validation results (train_set)
diabetes_baggingcv$error

###Using caret package to perform 10-fold cross validation

#Perform cross-validation using caret package
trainctrl <- trainControl(method="cv", number=10) #10-fold cross-validation
#fit predictive model over the tuning parameters
bag_caretcv <- train(class~., data=train_set, method="treebag", trControl=trainctrl)
#apply predictive model to the test set
prediction_caretbagcv <-predict(bag_caretcv, newdata = test_set)
#confusion matrix for 10-fold cross-validation using caret
confusionMatrix(table(test_set$class, prediction_caretbagcv))

###Boosting
set.seed(123)
#train boosting classification model
diabetes.boost <- boosting(class ~., data=train_set, mfinal=20, coeflearn = "Freund", boos=FALSE, control=rpart.control(maxdepth=3))

#make predictions on the test set using the boosted model
diabetes.boost.pred <- predict.boosting(diabetes.boost, newdata = test_set)

#retrieve classification table with prediction results
diabetes.boost.pred$confusion
#confusion matrix and other statistics
confusionMatrix(diabetes.boost.pred$confusion)

#average error from the predicted results
diabetes.boost.pred$error

###10-fold cross validation with boosting

#cross-validate training dataset
diabetes.boostcv <- boosting.cv(class ~ ., v=10, data = train_set, mfinal = 5, control = rpart.control(cp=0.01))
#obtain classification results
diabetes.boostcv$confusion
#confusion matrix and other statistics
confusionMatrix(diabetes.boostcv$confusion)
#average errors 
diabetes.boostcv$error

###Using caret package to perform 10-fold cross validation with boosting 

#Perform cross-validation using caret package
trainctrl2 <- trainControl(method="cv", number=10) #10-fold cross-validation
#fit predictive model over the tuning parameters
boost_caretcv <- train(class~., data=train_set, method="adaboost", trControl=trainctrl2)

#apply predictive model to the test set
prediction_caretboostcv <-predict(boost_caretcv, newdata = test_set)
#confusion matrix for 10-fold cross-validation using caret
confusionMatrix(table(test_set$class, prediction_caretboostcv))


###Calculating the margins of the boosting classifier

boost_margins <-margins(diabetes.boost, train_set)
boost_predict_margins <- margins(diabetes.boost.pred, test_set)

#plot marginal cumulative distribution for the boosting classifier
par(mfrow=c(1,1))
plot(sort(boost_margins[[1]]), (1:length(boost_margins[[1]]))/length(boost_margins[[1]]), type ="l", xlim=c(-1,1), main="Boosting: Margin Cumulative distribution graph", xlab="margin", ylab="%observations", col="blue")
lines(sort(boost_predict_margins[[1]]), (1:length(boost_predict_margins[[1]]))/length(boost_predict_margins[[1]]), type ="l", col="green")
abline(v=0, col="red", lty=2)

#percentage of negative margin matches training errors
boosting_training_margin <- table(boost_margins[[1]] >0)
boosting_negatvie_training <- as.numeric(boosting_training_margin[1]/boosting_training_margin[2])
boosting_negatvie_training

#percentage of negative margin matches testing errors
boosting_testing_margin <- table(boost_predict_margins[[1]] >0)
boosting_negatvie_testing <- as.numeric(boosting_testing_margin[1]/boosting_testing_margin[2])
boosting_negatvie_testing


###Calculating margins of the bagging classifier

bagging_margins <- margins(diabetes_bagging, train_set)
bagging_predict_margins <- margins(diabetes_predict.baggig, test_set)

#plot a marginh cumulative distribution graph od the bagging classifiers
plot(sort(bagging_margins[[1]]), (1:length(bagging_margins[[1]]))/length(bagging_margins[[1]]), type="l", xlim = c(-1,1), main="Bagging:Marging cumulative distribution graph", xlab="margin", ylab="% observations", col="blue")
lines(sort(bagging_predict_margins[[1]]), (1:length(bagging_predict_margins[[1]]))/length(bagging_predict_margins[[1]]), type="l", col="green")
abline(v=0, col="red", lty=2)

#calculate percentage of negative margin matches training errors
bagging_training_margin <- table(bagging_margins[[1]]>0)
bagging_negative_training <- as.numeric(bagging_training_margin[1]/bagging_training_margin[2])
bagging_negative_training

#calculating percentage of negative margin matches testing errors
bagging_testing_margin <- table(bagging_predict_margins[[1]] >0)
bagging_negative_testing <- as.numeric(bagging_testing_margin[1]/bagging_testing_margin[2])
bagging_negative_testing

