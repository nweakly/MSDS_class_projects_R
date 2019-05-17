###Assignment:  SVMs and ANNs 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Use SVMS and ANNs to build an edible/poisonois mushrooms classifier
#Mushroom Dataset from http://archive.ics.uci.edu/ml/datasets/Mushroom

#Load packages
library(caret)
library(e1071)
library(nnet) 

#Load data
shroom <- read.table("agaricus-lepiota.data", header = FALSE, sep = ",")

#Check 
View(shroom) #Preview data
str(shroom) #Check internal structure of the data frame

#Change column names
names(shroom) <- c("class", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
str(shroom) #Check results

#Summary statistics for all variables in the dataset
summary(shroom)

#Delete veil-type column
shroom <- shroom[,-17]
summary(shroom)

#Replace ? with level m (missing) in stalk_root variable
#add m to the factor levels
levels(shroom$stalk_root) <- c(levels(shroom$stalk_root), 'm')
#Change level "?" to "m"
shroom$stalk_root[shroom$stalk_root == "?"] <- "m"
summary(shroom$stalk_root) #check results


####One-hot full rank encoding

shroom_var <- shroom[, -1] #df with independent variables only

shroom_fr <- dummyVars(~ ., data = shroom_var, fullRank = TRUE)
shroom_varfr <- predict(shroom_fr, shroom)

#Check results
str(shroom_varfr)
summary(shroom_varfr)

#Add class variable back to the data frame

shroom_fullfr <- as.data.frame(cbind(shroom$class, shroom_varfr)) #add the first column
colnames(shroom_fullfr)[1] <- "class" #add column name
shroom_fullfr$class <- as.factor(shroom_fullfr$class)
summary(shroom_fullfr) #Check results
str(shroom_fullfr) #Look at the resulting df

#Split the dataset into training and testing data
#(80% training, 20% testing)

set.seed(15)
ind <- sample.int(n=nrow(shroom_fullfr), size=floor(0.8*nrow(shroom_fullfr)), replace = FALSE)
shroom_train <-  shroom_fullfr[ind, ] #training set
shroom_test <- shroom_fullfr[ -ind, ] #testing set


#Check resulting datasets
dim(shroom_train)
dim(shroom_test)

#Compare proportions between edible and poisonous mashrooms in testing and training data
prop.table(table(shroom_train$class))
prop.table(table(shroom_test$class))

######Models##############

####SVM

svm_model1 <- svm(class ~ ., data = shroom_train, kernel = "radial", cost = 1, gamma = 1/ncol(shroom_train))
summary(svm_model1)

#Using the model for predictions

svm_prediction1 <- predict(svm_model1, shroom_test[ , !names(shroom_test) %in% c("class")])
#generate classification table
svm_table1 <- table(svm_prediction1, shroom_test$class)
svm_table1 

#Evaluate results
classAgreement(svm_table1)
confusionMatrix(svm_table1)

#Try out other kernel types
#linear
svm_model2 <- svm(class ~ ., data = shroom_train, kernel = "linear", cost = 1, gamma = 1/ncol(shroom_train))
summary(svm_model2)

#Using the model for predictions
shroom_testnoclass <- shroom_test[-1] #Test dataset without the class column
svm_prediction2 <- predict(svm_model1, shroom_testnoclass)

#generate classification table
svm_table2 <- table(svm_prediction2, shroom_test$class)
svm_table2 

#Evaluate results
classAgreement(svm_table2)
confusionMatrix(svm_table2)

#polynomial
svm_model3 <- svm(class ~ ., data = shroom_train, kernel = "polynomial", cost = 1, gamma = 1/ncol(shroom_train))
summary(svm_model3)

#Using the model for predictions
svm_prediction3 <- predict(svm_model3, shroom_testnoclass)

#generate classification table
svm_table3 <- table(svm_prediction3, shroom_test$class)
svm_table3 

#Evaluate results
classAgreement(svm_table3)
confusionMatrix(svm_table3)

#sigmoid 
svm_model4 <- svm(class ~ ., data = shroom_train, kernel = "sigmoid", cost = 1, gamma = 1/ncol(shroom_train))
summary(svm_model4)

#Using the model for predictions
svm_prediction4 <- predict(svm_model4, shroom_testnoclass)

#generate classification table
svm_table4 <- table(svm_prediction4, shroom_test$class)
svm_table4 

#Evaluate results
classAgreement(svm_table4)
confusionMatrix(svm_table4)

##########ANN################
#train the neural network with nnet
nn_model1 <- nnet(class ~  ., data = shroom_train, size = 2, rang = 0.1, decay = 5e-4, maxit = 500 )
summary(nn_model1)
nn_prediction1 <- predict (nn_model1, shroom_testnoclass, type="class")
nn_table1 <- table(shroom_test$class, nn_prediction1)
nn_table1

confusionMatrix(nn_table1)

###ANN model adjustments
#model2 - restrict number of iterations
nn_model2 <- nnet(class ~  ., data = shroom_train, size = 2, rang = 0.1, decay = 5e-4, maxit = 50 )
summary(nn_model2)
nn_prediction2 <- predict (nn_model2, shroom_testnoclass, type="class")
nn_table2 <- table(shroom_test$class, nn_prediction2)
nn_table2

confusionMatrix(nn_table2)

#model 3 - default number of iterations = 100
nn_model3 <- nnet(class ~  ., data = shroom_train, size = 2, rang = 0.1, decay = 5e-4, maxit = 100 )
summary(nn_model3)
nn_prediction3 <- predict (nn_model3, shroom_testnoclass, type="class")
nn_table3 <- table(shroom_test$class, nn_prediction1)
nn_table3

confusionMatrix(nn_table3)

#model4 - size =1
nn_model4 <- nnet(class ~  ., data = shroom_train, size = 1, rang = 0.1, decay = 5e-4, maxit = 500 )
summary(nn_model4)
nn_prediction4 <- predict (nn_model4, shroom_testnoclass, type="class")
nn_table4 <- table(shroom_test$class, nn_prediction4)
nn_table4

confusionMatrix(nn_table4)

#model5 - size=5
nn_model5 <- nnet(class ~  ., data = shroom_train, size = 5, rang = 0.1, decay = 5e-4, maxit = 500 )
summary(nn_model5)
nn_prediction5 <- predict (nn_model5, shroom_testnoclass, type="class")
nn_table5 <- table(shroom_test$class, nn_prediction5)
nn_table5

confusionMatrix(nn_table5)

#model6 change decay 
nn_model6 <- nnet(class ~  ., data = shroom_train, size = 2, rang = 0.1, decay = 5e-2, maxit = 500 )
summary(nn_model6)
nn_prediction6 <- predict (nn_model6, shroom_testnoclass, type="class")
nn_table6 <- table(shroom_test$class, nn_prediction6)
nn_table6

confusionMatrix(nn_table6)

#model7 change decay and decrease max number of iterations 
nn_model7 <- nnet(class ~  ., data = shroom_train, size = 2, rang = 0.1, decay = 5e-2, maxit = 50 )
summary(nn_model7)
nn_prediction7 <- predict (nn_model7, shroom_testnoclass, type="class")
nn_table7 <- table(shroom_test$class, nn_prediction7)
nn_table7

confusionMatrix(nn_table7)

#model8 change decay, decrease max number of iterations and size 
nn_model8 <- nnet(class ~  ., data = shroom_train, size = 1, rang = 0.1, decay = 5e-2, maxit = 40 )
summary(nn_model8)
nn_prediction8 <- predict (nn_model8, shroom_testnoclass, type="class")
nn_table8 <- table(shroom_test$class, nn_prediction8)
nn_table8

confusionMatrix(nn_table8)

#model9 change rang 
nn_model9 <- nnet(class ~  ., data = shroom_train, size = 2, rang = 0.5, decay = 5e-4, maxit = 500 )
summary(nn_model9)
nn_prediction9 <- predict (nn_model9, shroom_testnoclass, type="class")
nn_table9 <- table(shroom_test$class, nn_prediction9)
nn_table9

confusionMatrix(nn_table9)

#model10 change size,  rang, decay and maxit at the same time 
nn_model10 <- nnet(class ~  ., data = shroom_train, size = 1, rang = 0.5, decay = 5e-2, maxit = 50 )
summary(nn_model10)
nn_prediction10 <- predict (nn_model10, shroom_testnoclass, type="class")
nn_table10 <- table(shroom_test$class, nn_prediction10)
nn_table10

confusionMatrix(nn_table10)