# Assignment: KNN  

rm(list=ls()) #Clear the environment
setwd("YOUR_DIRECTORY_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Load libraries

library("class")
library("gmodels")
library("caret")

#Load the data

data1 <- read.table("processed.cleveland.data", header=FALSE, sep=",", stringsAsFactors = FALSE)
#Check that the data was loaded correctly
head(data1)
str(data1)

#Name the columns
colnames(data1) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
str(data1)


#Group target variable into 2 classes
data1$num[data1$num >0] = 1
str(data1) 

###Data types conversions

#convert the target variable data1$num to a factor
data1$num <- as.factor(data1$num)

#convert ca and thal to numeric
data1$ca <- as.numeric(data1$ca)
data1$thal<-as.numeric(data1$thal)

str(data1)

###Missing values detection

#Total number of missing values
sum(is.na(data1))

sum(is.na(data1$thal)) #missing thal values
sum(is.na(data1$ca))   #missing ca values

#list rows of data that have missing values
data1[!complete.cases(data1), ]

#check number of heart disease and no heart disease diagnosis for the target variable 
table(data1$num)

#remove incomplete records from the data set
data2_complete <- na.omit(data1)
sum(is.na(data2_complete))

str(data2_complete) #This is the full DF before transformations 

######################

###EDA to see it additional clean-up or transformations are needed
str(data2_complete)
summary(data2_complete)

#Side-by-side box plots
boxplot(data2_complete$age,data2_complete$trestbps, data2_complete$chol, data2_complete$thalach, data2_complete$oldpeak)
#Box plots for each continuolus variable
par(mfrow=c(1,2)) #Display 2 graphs in row
boxplot(data2_complete$age)
boxplot(data2_complete$trestbps)
boxplot(data2_complete$chol)
boxplot(data2_complete$thalach)
boxplot(data2_complete$oldpeak)


#Histograms for all continuous variables
hist(data2_complete$age)
hist(data2_complete$trestbps)
hist(data2_complete$chol)
hist(data2_complete$thalach)
hist(data2_complete$oldpeak)


#####################

#convert categorical variables to dummy variable
#create a df with variables that need to be converted to dummy variables

data3_dummies <- data2_complete[ , -1]
data3_dummies$trestbps <- NULL
data3_dummies$chol <- NULL
data3_dummies$thalach <- NULL
data3_dummies$oldpeak <- NULL


str(data3_dummies)

#convert variables from num to factors
##something to try later
#numvar <- ("names of the colomns", "  ", )
todummies <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal", "ca")
#variables that need to be substituted with dummies need to be converted to factors 
dumvars <- as.data.frame(lapply(data3_dummies[todummies], as.factor))
#dumvars - dataframe with 8 variables as factors
str(dumvars)

#create vector with the target num variable 
target <- c(data3_dummies$num)
target <- as.factor(target)

#add target vector back to the df with categorical variables
data3_dummies <- cbind(dumvars, target)

#use dummyVars() from caret to create dummy variables
dV <- dummyVars(formula = target ~., data =data3_dummies)
dV
data4_dummies <- as.data.frame(predict(object=dV, newdata = data3_dummies))
str(data4_dummies)

#########
#Create a data frame with remaining numeric variables and the target variable
#str(data2_complete) 
data5_numeric <- data2_complete[-2]
data5_numeric$cp <- NULL 
data5_numeric$fbs <- NULL
data5_numeric$restecg <- NULL
data5_numeric$exang <- NULL
data5_numeric$slope <- NULL
data5_numeric$thal <- NULL
data5_numeric$ca<- NULL 

#Check the results
str(data5_numeric) #DF with continuous variables and target
str(data4_dummies) #DF with dummy variables

######Normalization#############

#create normalize function
normalize <- function(x) {
  return((x-min(x)) / (max(x) - min(x)))
}

#columns that need normalization
neednorm <- c("age", "trestbps", "chol", "thalach", "oldpeak")

data7_norm <-as.data.frame(lapply(data5_numeric[neednorm], normalize)) 
str(data7_norm)

#Put together full normalized DF
data_normbound <- cbind(data4_dummies, data7_norm, target)
str(data_normbound) #This is FULL NORMALIZED df before splitting

#Create training and testing data sets
#using createDataPartition from caret package

trainingindex <-  createDataPartition(data_normbound$target,p=0.7, list=FALSE)

data_train <- data_normbound[trainingindex,] #full training data set
data_test <- data_normbound[-trainingindex,] #full testing data set

#labels
data_trainlbl <- data_train$target
data_testlbl <- data_test$target

#training and testing sets without the target variable
data_trainmodel <- data_train[, -29]
data_testmodel <- data_test[ , -29]


#######Fit the model##############
#use k=1
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=1)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=3
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=3)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=5
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=5)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=7
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=7)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=9
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=9)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=11
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=11)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=13
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=13)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=15
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=15)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=17
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=17)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

#use k=19
knn_pred <- knn(train=data_trainmodel, test = data_testmodel, cl=data_trainlbl, k=19)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_pred, prop.chisq = FALSE)

####Use z-score standartization instead of normalization

#Create a data frame with standardized values
data7_z <-as.data.frame(scale(data5_numeric[neednorm])) 
str(data7_z)

#Put together full standardized DF
data_zbound <- cbind(data4_dummies, data7_z, target)
str(data_zbound) #This is FULL STANDARTIZED df before splitting

#Create training and testing data sets
#using createDataPartition from caret package

data_ztrain <- data_zbound[trainingindex,] #full training data set
data_ztest <- data_zbound[-trainingindex,] #full testing data set

#training and testing sets without target variable
data_ztrainmodel <- data_ztrain[, -29]
data_ztestmodel <- data_ztest[ , -29]


#######Fit the model###########

#use k=1
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=1)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=3
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=3)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=5
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=5)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=7
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=7)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=9
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=9)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=11
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=11)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=13
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=13)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=15
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=15)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)

#use k=17
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=17)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)


#use k=19
knn_zpred <- knn(train=data_ztrainmodel, test = data_ztestmodel, cl=data_trainlbl, k=19)

#Evaluate the model using CrossTable() from gmodels package
CrossTable(x = data_testlbl, y = knn_zpred, prop.chisq = FALSE)