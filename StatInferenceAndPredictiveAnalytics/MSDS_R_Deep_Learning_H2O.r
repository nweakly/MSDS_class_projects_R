### Deep Learning   
###Wisconsin Breast Cancer Dataset

###Dataset source: https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/ 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory


#Libraries
library(h2o) #load library
localH2O = h2o.init(nthreads = -1) #connect to a local cluster
library(tidyverse) 
library(ggplot2)
library(caret)

#Load data
bcancer <- read.table("breast-cancer-wisconsin.data", header = FALSE, sep = ",", stringsAsFactors = FALSE)

#EDA and data pre-processing
str(bcancer)
head(bcancer)
tail(bcancer)

bcancer <- bcancer[,-1] #Drop the ID column
#add column names
colnames(bcancer) <- c("thickness", "sizeunif", "shapeunif", "adhesion", "cellsize", "barenuclei", "chromatin", "normnucleoli", "mitoses", "class") 

#summary stats for all variables
summary(bcancer)

sum(is.na(bcancer))
sum(bcancer$barenuclei == "?")

#type conversions
bcancer$barenuclei <- as.numeric(bcancer$barenuclei)
bcancer$class<- as.factor(bcancer$class)
sum(is.na(bcancer))

###Visualize the data

#using clump thicknewss and uniform cell size as independent variables
qplot(data=bcancer, x=thickness, y=cellsize, color=class) + geom_jitter()

#class=2 - benign; class = 4 cancer
table(bcancer$class)

#density distribution for all independent variables by class
gather(bcancer, x, y, thickness:mitoses) %>% ggplot(aes(x = y, color = class, fill = class)) + 
  geom_density(alpha = 0.3) + facet_wrap( ~ x, scales = "free", ncol = 3)

#make a copy of the df
bcancer2 <- bcancer

#impute missing values with the median value
bcancer2$barenuclei <- ifelse(is.na(bcancer2$barenuclei), median(bcancer2$barenuclei, na.rm = TRUE), bcancer2$barenuclei)

#Check results
sum(is.na(bcancer2))
summary(bcancer2)

#split into training and testing data
set.seed(123)
trainIndex <- createDataPartition(bcancer2$class, p= 0.7, list = FALSE)
bcancer_train <-bcancer2[trainIndex,]
bcancer_test <- bcancer2[-trainIndex,]

#Check results
str(bcancer_test)
str(bcancer_train)
#check class proportions
prop.table(table(bcancer$class))
prop.table(table(bcancer_test$class))
prop.table(table(bcancer_train$class))

########h2o############
###Convert training and testing data frames into H2O objects
cancer_h20train <- as.h2o(bcancer_train, key="train")
cancer_h2otest <- as.h2o(bcancer_test, key="test")

#check results
class(cancer_h20train)
class(cancer_h2otest)
summary(cancer_h20train)
summary(cancer_h2otest)

######train and test models with different parameters

####dlmodel1 with 3 hidden layers 50 nodes  without regularization (dropout)
#train
dlmodel1 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="Tanh", hidden=c(50,50,50), epochs = 500)
#model characteristics and performance on triaing data
dlmodel1
summary(dlmodel1)
#model performance on test data
dlmodel1_perform <- h2o.performance(dlmodel1, cancer_h2otest)
dlmodel1_perform

####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel1_perform)

#h2o.performance() returns an H2OModelMetrics object
#H2OModelMetrics object has several accessor functions
h2o.mse(dlmodel1_perform)
h2o.auc(dlmodel1_perform)

###dlmodel2 with 3 hidden layers 30 nodes each without regularization (dropout)
#train
dlmodel2 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="Tanh", hidden=c(30,30,30), epochs = 500)
#model characteristics and performance on traing data
dlmodel2
#model performance on test data
dlmodel2_perform <- h2o.performance(dlmodel2, cancer_h2otest)
dlmodel2_perform

####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel2_perform)

#h2o.performance() returns an H2OModelMetrics object
#H2OModelMetrics object has several accessor functions
h2o.mse(dlmodel2_perform)
h2o.auc(dlmodel2_perform)

###dlmodel3 with 2 hidden layers 50 nodes each without regularization (dropout)
#train
dlmodel3 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="Tanh", hidden=c(50,50), epochs = 500)
#model characteristics and performance on triaing data
dlmodel3
#model performance on test data
dlmodel3_perform <- h2o.performance(dlmodel3, cancer_h2otest)
dlmodel3_perform

####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel3_perform)
h2o.mse(dlmodel3_perform)
h2o.auc(dlmodel3_perform)

####dlmodel4 with 2 hidden layers 30 nodes each without regularization (dropout)
#train
dlmodel4 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="Tanh", hidden=c(30,30), epochs = 500)
#model characteristics and performance on traing data
dlmodel4
#model performance on test data
dlmodel4_perform <- h2o.performance(dlmodel4, cancer_h2otest)
dlmodel4_perform

####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel4_perform)
h2o.mse(dlmodel4_perform)
h2o.auc(dlmodel4_perform)


####dlmodel5 with 3 hidden layers 50 nodes  with regularization (dropout)
#train
dlmodel5 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="TanhWithDropout", input_dropout_ratio = 0,
                             hidden_dropout_ratios = c(0.5, 0.5, 0.5), hidden=c(50,50,50), epochs = 500)
#model characteristics and performance on triaing data
dlmodel5
#model performance on test data
dlmodel5_perform <- h2o.performance(dlmodel5, cancer_h2otest)
dlmodel5_perform

####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel5_perform)
h2o.mse(dlmodel5_perform)
h2o.auc(dlmodel5_perform)

###dlmodel6 with 3 hidden layers 30 nodes each with regularization (dropout)
#train
dlmodel6 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="TanhWithDropout", input_dropout_ratio = 0,
                             hidden_dropout_ratios = c(0.5, 0.5, 0.5), hidden=c(30,30,30), epochs = 500)
#model characteristics and performance on traing data
dlmodel6
#model performance on test data
dlmodel6_perform <- h2o.performance(dlmodel6, cancer_h2otest)
dlmodel6_perform

####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel6_perform)
h2o.mse(dlmodel6_perform)
h2o.auc(dlmodel6_perform)

###dlmodel7 with 2 hidden layers 50 nodes each with regularization (dropout)
#train
dlmodel7 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="TanhWithDropout", input_dropout_ratio = 0,
                             hidden_dropout_ratios = c(0.5, 0.5), hidden=c(50,50), epochs = 500)
#model characteristics and performance on triaing data
dlmodel7
#model performance on test data
dlmodel7_perform <- h2o.performance(dlmodel7, cancer_h2otest)
dlmodel7_perform
####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel7_perform)
h2o.mse(dlmodel7_perform)
h2o.auc(dlmodel7_perform)

####dlmodel8 with 2 hidden layers 30 nodes each with regularization (dropout)
#train
dlmodel8 <- h2o.deeplearning(x=1:9, y=10, training_frame = cancer_h20train, activation="TanhWithDropout", input_dropout_ratio = 0,
                             hidden_dropout_ratios = c(0.5, 0.5), hidden=c(30,30), epochs = 500)
#model characteristics and performance on traing data
dlmodel8
#model performance on test data
dlmodel8_perform <- h2o.performance(dlmodel8, cancer_h2otest)
dlmodel8_perform

####evaluate
############h2o.ModelMetrics

h2o.confusionMatrix(dlmodel8_perform)
h2o.mse(dlmodel8_perform)
h2o.auc(dlmodel8_perform)

############h2o.ModelMetrics
#h2o.performance() returns an H2OModelMetrics object
#H2OModelMetrics object has several accessor functions
h2o.precision(dlmodel1_perform)

h2o.mse(dlmodel1_perform)
h2o.auc()
h2o.giniCoef()

h2o.confusionMatrix(dlmodel1_perform)


####model tuning
hyper_params <- list(
  activation = c("Tanh", "TanhWithDropout"), 
  hidden = list(c(30,30), c(40, 40), c(50,50), c(30,30,30), c(40,40,40), c(50,50,50)), 
  input_dropout_ratio=c(0, 0.5), 
  rate = c(0.01, 0.25)
)

search_criteria <- list(
  strategy = "RandomDiscrete", 
  max_models=100, seed =123, stopping_rounds =5,
  stopping_tolerance = 0.01
)

randomSearch <- h2o.grid(
  algorithm ="deeplearning",
  grid_id="randomSearch",
  training_frame = cancer_h20train,
  validation_frame = cancer_h2otest,
  x=1:9, y=10,
  epochs=1,
  stopping_metric ="misclassification",
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

grid <- h2o.getGrid("randomSearch", sort_by="auc", decreasing = TRUE)
grid

###best model
dlmodel_best <- h2o.getModel(grid@model_ids[[1]])
h2o.confusionMatrix(dlmodel_best, valid = T)
dlmodel_best_perform <- h2o.performance(dlmodel_best, cancer_h2otest)
dlmodel_best_perform 

#variable importance
dlmodel_best@model$variable_importances

####shut down h2o 
h2o.shutdown()



