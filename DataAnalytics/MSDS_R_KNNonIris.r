#Supervised Learning 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#####################################################################
#######Practice EXERCISE USING IRIS DATA SET AND KNN ALGORITHM#######
#####################################################################

data("iris") #load the data set
View(iris)

# download the dataset from the UC Irvine Machine Learning Repository:
ir_url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data'
iris <-read.csv(url(ir_url), header=F)
#Add column names
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

#Explore the data
head(iris)
summary(iris)

#concatenate the data using c to view only
#the summary of the petal and sepal width
summary(iris[c("Petal.Width", "Sepal.Width")])
#concatenate the data using c to view only
#the summary of the patal and sepal length
summary(iris[c("Petal.Length", "Sepal.Length")])


#Load the ggvis package
install.packages('ggvis')
library(ggvis) #include the ggvis library

#Make a scatterplot of the dataset
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>% ggvis (~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points() 

#Download class package
install.packages("class")
library(class)

set.seed(3465)
ind <- sample(2, nrow (iris), replace=TRUE, prob=c(0.8, 0.2))

#use the array, ind, to define the training and test sets
irisTrain <- iris[ind==1, 1:4]
iristTest <- iris[ind==2, 1:4]
irisTrainLabels <- iris[ind==1,5]
irisTestLabels <-iris[ind==2,5]

iris_pred <- knn(train=irisTrain, test=iristTest, cl=irisTrainLabels, k=3)
iris_pred #view results of knn function

#Evaluate the results
install.packages("gmodels")
library(gmodels)
#CrossTable(x=irisTestLabels, y=iris_pred, prop.chisq = F, prop.r = F, prop.c = F, prop.t = F)

#Confusion Matrix
CrossTable(x=irisTestLabels, y=iris_pred, prop.chisq = FALSE)
