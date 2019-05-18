#Unsupervised Learning

rm(list=ls()) #Clear the environment
setwd("Your_PATH") #Set working directory for the assignment
getwd() #Check working directory

#####################################################################
####Practice EXERCISE USING IRIS DATA SET AND K-MEANS CLUSTERING######
#####################################################################

data(iris) #include iris dataset
head(iris) #view the first few rows of data

####using k-means clustering

set.seed(42) #set seed for the algorithm to ensure reproducible results
km <-kmeans(iris[,1:4], 3, nstart=25) #chose 3 clusters as there are three species in the dataset
km #output results

#Compare the clusters with the species and plot results
table(km$cluster, iris$Species)

#Plot the results
plot(iris[,1], iris[,2], col=km$cluster) #Plot cluster centers by Sepal Length by Sepal width
#add center points
points(km$centers[,c(1,2)], col=1:3, pch=8, cex=2)

#Plot Petal length and width
plot(iris[,3], iris[,4], col=km$cluster)
#add center points
points(km$centers[,c(3,4)], col=1:3, pch=8, cex=2)