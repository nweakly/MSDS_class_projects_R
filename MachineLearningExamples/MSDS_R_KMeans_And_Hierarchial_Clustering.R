###Assignment:  K-means clustering and HCA  

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Apply K-means clustering and HCA to the customer's dataset
#https://archive.ics.uci.edu/ml/datasets/Wholesale+customers 

#Load packages
library(stats)
library(fpc)
library(cluster)
library(NbClust)
library(factoextra)

###Load data
customers <- read.csv2("WholesaleCustomersData.csv", header = TRUE, sep = ",")
str(customers) #Check structure of the df

###EDA

head(customers) #First few rows
summary(customers) #summary stats for all variables

sum(is.na(customers)) #Missing values?

#Boxplots for all numerical variables in the daataset
par(mfrow=c(2,3))
for (i in 3:8){
  boxplot(customers[i], main=i)
}

###Data Prep - Encoding Channel and Region Variables
table(customers$Channel)

#Retail variable
customers$Retail <- ifelse(customers$Channel == 2, 1, 0)
#Check results
table(customers$Retail)  
str(customers)

#Region
table(customers$Region)

customers$Lisbon <- ifelse(customers$Region == 1, 1, 0)
customers$Oporto <- ifelse(customers$Region ==2, 1, 0)

#Check results
table(customers$Lisbon)
table(customers$Oporto)
str(customers)

cust_data <- customers[3:8]

head(cust_data)
str(cust_data)

#Z-score standartization for columns with sales data
cust_dataz <- as.data.frame(lapply(cust_data[, 1:6], scale))
head(cust_dataz)
str(cust_dataz)

#attach dummy-coded columns to the dataframe witht the sales data
cust_fullz <- cbind(cust_dataz, customers$Retail, customers$Lisbon,customers$Oporto)
head(cust_fullz)
str(cust_fullz) #full df for clustering

#####Clustering#############
#####K-means cluster

###Optimum number of clusters

#Calculate within sum of squares
nk <- 2:23
set.seed(123)
wss <- sapply(nk, function(k){
  kmeans(cust_fullz, centers=k)$tot.withinss
})

wss

par(mfrow=c(1,1))
#Plot within sum of squares
plot(nk, wss, type="l", xlab="number of k", ylab="within sum of squares")


#average silhouette width
sw <- sapply(nk, function(k) {
  cluster.stats(dist(cust_fullz), kmeans(cust_fullz, centers=k)$cluster)$avg.silwidth
})
sw

#plot average silhouette width
plot(nk, sw, type="l", xlab="number of clusters", ylab = "average silhouette width")
#max number of clusters
nk[which.max(sw)]

#gap statistics
set.seed(123)
gap_stat <- clusGap(cust_fullz, FUN=kmeans, nstart = 25, K.max = 23, B = 50)
gap_stat

#Fit the model

#Compute k-means clustering with k=2
set.seed(123)
customers_2clusters <- kmeans(cust_fullz, 2)
customers_2clusters

#Compute k-means clustering with k=3 
set.seed(123)
customers_3clusters <- kmeans(cust_fullz, 3)
customers_3clusters

#Compute k-means clustering with k=4 
set.seed(123)
customers_4clusters <- kmeans(cust_fullz, 4)
customers_4clusters

#Plot 2 clusters
plot(cust_fullz[c("Fresh", "Grocery")], col=customers_2clusters$cluster)
points(customers_2clusters$centers[, c("Fresh", "Grocery")], col=1:2, pch=4, cex=3)

#Plot 3 clusters
plot(cust_fullz[c("Fresh", "Grocery")], col=customers_3clusters$cluster)
points(customers_3clusters$centers[, c("Fresh", "Grocery")], col=1:3, pch=4, cex=3)

#Plot 4 clusters
plot(cust_fullz[c("Fresh", "Grocery")], col=customers_4clusters$cluster)
points(customers_4clusters$centers[, c("Fresh", "Grocery")], col=1:4, pch=4, cex=3)

#Plot 2 clusters
plot(cust_fullz[c("Detergents_Paper", "Delicassen")], col=customers_2clusters$cluster)
points(customers_2clusters$centers[, c("Detergents_Paper", "Delicassen")], col=1:2, pch=4, cex=3)

#Plot 3 clusters
plot(cust_fullz[c("Detergents_Paper", "Delicassen")], col=customers_3clusters$cluster)
points(customers_3clusters$centers[, c("Detergents_Paper", "Delicassen")], col=1:3, pch=4, cex=3)

#Plot 4 clusters
plot(cust_fullz[c("Detergents_Paper", "Delicassen")], col=customers_4clusters$cluster)
points(customers_4clusters$centers[, c("Detergents_Paper", "Delicassen")], col=1:4, pch=4, cex=3)


#Bivariate cluster plot
#2 clusters
clusplot(cust_fullz, customers_2clusters$cluster, color=TRUE, shade = TRUE)
#3 clusters
clusplot(cust_fullz, customers_3clusters$cluster, color=TRUE, shade = TRUE)
#4 clusters
clusplot(cust_fullz, customers_4clusters$cluster, color=TRUE, shade = TRUE)


#######Hierarchial clustering##############

#####Ward.D2 
#Optimal number of clusters
set.seed(123)
num_clust_ward <- NbClust(cust_fullz,  distance = "euclidean", min.nc = 2, max.nc = 23, method= "ward.D2", index = "all")
num_clust_ward
fviz_nbclust(num_clust_ward, ggtheme = theme_minimal())

#Clustering
customers_hc <- hclust(dist(cust_fullz, method="euclidean"), method= "ward.D2")
customers_hc
plot(customers_hc, hang = -0.01, cex = 0.7)


#Cutting trees into clusters
hc1_cut <- cutree(customers_hc, k=3)
table(hc1_cut)

hc1_cut2 <- cutree(customers_hc, k=2)
table(hc1_cut2)

hc1_cut3 <- cutree(customers_hc, k=4)
table(hc1_cut3)

#Plot the tree
#k=3
plot(customers_hc)
rect.hclust(customers_hc, k=3, border="red")

#K=2
plot(customers_hc)
rect.hclust(customers_hc, k=2, border="red")

#k=4
plot(customers_hc)
rect.hclust(customers_hc, k=4, border="red")


###Average linkage

#Optimal number of clusters
set.seed(123)
num_clust_av <- NbClust(cust_fullz,  distance = "euclidean", min.nc = 2, max.nc = 23, method= "average", index = "all")
num_clust_av
fviz_nbclust(num_clust_av, ggtheme = theme_minimal())

#Clustering
customers_hc2 <- hclust(dist(cust_fullz, method="euclidean"), method = "average")
customers_hc2
plot(customers_hc2, hang = -0.01, cex = 0.7)

#Cutting trees into clusters
hc2_cut <- cutree(customers_hc2, k=3)
table(hc2_cut)

hc2_cut2 <- cutree(customers_hc2, k=2)
table(hc2_cut2)

hc2_cut3 <- cutree(customers_hc, k=4)
table(hc2_cut3)

#Plot the tree
#K=2
plot(customers_hc2)
rect.hclust(customers_hc2, k=2, border="red")

#k=3
plot(customers_hc2)
rect.hclust(customers_hc2, k=3, border="red")

#k=4
plot(customers_hc2)
rect.hclust(customers_hc2, k=4, border="red")


