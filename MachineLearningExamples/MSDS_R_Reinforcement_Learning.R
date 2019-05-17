### Traveling salesman Problem
###Chose Itenerary for a Dream Vacation in Europe 

#Used as an inspiration
#https://www.r-bloggers.com/travelling-salesman-with-ggmap/
#https://github.com/mhahsler/TSP
#Driving distances data from https://www.engineeringtoolbox.com/driving-distances-d_1029.html
#Coordinates daty from https://www.latlong.net/

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Load Libraries
library(TSP)
library(dplyr)
library(purrr)
library(rworldmap)

#Load distances  between the cities in km
distances <- read.csv("Distances.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
distances 

distances <- distances[, -1] #Delete first column
distances <- distances[-1, ] #Delete first row
distances <- as.dist(distances) #create distance matrix
#Check the matrix
distances

#Load coordinates
coordinates <- read.csv("Coordinates.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
names(coordinates) <- c("city","country", "lat", "lon")
#check results
coordinates 
str(coordinates)

####Traveling Salesman Problem
###use TSP package 

#constructor creating an instance od a symmetric TSP problem
tsp <- TSP(distances)

methods <- c(
  "nearest_insertion",
  "farthest_insertion",
  "cheapest_insertion",
  "arbitrary_insertion",
  "nn",
  "repetitive_nn",
  "two_opt"
)

tours <- methods %>% map(function(method) {
  solve_TSP(tsp, method)
})

#tour - stores solution of the TSP
tour <- solve_TSP(tsp)

# Order cities
tour_order <- as.integer(tour)

#Total driving distance
tour_length(tour)

# Sort destination cities 
coordinates <- coordinates[tour_order, ] 

#Print vacation itenerary
coordinates[1:2]


#####Visualization using rworldmap library######

#Get low resolution map of Europe
euro_map<-getMap(resolution="low")

#Plot the map
plot(euro_map, xlim=c(-20,59), ylim = c(35,71), asp = 1)

#Add trip destinations to the map
points(coordinates$lon, coordinates$lat, col="red", cex = 1.5)

#Add  title to the map
title(main=paste("Our Dream European Vacation"), cex=3)








