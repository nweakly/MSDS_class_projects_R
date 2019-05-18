#Nonparametric statistics

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Download BSDA package 
install.packages("BSDA")
#Download exactRankTests package 
install.packages("exactRankTests")

#######Part 1. "Souperb" restaurant chain

#Include BSDA package (for SIGN.test function)
library(BSDA)  

#load ratings data
ratings <- c(5,3,2,1,4,3,5,1,5,2,3,4,2,1,3)

#EDA 
plot(ratings)
boxplot(ratings)
hist(ratings)

#Perform Sign Test (median = 3)
SIGN.test(ratings, md=3, alternative = "less")


#######Part 2. Operating systems "M" and "W"

#Include exacdtRankTests package
library(exactRankTests)

#Load ratings data
mrating <- c(9,8,5,3,6,10,4,2,8,7) #Ratings for "M" operating system
wrating <- c(7,6,8,2,9,5,1,4,7,10) #Ratings for "W" operating system

#EDA
plot(mrating)
plot (wrating)
boxplot(mrating, wrating)
plot(mrating, wrating)
hist (mrating)
hist(wrating)

#Perform Wilcoxon Rank-Sum Test for a difference in mrating and wrating
#since ratings are assumed to be paired
wilcox.exact(mrating, wrating, paired = TRUE)

