# Exploratory Data Analysis 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignemnt
getwd() #Check working directory

install.packages("ggplot2", dependencies = TRUE) #Install ggplot2

# Wine Quality dataset, source: http://archive.ics.uci.edu/ml/datasets/Wine+Quality 

redwine<-read.csv(file.choose(), header=T, sep=";") # Load the dataset from a csv file , use";" as delimiter

str(redwine) #Display the internal structure of the data set
View(redwine) #Preview the dataset
head(redwine) #Display first few rows of data 
tail(redwine) #Display last few rows of data

sum(is.na(redwine)) #Check for missing values and display the number of missing values

summary(redwine) #Display basic summary statistics for all variables in the dataset

attach(redwine) # attach the dataset in order to be able to access the variables directly

##Visulally inspect distributions for quality, ssulphates and alcohol variables

hist(quality) #histogram for quality ratings
boxplot(quality) #Displaying boxplot for quality variable

hist(sulphates) #Histogram for sulphates content
boxplot(sulphates) #Boxplot for sulphates variable 
plot(sulphates) #Display scatter plot for sulphates

sd(sulphates) #Standard deviation for sulphates


hist(alcohol) #Histogram for alcohol content
boxplot(alcohol) #Boxplot for alcohol variable 
plot(alcohol) #Display scatter plot for alcohol

qqnorm(sulphates)#Norm Q-Q plot for sulphates values
qqline(sulphates)

qqnorm(alcohol)#Norm Q-Q plot for alcohol values
qqline(alcohol)

pairs(~sulphates+alcohol+quality, data=redwine) #Display scatterplot matrix for three variables


#Dsiplaying all three variables on the same graph

library(ggplot2)
gg <- ggplot(redwine, aes(x=sulphates, y=alcohol)) + 
  geom_point(aes(col=quality)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 2.0)) + 
  ylim(c(8, 15)) + 
  labs(subtitle="Sulphates Vs Alcohol", 
       y="Alcohol", 
       x="Sulphates", 
       title="Scatterplot", 
       caption = "Source: redwine")

plot(gg)

cor(redwine) ##Display correlation matrix for the variables in the redwinde dataset