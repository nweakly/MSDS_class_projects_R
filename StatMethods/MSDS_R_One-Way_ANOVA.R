#One-Way ANOVA

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory


#######Input data

#Time to relief (in minutes) 
a <- c(14,24, 12, 25) #Time to relief for A treatment group
b <- c(20, 14, 17, 18) #time to relief for B treatment group
c <- c(22, 29, 36, 20) #time to relief for C treament group
ttrelief <- c(a, b, c) #combine all time to relief values into one vector 
ttrelief #check the ttrelief content

#Treatment levels

k<-3 #number of treatment levels
n<- 4 #number of observation per treatment level
#use gl() function to generate factor levels by the pattern of the levels
treatm <-gl(k,n,12, labels=c("a", "b", "c")) #creating a vector of treatment factors
treatm #check treatm content

######Look at the data, check assumptions

#Create side-by-side box plot time to relief for each treatment group
plot(ttrelief~treatm)

#Check normality using Shapiro-Wilk normality test
shapiro.test(a) #for treatment group A
shapiro.test (b) #for treatment group b
shapiro.test(c) #for treatment group c

#use Levene test to check for equality of variance
library(car)
leveneTest(ttrelief~treatm) #Levene test with one independent variable


#####ANOVA testing

#fit an analysis of variance model
avmodel <-  aov(ttrelief~treatm)
summary(avmodel)

#Use lm() function to fit the model
avm <- lm(ttrelief~treatm)
summary(avm)


#####Model Diagnostic
#check assumptions using graphs

#diagnostic plots for avm constructed using aov() function
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(avm)

#diagnostic plots for avmodel constucted using lm() function
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(avmodel)

