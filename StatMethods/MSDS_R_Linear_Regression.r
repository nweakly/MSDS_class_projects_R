# Simple Linear Regression

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignemnt
getwd() #Check working directory

install.packages("MASS") #Install the MASS package for the first time
library(MASS) #load the MASS package

data("Boston") #Load the Boston dataset
?Boston #INfo about the dataset
View(Boston) #Preview the dataset
names(Boston) #List the names of the variable in the Boston dataset
str(Boston) #Display the internal structure of the dataset
head(Boston) #Display first few rows of data
tail (Boston) #Display last few rows of data
summary(Boston) #Display basic summary statistics for each variable

attach(Boston)
hist(medv) #Create histogram for median house values
boxplot(medv)#Create boxplot for median house values
plot(medv) #Create scatter plot for median house values 
sd(medv) #Standard Deviation for median house values
qqnorm(medv)#Norm Q-Q plot for median house values
qqline(medv) #Add theoretical Q-Q line
pairs(~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=Boston) #Create scatterplot matrix
pairs(~crim+zn+indus+nox+rm+age+tax+ptratio+lstat+medv, data=Boston) #Create scatterplot matrix without chas, dis, rad and black variables
cor(Boston) #Display correlation between variables

#Simple Linear Regression Model
plot(rm, medv, main="Median Values vs. Number of Rooms", xlab="Average Number of Rooms", ylab="Median House Value")
lm(medv~rm, Boston) #build linear regression model # of rooms - independent, median house value - dependent variables
summary(lm_model)#Display detailed info about the model
abline(lm(medv~rm), col="red") #Add regression line on top of the scatter plot

#Model Diagnostic
hist(lm_model$residuals) #histogram of residuals to check for normality
plot(lm_model) #Fit information