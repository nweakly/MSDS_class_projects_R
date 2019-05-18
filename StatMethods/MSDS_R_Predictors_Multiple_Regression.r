#Multiple Linear Regression

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignemnt
getwd() #Check working directory

#########Multiple Linear Regression##########

#Prepare the dataset
library(MASS) #load the MASS package
data("Boston") #Load the Boston dataset
attach(Boston)
View(Boston)
str(Boston) #Display the internal structure of the dataset
summary(Boston) #Display basic summary statistics for each variable

#Summary statistics for the predictor and response variables
summary(rm)
summary(ptratio)
summary(lstat)
summary(medv)

####Visually exploring variables#####

#Response variable -medv - median house values
hist(medv) #histogram
boxplot(medv)#boxplot
plot(medv) #scatter plot
sd(medv) #Standard Deviation
qqnorm(medv)#Norm Q-Q plot 
qqline(medv) #Add theoretical Q-Q line
shapiro.test(medv)  #Shapiro-Wilk normality test

#Predictor variable -rm- average number of rooms per dwelling
hist(rm) #histogram
boxplot(rm)#boxplot
plot(rm) #scatter plot
sd(rm) #Standard Deviation
qqnorm(rm)#Norm Q-Q plot 
qqline(rm) #Add theoretical Q-Q line
shapiro.test(rm)  #Shapiro-Wilk normality test 

#Predictor variable -ptratio- pupil-teacher ration by town
hist(ptratio) #histogram
boxplot(ptratio)#boxplot
plot(ptratio) #scatter plot
sd(ptratio) #Standard Deviation
qqnorm(ptratio)#Norm Q-Q plot 
qqline(ptratio) #Add theoretical Q-Q line
shapiro.test(ptratio) #Shapiro-Wilk normality test

#Predictor variable -lstat- lower status of the population in $
hist(lstat) #histogram
boxplot(lstat)#boxplot
plot(lstat) #scatter plot
sd(lstat) #Standard Deviation
qqnorm(lstat)#Norm Q-Q plot 
qqline(lstat) #Add theoretical Q-Q line
shapiro.test(lstat)  #Shapiro-Wilk normality test 

#pairwise scatter plots
pairs(~medv + rm + ptratio + lstat, data=Boston) #scatter plot matrix of four variables

cor(Boston) #Display correlation between variables in the Boston data set

#####Fitting MLR Model######

medv.mlr <- lm(medv ~ rm + ptratio + lstat, data=Boston)
summary(medv.mlr) # show results

summary(medv.mlr)$coefficient  #Display model coefficients
confint(medv.mlr) #Confidence intervals

#Checking for multicollinearity
vif(medv.mlr) #VIF - Variance Inflation Factor


##########Comparison of 2 models################


##Create a new model with 2 explanatory variables -rm and ptratio

medv.mlr2 <- lm(medv ~ rm + ptratio, data=Boston)
summary(medv.mlr2) # Display model characteristics 


###Compare 2 models using ANOVA
anova(medv.mlr, medv.mlr2) 

####Model Diagnostic######
vif(medv.mlr2) #checking for multicollinearity

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(medv.mlr2)

#Checking for normality of residuals

# qq plot for studentized residuals
qqPlot(medv.mlr2, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(medv.mlr2) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Assessing Outliers
outlierTest(medv.mlr2) # Bonferonni p-value for most extreme observation
qqPlot(medv.mlr2, main="QQ Plot") #qq plot for studentized residuals 
leveragePlots(medv.mlr2) # leverage plots

# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(medv.mlr2$coefficients)-2)) 
plot(medv.mlr2, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(medv.mlr2,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
