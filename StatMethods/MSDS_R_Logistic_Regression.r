#Logistic Regression

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#######Input data from a csv file

disease<-read.csv("heart_disease.csv", header=TRUE) 
disease #Check to make sure it imported correctly 

str(disease) #Display internal table structure

#Visually inspect the relationship between variables
plot(disease$CoronaryHeartDisease ~ disease$Age)


#Fit the logistic regression model and display results

diseasemodel <- glm(factor(CoronaryHeartDisease) ~ Age, data = disease, family = binomial) #Fit the model
summary(diseasemodel) #Display the detailed summary table

confint(diseasemodel) #confidence intervals
exp(diseasemodel$coefficients) #exponentiated coefficients
exp(confint(diseasemodel)) #95% CI for exponentiated coefficeoents

#Compare the null model with the diseasemodel
anova(diseasemodel, test='Chisq')


###Predictions
#Using the diseasemodel to generate predictions for the original dataset
predict(diseasemodel, disease, type='response')

#Extend prediction interval to people older than 60
testdata <- data.frame(Age = 61:65)
predict(diseasemodel, newdata = testdata, type = 'response')

#Predict probsbilities of coronary heart disease for 40-50 year olds
testdata2 <-data.frame(Age=40:50)
predict(diseasemodel, newdata=testdata2, type='response')

