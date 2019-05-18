# Two-Way ANOVA

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory


#######Input data from a csv file

salary<-read.csv("engineer.csv", header=TRUE) 
salary #Check to make sure it imported correctly 

str(salary) #Display internal table structure


#Make side-by-side boxplots

par(mfrow=c(1,2)) #Display 2 graphs in a row
plot(Salary ~ Profession + Region, data=salary) 

#Display interaction plots
interaction.plot(salary$Profession, salary$Region, salary$Salary)


#Display interaction plots, reverse factors
interaction.plot(salary$Region, salary$Profession, salary$Salary)


####Fit two-way ANOVA model with interactions and display the results

#Fit the model
salarymodel<-lm(salary$Salary ~ salary$Profession + salary$Region + salary$Profession * salary$Region, data=salary)
anova(salarymodel) #Display the ANOVA table for the model


#Display the detailed summary for the model
summary (salarymodel)

#Post-Hoc Testing
#TukeyHSD test
TukeyHSD(aov(salary$Salary ~ salary$Profession  + salary$Region, data=salary))
#visualise Tukey HSD test results
plot(TukeyHSD(aov(salary$Salary ~ salary$Profession  + salary$Region, data=salary)))

#####Model Diagnostic
#check assumptions using graphs

#diagnostic plots for salarymodel 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(salarymodel)

#Test normaliity of the error
shapiro.test(residuals(salarymodel))

#Test homogenity of variance
library(car)
leveneTest(salarymodel$residuals ~ salary$Profession)

#Test homogenity of variance
leveneTest(salarymodel$residuals ~ salary$Region)

#Test homogenity of variance
leveneTest(salarymodel$residuals ~ salary$Profession *salary$Region)