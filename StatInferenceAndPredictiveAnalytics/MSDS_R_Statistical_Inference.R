### Statistical Inference

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory


###Load packages
library(ggplot2)
library(caret)
library(e1071)

###########
#Question 1
###########
#Each student in a large statistics class of 600 students is asked to toss a fair coin 100 times, 
#count the resulting number of Heads, and construct a 0.95 level confidence interval for the 
#probability of Heads. Assume that each student uses a fair coin and constructs the confidence 
#interval correctly. 
#True or False: We would expect approximately 570 of the confidence intervals to contain the number 0.5.
 
#total number of tosses
tosses = 600*100
tosses

#Use rbinom() function to generate binomial distribution
#100 - number of observations , 600 - number of trials(students), p =0.5 probability for a fair coin
set.seed(123)
results1 <-rbinom(600, 100, 0.5)

#Simulation results
table(results1) #frequency distribution for the number of heads received by each student in the simulation
results1

mean(results1) #mean number of heads 
sd(results1) #standard deviation

#Plot of the frequency distribution
barplot(table(results1), xlab = 'Number of heads received by each student', ylab = 'Frequency', col = 'blue')
#total number od successes (heads) in the simulated trial
heads <- sum(results1)
heads

#Test hypotesis using simulation results
#Ho: coin is fair (probability=0.5)
#Ha: coin is not fair (probability != 0.5)
binom.test(heads, tosses, p=0.5, alternative = "two.sided", conf.level = 0.95)


###########
#Question 2
###########
#A light bulbs manufactirer claims  that its 75- watt bulbs burn an average of 800 hours before failing.
#Customeres disagree. Consumer watchdog organization purchased and tested 100 of the disputed light bulbs.
#In this experiment, the 100 light bulbs burned an average of x = 745.1 hours before failing,
#with a sample standard deviation of s = 238.0 hours. 
#Formulate appropriate null and alternative hypotheses. Calculate a significance probability.
#Do these results warrant rejecting the null hypothesis at a significance level of 0.05?

#input data profm the experiment
sample_mean <- 745.1 #mean number of hours
n <- 100 #sample size
s <- 238.0 #sample standard deviation
 
mu <- 800.0 #estimated population
teststat <- (sample_mean - mu)/(s * 1/sqrt(n))
teststat

#Calculate significance probability at 0.05 significance level
p_value <- pnorm(teststat, lower.tail = TRUE)
p_value

###########
#Question 3
###########
# http://lib.stat.cmu.edu/datasets/1993.expo/cereal # cereal data set
# http://lib.stat.cmu.edu/datasets/1993.expo/ # description
# Analyze the cereal data set. Write a report of your findings, including commands, corresponding plots/tables, and interpretations.
# For ideas, see sample questions on the cereal description page or create your own. Your group should answer at least 5 questions (among these, you should include at least 2 challenging problems).

#Load Data into a table 
cereal1 <- read.table("cereal.csv", header = FALSE, sep=" ", quote="", stringsAsFactors = FALSE)
cereal1

str(cereal1)

#Add column names
names(cereal1) <- c("name", "mfr", "type", "calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", "shelf", "potass", "vitamins", "weight", "cups")
str(cereal1)


#EDA

summary(cereal1) #Summary statistics for all variables
head(cereal1) #First few rows
sum(is.na(cereal1)) #Missing values?
#Boxplots for all numerical variables in the dataset
par(mfrow=c(2,4))
for (i in 4:15){
    boxplot(cereal1[i], main=i)
    }

#For impute values = -1  (N/A) with mean values for the carbo, sugars, potass, weight and cups variables
cereal2 <- cereal1
cereal2$carbo <- ifelse(cereal2$carbo == -1.00, mean(cereal2$carbo), cereal2$carbo) #impute -1 (N/A) with mean values
cereal2$sugars <- ifelse(cereal2$sugars == -1, mean(cereal2$sugars), cereal2$sugars) #impute -1 (N/A) with mean values
cereal2$potass <- ifelse(cereal2$potass == -1, mean(cereal2$potass), cereal2$potass) #impute -1 (N/A) with mean values
cereal2$weight <- ifelse(cereal2$weight == -1, mean(cereal2$weight), cereal2$weight) #impute -1 (N/A) with mean values
cereal2$cups <- ifelse(cereal2$cups == -1, mean(cereal2$cups), cereal2$cups) #impute -1 (N/A) with mean values

#Check the results
summary(cereal2)

#Boxplots for all numerical variables in the dataset
par(mfrow=c(2,4))
for (i in 4:15){
   boxplot(cereal2[i], main=i)
 }


#Q1: Is there a link between the manufacurer and the location shelf?
qplot(as.factor(mfr), shelf, data = cereal2, geom = "boxplot", main="Shelf level Vs Manufacturer") + theme_light()

#Use ANOVA to check for relationship between the manufacturer and the level of shelf
shelf_mfr <- aov(shelf ~ as.factor(mfr), data = cereal2)
summary(shelf_mfr)

#Q2: Relationship between sugar and calories
qplot(sugars, calories, data=cereal2) + geom_smooth(method = "lm") + theme_light()
#Fit a linear regression model
sugar_calories_model = lm(calories ~ sugars, data = cereal2)
summary(sugar_calories_model)

#Q3: Do any of the manufacturers consistnetly offer healthier cereals (higher vitamin content)?
qplot(mfr,vitamins, data = cereal2, geom = "boxplot", main="Vitamins Vs Manufacturers") + theme_light()
#Use ANOVA to find out if there is a relationhip between manufacturer and vitalmins
vitamins_mfr <- aov(vitamins ~ as.factor(mfr), data = cereal2)
summary(vitamins_mfr)
TukeyHSD(vitamins_mfr, conf.level = 0.95)

#Q4: Can the type of the cereal can be predicted using its nutritional content?

#Data preparation
cereal3<-cereal2
str(cereal3)
cereal3 <- cereal3[-1] #Drop name variable
cereal3 <- cereal3[-1] #Drop mfr variable
cereal3$type <-as.factor(cereal3$type) #Change type variable to a factor
str(cereal3)

table(cereal3$type)

#Split the dataset into 80% training and  20% testing data
set.seed(123)
split = 0.08
trainIndex <- createDataPartition(cereal3$type, p=split, list = FALSE)
cereal_test<- cereal3[trainIndex, ]
cereal_train <- cereal3[-trainIndex, ]

table(cereal_train$type)
table(cereal_test$type)

#Test dataset without the type column
cereal_test_notype <- cereal_test[-1]

#Use Naive Bayes to fit the model
set.seed(123)
nb_model <- naiveBayes(type ~ ., data=cereal_train)
#Generate predictions
nb_prediction <- predict(nb_model,newdata = cereal_test_notype)

#generate classification table
nb_table1 <- table(nb_prediction, cereal_test$type)
nb_table1
#Evaluate model performance
confusionMatrix(nb_table1)

#Q5: Do manufactures use smaller servings sizes in order to decrease the number of calories on mandatory labels?
#(is there a correlation between serving size and calorie content)?

qplot(calories, cups, data=cereal2) +  geom_smooth(method = "lm") + theme_light()

#Pearson's product correlation
cor.test(cereal2$calories, cereal2$cups)



