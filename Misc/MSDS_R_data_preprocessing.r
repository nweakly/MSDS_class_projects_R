#Data cleaning and pre-processing

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Load libraries
library("openxlsx")
library("dplyr")
library("Amelia")
library("Hmisc")

####National Occupational Statistics data previousely downloaded from https://www.bls.gov/oes/
####Load the data
#load the work book
wb <- loadWorkbook("national_M2017_dl.xlsx")
#Read the worksheet into a dataframe
df3 <- readWorkbook("national_M2017_dl.xlsx", sheet=1, colNames=TRUE, rowNames=FALSE)
#Dispaly first few rows
head(df3)   


###Cleaning and Pre-processing
#Rename the columns
colnames(df3) <- c("code", "title", "level", "employment", "employmentRSA", "meanhourlywage", "meanannualwage", "meanwageRSA", "hourly10pct", "hourly25pct", "medianhourlywage", "hourly75pct", "hourly90pct", "annual10pct", "annual25pct", "medianannualwage", "annual75pct", "annual90pct", "annual", "hourly")
#check new column names
names(df3)

#check internal structure of the data frame
str(df3)

#Data types conversions

#convert df3$level to a factor
df3$level <- as.factor(df3$level)

#convert meanhourlywage and meanannualwwage to numeric
df3$meanannualwage <- as.numeric(df3$meanannualwage)
df3$meanhourlywage<-as.numeric(df3$meanhourlywage)

#convert columns #9-18 to numeric
df3$hourly10pct <- as.numeric(df3$hourly10pct)
df3$hourly25pct <- as.numeric(df3$hourly25pct)
df3$hourly75pct <- as.numeric(df3$hourly75pct)
df3$hourly90pct <- as.numeric(df3$hourly90pct)
df3$medianhourlywage <- as.numeric(df3$medianhourlywage)
df3$annual10pct <- as.numeric(df3$annual10pct)
df3$annual25pct <- as.numeric(df3$annual25pct)
df3$annual75pct <- as.numeric(df3$annual75pct)
df3$annual90pct <- as.numeric(df3$annual90pct)
df3$medianannualwage <-as.numeric(df3$medianannualwage)

#check datatype conversion results
str(df3)

##Use different ways to drop unwanted columns
#Drop the hourly10pct, hourly25pct, hourly75, and hourly90pct columns
df3 <- df3[ , -9]

df3$hourly25pct <- NULL

#remove percentile columns for annual wages
df4 <- within(df3, rm(hourly75pct, hourly90pct, annual10pct, annual25pct, annual75pct, annual90pct))

#check results
str(df4)

#Drop the first row of data as it represents totals for all occupations
df4  <- df4[-1,]
#check results
head(df4)

###missing values detection

#Total number of missing values
sum(is.na(df4))

#Visualize missing values using missmap from Amelia package
missmap(df4, main="Missing Values")

#drop columns hourly and annual
df4$hourly <- NULL 
df4$annual <- NULL

#After driopping two columns check for missing values again
#Total number of missing values
sum(is.na(df4))

#Visualize missing values using missmap from Amelia package
missmap(df4, main="Missing Values")

#list rows of data that have missing values
df4[!complete.cases(df4),]

###impute missing values

#Impute NA values with the mean value of the column
df4$medianannualwage <- impute(df4$medianannualwage, fun=mean)

#Impute NA values with the median value of the column
df4$medianhourlywage <- impute(df4$medianhourlywage, fun=median)

#Impute NA values with the minimum value of the column
df4$meanannualwage <- impute(df4$meanannualwage, fun=min)

#Total remaining number of missing values
sum(is.na(df4))

#remove the remaining incomplete records from the data set
df_complete <- na.omit(df4)
sum(is.na(df_complete))

#delete first two columns
str(df_complete)
df5 <- df_complete[,-1]
df5 <- df5[, -1]
#Check results
head(df5)

#Filter data, include only records with median hourly wage >= 60
filter(df5, medianhourlywage >= 60)

#Group records by levels

df6 <- group_by(df5, level)       
head(df6)

