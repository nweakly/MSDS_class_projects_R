###  Outlier Detection

###Dataset source: https://datamarket.com/data/set/22p8/number-of-earthquakes-per-year-magnitude-70-or-greater-1900-1998#!ds=22p8&display=line  

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Load libraries
library(openxlsx)
library(tseries)
library(tsoutliers)
library(forecast)
library(outliers)
library(AnomalyDetection)
library(devtools)
library(ggplot2)

###Load Data - file in .xlsx format
equakes <- read.xlsx("number-of-earthquakes-per-year-m.xlsx", sheet = 1, startRow = 14, colNames = FALSE, rowNames = FALSE, rows = c(14:112), detectDates = TRUE, fillMergedCells = FALSE )

#Check data
head(equakes)
tail(equakes)
#check structure of the df
str(equakes)

#add column names
colnames(equakes) <- c("year", "earthquakes")
equakes$year <- as.numeric(format.Date(equakes$year, format = "%Y", origin ='1900-01-01'))

#change 1989 to 1900 (as the excel date of 1/1/1990 loaded as 12/1/1899) 
equakes[1,1] <- 1900

#Check results
#Check data
head(equakes)
tail(equakes)
#check structure of the df
str(equakes)

###EDA, check for normal distribution
summary(equakes)
boxplot(equakes$earthquakes) #boxplot
plot(equakes$earthquakes ~ equakes$year, type='l')

hist(equakes$earthquakes) #histogram
qqnorm(equakes$earthquakes) #normal quantile-quantile plot

#Shapiro-Wilk normalisty test
shapiro.test(equakes$earthquakes)

#####Statistical tests (parametric)
#Grubbs' test for outliers

grubbs.test(equakes$earthquakes, type = 10, opposite = FALSE, two.sided = FALSE)

#Chi-Square test 
chisq.out.test(equakes$earthquakes)

#####Using ARIMA to find outliers

#create a time series object
eqts <- ts(equakes$earthquakes, start=c(1900), end=c(1998), frequency = 1)
#automatically choose model parameteres
eq_fit <- auto.arima(eqts)
eq_fit

#use ARIMA (1,0,1) model and the tsp function to highlight the outliers from the dataset
eq_outliers <- tso(eqts, tsmethod = 'arima', args.tsmethod = list(order = c(1,0,1)))
eq_outliers #detailed model info
plot(eq_outliers) #plot results


#####Using AnomalyDetection package

#df dates from 01/01/1900 to 01/01/1998
years <- as.data.frame(seq(as.Date("1900/1/1"), as.Date("1998/1/1"), "years"))
colnames(years) <- c('year') 
#convert date to POSIXlt format
years$year<-as.data.frame(as.POSIXlt(years$year,format = '%Y'))

#create df to be used with the AnomalyDetection package
equakes2<- cbind.data.frame(years$year,equakes$earthquakes)
colnames(equakes2) <- c("year", "earthquakes") #rename columns
str(equakes2) #check results

#plot initial data
ggplot(equakes2, aes(year, earthquakes)) + geom_line() + scale_x_datetime() + xlab("Years") + ylab("Number of Earthquakes")

#AnomalyDetection.AnomalyDetectionTs(equakes, plot=TRUE)$plot
anomalies<- AnomalyDetectionTs(equakes2, direction = 'both', max_anom = 0.1, plot  = TRUE)
anomalies$plot
anomalies$anoms

#alpha determines level of significance for anomalies, default alpha = 0.05
#decrease to alpha= 0.25
anomalies2<- AnomalyDetectionTs(equakes2, direction = 'both', alpha=0.25,max_anom = 0.1, plot  = TRUE)
anomalies2$plot
anomalies2$anoms




