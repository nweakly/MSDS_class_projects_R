###  Time Series
### Median House Sales Prices

###Dataset source: https://www.census.gov/construction/nrs/historical_data/index.html 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Load libraries
library(openxlsx)
library(stats)
library(tseries)
library(forecast)

###Load Data - file was previousely saved as .xlsx (instead of xls)
prices <- read.xlsx("pricereg_cust.xlsx", sheet = 2, startRow = 4, colNames = TRUE, rowNames = FALSE, rows = c(4:228), fillMergedCells = FALSE)

#Check data
head(prices)
tail(prices)
#check structure of the df
str(prices)

###Data clean-up
prices <- prices[-1,] #delete first row
prices<- prices[,-c(7:11)] #drop columns 7 through 11 (average sale prices)
names(prices)[2] <- "US" #Rename the second column
prices$US <- as.numeric(prices$US) #convert US prices to numeric 

#check results
head(prices)
tail(prices)
str(prices)

###Create a time series object for the median prices, South region
south_ts <- ts(prices[,5], start=c(1963,1), end=c(2018,3), frequency = 4)
#check content
south_ts

##EDA
#plot time series
plot.ts(south_ts)
#trendline
abline(reg=lm(south_ts~time(south_ts)))

#test for stationary time series
adf.test(south_ts, alternative = "stationary")

#smoothing using simple moving average ms() from the forecast package
par(mfrow=c(1,2))
plot(ma(south_ts, 3), main= "Simple Moving Average (k=3)")
plot(ma(south_ts, 5), main= "Simple Moving Average (k=5)")
par(mfrow=c(1,1))

###Decomposing 

#decomposition by loess smoothing
south_decomposed <- stl(south_ts, s.window = "periodic")
plot(south_decomposed)

#estimated values for the components
south_decomposed$time.series
plot(south_decomposed$time.series)

#Visualizing seasonal components
par(mfrow=c(2,1))
monthplot(south_ts, xlab=", ylab=")
seasonplot(south_ts, year.labels="TRUE", main="")
par(mfrow=c(1,1))

###Data transformation

#Log transformation
south_logts <- log(south_ts) #stabilizging using log function
plot.ts(south_logts)

#Differencing
south_logdiff <- diff(south_logts, difference =1)
plot.ts(south_logdiff)

#checking for stationary ts
adf.test(south_logdiff, alternative = "stationary")


################Models#############

###Simple exponential smoothing

fit_ets <- ets(south_logdiff) #autoselect the best fit
fit_ets 
#check the residuals
acf(fit_ets$residuals)
pacf(fit_ets$residuals)

Box.test(fit_ets$residuals, type="Ljung-Box") #test independence between successive errors

plot.ts(fit_ets$residuals)


###Holt-Winters exponential smoothing

fit_hw <- HoltWinters(south_logdiff) #automatically select parameters
fit_hw

#plot obbserved and fit
plot(fit_hw)

#evaluate model fit - look at the residuals 
prediction_hw1 <- forecast(fit_hw)
prediction_hw1$residuals

plot.ts(prediction_hw1$residuals)

acf(prediction_hw1$residuals, na.action = na.omit)
pacf(prediction_hw1$residuals, na.action = na.omit)

Box.test(prediction_hw1$residuals, type="Ljung-Box") #test independence between successive errors

##Use Holt-Winters model for forecast
prediction_hw <- forecast(fit_hw, h=12) #forecast values of the next three years
prediction_hw #predicted values in log thousands
plot(prediction_hw)

#converting prediction back into the initial units 
low_hw<- exp(prediction_hw$lower)
upper_hw <- exp(prediction_hw$upper)
mean_hw <- exp(prediction_hw$mean)
results_hw <- cbind(mean_hw, low_hw, upper_hw)
dimnames (results_hw)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
results_hw

##ARIMA model

#ARIMA model autofit
#differenecing 1 time so for ARIMA (p,d,q) model d=1
fit_arima <- auto.arima(south_ts, d=1) #autofit arima model to the initial ts
fit_arima

#look at the  residuals to check nodel fit
plot.ts(fit_arima$residuals)
acf(fit_arima$residuals)#autocorrelation of the residuals 
pacf(fit_arima$residuals)#partial authocorrelation of the residuals
Box.test(fit_arima$residuals, type = "Ljung-Box")

#Using ARIMA model for predictions
prediction_arima <- forecast(fit_arima, h=12) #forecast for the next 12 quarters (3 years)
prediction_arima
plot(prediction_arima) #plot predictions

###
###Can't directly compare the models as they use different units
#Accuracy calculated on the training set
accuracy(fit_arima) 
accuracy(prediction_hw) 

fit_hw$SSE

AIC(fit_ets)  #did not use the model for forecasting as Box.test() showed poor fit
AIC(fit_arima)
#AIC can not be applied to an objet of class "Holtwinters", so can't use for AIC(fit_hw)








