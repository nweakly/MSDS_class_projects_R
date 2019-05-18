# Statistical Testing 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignemnt
getwd() #Check working directory

#########################################
######## Statistical Testing ###########
########################################

#Exercise 1
x<-rnorm(1000, mean =0) # Generate a sample of 1000 random numbers with the mean of 0
shapiro.test(x) #test sample x to see if it is normally distributed


#Exercise 2
CO2 #Display the CO2 dataset
y<-CO2[,5] #Load the fifth column values ("uptake") into vector y
shapiro.test(y) #Use Shapiro-Wilk test on values in variable y

#Normality and Testing for Normality
#By Thomas Hopper
#https://www.r-bloggers.com/normality-and-testing-for-normality/ 


#Load libraries
library(ggplot2)
library(reshape2)

#Function to perform multiple Shapiro-Wilk tests
#for sample sizes n=5, 10 and 1000 drawn from the same data

#Variables:
#' @name assign_vector
#' @param data A vector of data to perform the t-test on.
#' @param n An integer indicating the number of t-tests to perform. Default is 1000
#' @return A data frame in "tall" format

assign_vector <- function(data, n = 1000) {
  # replicate the call to shapiro.test n times to build up a vector of p-values
  p.5 <- replicate(n=n, expr=shapiro.test(sample(my.data, 5, replace=TRUE))$p.value)
  p.10 <- replicate(n=n, expr=shapiro.test(sample(my.data, 10, replace=TRUE))$p.value)
  p.1000 <- replicate(n=n, expr=shapiro.test(sample(my.data, 1000, replace=TRUE))$p.value)
  #' Combine the data into a data frame, 
  #' one column for each number of samples tested.
  p.df <- cbind(p.5, p.10, p.1000)
  p.df <- as.data.frame(p.df)
  colnames(p.df) <- c("5 samples","10 samples","1000 samples")
  #' Put the data in "tall" format, one column for number of samples
  #' and one column for the p-value.
  p.df.m <- melt(p.df)
  #melt function from reshape2 package stacks several groups into the same column and 
  #creates a factor vaqriable to indicate which group of variables in corresponds to
    #' Make sure the levels are sorted correctly.
  p.df.m <- transform(p.df.m, variable = factor(variable, levels = c("5 samples","10 samples","1000 samples")))
  return(p.df.m)  
}

#Generate Test Data

n.rand <- 100000 
n.test <- 10000
my.data <- rnorm(n.rand)
p.df.m <- assign_vector(my.data, n = n.test)


#Create histograms to visualize p-values
ggplot(p.df.m, aes(x = value)) + 
  geom_histogram(binwidth = 1/10) + 
  facet_grid(facets=variable ~ ., scales="free_y") + 
  xlim(0,1) +
  ylab("Count of p-values") +
  xlab("p-values") +
  theme(text = element_text(size = 16))


#Compare normal distribution to a t-distribution
ggplot(NULL, aes(x=x, colour = distribution)) + 
  stat_function(fun=dnorm, data = data.frame(x = c(-6,6), distribution = factor(1))) + 
  stat_function(fun=dt, args = list( df = 20), data = data.frame(x = c(-6,6), distribution = factor(2)), linetype = "dashed") + 
  scale_colour_manual(values = c("blue","red"), labels = c("Normal","T-Distribution"))


my.data <- rt(n.rand, df = 20)

#Code for the first graph re-used to produce graph 3 with changes to 
#correctly display <0.05 and >0.95
p.df.m <- assign_vector(my.data, n = n.test)
#Graph 3 with adjustments for values <0.05 and >0.95
ggplot(p.df.m, aes(x = value)) + 
  geom_histogram(breaks = seq(0, 1, by=0.1)) + 
  facet_grid(facets=variable ~ ., scales="free_y") + 
  ylab("Count of p-values") +
  xlab("p-values") +
  theme(text = element_text(size = 16))



##########Testing: the tails in t-distribution
#t-distribution in the middle and  normal distribution for the tails


#Construct a test dataset: t-distribution for the middle 99% of data
#and normal distribution for the tails
my.data <- rt(n.rand, df = 20)
my.data.2 <- rnorm(n.rand)
# Trim off the tails
my.data <- my.data[which(my.data < 3 & my.data > -3)]
# Add in tails from the other distribution
my.data <- c(my.data, my.data.2[which(my.data.2 < -3 | my.data.2 > 3)])

#with adjustments to correctly display values <0.05 and >0.95 
ggplot(p.df.m, aes(x = value)) + 
  geom_histogram(breaks = seq(0, 1, by=0.1)) + 
  facet_grid(facets=variable ~ ., scales="free_y") + 
  ylab("Count of p-values") +
  xlab("p-values") +
  theme(text = element_text(size = 16))


##########Testing the tails in normal distribution

#Construct a test dataset: normal distribution for the middle 99% of data
#and t-distribution for the extreme tails

my.data <- rnorm(n.rand)
my.data.2 <- rt(n.rand, df = 20)
# Trim off the tails
my.data <- my.data[which(my.data < 3 & my.data > -3)]
# Add in tails from the other distribution
my.data <- c(my.data, my.data.2[which(my.data.2 < -3 | my.data.2 > 3)])
#Display the results
#with adjustments to correctly display values <0.05 and >0.95 
ggplot(p.df.m, aes(x = value)) + 
  geom_histogram(breaks = seq(0, 1, by=0.1)) + 
  facet_grid(facets=variable ~ ., scales="free_y") + 
  ylab("Count of p-values") +
  xlab("p-values") +
  theme(text = element_text(size = 16))

#####Testing: Highly Skewed Data##############

#Generate a skwwed test dataset
my.data <- rlnorm(n.rand, 0, 0.4)

#Use qplot to display the sample   
qplot(my.data)


#Display the results
#with adjustments to correctly display values <0.05 and >0.95 
ggplot(p.df.m, aes(x = value)) + 
  geom_histogram(breaks = seq(0, 1, by=0.1)) + 
  facet_grid(facets=variable ~ ., scales="free_y") + 
  ylab("Count of p-values") +
  xlab("p-values") +
  theme(text = element_text(size = 16))