#Experimental Design

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignemnt
getwd() #Check working directory

#########################################
##### Randomized block design ##########
########################################

#6 restaurants test 3 menu items, one item per week

#Read Data
df2=read.table("fastfood-2.csv", header=TRUE, sep=",") #Read data from the file into a data frame
df2

#Create Response vector
r = c(t(as.matrix(df2))) # concatenate response data into a single vector 
                        #as.matrix() function returns a vector of cell values; function t() returns the transpose of the argument 
                        #and it is concatenated into vector r  
r

#New variables for Treatment factors
f = c("Item1", "Item2", "Item3")   # treatment levels 
k = 3                    # number of treatment levels 
n = 6                    # number of control blocks


#Create a Vector of Treatment Factors (tm)
tm = gl(k, 1, n*k, factor(f))   # use gl function to create a vector of treatment factors corresponding  
                                #elements in r
                                # function gl() generates factor levels by the pattern of the levels
tm                              #

#Create a vector of blocking factors for each element in r

blk = gl(n, k, k*n)             # blocking factor 
blk 

#Use aov function to describe the r response by both treatment factor tm and the block control blk
av= aov(r~ tm+blk)

#Display the results

summary(av)




