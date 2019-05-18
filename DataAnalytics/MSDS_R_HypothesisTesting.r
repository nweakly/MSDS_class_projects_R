# Hypothesis Testing

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignemnt
getwd() #Check working directory

#########################################
######## Hypothesis Testing ###########
########################################

###Exercise Step 1

#Import "before and after" data

#a_training vector with before new training results
a_training <- c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
#b_training vector with after training results
b_training <- c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

#Run a paired-t test by comparing difference in means before and after training
t.test(a_training, b_training, paired=TRUE)

###Exercise Step 2

#Import "before and after" data with a coach #2

#a_training vector with before new training results
a_training <- c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
#b_training vector with after training results with coach #2
b_training <- c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)

#Run a paired t-test: testing for the mean "before" being less than "after" 
#testing for the mean of the values in a_training to be less that mean of the values
#in b_training
t.test(a_training, b_training, paired=TRUE, alt="less") 



#################Testing the Results of New Drugs ##########################

###Trial 1. XYZ Drug

#Import Blood pressure data for "Before" and "afte drug XYZ"

bp_before <-c(155, 142, 145, 160, 149, 152, 157, 159, 166, 163, 158, 161) #Blood pressure before the trial
bp_zyz <-c(152, 142, 144, 159, 150, 153, 156, 160, 165, 162, 159, 160) #Blood pressure after XYZ drug trial

#Run a paired t-test by comparing mean blood pressure before and after the XYZ trial
t.test(bp_before, bp_zyz, paired=TRUE)

#Trial 2. ABC Drug
#Import Blood pressure data for "Before" and "afte drug ABC"

bp_before <-c(155, 142, 145, 160, 149, 152, 157, 159, 166, 163, 158, 161) #Blood pressure before the trial
bp_abc <-c(150, 135, 142, 153, 142, 147, 152, 149, 158, 155, 150, 150) #Blood pressure after ABC drug trial

#Run a paired t-test by comairing mean blood pressure before and after the ABC trial
#Since all 12 subject showed a decrease in blood pressure after the trial,
#check for bp_before < bp_abc
t.test(bp_before, bp_zyz, paired=TRUE, alt = "less")
