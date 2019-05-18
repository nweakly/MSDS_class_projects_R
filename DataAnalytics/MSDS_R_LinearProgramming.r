#Linear Programming 

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory


#######################################################################
#################Linear Programming####################################
############# Choosing what to plant: wheat or rye?###################
#########Based on Exercise #1 Week 8: Linear Programming###############
#######################################################################

install.packages("lpSolveAPI")

###Load packages
library("lpSolveAPI")
#Create lpSolve linear programming model object
lprec <- make.lp(0, 2) #model with 0 rows and 2 columns as we have 2 crops
lp.control(lprec, sense="max")#maximization problem

#set objective function
set.objfn(lprec, c(500, 300)) #profit $500/acre of wheat and $300/acre of rye

#######Add constraints############

#cost constraint: $200 to plant each acre of wheat, $100 to plant each acre of rye,
#total cost not to exceed  1200
add.constraint(lprec, c(200, 100), "<=", 1200)

#Hours of labor constraint
#1 hour of labor per acre of wheat, 2 hours of labor per acre of rye
#Total planting time not to exceed 12 hours
add.constraint(lprec, c(1,2), "<=", 12)

#Area constraint
#10 acres available to plant, need to palnt at least 7 acres
add.constraint(lprec, c(1,1), "<=", 10) # 10 acre total available
add.constraint(lprec, c(1,1), ">=", 7) #need to seed 7 acres mimimum

lprec #Display model parameteres

#find solution
solve(lprec)
get.objective(lprec)
get.variables(lprec)


