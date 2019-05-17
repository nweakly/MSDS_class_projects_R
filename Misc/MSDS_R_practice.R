#Practice: Data Imports and Graphs

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

#Load libraries
library("readtext")
library("XML")
library("openxlsx")
library ("ggplot2")

#######Data imports######

#Working with csv files

#Wine Quality dataset downloaded from  http://archive.ics.uci.edu/ml/datasets/Wine+Quality as a csv file
#Used file fir White Wines
data1<-read.csv(file.choose(), header=T, sep=";") # Load the dataset from a csv file , use";" as delimiter
str(data1) #Display internal structure
#Display boxplot for fixed acidity
boxplot(data1$fixed.acidity, main="Boxplot of fixed acidity", ylab="Fixed Acidity")
#scatter plot pH vs. Citric acid
plot(data1$pH ~ data1$citric.acid) 
#create pairwise scatterplots for the first five variables in the dataset
pairs(data1[1:5])

#Working with zip files

#Statistical data for Denver Metro area 1986 - 2016
#downloaded from https://demography.dola.colorado.gov/population/data/profile-regions/?counties=T
#as a csv file and zipped for the assignment

#unzip the file
fileDM <- unzip("DenverMetro.zip")
#Load data from the file into a dataframe
denver <- read.csv(fileDM)
#Display first few rows of the the dataframe 
head(denver)
#Histogram Number of Births in the Denver Metro Area 
hist(denver$Births, main="Histogram of Number of Births, Denver Metro Area, 1985-2016")

#Graph changes in population
ggplot(denver, aes(Year, Total.Population)) + geom_line() +
  labs(x = "Year", y = "Total Population", title = "Denver Metro Populattion 1985 - 2016")


#Working with Excel files

#National Occupational Statistics download from https://www.bls.gov/oes/
#load the work book
wb <- loadWorkbook("national_M2017_dl.xlsx")
#Read the worksheet into a dataframe
df3 <- readWorkbook("national_M2017_dl.xlsx", sheet=1, colNames=TRUE, rowNames=FALSE)
#Dispaly first few rows
head(df3)                  



#Working with xml files
#Regions in the Worlds by population
#Data from http://www.worldometers.info/world-population/population-by-region/ 
 
regions <-htmlParse("http://www.worldometers.info/world-population/population-by-region/")

regions.table <- readHTMLTable(regions, header=TRUE, stringsAsFactors= FALSE)
regions.table
###

regionsdf <- data.frame(regions.table) #convert to a data frame
#Make changes needed for graphing
#Change column names
colnames(regionsdf) <- c("" , "region", "population", "change", "net change", "density", "area", "migration") 
#Convert population stats from char to numeric
regionsdf$population2018 <- as.numeric(gsub(",", "", regionsdf$population))
str(regionsdf) #Check the structure
regionsdf  #Display the dataframe

#Create Bar chart  population bu Region
ggplot(regionsdf, aes( x=region, y=population2018)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Bar Chart", 
       subtitle="Population by Region", 
       caption="source: worldometers") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



