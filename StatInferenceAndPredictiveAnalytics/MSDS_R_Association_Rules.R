### Association Rules using Book Data dataset  

rm(list=ls()) #Clear the environment
setwd("YOUR_PATH") #Set working directory for the assignment
getwd() #Check working directory

###Load packages
library(arules)
library(arulesViz)
library(sqldf)
library(ggplot2)

#Load data
load("bxBooks.RData")

###############EDA and data pre-processing###############################

#bxBookRatings dafa frame
str(bxBookRatings)
#delete periods in the column names
colnames(bxBookRatings) <- gsub(".", "", colnames(bxBookRatings), fixed=T)
#Look at the first few rows
head(bxBookRatings) 

#bxUsers dataframe
str(bxUsers)
#delete periods in the column names
colnames(bxUsers) <- gsub(".", "", colnames(bxUsers), fixed=T)
#Look at the first few rows
head(bxUsers)

#bxBooks dataframe
str(bxBooks)
bxBooks <- bxBooks[-c(6:8)] #drop columns containing pictures URLs
#delete periods in the column names
colnames(bxBooks) <- gsub(".", "", colnames(bxBooks), fixed=T)
#Look at the first few rows
head(bxBooks)

#Code source for clean-up:https://github.com/WinVector/zmPDSwR/blob/master/Bookdata/create_bookdata.R 
#bxBooks dataframe
Sys.setlocale('LC_ALL','C') # for non-English characters
# Clean up book titles (delete parenthesis,)
bxBooks$BookTitle <- gsub("(", "#", bxBooks$BookTitle, fixed=T)
bxBooks$BookTitle <- gsub("^#", "(", bxBooks$BookTitle)
bxBooks$BookTitle <- gsub("#.*$", "", bxBooks$BookTitle) 
bxBooks$BookTitle <- sub("[[:space:]]+$","", bxBooks$BookTitle)  #save cleaned-up titles
bxBooks$BookTitle<- tolower(bxBooks$BookTitle) #convert titles to the lower case

#Check results
head(bxBooks)
str(bxBooks)

#merge by ISBN
books_merged <-merge(bxBookRatings, bxBooks, by="ISBN")

#check the resulting daat frame
str(books_merged)
head(books_merged)

#create a histogram of top 20 book titles
sorted_titles <- sort(table(books_merged$BookTitle), decreasing = TRUE)
top20titles <- sorted_titles[1:20]
top20titles
op <- par(mar=c(10,4,4,2))#set margins sizes for book title space
barplot(top20titles,  las=2)#las=2 labels are perpendicular to axis
rm(op)#remove margin settings

#create a histogram of top 20 authors
sorted_authors <- sort(table(books_merged$BookAuthor), decreasing = TRUE)
top20authors <- sorted_authors[1:20]
top20authors
op <- par(mar=c(10,4,4,2))#set margins sizes for book title space
barplot(top20authors,  las=2)#las=2 labels are perpendicular to axis
rm(op)#remove margin settings

#create a histogram of top 20 publishers
sorted_publishers <- sort(table(books_merged$Publisher), decreasing = T)
top20publishers <- sorted_publishers[1:20]
top20publishers 
op <- par(mar=c(10,4,4,2))#set margins sizes for book title space
barplot(top20publishers,  las=2)#las=2 labels are perpendicular to axis
rm(op)#remove margin settings

#######################################################################
#convert each attributes to a factor
books_merged$ISBN <- as.factor(books_merged$ISBN)
books_merged$UserID <- as.factor(books_merged$UserID)
books_merged$BookRating <- as.factor(books_merged$BookRating)
books_merged$BookTitle <- as.factor(books_merged$BookTitle)
books_merged$BookAuthor <- as.factor(books_merged$BookAuthor)
books_merged$YearOfPublication <- as.factor(books_merged$YearOfPublication)
books_merged$Publisher <- as.factor(books_merged$Publisher)

###save the file in the tab-separated file format
write.table(books_merged, file="books_merged.tsv", sep="\t", row.names = FALSE, col.names = TRUE)

###convert data file to the transaction class 
bookbsk <- read.transactions('books_merged.tsv', cols=c("UserID", "BookTitle"), format = "single", sep="\t", rm.duplicates = TRUE)

#Explore the transaction
class(bookbsk)
colnames(bookbsk)[1:10]
summary(bookbsk)
bookbsk #prints object type and dimentions
dim(bookbsk) #dimentions
colnames(bookbsk)[1:5] #first five columns
rownames(bookbsk)[1:5] #first five rows - user IDs
length(bookbsk)#number of observations
size(bookbsk) #number of itemsin each observation

#transaction sizes
transactionsizes<-size(bookbsk)
summary(transactionsizes) #distribution of transaction sizes
quantile(transactionsizes, probs=seq(0, 1, 0.1)) #transaction size distribution in 10% increments
#plot the distribution
ggplot(data.frame(count=transactionsizes)) + geom_density(aes(x=count)) + scale_x_log10()
#99% of customers
quantile(transactionsizes, probs = c(0.99, 1))

#book frequency
booksfrequency <- itemFrequency(bookbsk)
summary(booksfrequency)
itemFrequencyPlot(bookbsk, topN=20)

#Calculate support of frequent items
frequentitems <- eclat(bookbsk, parameter = list(suppo = 0.005, maxlen=15))
summary(frequentitems)

###Filter data for users interested in more than one book
dim(bookbsk) #dimension before redaction
bksize<- size(bookbsk)
bookbsk_2up <- bookbsk[bksize>1] #saving only transactions with >1 book
dim(bookbsk_2up) #dimension after dropping transactions with 1 book

###Use apriori() to find assoiciation rules
rules1 <- apriori(bookbsk_2up, parameter = list(support = 0.005, confidence = 0.70))
summary(rules1)
inspect(rules1) #print out rules
inspect(sort(rules1, by="confidence", decreasing=TRUE))
#visualizing rules
plot(rules1)
plot(rules1, method="graph", interactive=TRUE, shading="confidence")
plot(rules1, measure=c("support", "lift"), shading="confidence")
plot(rules1, method="graph", control=list(type="items"))
plot(rules1, method="paracoord", control = list(reorder=TRUE))

df_rules1 <- as(rules1, "data.frame")

#Change parameteres using apriori()
rules2 <- apriori(bookbsk_2up, parameter = list(support = 0.001, confidence = 0.70))
summary(rules2)

#Change parameteres using apriori()
rules3 <- apriori(bookbsk_2up, parameter = list(support = 0.001, confidence = 0.75))
summary(rules3)
#Remove redundant rules
redundant1 <- which (colSums(is.subset(rules3, rules3))>1) #vector of redundant rules
rules3a<-rules3[-redundant1] #remove redundant rules
summary(rules3a)
rules3b<- head(sort(rules3a, by="confidence", decreasing=TRUE), n=20) #first 20 rules in decreasing order of confidence
inspect(rules3b)
#visualizing rules
plot(rules3b)
plot(rules3b, method="graph", interactive=TRUE, shading="confidence")
plot(rules3b, measure=c("support", "lift"), shading="confidence")
plot(rules3b, method="graph", control=list(type="items"))
plot(rules3b, method="paracoord", control = list(reorder=TRUE))


#sort by lift
rules3c<- head(sort(rules3a, by="lift", decreasing=TRUE), n=20) #first 20 rules in decreasing order of lift
inspect(rules3c)
#visualizing rules
plot(rules3c)
plot(rules3c, method="graph", interactive=TRUE, shading="confidence")
plot(rules3c, measure=c("support", "lift"), shading="confidence")
plot(rules3c, method="graph", control=list(type="items"))
plot(rules3c, method="paracoord", control = list(reorder=TRUE))

#Change parameteres using apriori()
rules4 <- apriori(bookbsk_2up, parameter = list(support = 0.002, confidence = 0.75))
summary(rules4)
#Remove redundant rules
redundant <- which (colSums(is.subset(rules4, rules4))>1) #vector of redundant rules
rules5<-rules4[-redundant] #remove redundant rules
summary(rules5)
inspect(sort(rules5, by="confidence", decreasing=TRUE)) #print rules in decreasing order of confidence
#visualizing rules
plot(rules5)
plot(rules5, method="graph", interactive=TRUE, shading="confidence")
plot(rules5, measure=c("support", "lift"), shading="confidence")
plot(rules5, method="graph", control=list(type="items"))
plot(rules5, method="paracoord", control = list(reorder=TRUE))

##############################
#what were users interested before the fellowship of the ring
rules6 <- apriori(data=bookbsk_2up, parameter=list(supp=0.001, conf=0.08), appearance = list(default="lhs", rhs="the fellowship of the ring"), control=list(verbose=FALSE))
summary(rules6)
inspect(sort(rules6, by="confidence", decreasing=TRUE))
plot(rules6, method="graph", interactive=TRUE, shading="confidence")

#what were users interested afther the fellowship of the ring
rules7 <- apriori(data=bookbsk_2up, parameter=list(supp=0.001, conf=0.08), appearance = list(default="rhs", lhs="the fellowship of the ring"), control=list(verbose=FALSE))
summary(rules7)
inspect(sort(rules7, by="confidence", decreasing=TRUE))
plot(rules7, method="graph", interactive=TRUE, shading="confidence")


