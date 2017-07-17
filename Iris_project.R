#getting working directory
getwd()
#Reading Iris data set in R
R = read.csv("Iris.txt")
#Adding column names in data set
colnames(R) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
#Checking names of column
names(R)
#Checking dimensionality of the data set
dim(R)
#Checking the data structure
str(R)
#Getting first 6 rows of the date set
head(R)
#Looking for distribution of each feature
summary(R)
#Loading library ggplot
library(ggplot2)
#Bar plot to find which species is count is high and which is low
ggplot(R, aes(x= Species, fill = Species)) + geom_bar(width = .5) +
  ggtitle("Bar plot of different species") + 
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
         axis.title.x = element_text(color="blue", size=14, face="plain"),
         axis.title.y = element_text(color="pink", size=14, face="plain"))
#Scatter plot of Sepal.Length and Sepal.Width 
ggplot(R, aes(x = Sepal.Length, y = Sepal.Width, color = Species, shape = Species)) + geom_point() + 
  ggtitle("Scatter plot of \n Sepal.Length and Sepal.Width")+ 
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=14, face="plain"),
  axis.title.y = element_text(color="pink", size=14, face="plain"))
#Scatter plot of Petal.Length and Petal.Width
ggplot(R, aes(x = Petal.Length, y = Petal.Width, color = Species, shape = Species)) +
  geom_point() + ggtitle("Scatter plot of \n Petal.Length and Petal.Width")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"),
         axis.title.x = element_text(color="blue", size=14, face="plain"),
         axis.title.y = element_text(color="pink", size=14, face="plain"))
#Loading library GGally
library(GGally)
#Finding correlation between length and width of Sppeal and Petal through pair diagram
ggpairs(R, aes(color=Species, shape = Species, alpaha = 0.5)) + ggtitle("Pair plot")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"))
#Performing K nearest neighbour technique on date set R
R
#Mixing the rows of the data set to perform K neighbour
#Generating random number
set.seed(231) #To fix the number of random numbers everytime
R_1 <-runif(nrow(R)) #Generate as many random numbers as number of rows in R dataset
R_1
R_2 <- R[order(R_1),] #Applying random numbers to R data set, to mix up rows and keeping all columns
str(R_2) #Checking if the structure is same or not
head(R_2, 15) #Checking if the rows have mixed up or not
#Looking at summary for ranges of 4 features
summary(R_2[,c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width")])
#Performing normalization to features so that features with larger range will not have any specific influence 
#Normalization will change the range of each feature to 0 and 1 another way of doing is standardisation with z score
normalize <- function(x) {return( (x-min(x))/(max(x)-min(x)) )} #User defined function
normalize(c(100,200,300,400)) #Checking if normalizing function working perfectly
#Normalizing R_2 data set with 4 features
R_normalize <- as.data.frame(lapply(R_2[,c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width")],normalize))
str(R_normalize) #Checking the structure of R_normalize
summary(R_normalize[,c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width")]) #Checking the range of features does it normalize
R_normalize
#Creating a training & testing data set
R_training <- R_normalize[1:120,]
R_testing <- R_normalize[121:149,]
R_training_target <- R_2[1:120, "Species"]
R_testing_target <- R_2[121:149, "Species"]
#Loading K nearest algorithm
library(class)
#Choosing k value here we choose k as sqrt of total values
sqrt(149)
R1 <- knn(train = R_training, test = R_testing, cl = R_training_target, k = 12)
R1
#Checking how well it does using confusion matrix
table(R_testing_target, R1)
