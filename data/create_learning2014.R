#Sezin Yaman
#03.02.2017
#Idk what it is yet

library(dplyr)
library(GGally)
library(ggplot2)

data <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE) #reading the table
#data <- read.csv("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt")
data.frame(data)
dim(data) #dimensions of the data which happens to be 183 * 60
str(data) #structure of data with mostly integer variables and a factor 

### DATA WRANGLING ###
# divide each number in the column vector
data$Attitude / 10
# create column 'attitude' by scaling the column "Attitude"
data$attitude <- data$Attitude / 10

# questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(data, one_of(deep_questions))
data$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(data, one_of(surface_questions))
data$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(data, one_of(strategic_questions))
data$stra <- rowMeans(strategic_columns)

columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(data, one_of(columns))

# select rows where points is greater than zero
learning2014 <- filter(learning2014, Points > 0)

# writing the the dataset as csv
write.csv(learning2014, file = "learning2014.csv", row.names = FALSE)
# reading it again
data2 <- read.csv("learning2014.csv", sep=",", header=TRUE)
# checking if it is in the right format
str(data2)
dim(data2)
# yeah it is



### ANAYSIS ###
#reading the student 2014 file
data2 <- read.csv("learning2014.csv", sep=",", header=TRUE)
# checking if it is in the right format
str(data2) 
dim(data2)
# data has 116*7 dimensions as it should be, and includes 6 variables named: gender, Age, attitude, deep, stra, surf and Points. 

# exploring the variables graphically first with the pairs function 
# we exlude the first variable, gender because it is a factor variable. Instead we use it for the colors to categorize each variable
pairs(learning2014[-1], col=learning2014$gender)

# more advanced plot indicating the relationship between each variables
ggpairs(learning2014, mapping = aes(col=gender,  alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

# multiple regression model
my_model <- lm(Points ~ attitude + stra + surf, data = data2)
# print out a summary of the model
summary(my_model)

# summary of the model shows that attitude and points have stronger relationship with the points compared to stra and surf
# as null hypothesis implies that there is no significant relationship between two observed phenomena, rejecting the null hypothesis indicates that there is a relationship between the observations. 
# points and attitude have seem to have a smallest p value. for the other two variables, there is no statistical significant with points variable.
# then if we reset the model with only the attitude variable:
my_model2 <- lm(Points ~ attitude, data = data2)
# print out a summary of the model
summary(my_model2)

# draw diagnostic plots using the plot() function. Choose the plots 1, 2 and 5
plot(my_model2, which=c(1, 2, 5), par(mfrow = c(2,2)))
# the modeeling involved a number of assumtions as one of the being the errors having distributed normally.
# qqplot shows that model goes pretty close around y = x direction which indicates that errors are distributed normally except that last part of the graph.
# Leverage of observations indicated how much impact a single observation have on the model. Distribution is quite normal in the beginning, however there are few outliers towards the right hand-side of the x axis which hinders the normality.  
