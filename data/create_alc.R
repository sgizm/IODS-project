#Sezin Yaman
#09.02.2017
#Idk what it is yet

library(dplyr)
library(GGally)
library(ggplot2)
library(boot)

### DATA WRANGLING ###

data1 <- read.csv("student-mat.csv", sep = ";", header = TRUE) #reading the csv
data2 <- read.csv("student-por.csv", sep = ";", header = TRUE) #reading

data.frame(data1)
data.frame(data2)

dim(data1) #dimensions of the data which happens to be 395 * 1
str(data1) #structure of data
dim(data2) #dimensions of the data which happens to be 649 * 1
str(data2) #structure of data

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(data1, data2, by = join_by, suffix = c(".data1", ".data2"))

# see the new column names
colnames(math_por)
glimpse(math_por)
str(math_por)
dim(math_por) #382 * 53

# create a new data frame with only the joined columns
alc <- select(math_por, one_of("school",   "sex",      "age",      "address",  "famsize",  "Pstatus" , "Medu",     "Fedu",     "Mjob",     "Fjob",     "reason",   "nursery", "internet"))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(data1)[!colnames(data1) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)
# glimpse at the new combined data again
glimpse(alc)
write.csv(alc, file = "alc.csv", row.names = FALSE)

### ANAYSIS ###
alc2 <- read.csv("alc.csv", sep=",", header=TRUE)
# checking if it is in the right format
str(alc2) 
dim(alc2)
# 382 * 32
colnames(alc2)

# high_use vs. failures, absences, sex, age and perhaps also the grades
# I believe that high consumption is positively correlated with high number of absenses, low grades and males. Unfortunately I cannot make an hypothesis about age :)
ggplot(alc2, aes(x = high_use, y = G3, col=sex)) + geom_boxplot()
cross1 <- table(alc2$high_use, alc2$sex)

#shows that male high consumption is correlated with lower grades. no significant difference for females
ggplot(alc2, aes(x = high_use, y = absences, col=sex)) + geom_boxplot()
#shows that no significant diff between no. of absences and high use, although male comsuption is bit higher than female
ggplot(alc2, aes(x = high_use, y = age, col=sex)) + geom_boxplot()
#shows that mean high use age is 17 for males, whereas 16.5 for females. 
#ggplot(alc2, aes(x = high_use, y = failures, col=sex)) + geom_boxplot() this one did not give some nice output



# find the model with glm()
m <- glm(high_use ~ G3 + absences + sex + age, data = alc2, family = "binomial")

# print out a summary of the model
summary(m) 
#according to the summary, absenses and sex seem to be correlated with high alcohol usage with p value being less than 0.05, where as grades and age is not significantly interesting
# print out the coefficients of the model
coef(m)


# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)
# sex, being a male, seems to be almost twice as likely to result in higher alcohol consumption as opposed to a low level of consumption.
# age also seem to be significant looking at the OR value, and absences are very close to the age's OR and is still bigger than 1, which indicated that they it is still likely to affect high alcohol consumption.


#Fit the new model
m2<-glm(high_use ~ absences + age, data=alc2, family="binomial")

#Predict() the probability of high_use
probabilities <- predict(m2, type="response")

#Add the predicted probabilities to 'alc2'
alc2<-mutate(alc2, probability = probabilities)

#Use the probabilities to make a prediction of high_use
alc2<-mutate(alc2, prediction = probability>0.5)

#Tabulate the target variable versus the predictions
table(high_use=alc2$high_use, prediction=alc2$prediction)

ggplot(alc2, aes(x = probability, y = high_use, col = prediction)) + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc2$high_use, prediction = alc2$prediction) %>% prop.table() %>% addmargins()
#There is 0.68 probability for both predicted and observed to be false and 0.02 probability for both predicted and observed to be true.

# training error
# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}
loss_func(class = alc2$high_use, prob = alc2$probability)
#training error is 0.28


cv <- cv.glm(data = alc2, cost = loss_func, glmfit = m2, K = 10)
# average number of wrong predictions in the cross validation
cv$delta[1]
#test error is 0.28 as well :)