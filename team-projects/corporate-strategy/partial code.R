#!READ! This file is only part of the entire code involved in the execution of the project.
#!!READ!! I picked up from where my former teammate left from line 55 onwards.


#Make sure that both the csv and r files are loaded.
data <- read.csv("nyc_census_tracts.csv")
#Base libraries
library(dplyr)
library(ggplot2)
library(caret)

########################################################################################
#Preliminary investigation of the data
str(data)
head(data,20)

#Too many unnecessary variables. Picking out the ones that are of interest.
data1 <- data
#data1 <- data %>% 
 # select(CensusTract,County,Borough,TotalPop,Drive,Carpool,Transit,Walk,Income)

#Checking to make sure that the cleaned set is as anticipated.
head(data1,20)
str(data1)


summary(data1[,5:9])
#data1$IncomeBracket <- cut(data1$Income, c(0,25000,50000,75000,100000,150000))

#ggplot(data1,aes(Income)) + geom_histogram()
#data1
#library(plyr)

#levels(data1$IncomeBracket)<- c("1","2","3","4","5")

#Cleaning dataset
#data1$IncomeBracket
data1
summary(data1[,-1])
head(data1)
data2<-data1[complete.cases(data1),]
head(data2)

#Combing Drive and Carpool variables into one
data2$DriveOrCarpool=data2$Drive + data2$Carpool
head(data2)

#Defining average income
aveInc <- 57782
data2$aboveAveInc <- data2$Income > aveInc
head(data2)

#Remove identificaiton number
data3 <- data2[,-1]
#Remove columns Drive and Carpool 
data3 <- subset(data3, select = -c(Drive, Carpool))
#Subset numeric data
numericData <- data3[sapply(data3, is.numeric)]
#Create correlation matrix
corrMatrix <- cor(numericData)
#Find highly correlated variables
highlyCorrelated <- findCorrelation(corrMatrix, cutoff=0.8)
#Remove highly correlated variables
data4 <- subset(data3, select = -c(TotalPop, Men, Women,IncomePerCap, Poverty,Professional, Employed, PublicWork))

#Separate into training and test sets
set.seed(100)
trainingRowIndex <- sample(1:nrow(data4), 0.7*nrow(data4))
trainingData <- data4[trainingRowIndex,]
testData <- data4[-trainingRowIndex,]

#Build linear model with all variables
library(MASS)
fit <- lm(DriveOrCarpool ~ factor(County) + factor(Borough) + Hispanic + White + Black + Native + Asian + Citizen + Income + IncomeErr + IncomePerCapErr + ChildPoverty + Service + Office + Construction + Production + WorkAtHome + PrivateWork + SelfEmployed + FamilyWork + Unemployment + factor(aboveAveInc), data = trainingData)
#Conduct stepwise regression with linear model
step <- stepAIC(fit, direction = "both")
#View simplified linear model
step$anova

#Create linear model using indendent variables given by stepwise regression on mean commute model
fit1 <- lm(MeanCommute ~ factor(County) + Hispanic + White + Black + Asian + 
             Citizen + IncomePerCapErr + ChildPoverty + Service + Construction + 
             Production + PrivateWork + SelfEmployed + Unemployment + 
             factor(aboveAveInc), data = data4)

#Create linear model using independent variables given my stepwise regression on driveorcarpool model
fit2 <- lm(DriveOrCarpool ~ factor(County) + White + Black + Asian + Citizen + 
             Income + IncomeErr + IncomePerCapErr + ChildPoverty + Office + 
             Construction + Production + WorkAtHome + PrivateWork + SelfEmployed + 
             factor(aboveAveInc), data = data4)

#summarize models
summary(fit1)
summary(fit2)

#test models
pred1 <- predict(fit1, testData)
pred2 <- predict(fit2, testData)

#combine actual values and predicted values for mean commute
actuals_preds1 <- data.frame(cbind(actuals=testData$MeanCommute, predicteds=pred1))  
#Find correlation accuracy for mean commute
correlation_accuracy1 <- cor(actuals_preds1)
correlation_accuracy1

#Find min max accuracy for mean commute
min_max_accuracy1 <- mean(apply(actuals_preds1, 1, min) / apply(actuals_preds1, 1, max)) 
min_max_accuracy1

#Find mean absolute percentage error for mean commute predictions
mape1 <- mean(abs((actuals_preds1$predicteds - actuals_preds1$actuals))/actuals_preds1$actuals)  
mape1

#Find most important variables that impact mean commute model
varImp(fit1, scale = FALSE)

#Combine actual and predicted values for drive or carpool model
actuals_preds2 <- data.frame(cbind(actuals=testData$DriveOrCarpool, predicteds=pred2))  
#Find correlation accuracy for drive or carpool model
correlation_accuracy2 <- cor(actuals_preds2)
correlation_accuracy2
#Find min max accuracy for drive or carpool model
min_max_accuracy2 <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max)) 
min_max_accuracy2

#Find mean absolute percentage error for drive carpool model
mape2 <- mean(abs((actuals_preds2$predicteds - actuals_preds2$actuals))/actuals_preds2$actuals)  
mape2

#Find most important variables that impact drive carpool model
varImp(fit2, scale = FALSE)
