###*** STAT 4220 - Section 1
# Group 1 - Factor Analysis

# Read in cleaned data file
set1 <- read.csv("Cleaned Data.csv")

#Required main packages
install.packages('psych')
library(psych)
install.packages("GPArotation")
library(GPArotation)
install.packages('nFactors')
library(nFactors)
install.packages('MVN')
library(MVN)
library("tidyverse")
install.packages("lavaan")
library("lavaan")
install.packages("corrplot")
library(corrplot)

################################################################################################################
# Start by exploring our data
head(set1)
str(set1)

# Five number summary, mean, n and missing obersvations:
x<-describe(set1)
a<-x[,c(2:5,8:9)]

fiv_num<-a%>%
  mutate(Q1=c(rep(1,5),2,rep(1,4)))%>%
  mutate(Q3=c(rep(3,5),4,rep(3,2),4,4))%>%
  mutate(missing_obs=rep(FALSE,10))

fiv_num

# Data visualization of spread of responses from each question 
set1%>%
  gather(key="question",value="measurement")%>%
  mutate(measurement=as.character(measurement))%>%
  ggplot(aes(x=factor(question,levels=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")),fill=measurement))+geom_histogram(stat="count")+
  coord_flip()+scale_fill_discrete(labels=c("Not at All Like Me","Slightly Like Me","Mainly Like Me","Very Much Like Me"))+ggtitle("Response Freuency by Question")+theme(plot.title = element_text(hjust = 0.5))+ylab("Count")+xlab("Question")

################################################################################################################
#Checking our assumptions:

#metric variables
  # this is true because our values are on a numeric scale (not concerned with gender, age, score)

# large sample size
nrow(set1)
  # a sample size of 3241 is sufficiently large for EFA (use >200 as rule of thumb)


# A correlation of at least 0.30 correlations are required
lowerCor(set1)
  
# no correlation is below .3
# Data visualization used in project
corr <- cor(set1)
corrplot(corr, method="number")


# Homogeneous sample: reliability analysis is conducted to test homogeneity. We can test this by looking at the Coefficient Alpha (Cronbach's Alpha) and the Average Split-Half Reliability of our data. This tests the internal consistency, or reliability, of our data. 
splitHalf(set1)
  # The above output shows us that the alpha (Guttman lambda 3) and the Average split half reliability both equal 0.9 
  # Since both measures of realibility are greater than 0.8, we can conclude the data is homogenous

# No outliers
boxplot.stats(set1$score)$out
  # no outliers were found in the score variable


################################################################################################################
# Preparing/Splitting the data set:

#Create 2 sets of indices to split the dataset. These indices will split 50% of data set for our Exploratory Factor Analysis and the other 50% for out Confirmatory Factor Analysis 
N <- nrow(set1)
indices <- seq(1, N)

indices_EFA <- sample(indices, floor((.5*N)))
indices_CFA <- indices[!(indices %in% indices_EFA)]

# Implementing indices to split the dataset into  EFA and CFA sets
set_EFA <- set1[indices_EFA, ]
set_CFA <- set1[indices_CFA, ]

head(set_EFA)
head(set_CFA)

# Compare split sets to make sure they are similar:
# create a grouping variable
group_var<-vector("numeric",nrow(set1))
group_var[indices_EFA] <- 1
group_var[indices_CFA] <- 2

#bind it to original data set. This helps us calculate key summary statistics and between-group statistics.
set_grouped<-cbind(set1,group_var)

# Key summary statistics/between-group statistics
describeBy(set_grouped, group = group_var)
statsBy(set_grouped, group = "group_var")

################################################################################################################

#Exploratory Factor Analysis:

# We first need to quantify the number of hidden factors present in our dataset. We will do this by calculating the eigenvalues. Eigenvalues are numeric indicators of the amount of variance explained by each factor or component. 
# To calculate eigenvalues, we need to first create a correlation matrix with our EFA set.
set_EFA_cor <- cor(set_EFA, use = "pairwise.complete.obs")
set_EFA_cor

# Use the correlation matrix to calculate the Eigenvalues. Typically, Eigenvalues greater than 1 indicate relevant factors.  
eigenvals <- eigen(set_EFA_cor)
eigenvals$values

# Can also visualize these eigenvalues with a Scree plot
scree(set_EFA_cor, factors = FALSE)

# We can see in both the eigenvalues output and the Scree Plot that there is only one hidden factor. 


# Run EFA with 1 factor
EFA_model <- fa(set_EFA, nfactors = 1)
EFA_model

# Look at the factor loadings that show how each question (item) relates to the underlying factor
EFA_model$loadings

# Look at the individual persons factor score for the one underlying factor.
head(EFA_model$scores)
range(EFA_model$scores)

# Investigation of model fit

#Absolute model fit: Absolute fit statistics (Chi-square test, Tucker-Lewis Index, Root Mean Square Error of Approximation) are helpful for deciding whether or not a model fits adequately. These statistics have intrinsic significance (can be useful in isolation) and suggested cutoff values to use as guidelines. All fit statistics try to represent the discrepancy between our observed data and the expected given our model. 
# All of these fit statistics can be found in our EFA model
EFA_model

# Chi-square test: 
  # We typically want a non-significant result from our chi-sqaure test (obeserved and expected model are not significantly different) but often times, with larger datasets, we obtain significant results. We see that 425.28 is sginifcantly different from 0. 


# Tucker Lewis Index (TLI): 
  # A TLI greater than .9 indicates our model has a good fit. With our TLI below the cutoff, this may indicate that our model is not an adequate fit.

# Root Mean Square Error of Approximation (RMSEA): 
  # This quantifies the difference between the oberved and expected data. A value less than .05 indicates our model has a good fit. With out RMSEA above the the cutoff, this may indicate that our model is not an adequate fit.



# Relative Model Fit: compares the fit between multiple different models using the Bayesian Information Criterion (BIC). The lower the BIC, the better the reletive fit. Based on the eigenvalues being close to the cutoff of 1 (confirmed in Scree Plot), the 2 and 3 factor models will be compared using the reletive fit statistics BIC. 

EFA_model_1 <- fa(set_EFA, nfactors = 1)
EFA_model_2 <- fa(set_EFA, nfactors = 2)
EFA_model_3 <- fa(set_EFA, nfactors = 3)

EFA_model_1$BIC 
EFA_model_2$BIC 
EFA_model_3$BIC

# We can see that based on the BIC's from the three different factor models, the model with 3 factors has the lowest BIC and therefore, reletively, the most adequate model. 