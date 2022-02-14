# Final Project
# Team:
# Members: Michel Libbos, Elie Feghali

#Install Packages
install.packages("fastDummies")

#Import libraries

library(tidyverse)
library(glmnet)
library(adabag)
library(randomForest)
library(fastDummies)
library(leaps)


#Import Historic Housing Dataset
housing.df <- read.csv('historic_property_data.csv')

#Remove Columns containing high number of NA values
housing.df <- housing.df[,-c(7,11,25,26,30,31,32,38,39,40,41,42,49,50,52,54,57,58,63)]
sum(is.na(housing.df))
housing.df$meta_deed_type <- as.factor(housing.df$meta_deed_type)
str(housing.df)

#remove all NA values
housing.df <- na.omit(housing.df)
sum(is.na(housing.df))

#Converting categorical variables from numeric to factor
housing.df[,c(2,3,4,10:26,28:30,37,38,39,40)] <- lapply(housing.df[,c(2,3,4,10:26,28:30,37,38,39,40)], factor)
str(housing.df)



#dummy.housing.df <- dummy_cols(housing.df, select_columns = 'meta_deed_type', remove_first_dummy = TRUE, remove_selected_columns = TRUE)
#head(dummydf)



# set seed for reproducing the partition 
set.seed(1)
# row numbers of the training set 
train.index <- sample(c(1:dim(housing.df)[1]), 0.7*dim(housing.df)[1])
head(train.index)
# training set 
train.df <- housing.df[train.index,]
dim(train.df)
# test set 
test.df <- housing.df[-train.index,]
dim(test.df)

#model with all predictors
lm.full <- lm(sale_price ~ ., data = train.df)
lm.full
#backward elimination
lm.step.backward <- step(lm.full, direction = "backward")
summary(lm.step.backward) 
#predictions
lm.step.pred.backward <- predict(lm.step.backward, test.df)
head(lm.step.pred.backward)

# MSE in the test set 
mean((test.df$sale_price-lm.step.pred.backward)^2)


