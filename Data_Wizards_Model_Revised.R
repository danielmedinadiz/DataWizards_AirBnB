##############################
#Initial setup
#
#Make sure to place the train.csv
#and test.csv from the competition
#into your workspace.
##############################

#This only works in RStudio
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

#Source helper methods
source("Data_Wizards_Functions.R")

#Install libraries
#Run this the first time and edit line 142 from Sys.sleep(0.5) to Sys.sleep(3)
#Select no when prompted to restart R
#install.deps()

#Libraries
library(dplyr)
library(tidyverse)
library(VIM)
library(RColorBrewer)
library(viridis)
library(DT)
library(magrittr)
library(scales)
library(ggstance)
library(mice)
library(stringr)
library(mice)
library(party)
library(caret)
library(ROCR)
library(e1071)
library(randomForest)
library(doParallel)
library(iterators)
library(parallel)
library(adabag)
library(ggstance)
library(wakefield)
library(Amelia)



##############################
#Import data
##############################
train <- read.csv("train.csv")
str(train)
test <- read.csv("test.csv")
str(test)

#Include price column to test
test$log_price <- 0
str(test)

#Adding a Tag to each data set
train$tag <- "Train"
test$tag <- "Test"

#Validate new column named "Tag" in the 2 datasets
str(train)
str(test)

#create a new data set with train and test
all_data <- rbind(test, train)
str(all_data)

AD<-train
str(AD)



##############################
#Data Exploration
##############################

#Identify missing Values in all_data
#Count of Missing values by variable
sapply(AD, function(x) sum(is.na(x)))

#Total Missing Values
table(is.na(AD))

#Visualize missing Values
missmap(AD, rank.order = TRUE)

#summary of variables
summary(AD)

###Target Variable Analysis

#Take the column "log_price" from the "train" dataset vizualize the data
#Visualize all data points
plot(AD$log_price, type='p')

#Box-Plot
boxplot(AD$log_price)

#Histogram
qplot(AD$log_price, geom="histogram", binwidth=0.1)

#Normalize Price
AD$log_price_norm<-(AD$log_price - mean(AD$log_price)) / sd(AD$log_price)

#Visualize normalized price
qplot(AD$log_price_norm, geom="histogram", binwidth=0.1)  

summary(AD$log_price_norm)

#AD Structure and creating a new data set
str(AD)

newAD <- AD[c(2,3,4,6,7,8,10,11,14,15,18,24,28,29)]
str(newAD)

# Multiple Linear Regression Example 
fit <- lm(log_price ~ ., data=newAD)
# show results
summary(fit) 
coefficients(fit) 
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

#Test the model
test_AD<-test
str(test_AD)
newtestAD <- test_AD[c(2,3,5,6,7,9,10,13,14,15,17,23,27,28,29)]
str(newtestAD)

#Predict
p <- predict(fit, newtestAD)
summary(p)


#Visualize normalized price
qplot(p, geom="histogram", binwidth=0.1)


#General Commands
pkg.list = available.packages()
download.packages(pkgs = pkg.list, destdir = "C:/Users/dmedina/Downloads/R-Packages")
ls()
rm(g1,g2)