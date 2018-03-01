############################################################
# Initial setup
#
# Make sure to place the train.csv and test.csv from the 
# competition into your workspace. Need R v3.4.3.
############################################################

# Run this the first time and edit line 142 from Sys.sleep(0.5) to Sys.sleep(3) to install libraries
# Select yes to restart R
#trace(utils:::unpackPkgZip, edit=TRUE)

# Install libraries
#install.packages("rstudioapi")
#install.packages("DMwR")
#install.packages("tm")
#install.packages("tidytext")
#install.packages("tidyr")
#install.packages("data.table")

# Libraries
library(rstudioapi)
library(DMwR)
library(tm)
library(tidytext)
library(tidyr)
library(data.table)

#This only works in RStudio
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

#Source helper methods
source("Data_Wizards_Functions.R")



############################################################
# Import data
############################################################
train.all <- read.csv("train.csv")[1:5000,]
test <- read.csv("test.csv")[1:5000,]

# Add log_price column to test
test$log_price <- -1

# Reorder properly
test <- test[, colnames(train.all)]

# Split train.all into train and validation
split.results <- split.train.validation(train.all, perc.validation=0.3, seed=42)
train <- split.results$train
validation <- split.results$validation



############################################################
# Data cleaning
############################################################
train.clean <- clean.data(train)
validation.clean <- clean.data(validation)
test.clean <- clean.data(test)

# Set levels - TODO
for (c in colnames(train.clean)) {
  if (is.factor(train.clean[, c])) {
    train.levels <- levels(train.clean[, c])
    validation.levels <- levels(validation.clean[, c])
    test.levels <- levels(test.clean[, c])
    levels(validation.clean[, c])[setdiff(validation.levels, train.levels)] <- NA
    levels(test.clean[, c])[setdiff(test.levels, train.levels)]  <- NA
  }
}



############################################################
# Model
############################################################

# Linear model
#lm.cols <- setdiff(colnames(df), c("id", "description_scaled_sentiment", "pc3", "pc4"))
#fit <- lm(log_price ~ ., data=df[, lm.cols])
fit <- lm(log_price ~ ., data=train.clean)
summary(fit)

# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)



############################################################
# Prediction
############################################################

# TODO
p <- predict.lm(fit, validation.clean)
validation$log_price_predicted <- p
rsme(validation$log_price, p)
summary(p)
