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
train.all <- read.csv("train.csv")
test <- read.csv("test.csv")

# Add log_price column to test
test$log_price <- -1

# Reorder properly
test <- test[, colnames(train.all)]
test$tag <- "test"

# Tag rows train.all as train or validation
train.all <- tag.train.validation(train.all, perc.validation=0.3, seed=42)

# All data
all.data <- rbind(train.all, test)



############################################################
# Data cleaning
############################################################
all.data.clean <- clean.data(all.data)
train.clean <- all.data.clean[which(all.data.clean$tag == "train"), ]
validation.clean <- all.data.clean[which(all.data.clean$tag == "validation"), ]
test.clean <- all.data.clean[which(all.data.clean$tag == "test"), ]



############################################################
# Model
############################################################

# Linear model
lm.cols <- setdiff(colnames(train.clean), c("id", "description_scaled_sentiment", "pc3", "pc4", "tag"))
fit <- lm(log_price ~ ., data=train.clean[, lm.cols])
#fit <- lm(log_price ~ ., data=train.clean)
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
