##############################
# Initial setup
#
# Make sure to place the train.csv
# and test.csv from the competition
# into your workspace.
##############################

# This only works in RStudio
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

#Source helper methods
source("Data_Wizards_Functions.R")



##############################
#Import data
##############################

#All of the training data
all.train <- read.csv("train.csv")

#Reserve a portion of the training data for our own test set (to avoid overfitting)
splitResult <- split.train.test(all.train, perc.test=0.3, seed=42)
train <- splitResult$train
str(train)
test <- splitResult$test
str(test)
