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

# Split train.all into train and validation
split.results <- split.train.validation(train.all, perc.validation=0.3, seed=42)
train <- split.results$train
validation <- split.results$validation



############################################################
# Data cleaning
############################################################
df <- train


##############################
# Sentiment analysis
##############################

# Get the sentiment score for each description
# Lots of boiler plate here
# Idea is to sum up the afinn sentiment of each word in a description
description.corpus <- SimpleCorpus(VectorSource(df$description), control=list(language="en"))
description.dtm <- DocumentTermMatrix(description.corpus, control=list(removePunctuation=TRUE, stopwords=TRUE))
#description.dtm <- DocumentTermMatrix(description.corpus, control=list(weighting=function(x) weightTfIdf(x, normalize=TRUE), stopwords=TRUE))
description.tidy <- tidy(description.dtm)
description.tidy$word <- description.tidy$term

####################
# afinn analysis
####################
description.sentiment <- as.data.table(merge(description.tidy, get_sentiments("afinn"), by="word"))
description.sentiment$sentiment <- description.sentiment$count * description.sentiment$score
#description.sentiment$sentiment <- (1 / description.sentiment$count) * description.sentiment$score
#description.sentiment.scores <- description.sentiment[order(document), list(score=sum(sentiment, na.rm=TRUE)), by=document]
#description.sentiment.scores <- description.sentiment[order(document), list(score=sum(score, na.rm=TRUE)), by=document]
description.sentiment.scores <- description.sentiment[order(document), list(sum_sentiments=sum(sentiment, na.rm=TRUE), total_count=sum(count, na.rm=TRUE)), by=document]
#description.sentiment.scores$score <- description.sentiment.scores$sum_sentiments/description.sentiment.scores$total_count
description.sentiment.scores$score <- description.sentiment.scores$total_count

####################
# bing analysis
####################
description.sentiment <- as.data.table(merge(description.tidy, get_sentiments("bing"), by="word"))
description.sentiment$score <- 0
description.sentiment$score[which(description.sentiment$sentiment == "positive")] <- 1
description.sentiment$score[which(description.sentiment$sentiment == "negative")] <- -1
description.sentiment$sentiment <- (1 / description.sentiment$count) * description.sentiment$score
description.sentiment.scores <- description.sentiment[order(document), list(score=mean(sentiment, na.rm=TRUE)), by=document]

####################
# nrc analysis
####################
description.sentiment <- as.data.table(merge(description.tidy, get_sentiments("nrc"), by="word"))

description.sentiment$anger <- 0
description.sentiment$anger[which(description.sentiment$sentiment == "anger")] <- 1

description.sentiment$anticipation <- 0
description.sentiment$anticipation[which(description.sentiment$sentiment == "anticipation")] <- 1

description.sentiment$disgust <- 0
description.sentiment$disgust[which(description.sentiment$sentiment == "disgust")] <- 1

description.sentiment$fear <- 0
description.sentiment$fear[which(description.sentiment$sentiment == "fear")] <- 1

description.sentiment$joy <- 0
description.sentiment$joy[which(description.sentiment$sentiment == "joy")] <- 1

description.sentiment$negative <- 0
description.sentiment$negative[which(description.sentiment$sentiment == "negative")] <- 1

description.sentiment$positive <- 0
description.sentiment$positive[which(description.sentiment$sentiment == "positive")] <- 1

description.sentiment$sadness <- 0
description.sentiment$sadness[which(description.sentiment$sentiment == "sadness")] <- 1

description.sentiment$surprise <- 0
description.sentiment$surprise[which(description.sentiment$sentiment == "surprise")] <- 1

description.sentiment$trust <- 0
description.sentiment$trust[which(description.sentiment$sentiment == "trust")] <- 1

description.sentiment.scores <- description.sentiment[order(document), list(score=max(positive, na.rm=TRUE)), by=document]



####################
# Merge
####################

# Prepare to merge sentiment score back into df
document <- 1:nrow(df)
description.sentiment.merge <- merge(x=data.frame(document), y=description.sentiment.scores, by="document", all.x=TRUE)

# Where there were no matching words, give a neutral score
description.sentiment.merge[is.na(description.sentiment.merge$score), "score"] <- 0

# Validate that the rows match up
stopifnot(nrow(description.sentiment.merge) == nrow(df))

# Merge with df
df$description_sentiment <- description.sentiment.merge$score

# Plot
plot(df$description_sentiment, df$log_price)


##############################
# Predictor reduction
##############################

# See which predictors are highly correlated
df_sample <- df[sample(nrow(df), 5000), ]
pairs(df_sample[c("log_price", "property_type", "room_type", "city", "neighbourhood", "accommodates", "bathrooms", "number_of_reviews", "review_scores_rating", "bedrooms", "beds", "description_sentiment")], lower.panel = panel.smooth, upper.panel = panel.cor)
# This includes beds, bedrooms, bathrooms, and accomodates
# Since accomodates has the highest correlation to log_price, use only accomodates


keep.cols <- c("log_price", "property_type", "room_type", "accommodates", "number_of_reviews", "review_scores_rating", )

# See what the missing values are
sapply(df, function(x) sum(is.na(x)))


df$review_scores_rating[is.na(df$review_scores_rating)] <- summary(df$review_scores_rating)["Median"]
sapply(df, function(x) sum(is.na(x)))


# Use k-nearest neighbor algorithm to impute NA values
# This takes a while so be patient
# k equal to sqrt of the number of instances is a good rule of thumb
df <- knnImputation(df, k=sqrt(nrow(df)), meth="median")

# Make sure the missing values are taken care of
sapply(df, function(x) sum(is.na(x)))

# Clean up review scores
#df$review_scores_rating[is.na(df$review_scores_rating)] <- summary(df$review_scores_rating)["Median"]
#sapply(df, function(x) sum(is.na(x)))



# Linear model
cols <- c("number_of_reviews", "review_scores_rating", "beds", "log_price")
fit <- lm(log_price ~ ., data=train[cols])
summary(fit) 

#Predict
p <- predict(fit, validation[cols[1:(length(cols)-1)]])
validation$log_price_predicted <- p
rsme(validation$log_price, p)
summary(p)






##############################
#Data cleaning
##############################

#Eventually will need to do this in a function
df <- train

# Missing vals
sapply(df, function(x) sum(is.na(x)))





# Normalize log price
log.price.mean <- mean(df$log_price)
log.price.sd <- sd(df$log_price)

df$log_price_norm <- (df$log_price - log.price.mean) / log.price.sd
qplot(df$log_price_norm, geom="histogram", binwidth=0.1)

# How to convert back to log price
df$log_price_reversed <- df$log_price_norm * log.price.sd + log.price.mean



# Linear model
cols <- c("number_of_reviews", "review_scores_rating", "beds", "log_price")
cols <- c("accommodates", "log_price")
cols <- c("description_sentiment", "log_price")
fit <- lm(log_price ~ ., data=df[cols])
summary(fit) 
coefficients(fit)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

#Predict
p <- predict(fit, test[cols[1:(length(cols)-1)]])
summary(p)
