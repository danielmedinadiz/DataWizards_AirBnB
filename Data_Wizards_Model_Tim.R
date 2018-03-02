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
#install.packages("ranger")
#install.packages("jsonlite")

# Libraries
library(rstudioapi)
library(DMwR)
library(tm)
library(tidytext)
library(tidyr)
library(data.table)
library(ranger)
library(jsonlite)

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
df <- all.data


##############################
# Date normalization
##############################
date.columns <- c("first_review", "last_review", "host_since")
for (c in date.columns) {
  df[, c] <- as.Date(df[, c])
}

# Now convert to numeric and normalize
# This time use the same min and max for all three
all.dates <- as.numeric(rbind(df$first_review, df$last_review, df$host_since))
date.min <- min(all.dates, na.rm=TRUE)
date.max <- max(all.dates, na.rm=TRUE)
for (c in date.columns) {
  df[, c] <- as.numeric(df[, c])
  df[, c] <- (df[, c] - date.min)/(date.max - date.min)
}


##############################
# Numeric normalization
##############################

# Remove percent from response rate
df$host_response_rate <- sub("%", "", df$host_response_rate)

# Make numeric columns numeric and normalize
numeric.columns <- c("accommodates", "bathrooms", "host_response_rate", "number_of_reviews", "review_scores_rating", "bedrooms", "beds")
df <- normalize.columns(df, numeric.columns)


##############################
# Sentiment analysis
##############################

# Make a corpus out of the description
description.corpus <- SimpleCorpus(VectorSource(df$description), control=list(language="en"))

sentiment.choice <- "weighted.average"

####################
# Weighted average
####################
if (sentiment.choice == "weighted.average") {
  description.dtm <- DocumentTermMatrix(description.corpus, control=list(removePunctuation=TRUE, stopwords=TRUE))
  description.tidy <- tidy(description.dtm)
  description.tidy$word <- description.tidy$term
  description.sentiment <- as.data.table(merge(description.tidy, get_sentiments("afinn"), by="word"))
  description.sentiment$sentiment <- description.sentiment$score * description.sentiment$count
  description.sentiment.scores <- description.sentiment[order(document), list(sum_sentiments=sum(sentiment, na.rm=TRUE), total_count=sum(count, na.rm=TRUE)), by=document]
  description.sentiment.scores$score <- description.sentiment.scores$sum_sentiments/description.sentiment.scores$total_count
}

# vs

####################
# tf-idf weighting
####################
if (sentiment.choice == "tf.idf") {
  description.dtm <- DocumentTermMatrix(description.corpus, control=list(weighting=function(x) weightTfIdf(x, normalize=TRUE), stopwords=TRUE))
  description.tidy <- tidy(description.dtm)
  description.tidy$word <- description.tidy$term
  description.sentiment <- as.data.table(merge(description.tidy, get_sentiments("afinn"), by="word"))
  description.sentiment$sentiment <- (1 / description.sentiment$count) * description.sentiment$score
  description.sentiment.scores <- description.sentiment[order(document), list(score=median(sentiment, na.rm=TRUE)), by=document]
}

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

# Also store the word count
df$description_word_count <- 0
for (r in 1:nrow(df)) {
  df$description_word_count[r] <- lengths(strsplit(as.character(df$description[r]), "\\s+"))
}

# Normalize word count
df <- normalize.columns(df, c("description_word_count"))

# Scaled sentiment
df$description_scaled_sentiment <- df$description_sentiment * df$description_word_count

# Normalize sentiments
df <- normalize.columns(df, c("description_sentiment", "description_scaled_sentiment"))

# Plot
plot(df$description_sentiment, df$log_price)
plot(df$description_scaled_sentiment, df$log_price)


##############################
# Factor reduction
##############################

# "amenities" and "description" are large text values and slow down computation
# "latitude" and "longitude" don't make sense as factors, only to join
# "name", "thumbnail_url" also don't make sense as factors
remove.cols <- c("amenities", "description", "latitude", "longitude", "name", "thumbnail_url")
keep.cols <- setdiff(colnames(df), remove.cols)
df <- df[, keep.cols]


##############################
# NA removal
##############################

# Use k-nearest neighbor algorithm to impute NA values
# This takes a while so be patient (20 minutes or so)
# k equal to sqrt of the number of instances is a good rule of thumb
# When imputing, take out log price, id, derived columns, and factors with many levels
imputation.columns <- setdiff(colnames(df), c("tag", "id", "log_price", "description_sentiment", "description_word_count", "description_scaled_sentiment", "city", "neighbourhood", "zipcode"))

# Only use a subset as well to impute to save time
df[, imputation.columns] <- knnImputation(df[, imputation.columns], k=sqrt(nrow(df)), meth="median", distData=df[sample(nrow(df), 1000), imputation.columns])
#df[, imputation.columns] <- knnImputation(df[, imputation.columns], k=sqrt(nrow(df)), meth="median", distData=df[sample(nrow(df), floor(nrow(df) * 0.1)), imputation.columns])
#df[, imputation.columns] <- knnImputation(df[, imputation.columns], k=sqrt(nrow(df)), meth="median")

# Make sure the missing values are taken care of
sapply(df, function(x) sum(is.na(x)))


##############################
# Factor engineering
##############################

# How "stale" are reviews
df$review_stale <- df$last_review - df$first_review
df <- normalize.columns(df, c("review_stale"))

# Property type
for (r in 1:nrow(df)) {
  if (df$property_type[r] == "Apartment") {
    df$property_type[r] <- "Apartment"
  } else if (df$property_type[r] == "House") {
    df$property_type[r] <- "House"
  } else if (df$property_type[r] == "Condominium") {
    df$property_type[r] <- "Condominium"
  } else if (df$property_type[r] == "Townhouse") {
    df$property_type[r] <- "Townhouse"
  } else if (df$property_type[r] == "Loft") {
    df$property_type[r] <- "Loft"
  } else {
    df$property_type[r] <- "Other"
  }
}


##############################
# PCA
##############################

# "log_price", "accommodates", "bathrooms", "bedrooms", "beds" are highly correlated
df_sample <- df[sample(nrow(df), floor(0.3 * nrow(df))), ]
pairs(df_sample[c("log_price", "accommodates", "bathrooms", "bedrooms", "beds")], lower.panel = panel.smooth, upper.panel = panel.cor)

# Normalize using center and scale
prc.columns <- c("accommodates", "bathrooms", "bedrooms", "beds")
prc <- prcomp(df[prc.columns], center=TRUE, scale=TRUE)

# Variance analysis
summary(prc)

# Validate that the rows match up
stopifnot(nrow(prc$x) == nrow(df))

# Add principal components into df
df$pc1 <- prc$x[, "PC1"]
df$pc2 <- prc$x[, "PC2"]
df$pc3 <- prc$x[, "PC3"]
df$pc4 <- prc$x[, "PC4"]

# Save cleaned data to file to save computation time
write.csv(df, "clean_data.csv", row.names=FALSE)



############################################################
# Add zip code data
############################################################
all.data.clean <- read.csv("clean_data.csv")


##############################
# Lat lng in case zip is missing
##############################
# This would be the query, but I would need to register
# http://maps.googleapis.com/maps/api/geocode/json?latlng=40.6965236299707,-73.9916168462426&sensor=true
#latitudes <- all.data$latitude
#longitude <- all.data$longitude


##############################
# Add new factors
##############################
all.data.clean$age <- NA
all.data.clean$pop <- NA
all.data.clean$non_us_citizens <- NA
all.data.clean$mean_commute_minutes <- NA
all.data.clean$income <- NA
all.data.clean$owner_occupied_housing_units <- NA
all.data.clean$median_property_value <- NA
all.data.clean$pop_rank <- NA
all.data.clean$income_rank <- NA
all.data.clean$us_citizens <- NA
all.data.clean$non_eng_speakers_pct <- NA

# Set the data
cols.to.update <- c("age", "pop", "non_us_citizens", "mean_commute_minutes", "income", "owner_occupied_housing_units", "median_property_value", "pop_rank", "income_rank", "us_citizens", "non_eng_speakers_pct")

# Just in case we are rerunning the below loop after an error
# Don't want to pull in data for all US for blank zips!
all.data.clean[which(all.data.clean$zipcode == ""), cols.to.update] <- rep(NA, 11)

# Queries
for (r in 1:nrow(all.data.clean)) {
  if (is.na(all.data.clean$age[r])) {
    print(r/nrow(all.data.clean))
    zipcode <- all.data.clean$zipcode[r]
    data <- get.location.data(zipcode)
    all.data.clean[r, cols.to.update] <- data
  }
  if (r %% 10000 == 0) {
    write.csv(all.data.clean, "clean_data_2.csv", row.names=FALSE)
  }
}

# Write non-normalized
write.csv(all.data.clean, "clean_data_2.csv", row.names=FALSE)
all.data.clean <- read.csv("clean_data_2.csv")

# Write normalized
all.data.clean <- normalize.columns(all.data.clean, cols.to.update)
write.csv(all.data.clean, "clean_data_3.csv", row.names=FALSE)



############################################################
# Train vs validation vs test
############################################################
all.data.clean <- read.csv("clean_data_3.csv")
train.clean <- all.data.clean[which(all.data.clean$tag == "train"), ]
validation.clean <- all.data.clean[which(all.data.clean$tag == "validation"), ]
test.clean <- all.data.clean[which(all.data.clean$tag == "test"), ]



############################################################
# Model
############################################################

# Add in a random number column for variable importance
train.clean$rand <- sample(100, size=nrow(train.clean), replace=TRUE)/100
train.clean$rand1 <- sample(100, size=nrow(train.clean), replace=TRUE)/100
train.clean$rand2 <- sample(100, size=nrow(train.clean), replace=TRUE)/100


##############################
# Linear model
##############################
lm.cols <- setdiff(colnames(train.clean), c("id", "rand", "rand1", "rand2", "description_scaled_sentiment", "pc3", "pc4", "tag", "neighbourhood", "zipcode", "accommodates", "bathrooms", "bedrooms", "beds"))
fit <- lm(log_price ~ ., data=train.clean[, lm.cols])
summary(fit)

# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# Predictions
p <- predict.lm(fit, validation.clean)
validation.clean$log_price_predicted <- p
rsme(validation.clean$log_price, validation.clean$log_price_predicted)


##############################
# Random forest
##############################
n.trees <- 500

# First do all and see which vars are important
fit.all <- ranger(log_price ~ ., data=train.clean, num.trees=n.trees, importance="impurity")
var.imp <- sort(fit.all$variable.importance)
View(var.imp)

# In order of least to most important
rf.cols <- c("log_price", "description_scaled_sentiment", "number_of_reviews", "review_stale", "first_review", "description_word_count", "last_review", "host_since", "city", "neighbourhood", "pc3", "pc2", "pc4", "zipcode", "pc1", "room_type")
fit <- ranger(log_price ~ ., data=train.clean[, rf.cols], num.trees=n.trees, importance="impurity")

# Predictions
p <- predict(fit, validation.clean)
validation.clean$log_price_predicted <- p$predictions
rsme(validation.clean$log_price, validation.clean$log_price_predicted)

# Test
p <- predict(fit, test.clean)
test.clean$log_price_predicted <- p$predictions

# Submission
submission <- data.frame(id=test.clean$id, log_price=test.clean$log_price_predicted)
write.csv(submission, "submission.csv", row.names = FALSE)

