clean.data <- function(df) {
  
  
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
  # When imputing, take out log price and id and derived columns
  imputation.columns <- setdiff(colnames(df), c("id", "log_price", "description_sentiment", "description_word_count", "description_scaled_sentiment"))
  df[, imputation.columns] <- knnImputation(df[, imputation.columns], k=sqrt(nrow(df)), meth="median")
  
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
  
  # Now remove the correlated columns
  df <- df[, setdiff(colnames(df), prc.columns)]
  
  return(df)
}

split.train.validation <- function(df, perc.validation, seed) {
  
  #Determine the size of the training and validation set
  all.size <- nrow(df)
  validation.size <- floor(all.size * perc.validation)
  train.size <- all.size - validation.size
  
  #Randomly sample training and validation set
  all.rows <- 1:all.size
  set.seed(seed) #Set the seed for consist results
  validation.rows <- sample.int(all.size, validation.size, replace=FALSE)
  train.rows <- setdiff(all.rows, validation.rows)
  
  #Throw an error if the length of the validation set + length of the train set !=  length of the df
  stopifnot(length(validation.rows) + length(train.rows) == length(all.rows))
  
  #Spit the data into train and validation
  validation <- df[validation.rows, ]
  train <- df[train.rows, ]
  
  #Return
  return(list("validation" = validation, "train" = train))
}

rsme <- function(p, o) {
  sqrt(mean((p - o)^2))
}

panel.cor <- function(x, y, digits = 2) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="na.or.complete"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  text(0.5, 0.5, txt, cex = r * 0.8/strwidth(txt))
}

normalize.columns <- function(df, columns) {
  for (c in columns) {
    df[, c] <- as.numeric(df[, c])
    c.min <- min(df[, c], na.rm=TRUE)
    c.max <- max(df[, c], na.rm=TRUE)
    df[, c] <- (df[, c] - c.min)/(c.max - c.min)
  }
  return(df)
}
