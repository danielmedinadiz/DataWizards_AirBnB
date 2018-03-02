tag.train.validation <- function(df, perc.validation, seed) {
  
  # Determine the size of the training and validation set
  all.size <- nrow(df)
  validation.size <- floor(all.size * perc.validation)
  train.size <- all.size - validation.size
  
  # Randomly sample training and validation set
  all.rows <- 1:all.size
  set.seed(seed) #Set the seed for consistent results
  validation.rows <- sample.int(all.size, validation.size, replace=FALSE)
  train.rows <- setdiff(all.rows, validation.rows)
  
  # Throw an error if the length of the validation set + length of the train set !=  length of the df
  stopifnot(length(validation.rows) + length(train.rows) == length(all.rows))
  
  # Tag the data into train and validation
  df$tag <- "train"
  df$tag[validation.rows] <- "validation"
  
  # Return
  return(df)
}

split.train.validation <- function(df, perc.validation, seed) {
  
  # Determine the size of the training and validation set
  all.size <- nrow(df)
  validation.size <- floor(all.size * perc.validation)
  train.size <- all.size - validation.size
  
  # Randomly sample training and validation set
  all.rows <- 1:all.size
  set.seed(seed) #Set the seed for consistent results
  validation.rows <- sample.int(all.size, validation.size, replace=FALSE)
  train.rows <- setdiff(all.rows, validation.rows)
  
  # Throw an error if the length of the validation set + length of the train set !=  length of the df
  stopifnot(length(validation.rows) + length(train.rows) == length(all.rows))
  
  # Spit the data into train and validation
  validation <- df[validation.rows, ]
  train <- df[train.rows, ]
  
  # Return
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

get.location.data <- function(zipcode) {
  if (length(zipcode) == 0) {
    return(rep(NA, 11))
  }
  if (grepl("-", zipcode)) {
    zipcode <- strsplit(zipcode, "-")[[1]][1]
  }
  result = tryCatch({
    zipJson <- fromJSON(paste0("https://api.datausa.io/attrs/search/?q=", zipcode, "&kind=geo"))
    if (length(zipJson$data) > 0) {
      geo.id <- zipJson$data[1, 1]
      query <- "https://api.datausa.io/api/?show=geo&year=2015&required=age,pop,non_us_citizens,mean_commute_minutes,income,owner_occupied_housing_units,median_property_value,pop_rank,income_rank,us_citizens,non_eng_speakers_pct&sumlevel=all&geo="
      dataJson <- fromJSON(paste0(query, geo.id))
      return(dataJson$data[3:13])
    } else {
      return(rep(NA, 11))
    }
  }, warning = function(w) {
    return(rep(NA, 11))
  }, error = function(e) {
    return(rep(NA, 11))
  }, finally = {
    # Nothing
  })
  return(result)
}
