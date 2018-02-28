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
