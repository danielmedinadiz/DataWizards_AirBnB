split.train.test <- function(df, perc.test, seed) {
  
  #Determine the size of the training and test set
  all.size <- nrow(df)
  test.size <- floor(all.size * perc.test)
  train.size <- all.size - test.size
  
  #Randomly sample training and test set
  all.rows <- 1:all.size
  set.seed(seed) #Set the seed for consist results
  test.rows <- sample.int(all.size, test.size, replace=FALSE)
  train.rows <- setdiff(all.rows, test.rows)
  
  #Throw an error if the length of the test set + length of the train set !=  length of the df
  stopifnot(length(test.rows) + length(train.rows) == length(all.rows))
  
  #Spit the data into train and test
  test <- df[test.rows, ]
  train <- df[train.rows, ]
  
  #Return
  return(list("test" = test, "train" = train))
}

install.deps <- function() {
  
  #Edit line 142 from Sys.sleep(0.5) to Sys.sleep(3)
  trace(utils:::unpackPkgZip, edit=TRUE)
  install.packages("dplyr")
  install.packages("tidyverse")
  install.packages("VIM")
  install.packages("RColorBrewer")
  install.packages("viridis")
  install.packages("DT")
  install.packages("magrittr")
  install.packages("scales")
  install.packages("ggstance")
  install.packages("mice")
  install.packages("stringr")
  install.packages("mice")
  install.packages("party")
  install.packages("caret")
  install.packages("ROCR")
  install.packages("e1071")
  install.packages("randomForest")
  install.packages("doParallel")
  install.packages("iterators")
  install.packages("parallel")
  install.packages("adabag")
  install.packages("ggstance")
  install.packages("wakefield")
  install.packages("Amelia")
}