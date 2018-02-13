#Importa data
train <- read.csv("C:/Users/dmedina/Desktop/ML Competition 2018 - AirBnb/AirBnB/train.csv")
str(train)

#Data Exploration
install.packages(ggplot2)
library(ggplot2)

#Identify missing Values



#Take the column "log_price" from the "train" dataset vizualize the data
#Visualize all data points
plot(train$log_price, type='p')

#Box-Plot
boxplot(train$log_price)

#Histogram
qplot(train$log_price, geom="histogram", binwidth=0.1)


#General Commands
ls()
rm(g1,g2)