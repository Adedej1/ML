getwd()
# load the data on R
air_data <- read.csv("C:/Users/VALTECH  COMPUTERS/Documents/updated_pollution_dataset.csv")
View(air_data)
# check the structure of the data
str(air_data)
# check the summary statistics of the data
summary(air_data)
table(air_data$Air.Quality)
# check the proportion of the air quality column
round(prop.table(table(air_data$Air.Quality))*100, digits = 1)

air_data$Air.Quality<- factor(air_data$Air.Quality, labels = c("Good", "Moderate", "Poor", "Hazardous"))
str(air_data)
# normalize the dataset using the max_min normalization
air_data_n <- as.data.frame(lapply(air_data[1:9], normalize))
str(air_data_n)
summary(air_data_n)
# create a train dataset
air_data_train <- air_data_n[1:4000,]
View(air_data_train)
# creat a test dataset
air_data_test <- air_data_n[4001:5000,]
View(air_data_test)
# extract the air quality label for the train dataset
air_data_train_label <- air_data[1:4000, 10]
air_data_train_label
# extract the air quality label for the test dataset
air_data_test_label<- air_data[4001:5000,10]
air_data_test_label
# load the class library
library(class)
# find k which is the squareroot of the train dataset
sqrt(4000)
k <- 69
# use the knn function from the class library to perform the classification
air_data_test_pred <- knn(
  train = air_data_train,
  test = air_data_test,
  cl = air_data_train_label,
  k = k
)

library(gmodels)

CrossTable(
  x= air_data_test_label,
  y= air_data_test_pred,
  prop.chisq = F
)
# evaluate the model
((411+69+295+151)/1000)*100

## ______________________________________________________________________
# using z_score Standardization 
air_data_z <- as.data.frame(scale(air_data[-10])) 
View(air_data_z)
air_data_train <- air_data_z[1:4000,]
View(air_data_train)
# creat a test dataset
air_data_test <- air_data_z[4001:5000,]
View(air_data_test)
# extract the air quality label for the train dataset
air_data_train_label <- air_data[1:4000, 10]
air_data_train_label
# extract the air quality label for the test dataset
air_data_test_label<- air_data[4001:5000,10]
air_data_test_label
# load the class library
library(class)
# find k which is the squareroot of the train dataset
sqrt(4000)
k <- 61
# use the knn function from the class library to perform the classification
air_data_test_pred <- knn(
  train = air_data_train,
  test = air_data_test,
  cl = air_data_train_label,
  k = k
)
CrossTable(
  x= air_data_test_label,
  y= air_data_test_pred,
  prop.chisq = F
)
((411+62+291+147)/1000)*100
