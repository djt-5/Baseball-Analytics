library(rsample)
library(klaR)
library(e1071) 
library(naivebayes)
library(caret)
library(dplyr)
library(tidyverse)

#Load Data 
data <- read_csv("Baserunning-Data.csv") 
data <- data |>
  dplyr::select(-delta_run_exp, -exchange_time)

#Downsample 
data <- groupdata2::downsample(data, "safe")

#Train/Test 
set.seed(6102024)
split <- initial_split(data, prop = 0.7) #Split data
data_train  <- training(split)
data_test   <- testing(split)

predictors_train <- data_train[,-1]
predictors_test <- data_test[,-1]

#Create Model
model <- train(predictors_train,
               factor(data_train$safe),
               'nb',
               trControl = trainControl(method = 'cv', number = 10))

#Testing Accuracy 
pred <- predict(model, newdata = data_test)
confusionMatrix(factor(pred),factor(data_test$safe))

#Variable Importance
plot(varImp(model))

#Save model
saveRDS(model, "naivebayes.RDS")
