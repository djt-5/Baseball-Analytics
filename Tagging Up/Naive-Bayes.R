library(rsample)
library(klaR)
library(e1071) 
library(naivebayes)
library(caret)
library(tidyverse)

#Load Data 
Tags <- read_csv("Tags.csv") |>
  filter(!is.na(dist_from_base) & !is.na(safe))

#Upsample
data <- groupdata2::downsample(Tags, "safe")

#Training/Testing Split 
set.seed(6102024)
split <- initial_split(data, prop = 0.7, strata = "safe") #Split data
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
pred <- predict(model, newdata = predictors_test)
confusionMatrix(factor(pred),factor(data_test$safe))

#Variable Importance 
plot(varImp(model))

#Save Model RDS
saveRDS(model, file = "bayesian_model.rds")

#Remove Elements
#rm(data, data_train, data_test, predictors_train, predictors_test, split, Tags, pred)
