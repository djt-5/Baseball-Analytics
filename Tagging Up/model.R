library(rsample)
library(caret)
library(corrplot)
library(tidyverse)

#Load Data
data <- read_csv("Bases.csv")

#Downsample 
data <- groupdata2::upsample(data, "safe")

#Train/Test
set.seed(6102024)
split <- initial_split(data, prop = 0.7, strata = "safe") 
data_train  <- training(split)
data_test   <- testing(split)

#Model 
model <- glm(safe ~ 0 + ., data = data_train)
summary(model)

#Prediction Accuracy
pred <- predict(model, newdata = data_test) 
pred <-  as.numeric(pred > 0.5) 
confusionMatrix(factor(pred),factor(data_test$safe)) 

#Variable Importance

vip::vip(model, num_features = 3)

