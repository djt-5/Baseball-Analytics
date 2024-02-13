{
  # Helper packages
  library(tidyverse)     # for data wrangling
  library(rsample)   # for data splitting
  
  # Modeling packages
  library(caret)     # for logistic regression modeling
  
  # Model interpretability packages
  library(vip)       # variable importance
  
  Data <- read_csv("Data.csv") |>
    mutate(safe = ifelse(safe == 1, "Yes", "No"))
  
  Data <- Data |>
    mutate_if(is.ordered, factor, ordered = FALSE)
  
  # Create training (70%) and test (30%) sets for the 
  # rsample::attrition data.
  set.seed(123)  # for reproducibility
  split <- initial_split(Data, prop = .7, strata = "safe")
  data_train <- training(split)
  data_test  <- testing(split)
  
  set.seed(123)
  cv_model <- train(
    safe ~ ., 
    data = data_train, 
    method = "glm",
    family = "binomial",
    trControl = trainControl(method = "cv", number = 10)
  )
  
  pred_class <- predict(cv_model, data_train)
  
  confusionMatrix(
    data = relevel(pred_class, ref = "Yes"), 
    reference = relevel(factor(data_train$safe), ref = "Yes")
  )
}

{
  vip(cv_model, num_features = 3)
}
