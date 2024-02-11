{
  # Helper packages
  library(dplyr)     
  library(ggplot2)   
  
  # Modeling process packages
  library(rsample)   
  library(caret)     
  library(gbm)      
  library(h2o)      
  library(xgboost)  

  #Downsampling
  library(tidyverse)
  SI_SL <- read_csv("SI_SL.csv") |>
    filter(!is.na(Chase)) |>
    rename(`Vertical Location` = plate_z,
           `Horizontal Location` = plate_x,
           `Release Spin Rate` = release_spin_rate,
           `Previous Pitch Vertical Location` = prev_plate_z,
           `Previous Pitch Horizontal Location` = prev_plate_x,
           `Vertical Break` = pfx_z,
           `Horizontal Break` = pfx_x,
           `Speed Difference` = speeddiff,
           `Release Speed` = release_speed,
           `Break To Tunnel Ratio` = breaktotunnelratio)
  data <- groupdata2::downsample(SI_SL, "Chase")

  #Training/Testing Splits
  data <- data |> mutate_if(is.ordered, factor, ordered = FALSE)
  data <- data |>
    select(Chase, everything())
  
  data <- data[,c(-3, -4, -14, -15)]
  
  set.seed(123)
  split <- initial_split(data, prop = 0.7, 
                         strata = "Chase")
  data_train  <- training(split)
  data_test   <- testing(split)

  #recipe
  library(recipes)
  xgb_prep <- recipe(Chase ~ ., data = data_train) |>
    step_integer(all_nominal()) |>
    prep(training = data_train, retain = TRUE) |>
    juice()
  
  X <- as.matrix(xgb_prep[setdiff(names(xgb_prep), "Chase")])
  Y <- xgb_prep$Chase

  # optimal parameter list
  params <- list(
    eta = 0.01,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.5,
    colsample_bytree = 0.5
  )
  
  # train final model
  xgb.fit.final <- xgboost(
    params = params,
    data = X,
    label = Y,
    nrounds = 500,
    objective = "binary:logistic",
    verbose = 0
  )

  # confusion matrix 
  
  xgb_test <- recipe(Chase ~ ., data = data_test) |>
    step_integer(all_nominal()) |>
    prep(training = data_test, retain = TRUE) |>
    juice()
  
  test <- as.matrix(xgb_test[setdiff(names(xgb_test), "Chase")])
  
  pred <- predict(xgb.fit.final, newdata = test)
  
  pred <-  as.numeric(pred > 0.5)
  
  confusionMatrix(factor(pred),factor(data_test$Chase))
}

{
  # variable importance plot
  vip::vip(xgb.fit.final, num_features = 20)
}
