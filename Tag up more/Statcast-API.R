#Code to update 2023 Statcast data ####
{
  library(tidyverse)
  library(bbd)
  previous <- read_csv("Data2023.csv")
  add_on <- statcast(
    start = max(previous$game_date) + 1, 
    end = Sys.Date(),
    process = TRUE,
    names = TRUE,
    verbose = TRUE
  )
  Data2023 <- rbind(previous, add_on)
  Data2023 <- Data2023[!duplicated(Data2023), ]
  write_csv(Data2023, "Data2023.csv")
}


