#Code to update 2023 Statcast data ####
{
  library(tidyverse)
  library(bbd)
  previous <- read_csv("statcast2023.csv")
  add_on <- statcast(
    start = tail(previous$game_date, 1) + 1, 
    end = Sys.Date(),
    process = TRUE,
    names = TRUE,
    verbose = TRUE
  )
  statcast2023 <- rbind(previous, add_on) 
  statcast2023 <- statcast2023[!duplicated(statcast2023), ]
  write_csv(statcast2023, "statcast2023.csv")
}
