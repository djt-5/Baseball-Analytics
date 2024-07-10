library(tidyverse)

Baserunning <- rbind(
  read_csv("Baserunning2016.csv"),
  read_csv("Baserunning2017.csv"),
  read_csv("Baserunning2018.csv"),
  read_csv("Baserunning2019.csv"),
  read_csv("Baserunning2021.csv"),
  read_csv("Baserunning2022.csv"),
  read_csv("Baserunning2023.csv")
)

Baserunning <- na.omit(Baserunning)

write_csv(Baserunning, "Baserunning-Data.csv")
