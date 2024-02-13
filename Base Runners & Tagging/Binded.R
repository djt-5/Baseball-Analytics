library(tidyverse)

Data <- rbind(read_csv("Data2B.csv"),
              read_csv("Data3B.csv"),
              read_csv("DataHP.csv")) |>
  mutate(safe = ifelse(dist_from_base < 60.99514, 0, safe)) |>
  mutate(safe = ifelse(dist_from_base > 124.1073, 1, safe)) |>
  filter(!is.na(safe)) |>
  select(-tag_attempt)

write_csv(Data, "Data.csv")
