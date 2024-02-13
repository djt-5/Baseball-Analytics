library(tidyverse)

Data <- rbind(read_csv("SecondBase.csv"),
              read_csv("ThirdBase.csv")) |>
  mutate(dist_from_base = 2.5 * dist_from_base)
 
#Magic Number is 220

df <- Data |>
  filter(!is.na(tag_attempt)) 

mean(df$tag_attempt)
