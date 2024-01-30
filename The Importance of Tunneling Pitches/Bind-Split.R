{
  #Bind Data Frames
  library(tidyverse)
  Tunnel <- rbind(
    read_csv("Tunnel2015.csv"),
    read_csv("Tunnel2016.csv"),
    read_csv("Tunnel2017.csv"),
    read_csv("Tunnel2018.csv"),
    read_csv("Tunnel2019.csv"),
    read_csv("Tunnel2021.csv"),
    read_csv("Tunnel2022.csv"),
    read_csv("Tunnel2023.csv")
  )
  
  write_csv(Tunnel, "Tunnel.csv")
}

#Clear Workspace

{
  #Pitcher and Batter Handedness
  library(tidyverse)
  Tunnel <- read_csv("Tunnel.csv") |>
    mutate(Sequence = paste(prev_pitch, pitch_type)) |>
    select(-prev_pitch, -pitch_type)
  
  RHP_RHH <- Tunnel |>
    filter(p_throws == "R" & stand == "R")
  RHP_LHH <- Tunnel |>
    filter(p_throws == "R" & stand == "L")
  LHP_RHH <- Tunnel |>
    filter(p_throws == "L" & stand == "R")
  LHP_LHH <- Tunnel |>
    filter(p_throws == "L" & stand == "L")
  
  write_csv(RHP_RHH, "RHP_RHH.csv")
  write_csv(RHP_LHH, "RHP_LHH.csv")
  write_csv(LHP_RHH, "LHP_RHH.csv")
  write_csv(LHP_LHH, "LHP_LHH.csv")
}
