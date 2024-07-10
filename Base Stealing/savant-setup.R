{
  library(tidyverse)
  
  Data <- read_csv("Data2023.csv")
  Catcher <- read_csv("Catchers2023.csv")
  Pitcher <- read_csv("Pitchers2023.csv")
  Sprint <- read_csv("Sprint2023.csv")
  Retro <- read_csv("retro2023.csv")

  #Rename and select exterior variables

  Catcher <- Catcher |> 
    rename(fielder_2 = player_id) |>
    select(fielder_2, pop_time, exchange_time, arm_strength)
  
  Pitcher <- Pitcher |> 
    rename(pitcher = player_id) |>
    select(pitcher, pos11_r_primary_lead_sb2cs2)
  
  Sprint <- Sprint |> 
    rename(on_1b = player_id) |>
    select(on_1b, sprint_speed)

#Join variables into large Data

  Baserunning <- Data |>
    left_join(Sprint, by = "on_1b") |>
    left_join(Catcher, by = "fielder_2") |>
    left_join(Pitcher, by = "pitcher") |>
    filter(!is.na(balls) & !is.na(strikes) & !is.na(outs_when_up) & !is.na(inning) & 
             !is.na(on_1b) & !is.na(batter) & !is.na(fielder_2) & !is.na(pitcher) & 
             !is.na(home_score) & !is.na(inning_topbot) & !is.na(away_score) & 
             !is.na(away_team) & !is.na(home_team) & !is.na(game_date)) |>
    filter(description != "foul" & description != "foul_bunt") |>
    mutate(swing = ifelse(description %in% c("foul_tip", "missed_bunt", "swinging_pitchout",
                                             "swinging_strike", "swinging_strike_blocked"), 1, 0)) |>
    mutate(pitch_id = paste(game_date, away_team, home_team, inning_topbot, inning,
                            away_score, home_score, pitcher, batter, fielder_2,
                            on_1b, balls, strikes, outs_when_up)) |>
    left_join(Retro, by = "pitch_id") |>
    filter(!is.na(safe)) |>
    select(safe, release_speed, plate_x, plate_z, pop_time, exchange_time, 
           arm_strength, pos11_r_primary_lead_sb2cs2, sprint_speed, swing, delta_run_exp) |>
    rename(lead = pos11_r_primary_lead_sb2cs2)
  
  write_csv(Baserunning, "Baserunning2023.csv")
  
  rm(Baserunning, Catcher, Data, Pitcher, Retro, Sprint)
}
