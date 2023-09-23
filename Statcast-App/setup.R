#Code to acquire 2023 Statcast data ####
{
  library(tidyverse)
  library(bbd)
  statcast2023 <- statcast(
    start = "2023-03-30", 
    end = Sys.Date(),
    process = TRUE,
    names = TRUE,
    verbose = TRUE
  )
  write_csv(statcast2023, "statcast2023.csv")
}

#Restart R 

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

#Restart R

#Setup Code####
{
  library(tidyverse)
  
  Data <- read_csv("statcast2023.csv")
  
  # Percentage stats ####
  
  Data$Swing <- ifelse(Data$description == "hit_into_play" | 
                         Data$description == "foul_tip" |
                         Data$description == "swinging_strike" |
                         Data$description == "foul" |
                         Data$description == "foul_bunt" |
                         Data$description == "swinging_strike_blocked" |
                         Data$description == "missed_bunt" |
                         Data$description == "bunt_foul_tip", 1, 0)
  
  Data$Strike <- ifelse(Data$description != "ball" & 
                          Data$description != "blocked_ball" &
                          Data$description != "hit_by_pitch", 1, 0)
  
  Data$Hard_Hit <- ifelse(Data$launch_speed >= 95, 1, 0)
  Data$Hard_Hit <- ifelse(is.na(Data$launch_speed),
                          NA,
                          Data$Hard_Hit)
  Data$GroundBall <- ifelse(Data$bb_type == "ground_ball", 1, 0)
  Data$GroundBall <- ifelse(is.na(Data$launch_speed),
                            NA,
                            Data$GroundBall)
  Data$LineDrive <- ifelse(Data$bb_type == "line_drive", 1, 0)
  Data$LineDrive <- ifelse(is.na(Data$launch_speed),
                           NA,
                           Data$LineDrive)
  Data$FlyBall <- ifelse(Data$bb_type == "fly_ball", 1, 0)
  Data$FlyBall <- ifelse(is.na(Data$launch_speed),
                         NA,
                         Data$FlyBall)
  
  Data$FoulBall <- ifelse(grepl("foul", Data$description), 1, 0)
  Data$FoulBall <- ifelse(is.na(Data$Swing), 
                          NA,
                          Data$FoulBall)
  
  Data$Chase <- ifelse((Data$plate_x < -0.8308333 | Data$plate_x > 0.8308333 | 
                          Data$plate_z < 1.5275 | Data$plate_z > 3.7725) &
                         Data$Swing == 1, 1, NA)
  
  Data$Chase <- ifelse((Data$plate_x < -0.8308333 | Data$plate_x > 0.8308333 | 
                          Data$plate_z < 1.5275 | Data$plate_z > 3.7725) &
                         Data$Swing == 0, 0, Data$Chase)
  
  Data$Whiff <- ifelse(Data$Swing == 0, NA, 0)
  Data$Whiff <- ifelse(Data$Swing == 1, 0, Data$Whiff)
  Data$Whiff <- ifelse(Data$description == "swinging_strike" |
                         Data$description == "swinging_strike_blocked" |
                         Data$description == "missed_bunt", 1, Data$Whiff)
  
  Data$Taken <- ifelse(Data$description == "called_strike" | 
                         Data$description == "ball" |
                         Data$description == "blocked_ball" |
                         Data$description == "hit_by_pitch" |
                         Data$description == "pitchout", 1, 0)
  
  Data$on_base <- ifelse(Data$events == "single" | Data$events == "double" |
                           Data$events == "triple" | Data$events == "home_run" |
                           Data$events == "walk" | Data$description == "hit_by_pitch", 
                         1, 
                         0)
  Data$on_base <- ifelse(is.na(Data$events),
                         NA,
                         Data$on_base)
  
  Data$slugging <- ifelse(Data$events == "single", 1, 0)
  Data$slugging <- ifelse(Data$events == "double", 2, Data$slugging)
  Data$slugging <- ifelse(Data$events == "triple", 3, Data$slugging)
  Data$slugging <- ifelse(Data$events == "home_run", 4, Data$slugging)
  
  Data$slugging <- ifelse(is.na(Data$events) | 
                            Data$events == "walk" |
                            Data$events == "sac_fly" |
                            Data$events == "hit_by_pitch" |
                            Data$events == "sac_bunt" |
                            Data$events == "caught_stealing_2b" |
                            Data$events == "catcher_interf" |
                            Data$events == "pickoff_1b" |
                            Data$events == "caught_stealing_home" |
                            Data$events == "wild_pitch" |
                            Data$events == "caught_stealing_3b" |
                            Data$events == "pickoff_caught_stealing_home" |
                            Data$events == "pickoff_caught_stealing_2b" |
                            Data$events == "stolen_base_2b" |
                            Data$events == "pickoff_3b" |
                            Data$events == "pickoff_error_3b" |
                            Data$events == "pickoff_2b" |
                            Data$events == "stolen_base_3b", NA,
                          Data$slugging)
  
  # Strike Zone Grouping ####
  
  Data$width <- "X"
  Data$width <- ifelse(Data$plate_x < -0.8308333, 
                       "Left Out Of Zone", Data$width)
  Data$width <- ifelse(Data$plate_x >= -0.8308333 & 
                         Data$plate_x < -0.2361112, 
                       "Left", Data$width)
  Data$width <- ifelse(Data$plate_x >= -0.2361112 & 
                         Data$plate_x < 0.2361112, 
                       "Middle", Data$width)
  Data$width <- ifelse(Data$plate_x >= 0.2361112 & 
                         Data$plate_x <= 0.8308333, 
                       "Right", Data$width)
  Data$width <- ifelse(Data$plate_x > 0.8308333, 
                       "Right Out Of Zone", Data$width)
  
  Data$height <- "Z"
  Data$height <- ifelse(Data$plate_z < Data$sz_bot, 
                        "Low Out Of Zone", Data$height)
  Data$height <- ifelse(Data$plate_z >= Data$sz_bot &
                          Data$plate_z < (Data$sz_bot + (Data$sz_top - Data$sz_bot)/3),
                        "Low", Data$height)
  Data$height <- ifelse(Data$plate_z >= (Data$sz_bot + (Data$sz_top - Data$sz_bot)/3) &
                          Data$plate_z < (Data$sz_top - (Data$sz_top - Data$sz_bot)/3),
                        "Middle", Data$height)
  Data$height <- ifelse(Data$plate_z >= (Data$sz_top - (Data$sz_top - Data$sz_bot)/3) &
                          Data$plate_z <= Data$sz_top,
                        "High", Data$height)
  Data$height <- ifelse(Data$plate_z > Data$sz_top,
                        "High Out Of Zone", Data$height)
  
  # Hit Coordinates ####
  
  Data <- Data |>
    filter(pitch_name != "Other") |>
    filter(!is.na(pitch_name)) |>
    filter(!is.na(pitcher_name))
  
  Data$pull <- ifelse((Data$stand == "L" & Data$hc_x > 128) | 
                        Data$stand == "R" & Data$hc_x >= 128, 1, 0)
  Data$pull <- ifelse(is.na(Data$launch_speed),
                      NA,
                      Data$pull)
  
  # Calculate Pitches Thrown in a Game ####
  
  Data$pitcher_team <- ifelse(Data$inning_topbot == "Top", 
                              Data$home_team,
                              Data$away_team)
  
  Pitcher_Arrange <- Data |>
    filter(pitcher_team == "CHC") |>
    arrange(pitcher_name, game_pk, pitch_number) |>
    mutate(Scenario = paste(pitcher_name, game_pk))
  
  Pitcher_List <- split(Pitcher_Arrange, Pitcher_Arrange$Scenario)
  
  Pitcher_List_2 <- lapply(Pitcher_List, function(x) {
    mutate(x, pitches_thrown = 1:nrow(x))
  })
  
  Pitcher_Data <- bind_rows(Pitcher_List_2)
  
  # Sequencing Setup ####
  
  PA_Seq <- Pitcher_Data |>
    arrange(pitcher_name, game_pk, inning, at_bat_number, pitch_number) |>
    mutate(seq_scenario = paste(pitcher_name, batter_name, game_pk, inning, 
                                at_bat_number))
  
  Seq_List <- split(PA_Seq, PA_Seq$seq_scenario)
  
  Seq_List_2 <- lapply(Seq_List, function(x) {
    mutate(x, prev_pitch = dplyr::lag(pitch_name))
  })
  
  Pitcher_Data <- bind_rows(Seq_List_2)
  
  # DeterCHCing Spin Efficiency: http://baseball.physics.illinois.edu/trackman/SpinAxis.pdf ####
  
  # Parameters: K = 1/2 * density of air * cross sectional area of ball / mass of ball
  K = 0.0053865
  
  # Add necessary columns
  Pitcher_Data <- Pitcher_Data |>
    mutate(yR = 60.5 - release_extension,
           tR = (- vy0 - sqrt(vy0^2 - 2 * ay * (50 - yR))) / ay,
           vxR = vx0 + ax * tR,
           vyR = vy0 + ay * tR,
           vzR = vz0 + az * tR,
           dv0 = release_speed - sqrt(vxR^2 + vyR^2 + vzR^2) * 3600 / 5280,
           tf = (-vyR - sqrt(vyR^2 - 2 * ay * (yR - 17 / 12))) / ay,
           vxbar = (2 * vxR + ax * tf) / 2,
           vybar = (2 * vyR + ay * tf) / 2,
           vzbar = (2 * vzR + az * tf) / 2,
           vbar = sqrt(vxbar^2 + vybar^2 + vzbar^2),
           adrag = - (ax * vxbar + ay * vybar + (az + 32.174) * vzbar) / vbar,
           Cd = adrag / (K * vbar^2),
           amagx = ax + adrag * vxbar / vbar,
           amagy = ay + adrag * vybar / vbar,
           amagz = az + adrag * vzbar / vbar + 32.174,
           amag = sqrt(amagx^2 + amagy^2 + amagz^2),
           Mx = (1 / 2) * amagx * tf^2 * 12,
           Mz = (1 / 2) * amagz * tf^2 * 12,
           Cl = amag / (K * vbar^2),
           S = 0.166 * log(0.336 / (0.336 - Cl)),
           spinT = 78.92 * S * vbar,
           spinTX = spinT * (vybar * amagz - vzbar * amagy) / (amag * vbar),
           spinTY = spinT * (vzbar * amagx - vxbar * amagz) / (amag * vbar),
           spinTZ = spinT * (vxbar * amagy - vybar * amagx) / (amag * vbar),
           spin_eff = spinT / release_spin_rate,
           phi_dummy = ifelse(amagz > 0, atan2(amagz, amagx) * 180 / pi + 90, atan2(amagz, amagx) * 180 / pi + 360 + 90),
           phi = ifelse(phi_dummy > 360, phi_dummy - 360, phi_dummy),
           theta = ifelse(spin_eff <= 1, acos(spin_eff) * 180 / pi, ""))
  
  
  # Save Data ####
  
  Pitcher_Data_Cubs <- Pitcher_Data |>
    mutate(count_dash = "-") |>
    mutate(count = paste(balls, count_dash, strikes)) |>
    select(-count_dash)
  
  write_csv(Pitcher_Data_Cubs, "Pitcher_Data_Cubs.csv")
}

  