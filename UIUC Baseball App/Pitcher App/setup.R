# READ BELOW





# Game Data collected via FileZilla, is also included in this repo
# Host name is ftp.trackmanbaseball.com
# Username = U Illinois
# Password = TzdsYUKUf9
# In the V3 folder is the 2023 folder. Download it. Set it as working directory.
# Practice Data posted on Github. Add all CSVs into the file.
# In console or script type the following:

## library(tidyverse)
## file_list <- list.files()[-30]
## list_of_dfs <- lapply(file_list, function(x) {
##   read_csv(x, col_types = cols(Date = col_character(), 
##                                UTCDate = col_character()))
## })
## Data <- bind_rows(list_of_dfs)
## Data <- Data[!duplicated(Data), ]
## write_csv(Data, "Data.csv")
## Done :)

{
  library(tidyverse)
  
  Data <- read_csv("Data.csv")
  
  # Change headers to statcast names for familiarity purposes
  
  Data <- Data |>
    rename(Top.Bottom = `Top/Bottom`) |>
    rename(pitcher_name = Pitcher) |>
    rename(pitcher = PitcherId) |>
    rename(batter_name = Batter) |>
    rename(batter = BatterId) |>
    rename(game_date = Date) |>
    rename(release_speed = RelSpeed) |>
    rename(release_pos_x = RelSide) |>
    rename(release_pos_z = RelHeight) |>
    rename(description = PitchCall) |>
    rename(events = PlayResult) |>
    rename(stand = BatterSide) |>
    rename(p_throws = PitcherThrows) |>
    rename(home_team = HomeTeam) |>
    rename(away_team = AwayTeam) |>
    rename(bb_type = AutoHitType) |>
    rename(balls = Balls) |>
    rename(strikes = Strikes) |>
    rename(pfx_x = pfxx) |>
    rename(pfx_z = pfxz) |>
    rename(plate_x = PlateLocSide) |>
    rename(plate_z = PlateLocHeight) |>
    rename(outs_when_up = Outs) |>
    rename(inning = Inning) |>
    rename(inning_topbot = Top.Bottom) |>
    rename(ax = ax0) |>
    rename(ay = ay0) |>
    rename(az = az0) |>
    rename(hit_distance_sc = Distance) |>
    rename(launch_speed = ExitSpeed) |>
    rename(launch_angle = Angle) |>
    rename(effective_speed = EffectiveVelo) |>
    rename(release_spin_rate = SpinRate) |>
    rename(release_extension = Extension) |>
    rename(game_pk = GameID) |>
    rename(pitch_number = PitchNo) |>
    rename(pitch_name = AutoPitchType) |>
    rename(spin_axis = SpinAxis) |>
    rename(Spray_Angle = Bearing)
  
  ## The Pitcher Name Formatting sucks. Let's change that
  
  Data$pitcher_name <- sub("(\\w+),\\s(\\w+)","\\2 \\1", Data$pitcher_name)
  Data$batter_name <- sub("(\\w+),\\s(\\w+)","\\2 \\1", Data$batter_name)
  
  # Percentage stats
  
  Data$Swing <- ifelse(!is.na(Data$launch_speed) | 
                         Data$description == "StrikeSwinging" |
                         Data$description == "FoulBall", 1, 0)
  
  Data$Strike <- ifelse(Data$description != "BallCalled" & 
                          Data$description != "HitByPitch", 1, 0)
  
  Data$Hard_Hit <- ifelse(Data$launch_speed >= 95, 1, 0)
  Data$Hard_Hit <- ifelse(is.na(Data$launch_speed),
                          NA,
                          Data$Hard_Hit)
  Data$GroundBall <- ifelse(Data$bb_type == "GroundBall", 1, 0)
  Data$GroundBall <- ifelse(is.na(Data$launch_speed),
                            NA,
                            Data$GroundBall)
  Data$LineDrive <- ifelse(Data$bb_type == "LineDrive", 1, 0)
  Data$LineDrive <- ifelse(is.na(Data$launch_speed),
                           NA,
                           Data$LineDrive)
  Data$FlyBall <- ifelse(Data$bb_type == "FlyBall", 1, 0)
  Data$FlyBall <- ifelse(is.na(Data$launch_speed),
                         NA,
                         Data$FlyBall)
  
  Data$FoulBall <- ifelse(Data$description == "FoulBall", 1, 0)
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
  Data$Whiff <- ifelse(Data$description == "StrikeSwinging", 1, Data$Whiff)
  
  Data$Taken <- ifelse(Data$description == "StrikeCalled" | 
                         Data$description == "BallCalled" |
                         Data$description == "BallinDirt" |
                         Data$description == "HitByPitch" , 1, 0)
  
  Data$events <- ifelse((Data$description == "StrikeSwinging" |
                           Data$description == "StrikeCalled") & Data$strikes == 2, 
                        "Strikeout", 
                        Data$events)
  
  Data$events <- ifelse(Data$description == "BallCalled" & Data$balls == 3,
                        "Walk",
                        Data$events)
  
  Data$on_base <- ifelse(Data$events == "Single" | Data$events == "Double" |
                           Data$events == "Triple" | Data$events == "Home Run" |
                           Data$events == "Walk" | Data$description == "HitByPitch", 
                         1, 
                         NA)
  Data$on_base <- ifelse(Data$events == "Out" |
                           Data$events == "Strikeout" |
                           Data$events == "Error" |
                           Data$events == "FieldersChoice",
                         0,
                         Data$on_base)
  
  Data$slugging <- ifelse(Data$events == "Single", 1, 0)
  Data$slugging <- ifelse(Data$events == "Double", 2, Data$slugging)
  Data$slugging <- ifelse(Data$events == "Triple", 3, Data$slugging)
  Data$slugging <- ifelse(Data$events == "HomeRun", 4, Data$slugging)
  
  Data$slugging <- ifelse(Data$events == "Undefined" | 
                            Data$events == "Walk" |
                            Data$events == "Sacrifice", NA,
                          Data$slugging)
  
  # Strike Zone Grouping
  
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
  Data$height <- ifelse(Data$plate_z < 1.5275, 
                        "Low Out Of Zone", Data$height)
  Data$height <- ifelse(Data$plate_z >= 1.5275 &
                          Data$plate_z < 2.316667,
                        "Low", Data$height)
  Data$height <- ifelse(Data$plate_z >= 2.316667 &
                          Data$plate_z < 2.983334,
                        "Middle", Data$height)
  Data$height <- ifelse(Data$plate_z >= 2.983334 &
                          Data$plate_z <= 3.7725,
                        "High", Data$height)
  Data$height <- ifelse(Data$plate_z > 3.7725,
                        "High Out Of Zone", Data$height)
  
  #Convert Release Stats from Feet to Inches
  
  Data$release_pos_x <- 12*Data$release_pos_x
  Data$release_pos_z <- 12*Data$release_pos_z
  Data$x0 <- -12*Data$x0
  Data$z0 <- 12*Data$z0
  
  # Hit Coordinates
  
  Data <- Data |>
    filter(pitch_name != "Other") |>
    filter(!is.na(pitch_name)) |>
    filter(stand != "Undefined") |>
    filter(!is.na(stand)) |>
    filter(!is.na(pitcher_name)) |>
    filter(p_throws != "Undefined") |>
    filter(!is.na(p_throws)) |> 
    filter(!is.na(release_spin_rate)) |>
    mutate(location_x = sin(Spray_Angle*pi/180) * hit_distance_sc) |>
    mutate(location_y = cos(Spray_Angle*pi/180) * hit_distance_sc) |>
    mutate(hc_x = (location_x/2.5) + 125.42) |>
    mutate(hc_y = 198.27 - (location_y/2.5))
  
  Data$pull <- ifelse((Data$stand == "Left" & Data$hc_x > 128) | 
                        Data$stand == "Right" & Data$hc_x >= 128, 1, 0)
  Data$pull <- ifelse(is.na(Data$launch_speed),
                      NA,
                      Data$pull)
  
  # Calculate Pitches Thrown in a Game
  
  Pitcher_Arrange <- Data |>
    filter(PitcherTeam == "ILL_ILL") |>
    arrange(pitcher_name, GameUID, pitch_number) |>
    mutate(Scenario = paste(pitcher_name, GameUID))
  
  Pitcher_List <- split(Pitcher_Arrange, Pitcher_Arrange$Scenario)
  
  Pitcher_List_2 <- lapply(Pitcher_List, function(x) {
    mutate(x, Pitches_Thrown = 1:nrow(x))
  })
  
  Pitcher_Data <- bind_rows(Pitcher_List_2)
  
  # Sequencing Setup
  
  PA_Seq <- Pitcher_Data |>
    arrange(pitcher_name, GameUID, inning, PAofInning, pitch_number) |>
    mutate(seq_scenario = paste(pitcher_name, batter_name, GameUID, PAofInning, 
                                inning))
  
  Seq_List <- split(PA_Seq, PA_Seq$seq_scenario)
  
  Seq_List_2 <- lapply(Seq_List, function(x) {
    mutate(x, prev_pitch = dplyr::lag(pitch_name))
  })
  
  Pitcher_Data <- bind_rows(Seq_List_2)
  
  # Save Data
  
  Pitcher_Data <- Pitcher_Data |>
    filter(!is.na(release_speed) & !is.na(VertRelAngle) & !is.na(HorzRelAngle) & 
             !is.na(release_spin_rate) & !is.na(spin_axis) & !is.na(release_pos_z) &
             !is.na(release_pos_x) & !is.na(release_extension) & !is.na(VertBreak) &
             !is.na(InducedVertBreak) & !is.na(HorzBreak) & !is.na(plate_z) &
             !is.na(plate_x) & !is.na(ZoneSpeed) & !is.na(VertApprAngle) & 
             !is.na(ZoneTime) & !is.na(pfx_x) & !is.na(pfx_z) & !is.na(x0) & !is.na(z0) &
             !is.na(effective_speed) & !is.na(SpeedDrop) & !is.na(Pitches_Thrown)) |>
    mutate(count_dash = "-") |>
    mutate(count = paste(balls, count_dash, strikes)) |>
    select(-count_dash)
  
  write_csv(Pitcher_Data, "Pitcher_Data.csv")
}

  