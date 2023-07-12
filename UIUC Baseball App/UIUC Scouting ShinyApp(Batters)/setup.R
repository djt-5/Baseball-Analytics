# READ BELOW





# Game Data collected via FileZilla
# Host name is ftp.trackmanbaseball.com
# Username is U Illinois
# Password is TzdsYUKUf9
# In the V3 folder is the 2023 folder. Download it. Set it as working directory.
# In console or script type the following:

## files  <- list.files(pattern = 'IllinoisField-1.csv')
## tables <- lapply(files, read.csv, header = TRUE)
## GameData <- do.call(rbind , tables)
## GameData |> rename(Top.Bottom = `Top/Bottom`)

# Practice Data posted on Github. Bind all files using method above. 
# Name it "Data.csv".

library(tidyverse)

Data <- read_csv("/Users/danielthompson/Desktop/Baseball/Illini-Data/Data.csv")

# Change headers to statcast names for familiarity purposes

Data <- Data |>
  rename(pitcher_name = Patter) |>
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
  rename(p_throws = BatterThrows) |>
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
  rename(spin_axis = SpinAxis) 

Data <- Data |>
  rename(Spray_Angle = Bearing)

Data$batter_name <- sub("(\\w+),\\s(\\w+)","\\2 \\1", Data$batter_name)

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
Data$FoulBall <- ifelse(Data$description == "StrikeCalled" |
                          Data$description == "BallCalled", 
                        NA,
                        Data$FoulBall)

Data$zone <- ifelse(Data$plate_x > -17/24 & Data$plate_x < 17/24 & 
                      Data$plate_z > 1.65 & Data$plate_z < 3.65, 1, 0)

Data$Chase <- ifelse(Data$zone == 0, 0, NA)
Data$Chase <- ifelse(Data$description == "FoulBall" | 
                       !is.na(Data$launch_speed) |
                       Data$description == "StrikeSwinging", 1, Data$Chase)

Data$Whiff <- ifelse(Data$description == "StrikeSwinging", 1, 0)

Data$Taken <- ifelse(Data$description == "StrikeCalled" | 
                       Data$description == "BallCalled" |
                       Data$description == "BallinDirt" |
                       Data$description == "HitByPitch" , 1, 0)

Data$width <- "X"
Data$width <- ifelse(Data$plate_x < -17/24, 
                     "Left Out Of Zone", Data$width)
Data$width <- ifelse(Data$plate_x >= -17/24 & 
                       Data$plate_x < -0.2361112, 
                     "Left", Data$width)
Data$width <- ifelse(Data$plate_x >= -0.2361112 & 
                       Data$plate_x < 0.2361112, 
                     "Middle", Data$width)
Data$width <- ifelse(Data$plate_x >= 0.2361112 & 
                       Data$plate_x <= 17/24, 
                     "Right", Data$width)
Data$width <- ifelse(Data$plate_x > 17/24, 
                     "Right Out Of Zone", Data$width)

Data$height <- "Z"
Data$height <- ifelse(Data$plate_z < 1.65, 
                      "Low Out Of Zone", Data$height)
Data$height <- ifelse(Data$plate_z >= 1.65 &
                        Data$plate_z < 2.316667,
                      "Low", Data$height)
Data$height <- ifelse(Data$plate_z >= 2.316667 &
                        Data$plate_z < 2.983334,
                      "Middle", Data$height)
Data$height <- ifelse(Data$plate_z >= 2.983334 &
                        Data$plate_z <= 3.65,
                      "High", Data$height)
Data$height <- ifelse(Data$plate_z > 3.65,
                      "High Out Of Zone", Data$height)

Data <- Data |>
  filter(pitch_name != "Other") |>
  filter(!is.na(pitch_name)) |>
  filter(stand != "Undefined") |>
  filter(!is.na(stand)) |>
  filter(p_throws != "Undefined") |>
  filter(!is.na(p_throws)) |> 
  mutate(location_x = sin(Spray_Angle*pi/180) * hit_distance_sc) |>
  mutate(location_y = cos(Spray_Angle*pi/180) * hit_distance_sc) |>
  mutate(hc_x = (location_x/2.5) + 125.42) |>
  mutate(hc_y = 198.27 - (location_y/2.5))

Data$events <- ifelse((Data$description == "StrikeSwinging" |
                         Data$description == "StrikeCalled") & Data$strikes == 2, 
                      "Strikeout", 
                      Data$events)

Data$events <- ifelse(Data$description == "BallCalled" & Data$balls == 3,
                      "Walk",
                      Data$events)

Data$pull <- ifelse((Data$stand == "Left" & Data$hc_x > 128) | 
                      Data$stand == "Right" & Data$hc_x >= 128, 1, 0)
Data$pull <- ifelse(is.na(Data$launch_speed),
                    NA,
                    Data$pull)

Data$strike <- ifelse(Data$description == "StrikeCalled" | 
                        Data$description == "FoulBall" |
                        Data$description == "SwingingStrike" |
                        !is.na(Data$launch_speed), 1, 0)

Data$on_base <- ifelse(Data$events == "Single" | Data$events == "Double" |
                         Data$events == "Triple" | Data$events == "Home Run" |
                         Data$events == "Walk" | Data$description == "HitByPitch", 
                       1, 
                       0)
Data$on_base <- ifelse(Data$events == "Undefined" & Data$description != "HitByPitch",
                       NA,
                       Data$on_base)

Data$slugging <- ifelse(Data$events == "Single", 1, 0)
Data$slugging <- ifelse(Data$events == "Double", 2, Data$slugging)
Data$slugging <- ifelse(Data$events == "Triple", 3, Data$slugging)
Data$slugging <- ifelse(Data$events == "HomeRun", 4, Data$slugging)

Data$slugging <- ifelse(is.na(Data$launch_speed) | Data$events == "Sacrifice",
                        NA,
                        Data$slugging)

Batter_Arrange <- Data |>
  filter(BatterTeam == "ILL_ILL") |>
  arrange(batter_name, GameUID) |>
  mutate(Scenario = paste(batter_name, GameUID, PAofInning, inning))

Batter_List <- split(Batter_Arrange, Batter_Arrange$Scenario)

Batter_List_2 <- lapply(Batter_List, function(x) {
  mutate(x, `Plate Appearances` = 1:nrow(x))
})

Batter_Data <- bind_rows(Batter_List_2)

write_csv(Batter_Data, "Batter_Data.csv")