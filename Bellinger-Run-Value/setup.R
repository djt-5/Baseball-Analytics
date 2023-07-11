#Download Statcast Data ####
{
library(tidyverse)
statcast2015 <- bbd::statcast(
  start = "2015-04-05",
  end = "2015-11-01",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2015, "statcast2015.csv")
  
statcast2016 <- bbd::statcast(
  start = "2016-04-03",
  end = "2016-11-02",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2016, "statcast2016.csv")

statcast2017 <- bbd::statcast(
  start = "2017-04-02",
  end = "2017-11-01",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2017, "statcast2017.csv")

statcast2018 <- bbd::statcast(
  start = "2018-03-29",
  end = "2018-10-28",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2018, "statcast2018.csv")

statcast2019 <- bbd::statcast(
  start = "2019-03-28",
  end = "2019-10-30",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2019, "statcast2019.csv")

statcast2020 <- bbd::statcast(
  start = "2020-07-23",
  end = "2020-10-27",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2020, "statcast2020.csv")

statcast2021 <- bbd::statcast(
  start = "2021-04-01",
  end = "2021-11-02",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2021, "statcast2021.csv")

statcast2022 <- bbd::statcast(
  start = "2022-04-07",
  end = "2022-11-05",
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2022, "statcast2022.csv")

statcast2023 <- bbd::statcast(
  start = "2023-03-30",
  end = Sys.Date(),
  process = TRUE,
  names = TRUE,
  verbose = TRUE
)

write_csv(statcast2023, "statcast2023.csv")
}

#Create Bellinger's Data ####
{
  Statcast_Data <- rbind(statcast2015, statcast2016, statcast2017,
                        statcast2018, statcast2019, statcast2020,
                        statcast2021, statcast2022, statcast2023)
  
  Belli_Data <- Statcast_Data |>
    filter(batter_name == "Cody Bellinger")
}

#Add Hard Hit, K-Zone Width & Height, Whiff, BB Type, and Chase ####

{
library(tidyverse)
#Hard Hit
Belli_Data$description <- ifelse(Belli_Data$description == "hit_into_play" & 
                                    grepl("bunt", Belli_Data$des), 
                                   "bunt_into_play", Belli_Data$description)
Belli_Data$hard_hit <- ifelse(Belli_Data$launch_speed >= 95 & 
                                Belli_Data$description != "bunt_into_play", 1, 0)

#K-Zone Width
Belli_Data$width <- "X"
Belli_Data$width <- ifelse(Belli_Data$plate_x < -0.7083335, 
                           "Left Out Of Zone", Belli_Data$width)
Belli_Data$width <- ifelse(Belli_Data$plate_x >= -0.7083335 & 
                             Belli_Data$plate_x < -0.2361112, 
                           "Left", Belli_Data$width)
Belli_Data$width <- ifelse(Belli_Data$plate_x >= -0.2361112 & 
                             Belli_Data$plate_x < 0.2361112, 
                           "Middle", Belli_Data$width)
Belli_Data$width <- ifelse(Belli_Data$plate_x >= 0.2361112 & 
                             Belli_Data$plate_x <= 0.7083335, 
                           "Right", Belli_Data$width)
Belli_Data$width <- ifelse(Belli_Data$plate_x > 0.7083335, 
                           "Right Out Of Zone", Belli_Data$width)

#K-Zone Height
Belli_Data$height <- "Z"
Belli_Data$height <- ifelse(Belli_Data$plate_z < 1.65, 
                            "Low Out Of Zone", Belli_Data$height)
Belli_Data$height <- ifelse(Belli_Data$plate_z >= 1.65 &
                              Belli_Data$plate_z < 2.316667,
                            "Low", Belli_Data$height)
Belli_Data$height <- ifelse(Belli_Data$plate_z >= 2.316667 &
                              Belli_Data$plate_z < 2.983334,
                            "Middle", Belli_Data$height)
Belli_Data$height <- ifelse(Belli_Data$plate_z >= 2.983334 &
                              Belli_Data$plate_z <= 3.65,
                            "High", Belli_Data$height)
Belli_Data$height <- ifelse(Belli_Data$plate_z > 3.65,
                            "High Out Of Zone", Belli_Data$height)

#Whiff
swinging <- c("hit_into_play", "foul", "swinging_strike", "bunt_into_play", 
              "swinging_strike_blocked", "foul_tip","missed_bunt", "foul_bunt")
Belli_Data$whiff <- ifelse(Belli_Data$description %in% swinging, 0, NA)
Belli_Data$whiff <- ifelse(Belli_Data$description == "swinging_strike", 
                           1, Belli_Data$whiff)

#Chase
Belli_Data$called_strike <- ifelse(Belli_Data$description == "called_strike",1,0)
not_swinging <- c("ball", "called_strike", "blocked_ball")
Belli_Data$chase <- ifelse(((Belli_Data$plate_z < 1.65 | 
                             Belli_Data$plate_z > 3.65 | 
                             Belli_Data$plate_x < -0.7083333 |
                             Belli_Data$plate_x > 0.7083333) & 
                            Belli_Data$description %in% swinging),1, 0)
Belli_Data$chase <- ifelse(Belli_Data$description %in% not_swinging, 
                           NA, Belli_Data$chase)


#Adjust Base State ####
Belli_Data$Bases <- NA
Belli_Data$Bases <- ifelse(is.na(Belli_Data$on_1b) & is.na(Belli_Data$on_2b) 
                           & is.na(Belli_Data$on_3b), "No Runners", Belli_Data$Bases)
Belli_Data$Bases <- ifelse(!is.na(Belli_Data$on_1b) & is.na(Belli_Data$on_2b) 
                           & is.na(Belli_Data$on_3b), "1st", Belli_Data$Bases)
Belli_Data$Bases <- ifelse(is.na(Belli_Data$on_1b) & !is.na(Belli_Data$on_2b) 
                           & is.na(Belli_Data$on_3b), "2nd", Belli_Data$Bases)
Belli_Data$Bases <- ifelse(is.na(Belli_Data$on_1b) & is.na(Belli_Data$on_2b) 
                           & !is.na(Belli_Data$on_3b), "3rd", Belli_Data$Bases)
Belli_Data$Bases <- ifelse(!is.na(Belli_Data$on_1b) & !is.na(Belli_Data$on_2b) 
                           & is.na(Belli_Data$on_3b), "1st & 2nd", Belli_Data$Bases)
Belli_Data$Bases <- ifelse(!is.na(Belli_Data$on_1b) & is.na(Belli_Data$on_2b) 
                           & !is.na(Belli_Data$on_3b), "1st & 3rd", Belli_Data$Bases)
Belli_Data$Bases <- ifelse(is.na(Belli_Data$on_1b) & !is.na(Belli_Data$on_2b) 
                           & !is.na(Belli_Data$on_3b), "2nd & 3rd", Belli_Data$Bases)
Belli_Data$Bases <- ifelse(!is.na(Belli_Data$on_1b) & !is.na(Belli_Data$on_2b) 
                           & !is.na(Belli_Data$on_3b), "Bases Loaded", Belli_Data$Bases)

#Batted Ball Type ####
Belli_Data$line_drive <- ifelse(Belli_Data$bb_type == "line_drive", 1, 0)
Belli_Data$ground_ball <- ifelse(Belli_Data$bb_type == "ground_ball", 1, 0)
Belli_Data$fly_ball <- ifelse(Belli_Data$bb_type == "fly_ball", 1, 0)

#Adjust for Full Description ####
Belli_Data$bb_type <- ifelse(is.na(Belli_Data$bb_type), 
                             Belli_Data$description,
                             Belli_Data$bb_type)

Belli_Data$events <- ifelse(Belli_Data$events == "field_out", 
                            Belli_Data$bb_type, Belli_Data$events)

Belli_Data$pitch_name <- ifelse(Belli_Data$pitch_name == "Knuckle Curve" |
                                  Belli_Data$pitch_name == "Slow Curve", 
                                "Curveball", Belli_Data$pitch_name)

Belli_Data$pitch_name <- ifelse(Belli_Data$pitch_name == "Slurve" |
                                  Belli_Data$pitch_name == "Sweeper", 
                                "Slider", Belli_Data$pitch_name)

#Only Select Needed Columns for App ####

Belli_Data <- Belli_Data |>
  select(batter_name, description, bb_type, events, Bases, outs_when_up, on_1b,
         on_2b, on_3b, delta_run_exp, hard_hit, chase, 
         whiff, height, width, plate_x, plate_z, release_speed, release_spin_rate, 
         launch_speed, p_throws, pitch_name, called_strike, line_drive, fly_ball, 
         ground_ball, game_year) |>
  filter(!is.na(delta_run_exp)) |>
  filter(!is.na(pitch_name)) |>
  filter(!is.na(pitch_name) & pitch_name != "Other" & pitch_name != "Eephus" &
           pitch_name != "Forkball")

#Save Bellinger's Data to Machine (with all other data as well) ####

write_csv(Belli_Data, "Belli_Data.csv")

}

#Clear Environment before starting app ;) ####


