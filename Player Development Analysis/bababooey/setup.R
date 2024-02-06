# Run block to load and organize data ####
# (do separate for each years 2015 to 2023 as to not overwhelm the RAM) 

{
  library(tidyverse)
  Data2023 <- read_csv("Data2023.csv")
  selected_data <- Data2023 |>
    select(game_date, events, description, des, home_team, away_team, 
           bb_type, game_year, on_1b, on_2b, on_3b, outs_when_up, inning,
           inning_topbot, hc_x, hc_y, hit_distance_sc, game_pk, bat_score,
           post_bat_score, delta_run_exp, at_bat_number, pitch_number,
           estimated_ba_using_speedangle) |>
    mutate(spray_angle = atan((hc_x-125.42)/(198.27-hc_y))) |>
    arrange(game_pk, inning, inning_topbot, at_bat_number, pitch_number) |>
    filter(!is.na(events)) |>
    mutate(scenario = paste(game_pk, inning, inning_topbot))
  
  selected_data$on_1b <- ifelse(!is.na(selected_data$on_1b), 1, 0)
  selected_data$on_2b <- ifelse(!is.na(selected_data$on_2b), 1, 0)
  selected_data$on_3b <- ifelse(!is.na(selected_data$on_3b), 1, 0)
  
  selected_data <- selected_data |>
    mutate(base_out_state = paste(on_1b, on_2b, on_3b, outs_when_up))
  
  List_1 <- split(selected_data, selected_data$scenario)
  
  List_2 <- lapply(List_1, function(x) {
    mutate(x, new_state = dplyr::lead(base_out_state))
  })
  
  Selected2023 <- bind_rows(List_2)
  
  Selected2023$new_state <- ifelse(is.na(Selected2023$new_state), "0 0 0 0",
                               Selected2023$new_state)
  
  Selected2023 <- Selected2023 |>
    filter(bb_type == "fly_ball") |>
    filter(!is.na(hc_x) & !is.na(hc_y)) |>
    filter(events != "home_run" & events != "sac_fly" &
             events != "sac_fly_double_play" & events != "fielders_choice") |>
    filter(!is.na(hit_distance_sc)) |>
    filter(!grepl("error", des)) |>
    filter(base_out_state == "1 0 0 0" |
             base_out_state == "1 0 0 1" |
             base_out_state == "0 1 0 0" |
             base_out_state == "0 1 0 1" |
             base_out_state == "1 1 0 0" |
             base_out_state == "1 1 0 1")
  
  
  
  write_csv(Selected2023, "Selected2023.csv")
}

# Combine all the data ####

{
  library(tidyverse)
  Data <- rbind(
    read_csv("Selected2015.csv"),
    read_csv("Selected2016.csv"),
    read_csv("Selected2017.csv"),
    read_csv("Selected2018.csv"),
    read_csv("Selected2019.csv"),
    read_csv("Selected2020.csv"),
    read_csv("Selected2021.csv"),
    read_csv("Selected2022.csv"),
    read_csv("Selected2023.csv")
  )
  
  write_csv(Data, "Data.csv")
}

# Data Wrangling ####

{
  library(tidyverse)
  Data <- read_csv("Data.csv")
  
  Data <- Data |>
    select(game_date, events, description, des, home_team, away_team, 
           bb_type, game_year, on_1b, on_2b, on_3b, outs_when_up, inning,
           inning_topbot, hc_x, hc_y, hit_distance_sc, game_pk, bat_score,
           post_bat_score, delta_run_exp, at_bat_number, pitch_number, 
           base_out_state, new_state, scenario, spray_angle, 
           estimated_ba_using_speedangle) |>
    mutate(dist_from_2b = sqrt((hc_x - 125.42)^2 + (hc_y - 147.87)^2)) |>
    mutate(dist_from_3b = sqrt((hc_x - 100.22)^2 + (hc_y - 173.07)^2)) 
    
  
  Data$events <- ifelse(grepl("double play", Data$des), "double_play",
                        Data$events)
  
  Data$events <- ifelse(grepl("triple play", Data$des), "triple_play",
                        Data$events)
  
  # Runs on Play
  
  Data$runs_on_play <- Data$post_bat_score - Data$bat_score
  
  # Measure if there was a tag attempt on an out or not, and NA if no outs
  
  Data$tag_att <- ifelse(Data$events == "field_out" |
                           Data$events == "double_play" |
                           Data$events == "triple_play", 0, NA)
  
  Data$tag_att <- ifelse(Data$tag_att == 0 &
                           Data$base_out_state == "1 0 0 0" &
                           Data$new_state != "1 0 0 1" &
                           !grepl("out at 1st", Data$des), 
                         1, Data$tag_att)
  
  Data$tag_att <- ifelse(Data$tag_att == 0 &
                           Data$base_out_state == "1 0 0 1" &
                           Data$new_state != "1 0 0 2" &
                           !grepl("out at 1st", Data$des), 
                         1, Data$tag_att)
  
  Data$tag_att <- ifelse(Data$tag_att == 0 &
                           Data$base_out_state == "0 1 0 0" &
                           Data$new_state != "0 1 0 0" &
                           !grepl("out at 2nd", Data$des), 
                         1, Data$tag_att)
  
  Data$tag_att <- ifelse(Data$tag_att == 0 &
                           Data$base_out_state == "0 1 0 1" &
                           Data$new_state != "0 1 0 2" &
                           !grepl("out at 2nd", Data$des), 
                         1, Data$tag_att)
  
  Data$tag_att <- ifelse(Data$tag_att == 0 &
                           Data$base_out_state == "1 1 0 0" &
                           Data$new_state != "1 1 0 1" &
                           (grepl("out at 3rd", Data$des) |
                              grepl("to 3rd", Data$des) |
                              grepl("scores", Data$des) |
                              grepl("out at home", Data$des)), 
                         1, Data$tag_att)
  
  Data$tag_att <- ifelse(Data$tag_att == 0 &
                           Data$base_out_state == "1 1 0 1" &
                           Data$new_state != "1 1 0 2" &
                           (grepl("out at 3rd", Data$des) |
                              grepl("to 3rd", Data$des) |
                              grepl("scores", Data$des) |
                              grepl("out at home", Data$des)), 
                         1, Data$tag_att)
  
  # Measures success of tagging
  
  Data$sac_safe_second <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "1 0 0 0" &
                                   grepl("to 2nd", Data$des), 1, NA)
  
  Data$sac_safe_second <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "1 0 0 0" &
                                   grepl("out at 2nd", Data$des), 
                                 0, Data$sac_safe_second)
  
  Data$sac_safe_second <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "1 0 0 1" &
                                   grepl("to 2nd", Data$des), 
                                 1, Data$sac_safe_second)
  
  Data$sac_safe_second <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "1 0 0 1" &
                                   grepl("double play", Data$des), 
                                 0, Data$sac_safe_second)
  
  Data$sac_safe_second <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "1 1 0 0" &
                                   grepl("to 2nd", Data$des),
                                 1, Data$sac_safe_second)
  
  Data$sac_safe_second <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "1 1 0 0" &
                                   (grepl("out at 2nd", Data$des) | 
                                      grepl("double play", Data$des) |
                                      grepl("triple play", Data$des)),
                                 0, Data$sac_safe_second)
  
  Data$sac_safe_third <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "0 1 0 0" &
                                  grepl("to 3rd", Data$des), 1, NA)
  
  Data$sac_safe_third <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "0 1 0 0" &
                                  grepl("out at 3rd", Data$des), 
                                 0, Data$sac_safe_third)
  
  Data$sac_safe_third <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "0 1 0 1" &
                                  grepl("to 3rd", Data$des), 
                                 1, Data$sac_safe_third)
  
  Data$sac_safe_third <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "0 1 0 1" &
                                  (grepl("double play", Data$des)), 
                                 0, Data$sac_safe_third)
  
  Data$sac_safe_third <- ifelse(Data$tag_att == 1 &
                                   Data$base_out_state == "1 1 0 0" &
                                   grepl("to 3rd", Data$des),
                                 1, Data$sac_safe_third)
  
  Data$sac_safe_third <- ifelse(Data$tag_att == 1 &
                                  Data$base_out_state == "1 1 0 0" &
                                  (grepl("out at 3rd", Data$des) |
                                     grepl("double play", Data$des) |
                                     grepl("triple play", Data$des)),
                                0, Data$sac_safe_third)
  
  # Adds hypothetical tag states on base hits
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 0" &
                                      Data$events == "single",
                                    "1 1 0 0", NA)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 0" &
                                      Data$events == "double",
                                    "0 1 1 0", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 0" &
                                     Data$events == "triple",
                                   "0 0 1 0", Data$base_hit_tag_state) #1 tag run
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 0" &
                                      Data$events == "force_out",
                                    Data$new_state, Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 1" &
                                      Data$events == "single",
                                    "1 1 0 1", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 1" &
                                      Data$events == "double",
                                    "0 1 1 1", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 1" &
                                      Data$events == "triple",
                                    "0 0 1 1", Data$base_hit_tag_state) #1 tag run
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 0 0 1" &
                                      Data$events == "force_out",
                                    Data$new_state, Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 0" &
                                      Data$events == "single",
                                    "1 0 1 0", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 0" &
                                      Data$events == "double",
                                    "0 1 0 0", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 0" &
                                      Data$events == "triple",
                                    "0 0 1 0", Data$base_hit_tag_state) #1 tag run
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 0" &
                                      Data$events == "force_out",
                                    Data$new_state, Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 1" &
                                      Data$events == "single",
                                    "1 0 1 1", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 1" &
                                      Data$events == "double",
                                    "0 1 0 1", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 1" &
                                      Data$events == "triple",
                                    "0 0 1 1", Data$base_hit_tag_state) #1 tag run
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "0 1 0 1" &
                                      Data$events == "force_out",
                                    Data$new_state, Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 0" &
                                      Data$events == "single",
                                    "1 1 1 0", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 0" &
                                      Data$events == "double",
                                    "0 1 1 0", Data$base_hit_tag_state) #1 tag run
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 0" &
                                      Data$events == "triple",
                                    "0 0 1 0", Data$base_hit_tag_state) #2 tag runs
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 0" &
                                      Data$events == "force_out",
                                    Data$new_state, Data$base_hit_tag_state) #Tag runs are runs_on_play
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 1" &
                                      Data$events == "single",
                                    "1 1 1 1", Data$base_hit_tag_state)
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 1" &
                                      Data$events == "double",
                                    "0 1 1 1", Data$base_hit_tag_state) #1 tag run
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 1" &
                                      Data$events == "triple",
                                    "0 0 1 1", Data$base_hit_tag_state) #2 tag runs
  
  Data$base_hit_tag_state <- ifelse(Data$base_out_state == "1 1 0 1" &
                                      Data$events == "force_out",
                                    Data$new_state, Data$base_hit_tag_state) #Tag runs are runs_on_play
  
  # Tag runs on base hits (typically lower than actual runs)
  
  Data$base_hit_tag_runs <- ifelse((Data$base_out_state == "1 1 0 1" &
                                      Data$events == "triple") |
                                     (Data$base_out_state == "1 1 0 0" &
                                        Data$events == "triple"),
                                   2, 0)
  
  Data$base_hit_tag_runs <- ifelse((Data$base_out_state == "1 1 0 1" &
                                      Data$events == "double") |
                                     (Data$base_out_state == "1 1 0 0" &
                                        Data$events == "double") |
                                     (Data$base_out_state == "0 1 0 1" &
                                        Data$events == "triple") |
                                     (Data$base_out_state == "0 1 0 0" &
                                        Data$events == "triple") |
                                     (Data$base_out_state == "0 1 0 0" &
                                        Data$events == "double") |
                                     (Data$base_out_state == "0 1 0 1" &
                                        Data$events == "double"), 1, 
                                   Data$base_hit_tag_runs)
  
  Data$base_hit_tag_runs <- ifelse(Data$events == "force_out", Data$runs_on_play,
                                   Data$base_hit_tag_runs)
  
  # Best and worst tag states on flyouts
  
  Data$best_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 0 0 0",
                                    "0 1 0 1", NA)
  
  Data$best_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 0 0 1",
                                    "0 1 0 2", Data$best_result_flyout)
  
  Data$best_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "0 1 0 0",
                                    "0 0 1 1", Data$best_result_flyout)
  
  Data$best_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "0 1 0 1",
                                    "0 0 1 2", Data$best_result_flyout)
  
  Data$best_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 1 0 0",
                                    "0 1 1 1", Data$best_result_flyout)
  
  Data$best_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 1 0 1",
                                    "0 1 1 2", Data$best_result_flyout)
  
  # Worst
  
  Data$worst_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 0 0 0",
                                    "0 0 0 2", NA)
  
  Data$worst_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 0 0 1",
                                    "0 0 0 0", Data$worst_result_flyout)
  
  Data$worst_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "0 1 0 0",
                                    "0 0 0 2", Data$worst_result_flyout)
  
  Data$worst_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "0 1 0 1",
                                    "0 0 0 0", Data$worst_result_flyout)
  
  Data$worst_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 1 0 0",
                                    "0 1 0 2", Data$worst_result_flyout)
  
  Data$worst_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                      Data$base_out_state == "1 1 0 1",
                                    "0 0 0 0", Data$worst_result_flyout)
  
  Data$second_worst_result_flyout <- ifelse(!is.na(Data$tag_att) &
                                              Data$base_out_state == "1 1 0 0",
                                            "0 0 1 2", NA)
  
  # Add in run expectancies
  
  run_matrix_1 <- data.frame(
    base_out_state = c("0 0 0 0", "0 0 0 1", "0 0 0 2",
                       "1 0 0 0", "1 0 0 1", "1 0 0 2",
                       "0 1 0 0", "0 1 0 1", "0 1 0 2",
                       "0 0 1 0", "0 0 1 1", "0 0 1 2",
                       "1 1 0 0", "1 1 0 1", "1 1 0 2",
                       "1 0 1 0", "1 0 1 1", "1 0 1 2",
                       "0 1 1 0", "0 1 1 1", "0 1 1 2",
                       "1 1 1 0", "1 1 1 1", "1 1 1 2"),
    base_out_run_exp = c(0.476, 0.254, 0.097,
                         0.865, 0.508, 0.205,
                         1.073, 0.667, 0.308,
                         1.272, 0.974, 0.377,
                         1.435, 0.902, 0.440,
                         1.753, 1.147, 0.500,
                         2.005, 1.390, 0.548,
                         2.367, 1.508, 0.767)
  )
  
  run_matrix_2 <- data.frame(
    new_state = c("0 0 0 0", "0 0 0 1", "0 0 0 2",
                       "1 0 0 0", "1 0 0 1", "1 0 0 2",
                       "0 1 0 0", "0 1 0 1", "0 1 0 2",
                       "0 0 1 0", "0 0 1 1", "0 0 1 2",
                       "1 1 0 0", "1 1 0 1", "1 1 0 2",
                       "1 0 1 0", "1 0 1 1", "1 0 1 2",
                       "0 1 1 0", "0 1 1 1", "0 1 1 2",
                       "1 1 1 0", "1 1 1 1", "1 1 1 2"),
    new_run_exp = c(0.476, 0.254, 0.097,
                         0.865, 0.508, 0.205,
                         1.073, 0.667, 0.308,
                         1.272, 0.974, 0.377,
                         1.435, 0.902, 0.440,
                         1.753, 1.147, 0.500,
                         2.005, 1.390, 0.548,
                         2.367, 1.508, 0.767)
  )
  
  run_matrix_3 <- data.frame(
    base_hit_tag_state = c("0 0 0 0", "0 0 0 1", "0 0 0 2",
                       "1 0 0 0", "1 0 0 1", "1 0 0 2",
                       "0 1 0 0", "0 1 0 1", "0 1 0 2",
                       "0 0 1 0", "0 0 1 1", "0 0 1 2",
                       "1 1 0 0", "1 1 0 1", "1 1 0 2",
                       "1 0 1 0", "1 0 1 1", "1 0 1 2",
                       "0 1 1 0", "0 1 1 1", "0 1 1 2",
                       "1 1 1 0", "1 1 1 1", "1 1 1 2"),
    base_hit_tag_run_exp = c(0.476, 0.254, 0.097,
                         0.865, 0.508, 0.205,
                         1.073, 0.667, 0.308,
                         1.272, 0.974, 0.377,
                         1.435, 0.902, 0.440,
                         1.753, 1.147, 0.500,
                         2.005, 1.390, 0.548,
                         2.367, 1.508, 0.767)
  )
  
  run_matrix_4 <- data.frame(
    best_result_flyout = c("0 0 0 0", "0 0 0 1", "0 0 0 2",
                       "1 0 0 0", "1 0 0 1", "1 0 0 2",
                       "0 1 0 0", "0 1 0 1", "0 1 0 2",
                       "0 0 1 0", "0 0 1 1", "0 0 1 2",
                       "1 1 0 0", "1 1 0 1", "1 1 0 2",
                       "1 0 1 0", "1 0 1 1", "1 0 1 2",
                       "0 1 1 0", "0 1 1 1", "0 1 1 2",
                       "1 1 1 0", "1 1 1 1", "1 1 1 2"),
    best_result_fly_run_exp = c(0.476, 0.254, 0.097,
                         0.865, 0.508, 0.205,
                         1.073, 0.667, 0.308,
                         1.272, 0.974, 0.377,
                         1.435, 0.902, 0.440,
                         1.753, 1.147, 0.500,
                         2.005, 1.390, 0.548,
                         2.367, 1.508, 0.767)
  )
  
  run_matrix_5 <- data.frame(
    worst_result_flyout = c("0 0 0 0", "0 0 0 1", "0 0 0 2",
                           "1 0 0 0", "1 0 0 1", "1 0 0 2",
                           "0 1 0 0", "0 1 0 1", "0 1 0 2",
                           "0 0 1 0", "0 0 1 1", "0 0 1 2",
                           "1 1 0 0", "1 1 0 1", "1 1 0 2",
                           "1 0 1 0", "1 0 1 1", "1 0 1 2",
                           "0 1 1 0", "0 1 1 1", "0 1 1 2",
                           "1 1 1 0", "1 1 1 1", "1 1 1 2"),
    worst_result_fly_run_exp = c(0.476, 0.254, 0.097,
                                0.865, 0.508, 0.205,
                                1.073, 0.667, 0.308,
                                1.272, 0.974, 0.377,
                                1.435, 0.902, 0.440,
                                1.753, 1.147, 0.500,
                                2.005, 1.390, 0.548,
                                2.367, 1.508, 0.767)
  )
  
  Data <- Data |>
    left_join(run_matrix_1, by = "base_out_state") |>
    left_join(run_matrix_2, by = "new_state") |>
    left_join(run_matrix_3, by = "base_hit_tag_state") |>
    left_join(run_matrix_4, by = "best_result_flyout") |>
    left_join(run_matrix_5, by = "worst_result_flyout")
  
  Data$second_worst_fly_run_exp <- ifelse(!is.na(Data$second_worst_result_flyout),
                                          0.377, NA)
  
  Data$base_hit_tag_runs <- ifelse(is.na(Data$base_hit_tag_runs), 0,
                                   Data$base_hit_tag_runs)
  
  Data$base_hit_tag_dre <- Data$base_hit_tag_run_exp - Data$base_out_run_exp +
    Data$base_hit_tag_runs
  Data$best_result_fly_dre <- Data$best_result_fly_run_exp - Data$base_out_run_exp
  Data$worst_result_fly_dre <- Data$worst_result_fly_run_exp - Data$base_out_run_exp
  Data$second_worst_result_fly_dre <- Data$second_worst_fly_run_exp - Data$base_out_run_exp
    
  
  Data$worst_result_fly_dre <- ifelse(Data$worst_result_flyout == "0 0 0 0",
                                      Data$delta_run_exp, Data$worst_result_fly_dre)
}

# Adding non-tag data to the model where tagging is either a sure-out or sure-safe

{
  library(tidyverse)
  Data2015 <- read_csv("Data2015.csv") |>
    filter(game_type == "R")
  
  M2B2015 <- Data2015 |>
    filter(bb_type %in% c("fly_ball", "popup")) |>
    filter(events == "field_out") |>
    mutate(dist_from_2b = sqrt((hc_x - 125.42)^2 + (hc_y - 147.87)^2)) |>
    filter(dist_from_2b < 11.22508 | dist_from_2b > 118.2761) 
  
  M2B2015$sac_safe_second <- ifelse(M2B2015$dist_from_2b < 11.22508, 0, 1)
  
  M3B2015 <- Data2015 |>
    filter(bb_type %in% c("fly_ball", "popup")) |>
    filter(events == "field_out") |>
    mutate(dist_from_3b = sqrt((hc_x - 100.22)^2 + (hc_y - 173.07)^2)) |>
    filter(dist_from_3b < 25.25 | dist_from_3b > 144.4003)
  
  M3B2015$sac_safe_third <- ifelse(M3B2015$dist_from_3b < 25.25, 0, 1)
  
  M2B2015 <- M2B2015 |>
    select(dist_from_2b, sac_safe_second)
  
  M3B2015 <- M3B2015 |>
    select(dist_from_3b, sac_safe_third)
  
  write_csv(M2B2015, "M2B2015.csv")
  write_csv(M3B2015, "M3B2015.csv")
}


{
  Model2B_tag <- Data2 |>
    filter(!is.na(sac_safe_second)) |>
    filter(!is.na(dist_from_2b)) |>
    select(dist_from_2b, sac_safe_second)
  
  Model2B <- rbind(
    Model2B_tag,
    read_csv("M2B2015.csv"),
    read_csv("M2B2016.csv"),
    read_csv("M2B2017.csv"),
    read_csv("M2B2018.csv"),
    read_csv("M2B2019.csv"),
    read_csv("M2B2020.csv"),
    read_csv("M2B2021.csv"),
    read_csv("M2B2022.csv"),
    read_csv("M2B2023.csv")
  )
  
  write_csv(Model2B, "Model2B.csv")
  
  Model3B_tag <- Data2 |>
    filter(!is.na(sac_safe_third)) |>
    filter(!is.na(dist_from_3b)) |>
    select(dist_from_3b, sac_safe_third)
  
  Model3B <- rbind(
    Model3B_tag,
    read_csv("M3B2015.csv"),
    read_csv("M3B2016.csv"),
    read_csv("M3B2017.csv"),
    read_csv("M3B2018.csv"),
    read_csv("M3B2019.csv"),
    read_csv("M3B2020.csv"),
    read_csv("M3B2021.csv"),
    read_csv("M3B2022.csv"),
    read_csv("M3B2023.csv")
  )
  
  write_csv(Model3B, "Model3B.csv")
}
  
# Modeling to determine likelihood of safety based on hit location

Model2BData <- read_csv("Model2B.csv")

Model2B <- glm(sac_safe_second ~ 0 + dist_from_2b, data = Model2BData)
summary(Model2B)

Model3BData <- read_csv("Model3B.csv")

Model3B <- glm(sac_safe_third ~ 0 + dist_from_3b, data = Model3BData)
summary(Model3B)

df <- add_predictions(Data2, Model2B, var = "prob_sac_safe_second", type = NULL)
Data3 <- add_predictions(df, Model3B, var = "prob_sac_safe_third", type = NULL)

Data3$prob_sac_safe_second <- ifelse(Data3$events %in% c("single", "double",
                                                       "triple"), NA,
                                    Data3$prob_sac_safe_second)
Data3$prob_sac_safe_second <- ifelse(Data3$on_1b == 0 & Data3$on_2b == 1, NA,
                                    Data3$prob_sac_safe_second)
Data3$prob_sac_safe_second <- ifelse(Data3$prob_sac_safe_second >= 1, 1,
                                    Data3$prob_sac_safe_second)
Data3$prob_sac_safe_third <- ifelse(Data3$events %in% c("single", "double",
                                                       "triple"), NA,
                                    Data3$prob_sac_safe_third)
Data3$prob_sac_safe_third <- ifelse(Data3$on_2b == 0 & Data3$on_1b == 1, NA,
                                    Data3$prob_sac_safe_third)
Data3$prob_sac_safe_third <- ifelse(Data3$prob_sac_safe_third >= 1, 1,
                                    Data3$prob_sac_safe_third)


# Calculate tag dre for flyout probabilities

Data3$tag_dre <- ifelse(Data3$base_out_state == "1 0 0 0" & !is.na(Data3$tag_att),
                       (Data3$prob_sac_safe_second * Data3$best_result_fly_dre) +
                         ((1 - Data3$prob_sac_safe_second) * Data3$worst_result_fly_dre), 
                       NA)

Data3$tag_dre <- ifelse(Data3$base_out_state == "1 0 0 1" & !is.na(Data3$tag_att),
                       (Data3$prob_sac_safe_second * Data3$best_result_fly_dre) +
                         ((1 - Data3$prob_sac_safe_second) * Data3$worst_result_fly_dre), 
                       Data3$tag_dre)

Data3$tag_dre <- ifelse(Data3$base_out_state == "0 1 0 0" & !is.na(Data3$tag_att),
                       (Data3$prob_sac_safe_third * Data3$best_result_fly_dre) +
                         ((1 - Data3$prob_sac_safe_third) * Data3$worst_result_fly_dre), 
                       Data3$tag_dre)

Data3$tag_dre <- ifelse(Data3$base_out_state == "0 1 0 1" & !is.na(Data3$tag_att),
                       (Data3$prob_sac_safe_third * Data3$best_result_fly_dre) +
                         ((1 - Data3$prob_sac_safe_third) * Data3$worst_result_fly_dre), 
                       Data3$tag_dre)

Data3$tag_dre <- ifelse(Data3$base_out_state == "1 1 0 0" & !is.na(Data3$tag_att),
                       (Data3$prob_sac_safe_second * Data3$prob_sac_safe_third * 
                          Data3$best_result_fly_dre) +
                         ((1 - Data3$prob_sac_safe_second) * Data3$prob_sac_safe_third *
                            Data3$second_worst_result_fly_dre) +
                         ((1 - Data3$prob_sac_safe_second) * (1 - Data3$prob_sac_safe_third) *
                            Data3$worst_result_fly_dre), Data3$tag_dre)

Data3$tag_dre <- ifelse(Data3$base_out_state == "1 1 0 1" & !is.na(Data3$tag_att),
                       (Data3$prob_sac_safe_second * Data3$prob_sac_safe_third * 
                          Data3$best_result_fly_dre) +
                         (1 - Data3$prob_sac_safe_third) * Data3$worst_result_fly_dre,
                       Data3$tag_dre)

Data3$tag_dre <- ifelse(is.na(Data3$tag_att), 
                             Data3$base_hit_tag_dre,
                       Data3$tag_dre)
# Tag Runs Added

Data3$tag_runs_added <- Data3$tag_dre - Data3$delta_run_exp

# Get rid of walkoff scenarios interfering with 0 0 0 0 new state

Data3$get_rid <- ifelse((Data3$base_out_state == "1 0 0 0" |
                          Data3$base_hit_tag_state == "0 1 0 0" |
                          Data3$base_out_state == "1 1 0 0") &
                         Data3$new_state == "0 0 0 0" &
                         Data3$inning >= 9 &
                         Data3$inning_topbot == "Bot" &
                         Data3$events != "triple_play", 1, 0)

Data3$get_rid <- ifelse((Data3$base_out_state == "1 0 0 1" |
                          Data3$base_hit_tag_state == "0 1 0 1" |
                          Data3$base_out_state == "1 1 0 1") &
                         Data3$new_state == "0 0 0 0" &
                         Data3$inning >= 9 &
                         Data3$inning_topbot == "Bot" &
                         Data3$events != "double_play", 1, Data3$get_rid)

Data3$get_rid <- ifelse(is.na(Data3$get_rid), 0, Data3$get_rid)

Data4 <- Data3 |>
  filter(get_rid == 0)

# One last minute touch

Data4$batting_team <- ifelse(Data4$inning_topbot == "Bot", Data4$home_team, Data4$away_team)
Data5 <- Data4[!duplicated(Data4), ]

Data5$prob_sac_safe_third <- ifelse(Data5$dist_from_3b < 25.25, 0,
                                    Data5$prob_sac_safe_third)
Data5$prob_sac_safe_third <- ifelse(Data5$dist_from_3b > 144.4003, 1,
                                    Data5$prob_sac_safe_third)

Data5$prob_sac_safe_second <- ifelse(Data5$dist_from_2b < 11.22508, 0,
                                     Data5$prob_sac_safe_second)
Data5$prob_sac_safe_second <- ifelse(Data5$dist_from_2b > 118.2761, 1,
                                     Data5$prob_sac_safe_second)

Data5$dist_from_2b <- Data5$dist_from_2b * 2.5
Data5$dist_from_3b <- Data5$dist_from_3b * 2.5

# Save

write_csv(Data5, "Data.csv")
