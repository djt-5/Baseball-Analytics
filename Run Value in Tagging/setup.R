# Run block to load and organize data ####
# (do separate for each years 2015 to 2023 as to not overwhelm the RAM) 

{
  library(tidyverse)
  statcast2023 <- read_csv("statcast2023.csv")
  selected_data <- statcast2023 |>
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
  
  Data2023 <- bind_rows(List_2)
  
  Data2023$new_state <- ifelse(is.na(Data2023$new_state), "0 0 0 0",
                               Data2023$new_state)
  
  Data2023 <- Data2023 |>
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
  
  
  
  write_csv(Data2023, "Data2023.csv")
}

# Combine all the data ####

{
  library(tidyverse)
  Data <- rbind(
    read_csv("Data2015.csv"),
    read_csv("Data2016.csv"),
    read_csv("Data2017.csv"),
    read_csv("Data2018.csv"),
    read_csv("Data2019.csv"),
    read_csv("Data2020.csv"),
    read_csv("Data2021.csv"),
    read_csv("Data2022.csv"),
    read_csv("Data2023.csv")
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
           estimated_ba_using_speedangle) 
  
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
  
  # Modeling to determine likelihood of safety based on hit location
  
  #Model_1_Data <- Data |>
  #  filter(!is.na(sac_safe_second))
  #Model_1 <- lm(sac_safe_second ~ hit_distance_sc, data = Model_1_Data)
  #summary(Model_1)
  
  Data$prob_sac_safe_second <- ifelse(!is.na(Data$tag_att) & 
                                        (Data$base_out_state == "1 0 0 0" |
                                           Data$base_out_state == "1 0 0 1" |
                                           Data$base_out_state == "1 1 0 0" |
                                           Data$base_out_state == "1 1 0 1"),
                                      0.055315 + (0.002242*Data$hit_distance_sc), 
                                      NA)
  
  Model_2_Data <- Data |>
    filter(!is.na(sac_safe_third))
  Model_2 <- lm(sac_safe_third ~ hit_distance_sc + spray_angle, data = Model_2_Data)
  summary(Model_2)
  
  Data$prob_sac_safe_third <- ifelse(!is.na(Data$tag_att) & 
                                       (Data$base_out_state == "0 1 0 0" |
                                          Data$base_out_state == "0 1 0 1" |
                                          Data$base_out_state == "1 1 0 0" |
                                          Data$base_out_state == "1 1 0 1"),
                                     6.315e-01 + (9.584e-04*Data$hit_distance_sc) + 
                                        (7.086e-02*Data$spray_angle), NA)
  
  # Calculate tag dre for flyout probabilities
  
  Data$tag_dre <- ifelse(Data$base_out_state == "1 0 0 0" & !is.na(Data$tag_att),
                         (Data$prob_sac_safe_second * Data$best_result_fly_dre) +
                           ((1 - Data$prob_sac_safe_second) * Data$worst_result_fly_dre), 
                         NA)
  
  Data$tag_dre <- ifelse(Data$base_out_state == "1 0 0 1" & !is.na(Data$tag_att),
                         (Data$prob_sac_safe_second * Data$best_result_fly_dre) +
                           ((1 - Data$prob_sac_safe_second) * Data$worst_result_fly_dre), 
                         Data$tag_dre)
  
  Data$tag_dre <- ifelse(Data$base_out_state == "0 1 0 0" & !is.na(Data$tag_att),
                         (Data$prob_sac_safe_third * Data$best_result_fly_dre) +
                           ((1 - Data$prob_sac_safe_third) * Data$worst_result_fly_dre), 
                         Data$tag_dre)
  
  Data$tag_dre <- ifelse(Data$base_out_state == "0 1 0 1" & !is.na(Data$tag_att),
                         (Data$prob_sac_safe_third * Data$best_result_fly_dre) +
                           ((1 - Data$prob_sac_safe_third) * Data$worst_result_fly_dre), 
                         Data$tag_dre)
  
  Data$tag_dre <- ifelse(Data$base_out_state == "1 1 0 0" & !is.na(Data$tag_att),
                         (Data$prob_sac_safe_second * Data$prob_sac_safe_third * 
                            Data$best_result_fly_dre) +
                           ((1 - Data$prob_sac_safe_second) * Data$prob_sac_safe_third *
                              Data$second_worst_result_fly_dre) +
                           ((1 - Data$prob_sac_safe_second) * (1 - Data$prob_sac_safe_third) *
                              Data$worst_result_fly_dre), Data$tag_dre)
  
  Data$tag_dre <- ifelse(Data$base_out_state == "1 1 0 1" & !is.na(Data$tag_att),
                         (Data$prob_sac_safe_second * Data$prob_sac_safe_third * 
                            Data$best_result_fly_dre) +
                           (1 - Data$prob_sac_safe_third) * Data$worst_result_fly_dre,
                         Data$tag_dre)
  
  Data$tag_dre <- ifelse(is.na(Data$tag_att), 
                               Data$base_hit_tag_dre,
                         Data$tag_dre)
  # Tag Runs Added
  
  Data$tag_runs_added <- Data$tag_dre - Data$delta_run_exp
  
  # Get rid of walkoff scenarios interfering with 0 0 0 0 new state
  
  Data$get_rid <- ifelse((Data$base_out_state == "1 0 0 0" |
                            Data$base_hit_tag_state == "0 1 0 0" |
                            Data$base_out_state == "1 1 0 0") &
                           Data$new_state == "0 0 0 0" &
                           Data$inning >= 9 &
                           Data$inning_topbot == "Bot" &
                           Data$events != "triple_play", 1, 0)
  
  Data$get_rid <- ifelse((Data$base_out_state == "1 0 0 1" |
                            Data$base_hit_tag_state == "0 1 0 1" |
                            Data$base_out_state == "1 1 0 1") &
                           Data$new_state == "0 0 0 0" &
                           Data$inning >= 9 &
                           Data$inning_topbot == "Bot" &
                           Data$events != "double_play", 1, Data$get_rid)
  
  Data$get_rid <- ifelse(is.na(Data$get_rid), 0, Data$get_rid)
  
  Data <- Data |>
    filter(get_rid == 0)
  
  # One last minute touch
  
  Data$batting_team <- ifelse(Data$inning_topbot == "Bot", Data$home_team, Data$away_team)
  Data <- Data[!duplicated(Data), ]
  
  # Save
  
  write_csv(Data, "Data.csv")
}
