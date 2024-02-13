{
  library(tidyverse)
  
  arm <- read_csv("arm_strength-2023.csv") |>
    select(player_id, arm_overall) |>
    rename(outfielder_id = player_id)
  
  sprint <- read_csv("sprint_speed-2023.csv") |>
    select(player_id, sprint_speed) |>
    rename(runner_id = player_id)
  
  Data2023 <- read_csv("Data2023.csv") |>
    select(events, description, bb_type, des, on_1b, on_2b, on_3b, outs_when_up, 
           hc_x, hc_y, fielder_7, fielder_8, fielder_9, delta_run_exp, home_team,
           away_team, inning_topbot, bat_score, post_bat_score) |>
    filter(!is.na(events)) |>
    filter(events != "home_run") |>
    filter(bb_type %in% c("fly_ball", "popup")) |>
    filter(!is.na(hc_x) & !is.na(hc_y)) |>
    mutate(dist_from_base = sqrt((hc_x - 125.42)^2 + (hc_y - 147.87)^2)) |>
    mutate(base_out_state = ifelse(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 0, "1 0 0 0", NA)) |>
    mutate(base_out_state = ifelse(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 1, "1 0 0 1", base_out_state)) |>
    filter(base_out_state %in% c("1 0 0 0", "1 0 0 1")) |>
    mutate(tag_attempt = ifelse(grepl("to 2nd", des) | grepl("out at 2nd", des), 
                                1, 0)) |>
    
    #Join Data into arm strength and sprint speed data
    
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_1b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    filter(!is.na(arm_overall) & !is.na(sprint_speed))
  
  Singles <- Data2023 |>
    filter(events == "single")
  
  Doubles <- Data2023 |>
    filter(events == "double")
  
  Triples <- Data2023 |>
    filter(events == "triple")
  
  Force_Outs <- Data2023 |>
    filter(events == "force_out")
  
  Flyouts <- Data2023 |>
    filter(events %in% c("field_out", "double_play")) 
  
  Flyouts_Selected <- Flyouts |>
    select(dist_from_base, arm_overall, sprint_speed)
  
  Flyouts$prediction <- predict(cv_model, as.matrix(Flyouts_Selected))
  
  Flyouts <- Flyouts |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 0" & prediction == "Yes",
           "0 1 0 1", NA)) |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 1" & prediction == "Yes",
           "0 1 0 2", predicted_state)) |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 0" & prediction == "No",
           "0 0 0 2", predicted_state)) |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 1" & prediction == "No",
           "0 0 0 0", predicted_state)) |>
    mutate(tag_run_exp = ifelse(predicted_state == "0 1 0 1", 0.667, NA)) |>
    mutate(tag_run_exp = ifelse(predicted_state == "0 1 0 2", 0.308, tag_run_exp)) |>
    mutate(tag_run_exp = ifelse(predicted_state == "0 0 0 2", 0.097, tag_run_exp)) |>
    mutate(tag_run_exp = ifelse(predicted_state == "0 0 0 0", delta_run_exp, tag_run_exp)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 0", 0.865, NA)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 1", 0.508, current_run_exp)) |>
    mutate(tag_dre = tag_run_exp - current_run_exp) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
    select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
  
  Singles <- Singles |>
    mutate(tag_attempt = NA) |>
    mutate(prediction = NA) |>
    mutate(tag_state = ifelse(base_out_state == "1 0 0 0", "1 1 0 0", NA)) |>
    mutate(tag_state = ifelse(base_out_state == "1 0 0 1", "1 1 0 1", tag_state)) |>
    mutate(tag_run_exp = ifelse(tag_state == "1 1 0 0", 1.435, NA)) |>
    mutate(tag_run_exp = ifelse(tag_state == "1 1 0 1", 0.902, tag_run_exp)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 0", 0.865, NA)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 1", 0.508, current_run_exp)) |>
    mutate(tag_dre = tag_run_exp - current_run_exp) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
    mutate(tag_attempt = NA) |>
    select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
  
  Doubles <- Doubles |>
    mutate(tag_attempt = NA) |>
    mutate(prediction = NA) |>
    mutate(tag_state = ifelse(base_out_state == "1 0 0 0", "0 1 1 0", NA)) |>
    mutate(tag_state = ifelse(base_out_state == "1 0 0 1", "0 1 1 1", tag_state)) |>
    mutate(tag_run_exp = ifelse(tag_state == "0 1 1 0", 2.005, NA)) |>
    mutate(tag_run_exp = ifelse(tag_state == "0 1 1 1", 1.390, tag_run_exp)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 0", 0.865, NA)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 1", 0.508, current_run_exp)) |>
    mutate(tag_dre = tag_run_exp - current_run_exp) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
    mutate(tag_attempt = NA) |>
    select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
  
  Triples <- Triples |>
    mutate(tag_attempt = NA) |>
    mutate(prediction = NA) |>
    mutate(tag_state = ifelse(base_out_state == "1 0 0 0", "0 0 1 0", NA)) |>
    mutate(tag_state = ifelse(base_out_state == "1 0 0 1", "0 0 1 1", tag_state)) |>
    mutate(tag_run_exp = ifelse(tag_state == "0 0 1 0", 2.272, NA)) |>
    mutate(tag_run_exp = ifelse(tag_state == "0 0 1 1", 1.974, tag_run_exp)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 0", 0.865, NA)) |>
    mutate(current_run_exp = ifelse(base_out_state == "1 0 0 1", 0.508, current_run_exp)) |>
    mutate(tag_dre = tag_run_exp - current_run_exp) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
    mutate(tag_attempt = NA) |>
    select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
  
  Force_Outs <- Force_Outs |>
    mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
    mutate(tag_attempt = NA) |>
    mutate(prediction = NA) |>
    mutate(tag_runs_added = 0) |>
    select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
  
  SecondBase <- rbind(Flyouts, Singles, Doubles, Triples, Force_Outs)
  
  write_csv(SecondBase, "SecondBase.csv")
}

