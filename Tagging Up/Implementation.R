{
  library(tidyverse)
  
  arm <- read_csv("arm_strength-2024.csv") |>
    select(player_id, arm_overall) |>
    rename(outfielder_id = player_id)
  
  sprint <- read_csv("sprint_speed-2024.csv") |>
    select(player_id, sprint_speed) |>
    rename(runner_id = player_id)
  
  Data2024 <- read_csv("Data2024.csv") |>
    select(events, description, bb_type, des, on_1b, on_2b, on_3b, outs_when_up, 
           hc_x, hc_y, fielder_7, fielder_8, fielder_9, delta_run_exp, home_team,
           away_team, inning_topbot, bat_score, post_bat_score) |>
    filter(!is.na(events)) |>
    filter(events != "home_run") |>
    filter(bb_type %in% c("fly_ball")) |>
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
  
  Singles <- Data2024 |>
    filter(events == "single")
  
  Doubles <- Data2024 |>
    filter(events == "double")
  
  Triples <- Data2024 |>
    filter(events == "triple")
  
  Force_Outs <- Data2024 |>
    filter(events == "force_out")
  
  Flyouts <- Data2024 |>
    filter(events %in% c("field_out", "double_play")) 
  
  Flyouts_Selected <- Flyouts |>
    select(sprint_speed, arm_overall, dist_from_base)
  
  model <- readRDS("bayesian_model.rds")
  
  Flyouts <- Flyouts |>
    mutate(prediction = predict(model, newdata = Flyouts_Selected)) |>
    mutate(pred = as.double(prediction)) |>
    mutate(pred = pred - 1) |>
    mutate(pred = ifelse(dist_from_base < 60.99514, 0, pred)) |>
    mutate(pred = ifelse(dist_from_base > 124.1073, 1, pred))
  
  Flyouts <- Flyouts |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 0" & pred == 1,
                                    "0 1 0 1", NA)) |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 1" & pred == 1,
                                    "0 1 0 2", predicted_state)) |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 0" & pred == 0,
                                    "0 0 0 2", predicted_state)) |>
    mutate(predicted_state = ifelse(base_out_state == "1 0 0 1" & pred == 0,
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
  
  SecondBase2024 <- rbind(Flyouts, Singles, Doubles, Triples, Force_Outs)
  rm(Flyouts, Singles, Doubles, Triples, Force_Outs, model, Data2024, arm, sprint, 
     Flyouts_Selected)
  
    library(tidyverse)
    
    arm <- read_csv("arm_strength-2024.csv") |>
      select(player_id, arm_overall) |>
      rename(outfielder_id = player_id)
    
    sprint <- read_csv("sprint_speed-2024.csv") |>
      select(player_id, sprint_speed) |>
      rename(runner_id = player_id)
    
    Data2024 <- read_csv("Data2024.csv") |>
      select(events, description, bb_type, des, on_1b, on_2b, on_3b, outs_when_up, 
             hc_x, hc_y, fielder_7, fielder_8, fielder_9, delta_run_exp, home_team,
             away_team, inning_topbot, bat_score, post_bat_score) |>
      filter(!is.na(events)) |>
      filter(events != "home_run") |>
      filter(bb_type %in% c("fly_ball")) |>
      filter(!is.na(hc_x) & !is.na(hc_y)) |>
      mutate(dist_from_base = sqrt((hc_x - 100.22)^2 + (hc_y - 173.07)^2)) |>
      mutate(base_out_state = ifelse(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) & 
                                       outs_when_up == 0, "0 1 0 0", NA)) |>
      mutate(base_out_state = ifelse(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) & 
                                       outs_when_up == 1, "0 1 0 1", base_out_state)) |>
      filter(base_out_state %in% c("0 1 0 0", "0 1 0 1")) |>
      mutate(tag_attempt = ifelse(grepl("to 3rd", des) | grepl("out at 3rd", des), 
                                  1, 0)) |>
      
      #Join Data into arm strength and sprint speed data
      
      mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
      mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
      mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
      rename(runner_id = on_2b) |>
      left_join(arm, by = "outfielder_id") |>
      left_join(sprint, by = "runner_id") |>
      filter(!is.na(arm_overall) & !is.na(sprint_speed))
    
    Singles <- Data2024 |>
      filter(events == "single")
    
    Doubles <- Data2024 |>
      filter(events == "double")
    
    Triples <- Data2024 |>
      filter(events == "triple")
    
    Force_Outs <- Data2024 |>
      filter(events == "force_out")
    
    Flyouts <- Data2024 |>
      filter(events %in% c("field_out", "double_play")) 
    
    Flyouts_Selected <- Flyouts |>
      select(dist_from_base, arm_overall, sprint_speed)
    
    model <- readRDS("bayesian_model.rds")
    
    Flyouts <- Flyouts |>
      mutate(prediction = predict(model, newdata = Flyouts_Selected)) |>
      mutate(pred = as.double(prediction)) |>
      mutate(pred = pred - 1) |>
      mutate(pred = ifelse(dist_from_base < 60.99514, 0, pred)) |>
      mutate(pred = ifelse(dist_from_base > 124.1073, 1, pred))
    
    Flyouts <- Flyouts |>
      mutate(predicted_state = ifelse(base_out_state == "0 1 0 0" & pred == 1,
                                      "0 0 1 1", NA)) |>
      mutate(predicted_state = ifelse(base_out_state == "0 1 0 1" & pred == 1,
                                      "0 0 1 2", predicted_state)) |>
      mutate(predicted_state = ifelse(base_out_state == "0 1 0 0" & pred == 0,
                                      "0 0 0 2", predicted_state)) |>
      mutate(predicted_state = ifelse(base_out_state == "0 1 0 1" & pred == 0,
                                      "0 0 0 0", predicted_state)) |>
      mutate(tag_run_exp = ifelse(predicted_state == "0 0 1 1", 0.974, NA)) |>
      mutate(tag_run_exp = ifelse(predicted_state == "0 0 1 2", 0.377, tag_run_exp)) |>
      mutate(tag_run_exp = ifelse(predicted_state == "0 0 0 2", 0.097, tag_run_exp)) |>
      mutate(tag_run_exp = ifelse(predicted_state == "0 0 0 0", 0, tag_run_exp)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 0", 1.073, NA)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 1", 0.667, current_run_exp)) |>
      mutate(tag_dre = tag_run_exp - current_run_exp) |>
      mutate(tag_runs_added = tag_dre - delta_run_exp) |>
      mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
      select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
    
    Singles <- Singles |>
      mutate(prediction = NA) |>
      mutate(tag_attempt = NA) |>
      mutate(tag_state = ifelse(base_out_state == "0 1 0 0", "1 0 1 0", NA)) |>
      mutate(tag_state = ifelse(base_out_state == "0 1 0 1", "1 0 1 1", tag_state)) |>
      mutate(tag_run_exp = ifelse(tag_state == "1 0 1 0", 1.753, NA)) |>
      mutate(tag_run_exp = ifelse(tag_state == "1 0 1 1", 1.147, tag_run_exp)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 0", 1.073, NA)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 1", 0.667, current_run_exp)) |>
      mutate(tag_dre = tag_run_exp - current_run_exp) |>
      mutate(tag_runs_added = tag_dre - delta_run_exp) |>
      mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
      select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
    
    Doubles <- Doubles |>
      mutate(tag_attempt = NA) |>
      mutate(prediction = NA) |>
      mutate(tag_state = ifelse(base_out_state == "0 1 0 0", "0 1 0 0", NA)) |>
      mutate(tag_state = ifelse(base_out_state == "0 1 0 1", "0 1 0 1", tag_state)) |>
      mutate(tag_run_exp = ifelse(tag_state == "0 1 0 0", 2.073, NA)) |>
      mutate(tag_run_exp = ifelse(tag_state == "0 1 0 1", 1.667, tag_run_exp)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 0", 1.073, NA)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 1", 0.667, current_run_exp)) |>
      mutate(tag_dre = tag_run_exp - current_run_exp) |>
      mutate(tag_runs_added = tag_dre - delta_run_exp) |>
      mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
      select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
    
    Triples <- Triples |>
      mutate(tag_attempt = NA) |>
      mutate(prediction = NA) |>
      mutate(tag_state = ifelse(base_out_state == "0 1 0 0", "0 0 1 0", NA)) |>
      mutate(tag_state = ifelse(base_out_state == "0 1 0 1", "0 0 1 1", tag_state)) |>
      mutate(tag_run_exp = ifelse(tag_state == "0 0 1 0", 2.272, NA)) |>
      mutate(tag_run_exp = ifelse(tag_state == "0 0 1 1", 1.974, tag_run_exp)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 0", 1.073, NA)) |>
      mutate(current_run_exp = ifelse(base_out_state == "0 1 0 1", 0.667, current_run_exp)) |>
      mutate(tag_dre = tag_run_exp - current_run_exp) |>
      mutate(tag_runs_added = tag_dre - delta_run_exp) |>
      mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
      select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
    
    Force_Outs <- Force_Outs |>
      mutate(batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
      mutate(tag_attempt = NA) |>
      mutate(prediction = NA) |>
      mutate(tag_runs_added = 0) |>
      select(batting_team, dist_from_base, tag_runs_added, tag_attempt, prediction)
    
    ThirdBase2024 <- rbind(Flyouts, Singles, Doubles, Triples, Force_Outs)
    rm(Flyouts, Singles, Doubles, Triples, Force_Outs, model, Data2024, arm, sprint, 
       Flyouts_Selected)
}

Data <- rbind(SecondBase2021, ThirdBase2021,
              SecondBase2022, ThirdBase2022,
              SecondBase2023, ThirdBase2023,
              SecondBase2024, ThirdBase2024)

Data |>
  filter(dist_from_base >= 115) |>
  summarise(.by = "batting_team",
            Rate = mean(tag_attempt, na.rm = TRUE)) |>
  View()
  ggplot(aes(x = reorder(batting_team, -Rate), y = Rate)) +
  geom_bar(stat = "identity")


