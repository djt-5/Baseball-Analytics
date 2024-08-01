{
  library(tidyverse)
  Data <- read_csv("Data2024.csv")
  
  arm <- read_csv("arm_strength-2024.csv") |>
    select(player_id, arm_of) |>
    rename(outfielder_id = player_id)
  
  sprint <- read_csv("sprint_speed-2024.csv") |>
    select(player_id, sprint_speed) |>
    rename(runner_id = player_id)
  
  run_exp <- read_csv("run_exp.csv") |>
    mutate(scenario = paste(outs_when_up, on_1b, on_2b, on_3b, balls, strikes)) |>
    select(scenario, run_exp)
  
  worst_run_exp <- read_csv("run_exp.csv") |>
    mutate(worst_scenario = paste(outs_when_up, on_1b, on_2b, on_3b, balls, strikes)) |>
    rename(worst_exp = run_exp) |>
    select(worst_scenario, worst_exp)
  
  best_run_exp <- read_csv("run_exp.csv") |>
    mutate(best_scenario = paste(outs_when_up, on_1b, on_2b, on_3b, balls, strikes)) |>
    rename(best_exp = run_exp) |>
    select(best_scenario, best_exp)
  
  tag_run_exp <- read_csv("run_exp.csv") |>
    mutate(tag_scenario = paste(outs_when_up, on_1b, on_2b, on_3b, balls, strikes)) |>
    rename(tag_run_exp = run_exp) |>
    select(tag_scenario, tag_run_exp)
  
  SecondBaseFlyouts <- Data |>
    filter(bb_type == "fly_ball") |>
    filter(events %in% c("field_out", "double_play")) |>
    filter(outs_when_up < 2) |>
    filter(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b)) |>
    mutate(tag_att = ifelse(grepl("to 2nd", des) | grepl("out at 2nd", des), 1, 0)) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_1b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    mutate(scenario = paste(outs_when_up, 1, 0, 0, balls, strikes)) |>
    mutate(worst_scenario = paste(outs_when_up + 2, 0, 0, 0, 0, 0)) |>
    mutate(best_scenario = paste(outs_when_up + 1, 0, 1, 0, 0, 0)) |>
    mutate(worst_scenario = ifelse(outs_when_up == 1, paste(0, 0, 0, 0, 0, 0), worst_scenario)) |>
    left_join(run_exp, by = "scenario") |>
    left_join(worst_run_exp, by = "worst_scenario") |>
    left_join(best_run_exp, by = "best_scenario") |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    mutate(dist_from_base = sqrt((location_x)^2 + (location_y - 126)^2)) |>
    mutate(worst_exp = ifelse(worst_scenario == paste(0, 0, 0, 0, 0, 0), 0, worst_exp)) |>
    filter(!is.na(dist_from_base) & !is.na(arm_of) & !is.na(sprint_speed)) |>
    mutate(prob = (exp(0.0077008*dist_from_base + 0.0102017*sprint_speed - 0.0222293*arm_of)) /
             (1 + (exp(0.0077008*dist_from_base + 0.0102017*sprint_speed - 0.0222293*arm_of)))) |>
    mutate(pred = ifelse(prob >= 0.5, 1, 0)) |>
    mutate(tag_dre = ifelse(pred == 1, best_exp - run_exp, worst_exp - run_exp)) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batter_team = ifelse(inning_topbot == "Top", away_team, home_team)) |>
    select(batter_team, tag_runs_added, dist_from_base, launch_angle, outs_when_up, babip_value,
           tag_att)
  
  ThirdBaseFlyouts <- Data |>
    filter(bb_type == "fly_ball") |>
    filter(events %in% c("field_out", "double_play")) |>
    filter(outs_when_up < 2) |>
    filter(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) |>
    mutate(tag_att = ifelse(grepl("to 3rd", des) | grepl("out at 3rd", des), 1, 0)) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_2b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    mutate(scenario = paste(outs_when_up, 0, 1, 0, balls, strikes)) |>
    mutate(worst_scenario = paste(outs_when_up + 2, 0, 0, 0, 0, 0)) |>
    mutate(best_scenario = paste(outs_when_up + 1, 0, 0, 1, 0, 0)) |>
    mutate(worst_scenario = ifelse(outs_when_up == 1, paste(0, 0, 0, 0, 0, 0), worst_scenario)) |>
    left_join(run_exp, by = "scenario") |>
    left_join(worst_run_exp, by = "worst_scenario") |>
    left_join(best_run_exp, by = "best_scenario") |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    mutate(dist_from_base = sqrt((location_x + 63)^2 + (location_y - 63)^2)) |>
    mutate(worst_exp = ifelse(worst_scenario == paste(0, 0, 0, 0, 0, 0), 0, worst_exp)) |>
    filter(!is.na(dist_from_base) & !is.na(arm_of) & !is.na(sprint_speed)) |>
    mutate(prob = (exp(0.0077008*dist_from_base + 0.0102017*sprint_speed - 0.0222293*arm_of)) /
             (1 + (exp(0.0077008*dist_from_base + 0.0102017*sprint_speed - 0.0222293*arm_of)))) |>
    mutate(pred = ifelse(prob >= 0.5, 1, 0)) |>
    mutate(tag_dre = ifelse(pred == 1, best_exp - run_exp, worst_exp - run_exp)) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batter_team = ifelse(inning_topbot == "Top", away_team, home_team)) |>
    select(batter_team, tag_runs_added, dist_from_base, launch_angle, outs_when_up, babip_value,
           tag_att)
  
  SecondBaseHits <- Data |>
    filter(bb_type == "fly_ball") |>
    filter(events %in% c("single", "double", "triple")) |>
    filter(outs_when_up < 2) |>
    filter(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b)) |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    mutate(dist_from_base = sqrt((location_x)^2 + (location_y - 126)^2)) |>
    mutate(scenario = paste(outs_when_up, 1, 0, 0, balls, strikes)) |>
    mutate(tag_scenario = ifelse(events == "single", paste(outs_when_up, 1, 1, 0, 0, 0), NA)) |>
    mutate(tag_scenario = ifelse(events == "double", paste(outs_when_up, 0, 1, 1, 0, 0), tag_scenario)) |>
    mutate(tag_scenario = ifelse(events == "triple", paste(outs_when_up, 0, 0, 1, 0, 0), tag_scenario)) |> #Add a run
    left_join(run_exp, by = "scenario") |>
    left_join(tag_run_exp, by = "tag_scenario") |>
    mutate(tag_run_exp = ifelse(events == "triple", tag_run_exp + 1, tag_run_exp)) |>
    mutate(tag_dre = tag_run_exp - run_exp) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batter_team = ifelse(inning_topbot == "Top", away_team, home_team)) |>
    select(batter_team, tag_runs_added, dist_from_base, launch_angle, outs_when_up, babip_value)
  
  ThirdBaseHits <- Data |>
    filter(bb_type == "fly_ball") |>
    filter(events %in% c("single", "double", "triple")) |>
    filter(outs_when_up < 2) |>
    filter(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    mutate(dist_from_base = sqrt((location_x + 63)^2 + (location_y - 63)^2)) |>
    mutate(scenario = paste(outs_when_up, 0, 1, 0, balls, strikes)) |>
    mutate(tag_scenario = ifelse(events == "single", paste(outs_when_up, 1, 0, 1, 0, 0), NA)) |>
    mutate(tag_scenario = ifelse(events == "double", paste(outs_when_up, 0, 1, 0, 0, 0), tag_scenario)) |> #Add a run
    mutate(tag_scenario = ifelse(events == "triple", paste(outs_when_up, 0, 0, 1, 0, 0), tag_scenario)) |> #Add a run
    left_join(run_exp, by = "scenario") |>
    left_join(tag_run_exp, by = "tag_scenario") |>
    mutate(tag_run_exp = ifelse(events %in% c("double", "triple"), tag_run_exp + 1, tag_run_exp)) |>
    mutate(tag_dre = tag_run_exp - run_exp) |>
    mutate(tag_runs_added = tag_dre - delta_run_exp) |>
    mutate(batter_team = ifelse(inning_topbot == "Top", away_team, home_team)) |>
    select(batter_team, tag_runs_added, dist_from_base, launch_angle, outs_when_up, babip_value)
    
    
}

FlyBalls <- rbind(SecondBaseFlyouts, ThirdBaseFlyouts, SecondBaseHits, ThirdBaseHits)

#Tag Runs Added

FlyBalls |>
  filter(dist_from_base >= 225) |>
  summarise(.by = "batter_team",
            `Tag Runs Added` = sum(tag_runs_added, na.rm = TRUE)) |>
  ggplot(aes(x= reorder(batter_team,-`Tag Runs Added`),`Tag Runs Added`))+
  geom_bar(stat ="identity") +
  xlab("Team") +
  ylab("Tag Runs Added")

Flyouts <- rbind(SecondBaseFlyouts, ThirdBaseFlyouts)

#Tag Attempt Rate

Flyouts |>
  filter(dist_from_base >= 225) |>
  summarise(.by = "batter_team",
            `Tag Attempt Rate` = mean(tag_att, na.rm = TRUE)) |>
  ggplot(aes(x= reorder(batter_team,-`Tag Attempt Rate`),`Tag Attempt Rate`))+
  geom_bar(stat ="identity") +
  xlab("Team") +
  ylab("Tag Attempt Rate")
