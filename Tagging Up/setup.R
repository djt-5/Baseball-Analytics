{
  library(tidyverse)
  
  arm <- read_csv("arm_strength-2023.csv") |>
    select(player_id, arm_overall) |>
    rename(outfielder_id = player_id)
  
  sprint <- read_csv("sprint_speed-2023.csv") |>
    select(player_id, sprint_speed) |>
    rename(runner_id = player_id)
  
  Data <- read_csv("Data2023.csv") |>
    filter(bb_type %in% c("fly_ball", "popup", "line_drive")) |>
    filter(!grepl("error", des)) |>
    filter(outs_when_up < 2) |>
    filter(events %in% c("field_out", "double_play", "sac_fly_double_play", "sac_fly")) 
  
  SecondBase <- Data |>
    mutate(dist_from_base = sqrt((hc_x - 125.42)^2 + (hc_y - 147.87)^2)) |>
    mutate(base_out_state = ifelse(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 0, "1 0 0 0", NA)) |>
    mutate(base_out_state = ifelse(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 1, "1 0 0 1", base_out_state)) |>
    filter(base_out_state %in% c("1 0 0 0", "1 0 0 1")) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_1b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    filter(!is.na(arm_overall) & !is.na(sprint_speed)) |>
    mutate(tag_attempt = ifelse(grepl("to 2nd", des) | grepl("out at 2nd", des), 
                                1, 0)) |>
    filter(tag_attempt == 1) |>
    mutate(safe = ifelse(grepl("to 2nd", des), 1, NA)) |>
    mutate(safe = ifelse(grepl("out at 2nd", des), 0, safe)) |>
    select(safe, sprint_speed, arm_overall, dist_from_base)
  
  ThirdBase <- Data |>
    mutate(dist_from_base = sqrt((hc_x - 100.22)^2 + (hc_y - 173.07)^2)) |>
    mutate(base_out_state = ifelse(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 0, "0 1 0 0", NA)) |>
    mutate(base_out_state = ifelse(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 1, "0 1 0 1", base_out_state)) |>
    filter(base_out_state %in% c("0 1 0 0", "0 1 0 1")) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_2b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    filter(!is.na(arm_overall) & !is.na(sprint_speed)) |>
    mutate(tag_attempt = ifelse(grepl("to 3rd", des) | grepl("out at 3rd", des), 
                                1, 0)) |>
    filter(tag_attempt == 1) |>
    mutate(safe = ifelse(grepl("to 3rd", des), 1, NA)) |>
    mutate(safe = ifelse(grepl("out at 3rd", des), 0, safe)) |>
    select(safe, sprint_speed, arm_overall, dist_from_base)
  
  HomePlate <- Data |>
    mutate(dist_from_base = hit_distance_sc/2.5) |>
    mutate(base_out_state = ifelse(is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) & 
                                     outs_when_up == 0, "0 0 1 0", NA)) |>
    mutate(base_out_state = ifelse(is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) & 
                                     outs_when_up == 1, "0 0 1 1", base_out_state)) |>
    filter(base_out_state %in% c("0 0 1 0", "0 0 1 1")) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_3b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    filter(!is.na(arm_overall) & !is.na(sprint_speed)) |>
    mutate(tag_attempt = ifelse(grepl("scores", des) | grepl("out at home", des), 
                                1, 0)) |>
    filter(tag_attempt == 1) |>
    mutate(safe = ifelse(grepl("scores", des), 1, NA)) |>
    mutate(safe = ifelse(grepl("out at home", des), 0, safe)) |>
    select(safe, sprint_speed, arm_overall, dist_from_base)
  
  Bases2023 <- rbind(SecondBase, ThirdBase, HomePlate)
  rm(arm, sprint, SecondBase, ThirdBase, HomePlate, Data)
}

Tags <- rbind(Bases2021, Bases2022, Bases2023, Bases2023)
write_csv(Tags, "Tags.csv")

mean(Tags$safe)
