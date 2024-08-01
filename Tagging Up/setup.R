{
  library(tidyverse)
  
  Data2024 <- read_csv("Data2024.csv")
  
  arm <- read_csv("arm_strength-2024.csv") |>
    select(player_id, arm_of) |>
    rename(outfielder_id = player_id)
  
  sprint <- read_csv("sprint_speed-2024.csv") |>
    select(player_id, sprint_speed) |>
    rename(runner_id = player_id)
  
  Data <- Data2024 |>
    filter(bb_type %in% c("line_drive", "fly_ball", "popup")) |>
    filter(outs_when_up < 2) |>
    filter(events %in% c("field_out", "sac_fly", "sac_fly_double_play", "double_play"))
  
  SecondBase <- Data |>
    filter(!is.na(on_1b) & is.na(on_2b) & is.na(on_3b)) |>
    filter(grepl("to 2nd", des) | grepl("out at 2nd", des)) |>
    mutate(safe = ifelse(grepl("to 2nd", des), 1, 0)) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_1b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    mutate(dist_from_base = sqrt((location_x)^2 + (location_y - 126)^2)) |>
    select(safe, dist_from_base, sprint_speed, arm_of) |>
    filter(!is.na(sprint_speed) & !is.na(arm_of) & !is.na(dist_from_base) & !is.na(safe))
  
  ThirdBase <- Data |>
    filter(!is.na(on_2b) & is.na(on_3b)) |>
    filter(grepl("to 3rd", des) | grepl("out at 3rd", des)) |>
    filter(!grepl("to 2nd", des) & !grepl("out at 2nd", des)) |>
    mutate(safe = ifelse(grepl("to 3rd", des), 1, 0)) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_2b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y)) |>
    mutate(dist_from_base = sqrt((location_x + 63)^2 + (location_y - 63)^2)) |>
    select(safe, dist_from_base, sprint_speed, arm_of) |>
    filter(!is.na(sprint_speed) & !is.na(arm_of) & !is.na(dist_from_base) & !is.na(safe))
  
  HomePlate <- Data |>
    filter(!is.na(on_3b)) |>
    filter(events == "sac_fly" | events == "sac_fly_double_play") |>
    filter(!grepl("to 3rd", des) & !grepl("out at 3rd", des)) |>
    filter(!grepl("to 2nd", des) & !grepl("out at 2nd", des)) |>
    mutate(safe = ifelse(grepl("scores", des), 1, 0)) |>
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_3b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    rename(dist_from_base = hit_distance_sc) |>
    select(safe, dist_from_base, sprint_speed, arm_of) |>
    filter(!is.na(sprint_speed) & !is.na(arm_of) & !is.na(dist_from_base) & !is.na(safe))
  
  Bases2024 <- rbind(SecondBase, ThirdBase, HomePlate)
  rm(arm, sprint, SecondBase, ThirdBase, HomePlate, Data, Data2024)
}

Bases <- rbind(Bases2020, Bases2021, Bases2022, Bases2023, Bases2024)
write_csv(Bases, "Bases.csv")

