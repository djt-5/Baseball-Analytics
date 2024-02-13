{
  #For the last four MLB seasons, get all the rows with a "1 0 0" base state 
  #with <2 outs where the event is not null. Filter this data down to flyballs 
  #and popups that were outs
  
  library(tidyverse)
  
  arm <- read_csv("arm_strength-2023.csv") |>
    select(player_id, arm_overall) |>
    rename(outfielder_id = player_id)
  
  sprint <- read_csv("sprint_speed-2023.csv") |>
    select(player_id, sprint_speed) |>
    rename(runner_id = player_id)
  
  Data2023 <- read_csv("Data2023.csv") |>
    select(events, description, bb_type, des, on_1b, on_2b, on_3b, outs_when_up, 
           hc_x, hc_y, fielder_7, fielder_8, fielder_9) |>
    filter(!is.na(events)) |>
    filter(!grepl("error", des)) |>
    filter(bb_type %in% c("fly_ball", "popup", "line_drive")) |>
    mutate(dist_from_base = sqrt((hc_x - 100.22)^2 + (hc_y - 173.07)^2)) |>
    filter(events == "field_out" | events == "double_play") |>
    mutate(base_out_state = ifelse(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 0, "0 1 0 0", NA)) |>
    mutate(base_out_state = ifelse(is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) & 
                                     outs_when_up == 1, "0 1 0 1", base_out_state)) |>
    filter(base_out_state %in% c("0 1 0 0", "0 1 0 1")) |>
    
    #Join Data into arm strength and sprint speed data
    
    mutate(outfielder_id = ifelse(grepl("left fielder", des), fielder_7, NA)) |>
    mutate(outfielder_id = ifelse(grepl("center fielder", des), fielder_8, outfielder_id)) |>
    mutate(outfielder_id = ifelse(grepl("right fielder", des), fielder_9, outfielder_id)) |>
    rename(runner_id = on_2b) |>
    left_join(arm, by = "outfielder_id") |>
    left_join(sprint, by = "runner_id") |>
    filter(!is.na(arm_overall) & !is.na(sprint_speed))
  
  #Redo this for the other years, keeping workspace
}

{
  
  Data <- rbind(Data2020, Data2021, Data2022, Data2023)
  
  #Using des, identify which of the flyballs were tagged on or not
  
  Data <- Data |>
    mutate(tag_attempt = ifelse(grepl("to 3rd", des) | grepl("out at 3rd", des), 
                                1, 0)) |>
    
    #Then, of those tagged, identify which were safe and which were out
    
    mutate(safe = ifelse(tag_attempt == 1 & grepl("to 3rd", des), 1, NA)) |>
    mutate(safe = ifelse(tag_attempt == 1 & grepl("out at 3rd", des), 0, safe)) |>
    select(safe, tag_attempt, dist_from_base, arm_overall, sprint_speed)
  
  write_csv(Data, "Data3B.csv")
  
}