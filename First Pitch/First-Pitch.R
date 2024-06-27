#First Pitch Project
#
#Is taking the first pitch for a strike harmful, beneficial, or not significant?
#  
#  Reasons why batters take
#1. Get an idea of the release, to help them in at bats 2-3
#1. Question 1: Does taking the first pitch at all provide this advantage (strikes only)?
#  2. It might be a ball
#1. Question 2: How many first pitches are balls?
#  
#  The harms of it
#1. Batting average in 0-1 is lower than 0-0
#1. Question 3: By how much?
#  
#  Could taking the first pitch doom your AB from the start?
#  Question 4: Compare at bats where the first strike was taken vs swung at. What are the statlines of these at bats in total?
#  BA/OBP/SLG and RE24
#
#Then create a visual of first pitch strikes and their locations, vs a visual of all other counts. Does the location look advantageous? (Athlete Lab heatmap)

library(tidyverse)
Data <- read_csv("Data2023.csv")

#Question 1 ####

#Step 1: Create a list of at bat results for each pitcher/batter match up per game

Question1a <- Data |>
  select(pitcher_name, batter_name, events, description, balls, strikes, inning, game_pk,
         sz_top, sz_bot, plate_x, plate_z, at_bat_number, pitch_number, delta_run_exp) |>
  arrange(pitcher_name, batter_name, game_pk, inning, at_bat_number, pitch_number) |>
  mutate(at_bat_id = paste(pitcher_name, batter_name, game_pk, inning, at_bat_number)) |>
  mutate(matchup_id = paste(pitcher_name, batter_name, game_pk)) |>
  filter(events %in% c("field_error", "walk", "field_out", "home_run", "strikeout",
                       "grounded_into_double_play", "single", "force_out", "double",
                       "triple", "sac_fly", "hit_by_pitch", "sac_bunt", "fielders_choice_out",
                       "fielders_choice", "double_play", "catcher_interf", "triple_play",
                       "sac_fly_double_play", "strikeout_double_play", "other_out"))

Matchup_List <- split(Question1a, Question1a$matchup_id)

Matchup_List_2 <- lapply(Matchup_List, function(x) {
  mutate(x, time_faced = 1:nrow(x))
})

Question1a <- bind_rows(Matchup_List_2)

rm(Matchup_List, Matchup_List_2)

#Step 2: For the first at bats, identify if the first pitches were taken or not (NA for out of zone)

Question1b <- Data |>
  select(pitcher_name, batter_name, events, description, balls, strikes, inning, game_pk,
         sz_top, sz_bot, plate_x, plate_z, at_bat_number, pitch_number, delta_run_exp) |>
  arrange(pitcher_name, batter_name, game_pk, inning, at_bat_number, pitch_number) |>
  mutate(at_bat_id = paste(pitcher_name, batter_name, game_pk, inning, at_bat_number)) |>
  mutate(InZone = ifelse(plate_x < -0.8308333 - 0.25 | plate_x > 0.8308333 + 0.25 | 
                           plate_z < sz_bot - 0.25 | 
                           plate_z > sz_top + 0.25 , 0, 1)) |>
  filter(balls == 0 & strikes == 0) |>
  filter(InZone == 1) |>
  mutate(swing = ifelse(description == "called_strike", 0, NA)) |>
  mutate(swing = ifelse(description %in% c("hit_into_play", "foul_tip", "swinging_strike",
                                           "foul", "swinging_strike_blocked"), 1, swing)) |>
  filter(!is.na(swing)) |>
  select(at_bat_id, swing)

First_At_Bats <- Question1a |>
  filter(time_faced == 1)

df <- First_At_Bats |>
  left_join(Question1b, by = "at_bat_id") |>
  select(at_bat_id, swing)

Question1 <- Question1a |>
  left_join(df, by = "at_bat_id")

rm(df, First_At_Bats, Question1a, Question1b)

Matchup_List_3 <- split(Question1, Question1$matchup_id)

Matchup_List_4 <- lapply(Matchup_List_3, function(x) {
  mutate(x, first_pitch_swing = mean(swing, na.rm = TRUE))
})

Question1 <- bind_rows(Matchup_List_4)

Question1 <- Question1 |>
  filter(!is.na(first_pitch_swing))

rm(Matchup_List_3, Matchup_List_4)

#Step 3: Collect stat lines of the further at-bats, with a column that says if the first at bat was taken or not
Question1 <- Question1 |>
  mutate(babip_value = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, NA)) |>
  mutate(babip_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                            "double_play", "field_error", "force_out",
                                            "fielders_choice", "fielders_choice_out",
                                            "strikeout_double_play",
                                            "sac_fly_double_play", "other_out", "triple_play"), 
                              0, babip_value)) |>
  mutate(obp_value = ifelse(events %in% c("single", "double", "triple", "home_run", 
                                          "walk", "hit_by_pitch"), 1, NA)) |>
  mutate(obp_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                          "double_play", "field_error", "force_out",
                                          "fielders_choice", "fielders_choice_out",
                                          "strikeout_double_play", "sac_fly",
                                          "sac_fly_double_play", "other_out", "triple_play"), 
                            0, obp_value)) |>
  mutate(slg_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                          "double_play", "field_error", "force_out",
                                          "fielders_choice", "fielders_choice_out",
                                          "strikeout_double_play",
                                          "sac_fly_double_play", "other_out", "triple_play"), 
                            0, NA)) |>
  mutate(slg_value = ifelse(events == "single", 1, slg_value)) |>
  mutate(slg_value = ifelse(events == "double", 2, slg_value)) |>
  mutate(slg_value = ifelse(events == "triple", 3, slg_value)) |>
  mutate(slg_value = ifelse(events == "home_run", 4, slg_value))

Matchup_List_5 <- split(Question1, Question1$matchup_id)

Matchup_List_6 <- lapply(Matchup_List_5, function(x) {
  mutate(x, max_time_faced = max(time_faced))
})

Question1 <- bind_rows(Matchup_List_6)

rm(Matchup_List_5, Matchup_List_6)

Question1 <- Question1 |>
  filter(max_time_faced > 1)

write_csv(Question1, "Question1.csv") 

# Question 2 - How many first pitches are out of the zone ####

Question2 <- Data |>
  select(balls, strikes, plate_x, plate_z, sz_top, sz_bot) |>
  filter(balls == 0 & strikes == 0) |>
  mutate(InZone = ifelse(plate_x < -0.8308333 - 0.25 | plate_x > 0.8308333 + 0.25 | 
                           plate_z < sz_bot - 0.25 | 
                           plate_z > sz_top + 0.25, 0, 1)) |>
  select(InZone)

write_csv(Question2, "Question2.csv")

# Question 3 - Stat lines on 0-0 and 0-1 pitches

Question3 <- Data |>
  filter((balls == 0 & strikes == 0) | (balls == 0 & strikes == 1)) |>
  mutate(babip_value = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, NA)) |>
  mutate(babip_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                            "double_play", "field_error", "force_out",
                                            "fielders_choice", "fielders_choice_out",
                                            "strikeout_double_play",
                                            "sac_fly_double_play", "other_out", "triple_play"), 
                              0, babip_value)) |>
  mutate(obp_value = ifelse(events %in% c("single", "double", "triple", "home_run", 
                                          "walk", "hit_by_pitch"), 1, NA)) |>
  mutate(obp_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                          "double_play", "field_error", "force_out",
                                          "fielders_choice", "fielders_choice_out",
                                          "strikeout_double_play", "sac_fly",
                                          "sac_fly_double_play", "other_out", "triple_play"), 
                            0, obp_value)) |>
  mutate(slg_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                          "double_play", "field_error", "force_out",
                                          "fielders_choice", "fielders_choice_out",
                                          "strikeout_double_play",
                                          "sac_fly_double_play", "other_out", "triple_play"), 
                            0, NA)) |>
  mutate(slg_value = ifelse(events == "single", 1, slg_value)) |>
  mutate(slg_value = ifelse(events == "double", 2, slg_value)) |>
  mutate(slg_value = ifelse(events == "triple", 3, slg_value)) |>
  mutate(slg_value = ifelse(events == "home_run", 4, slg_value))|>
  filter(events %in% c("field_error", "walk", "field_out", "home_run", "strikeout",
                       "grounded_into_double_play", "single", "force_out", "double",
                       "triple", "sac_fly", "hit_by_pitch", "sac_bunt", "fielders_choice_out",
                       "fielders_choice", "double_play", "catcher_interf", "triple_play",
                       "sac_fly_double_play", "strikeout_double_play", "other_out")) |>
  select(babip_value, obp_value, slg_value, delta_run_exp, balls, strikes)

write_csv(Question3, "Question3.csv")

# Question 4 - Statlines of at bats in general, not just the first pitch

Question4 <- Data |>
  select(pitcher_name, batter_name, events, description, balls, strikes, inning, game_pk,
         sz_top, sz_bot, plate_x, plate_z, at_bat_number, pitch_number, delta_run_exp) |>
  arrange(pitcher_name, batter_name, game_pk, inning, at_bat_number, pitch_number) |>
  mutate(at_bat_id = paste(pitcher_name, batter_name, game_pk, inning, at_bat_number)) |>
  mutate(InZone = ifelse(plate_x < -0.8308333 - 0.25 | plate_x > 0.8308333 + 0.25 | 
                           plate_z < sz_bot - 0.25 | 
                           plate_z > sz_top + 0.25 , 0, 1)) |>
  mutate(swing = ifelse(description == "called_strike", 0, NA)) |>
  mutate(swing = ifelse(description %in% c("hit_into_play", "foul_tip", "swinging_strike",
                                           "foul", "swinging_strike_blocked"), 1, swing)) |>
  mutate(swing = ifelse(balls == 0 & strikes == 0, swing, NA))

At_Bat_List <- split(Question4, Question4$at_bat_id)

At_Bat_List_2 <- lapply(At_Bat_List, function(x) {
  mutate(x, first_pitch_taken = mean(swing, na.rm = TRUE))
})

Question4 <- bind_rows(At_Bat_List_2)

Question4 <- Question4 |>
  filter(first_pitch_taken %in% c(0,1)) |>
  mutate(babip_value = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, NA)) |>
  mutate(babip_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                            "double_play", "field_error", "force_out",
                                            "fielders_choice", "fielders_choice_out",
                                            "strikeout_double_play",
                                            "sac_fly_double_play", "other_out", "triple_play"), 
                              0, babip_value)) |>
  mutate(obp_value = ifelse(events %in% c("single", "double", "triple", "home_run", 
                                          "walk", "hit_by_pitch"), 1, NA)) |>
  mutate(obp_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                          "double_play", "field_error", "force_out",
                                          "fielders_choice", "fielders_choice_out",
                                          "strikeout_double_play", "sac_fly",
                                          "sac_fly_double_play", "other_out", "triple_play"), 
                            0, obp_value)) |>
  mutate(slg_value = ifelse(events %in% c("strikeout", "field_out", "grounded_into_double_play",
                                          "double_play", "field_error", "force_out",
                                          "fielders_choice", "fielders_choice_out",
                                          "strikeout_double_play",
                                          "sac_fly_double_play", "other_out", "triple_play"), 
                            0, NA)) |>
  mutate(slg_value = ifelse(events == "single", 1, slg_value)) |>
  mutate(slg_value = ifelse(events == "double", 2, slg_value)) |>
  mutate(slg_value = ifelse(events == "triple", 3, slg_value)) |>
  mutate(slg_value = ifelse(events == "home_run", 4, slg_value))|>
  filter(events %in% c("field_error", "walk", "field_out", "home_run", "strikeout",
                       "grounded_into_double_play", "single", "force_out", "double",
                       "triple", "sac_fly", "hit_by_pitch", "sac_bunt", "fielders_choice_out",
                       "fielders_choice", "double_play", "catcher_interf", "triple_play",
                       "sac_fly_double_play", "strikeout_double_play", "other_out")) |>
  select(first_pitch_taken, babip_value, obp_value, slg_value, delta_run_exp)

write_csv(Question4, "Question4.csv")
