{
  library(tidyverse)
  data <- read_csv("all2023.csv", 
                   col_names = FALSE)
  fields <- read.csv("http://bayesball.github.io/baseball/fields.csv")
  names(data) <- fields[, "Header"]
  
  Players <- read_csv("Players.csv")
  
  update_id <- data |>
    left_join(Players, by = c("BAT_ID" = "key_retro")) |>
    mutate(BAT_ID = key_mlbam) |>
    select(-key_mlbam) |>
    left_join(Players, by = c("PIT_ID" = "key_retro")) |>
    mutate(PIT_ID = key_mlbam) |>
    select(-key_mlbam) |>
    left_join(Players, by = c("BASE1_RUN_ID" = "key_retro")) |>
    mutate(BASE1_RUN_ID = key_mlbam) |>
    select(-key_mlbam) |>
    left_join(Players, by = c("POS2_FLD_ID" = "key_retro")) |>
    mutate(POS2_FLD_ID = key_mlbam) |>
    select(-key_mlbam)

  retro2023 <- update_id |>
    filter(!is.na(BASE1_RUN_ID) & is.na(BASE2_RUN_ID) & is.na(BASE3_RUN_ID)) |>
    filter(RUN1_CS_FL == TRUE | RUN1_SB_FL == TRUE) |>
    rename(inning_topbot = BAT_HOME_ID) |>
    mutate(inning_topbot = ifelse(inning_topbot == 0, "Top", "Bot")) |>
    rename(batter = BAT_ID) |>
    rename(pitcher = PIT_ID) |>
    rename(fielder_2 = POS2_FLD_ID) |>
    rename(on_1b = BASE1_RUN_ID) |>
    rename(balls = BALLS_CT) |>
    rename(strikes = STRIKES_CT) |>
    rename(outs_when_up = OUTS_CT) |>
    rename(inning = INN_CT) |>
    rename(home_score = HOME_SCORE_CT) |>
    rename(away_score = AWAY_SCORE_CT) |>
    rename(away_team = AWAY_TEAM_ID) |>
    mutate(home_team = substr(GAME_ID, 1, 3)) |>
    mutate(game_date = substr(GAME_ID, nchar(GAME_ID) - 9 + 1, nchar(GAME_ID))) |>
    mutate(game_date = substr(game_date, 1, 8)) |>
    mutate(game_date = ymd(game_date)) |>
    mutate(away_team = ifelse(away_team == "LAN", "LAD", away_team)) |>
    mutate(away_team = ifelse(away_team == "ANA", "LAA", away_team)) |>
    mutate(away_team = ifelse(away_team == "SFN", "SF", away_team)) |>
    mutate(away_team = ifelse(away_team == "SDN", "SD", away_team)) |>
    mutate(away_team = ifelse(away_team == "ARI", "AZ", away_team)) |>
    mutate(away_team = ifelse(away_team == "CHN", "CHC", away_team)) |>
    mutate(away_team = ifelse(away_team == "CHA", "CWS", away_team)) |>
    mutate(away_team = ifelse(away_team == "NYA", "NYY", away_team)) |>
    mutate(away_team = ifelse(away_team == "NYN", "NYM", away_team)) |>
    mutate(away_team = ifelse(away_team == "TBA", "TB", away_team)) |>
    mutate(away_team = ifelse(away_team == "KCA", "KC", away_team)) |>
    mutate(away_team = ifelse(away_team == "SLN", "STL", away_team)) |>
    mutate(away_team = ifelse(away_team == "WAS", "WSH", away_team)) |>
    mutate(home_team = ifelse(home_team == "LAN", "LAD", home_team)) |>
    mutate(home_team = ifelse(home_team == "ANA", "LAA", home_team)) |>
    mutate(home_team = ifelse(home_team == "SFN", "SF", home_team)) |>
    mutate(home_team = ifelse(home_team == "SDN", "SD", home_team)) |>
    mutate(home_team = ifelse(home_team == "ARI", "AZ", home_team)) |>
    mutate(home_team = ifelse(home_team == "CHN", "CHC", home_team)) |>
    mutate(home_team = ifelse(home_team == "CHA", "CWS", home_team)) |>
    mutate(home_team = ifelse(home_team == "NYA", "NYY", home_team)) |>
    mutate(home_team = ifelse(home_team == "NYN", "NYM", home_team)) |>
    mutate(home_team = ifelse(home_team == "TBA", "TB", home_team)) |>
    mutate(home_team = ifelse(home_team == "KCA", "KC", home_team)) |>
    mutate(home_team = ifelse(home_team == "SLN", "STL", home_team)) |>
    mutate(home_team = ifelse(home_team == "WAS", "WSH", home_team)) |>
    mutate(safe = ifelse(grepl("SB", EVENT_TX), 1, 0)) |>
    filter(!is.na(balls) & !is.na(strikes) & !is.na(outs_when_up) & !is.na(inning) & 
             !is.na(on_1b) & !is.na(batter) & !is.na(fielder_2) & !is.na(pitcher) & 
             !is.na(home_score) & !is.na(inning_topbot) & !is.na(away_score) & 
             !is.na(away_team) & !is.na(home_team) & !is.na(game_date)) |>
    mutate(pitch_id = paste(game_date, away_team, home_team, inning_topbot, inning,
                            away_score, home_score, pitcher, batter, fielder_2,
                            on_1b, balls, strikes, outs_when_up)) |>
    select(safe, pitch_id)
  
  retro2023 <- na.omit(retro2023)
  
  write_csv(retro2023, "retro2023.csv")
  
  rm(data, fields, Players, retro2023, update_id)
}
