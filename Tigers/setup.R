{
  #Load Data ####
  library(tidyverse)
  Data <- read_csv("AnalyticsQuestionnairePitchData.csv") |>
    select(-PostStrikes, -PostBalls, -PostOuts, -VenueID, -TrajectoryPfxHorz,
           -TrajectoryPfxVert, -TrajectoryVerticalBreak, -TrajectoryLocationY, -SpinVectorX,
           -SpinVectorY, -SpinVectorZ, -SpinFitError, -TrajectoryPolynomialX0,
           -TrajectoryPolynomialX1, -TrajectoryPolynomialX2, -TrajectoryPolynomialY0,
           -TrajectoryPolynomialY1, -TrajectoryPolynomialY2, -TrajectoryPolynomialZ0,
           -TrajectoryPolynomialZ1, -TrajectoryPolynomialZ2, -TrajectoryX0, -TrajectoryX1,
           -TrajectoryX2) |>
    mutate(inning_topbot = ifelse(inning_topbot == 0, "Bot", "Top")) |>
    mutate(speed_drop = release_speed - zone_speed) |>
    
    #Add Tunnel ####
  
    mutate(yR = 36.83333 - release_extension,
           tR = (-vy0 - (vy0**2-2*ay*(50-yR))**0.5)/ay,
           vxR = vx0 + ax*tR,
           vyR = vy0 + ay*tR,
           vzR = vz0 + az*tR,
           tf = (-vyR-(vyR**2-2*ay*(yR-17/12))**0.5)/ay,
           x_tunnel = release_pos_x + vxR*tf + ax*tf*tf,
           z_tunnel = release_pos_z + vzR*tf + az*tf*tf) |>
    select(-yR, -tR, -vxR, -vyR, -vzR, -tf)
    
    #Calculate Spin Efficiency
  
  Data <- Data |>
    mutate(K = 0.0053865,
           yR = 60.5 - release_extension,
           tR = (-vy0-sqrt(vy0^2-2*ay*(50 - yR)))/ay,
           vxR = vx0 + ax*tR,
           vyR = vy0 + ay*tR,
           vzR = vz0 + az*tR,
           tf = (-vyR-sqrt(vyR^2-2*ay*(yR-17/12)))/ay,
           vxbar = (2*vxR + ax*tf)/2,
           vybar = (2*vyR + ay*tf)/2,
           vzbar = (2*vzR + az*tf)/2,
           vbar = sqrt(vxbar^2 + vybar^2 + vzbar^2), 
           adrag = -(ax*vxbar + ay*vybar + (az+32.174)*vzbar)/vbar,
           amagx = ax + adrag*vxbar/vbar,
           amagy = ay + adrag*vybar/vbar,
           amagz = az + adrag*vzbar/vbar + 32.174,
           amag = sqrt(amagx^2 + amagy^2 + amagz^2) ,
           Cl = amag/(K*vbar^2),
           S = 0.166*log(0.336/(0.336-Cl)),
           spinT = 78.92*S*vbar,
           spin_eff = spinT / release_spin_rate) |>
    select(-K, -yR, -tR, -vxR, -vzR, -tf, -vxbar, -vybar, -vzbar, -vbar, -adrag, -amagx, -amagy,
           -amagz, -amag, -Cl, -S, -spinT)
  
  Data <- Data |>
    filter(description != "pickoff_attempt_1b" & description != "stolen_base_3b" &
             description != "stolen_base_2b") |>
    mutate(events = description) |>
    mutate(description = ifelse(PitchId %in% c(176,584,38,490,217,539,175), "called_strike", description)) |>
    mutate(description = ifelse(PitchId == 481, "foul_bunt", description)) |>
    mutate(description = ifelse(PitchId %in% c(282, 592, 219, 328), "foul_tip", description)) |>
    mutate(description = ifelse(PitchId %in% c(352,286,497), "swinging_strike_blocked", description)) |>
    mutate(description = ifelse(PitchId %in% c(132,257,460,385,597,204,193,33,415,140,274,
                                               234,324,334,356,4,71,106,350,220,433,68,497),
                                "swinging_strike", description)) |>
    mutate(description = ifelse(description %in% c("field_out", "single", "double", 
                                                   "grounded_into_double_play", 
                                                   "home_run", "force_out",
                                                   "field_error", "triple", "sac_bunt"), 
                                "hit_into_play", description)) |>
    mutate(description = ifelse(description %in% c("walk", "wild_pitch", "blocked_ball"), "ball", description)) |>
    mutate(events = ifelse(events %in% c("field_out", "single", 
                                         "walk", "strikeout", "double",
                                         "grounded_into_double_play", "home_run",
                                         "force_out", "sac_bunt"), events, NA)) |>
    mutate(Swing = ifelse(description %in% c("hit_into_play","foul_tip","swinging_strike",
                                             "foul","swinging_strike_blocked"), 1, 0)) |>
    mutate(Swing = ifelse(description %in% c("foul_bunt", "sac_bunt"), NA, Swing)) |>
    mutate(Strike = ifelse(description != "ball", 1, 0)) |>
    mutate(InZone = ifelse(plate_x < -0.8308333 | plate_x > 0.8308333 | 
                             plate_z < sz_bot | 
                             plate_z > sz_top, 0, 1)) |>
    mutate(Chase = ifelse(InZone == 0 & Swing == 1, 1, NA)) |>
    mutate(Chase = ifelse(InZone == 0 & Swing == 0, 0, Chase)) |>
    mutate(Whiff = ifelse(Swing == 0, NA, 0)) |>
    mutate(Whiff = ifelse(Swing == 1, 0, Whiff)) |>
    mutate(Whiff = ifelse(description %in% 
                            c("swinging_strike", "swinging_strike_blocked",
                              "foul_tip"), 1, Whiff)) |>
    mutate(InZoneWhiff = ifelse(InZone == 1 & Whiff == 1, 1, NA)) |>
    mutate(InZoneWhiff = ifelse(InZone == 1 & Whiff == 0, 0, InZoneWhiff)) |>
    mutate(Bauer = release_spin_rate / release_speed) |>
  
  #Calculate Pitcher and Batter Teams
  
    mutate(pitcher_team = ifelse(inning_topbot == "Top", home_team, away_team)) |>
    mutate(batter_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
    mutate(pfx_x = pfx_x * 12,
           pfx_z = pfx_z * 12,
           release_pos_x = release_pos_x * 12,
           release_pos_z = release_pos_z * 12) |>
    mutate(IP = ifelse(events %in% c("field_out", "force_out", "strikeout"), 1/3, 0)) |>
    mutate(IP = ifelse(events == "grounded_into_double_play", 2/3, IP)) |>
    mutate(PA = ifelse(events %in% c("force_out", "field_out", "strikeout", 
                                     "single", "home_run", "walk",
                                     "grounded_into_double_play", "double", "triple",
                                     "sac_bunt", "field_error", "sac_bunt"), 1, 0)) |>
    mutate(PA = ifelse(is.na(events), NA, PA)) |>
    mutate(HR = ifelse(events == "home_run", 1, 0)) |>
    mutate(HR = ifelse(PA != 1, NA, HR)) |>
    mutate(BB = ifelse(events == "walk", 1, 0)) |>
    mutate(BB = ifelse(PA != 1, NA, BB)) |>
    mutate(K = ifelse(grepl("strikeout", events), 1, 0)) |>
    mutate(K = ifelse(PA != 1, NA, K)) |>
    mutate(babip_value = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, NA)) |>
    mutate(babip_value = ifelse(events %in% c("field_out", "force_out", "strikeout", 
                                              "field_error"), 0, babip_value)) |>
    mutate(AB = ifelse(events %in% c("home_run", "field_out", "strikeout",
                                     "single", "double", "force_out","grounded_into_double_play",
                                     "triple", "field_error"), 1, NA)) |>
    mutate(slugging = ifelse(events == "single", 1, 0)) |>
    mutate(slugging = ifelse(events == "double", 2, slugging)) |>
    mutate(slugging = ifelse(events == "triple", 3, slugging)) |>
    mutate(slugging = ifelse(events == "home_run", 4, slugging)) |>
    mutate(slugging = ifelse(is.na(AB), NA, slugging)) |>
    mutate(spin_eff = ifelse(spin_eff >= 1, 1, spin_eff))
  #Save Data ####
  
  write_csv(Data, "Data.csv")
}
