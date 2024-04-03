{
  #Load Data ####
  library(tidyverse)
  Data <- read_csv("Spring2024.csv") |>
    
    #Calculate Release Angles
    
    mutate(x0=release_pos_x, 
           y0=60.5-release_extension,
           z0=release_pos_z) |>
    mutate(vyR=-sqrt(vy0^2+2*ay*(y0-50)),
           tR=(vyR-vy0)/ay,
           vxR=vx0+ax*tR,
           vzR=vz0+az*tR) |>
    mutate(release_angle=atan(vzR/sqrt(vxR^2+vyR^2))*180/pi,
           release_direction=atan(-vxR/vyR)*180/pi) |>
    select(-x0, -y0, -z0, -vyR, -tR, -vxR, -vzR) |>
    
    #Tunnel Point
    mutate(yR = 36.83333 - release_extension,
           tR = (-vy0 - (vy0**2-2*ay*(50-yR))**0.5)/ay,
           vxR = vx0 + ax*tR,
           vyR = vy0 + ay*tR,
           vzR = vz0 + az*tR,
           tf = (-vyR-(vyR**2-2*ay*(yR-17/12))**0.5)/ay,
           x_tunnel = release_pos_x + vxR*tf + ax*tf*tf,
           z_tunnel = release_pos_z + vzR*tf + az*tf*tf) |>
    select(-yR, -tR, -vxR, -vyR, -vzR, -tf) |>
    
    #Calculate Spin Efficiency
    
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
    mutate(Bauer = release_spin_rate / release_speed) |>
    #Attack Angle
    mutate(launch_angle = ifelse(description == "foul", NA, launch_angle)) |>
    mutate(attack_angle = 5.6 + 0.6*launch_angle) |>
    
    #Vertical Approach Angle
    mutate(vy_f = -sqrt(vy0^2 - (2 * ay * (50 - 17/12))),
           t = (vy_f - vy0) / ay,
           vz_f = vz0 + (az * t),
           vert_appr_angle = -atan(vz_f/vy_f) * (180 / pi)) |>
    select(-K, -yR, -tR, -vxR, -vyR, -vzR, -tf, -vxbar, -vybar, -vzbar, -vbar, 
           -adrag, -amagx, -amagy, -amagz, -amag, -Cl, -S, -spinT, -vy_f, -t, -vz_f) |>
  
  #Calculate Pitcher and Batter Teams
  
    mutate(pitcher_team = ifelse(inning_topbot == "Top", home_team, away_team)) |>
    mutate(batter_team = ifelse(inning_topbot == "Bot", home_team, away_team)) |>
    
    #Simplify Pitch Types
    
    mutate(pitch_name = ifelse(pitch_name %in% c("Knuckle Curve", "Slow Curve",
                                                  "Slurve"), "Curveball", pitch_name)) |>
    mutate(pitch_name = ifelse(pitch_name == "4-Seam Fastball", "Fastball", pitch_name)) |>
    mutate(pitch_name = ifelse(pitch_name == "Split-Finger", "Splitter", pitch_name)) |>
    mutate(pfx_x = pfx_x * 12,
           pfx_z = pfx_z * 12,
           release_pos_x = release_pos_x * 12,
           release_pos_z = release_pos_z * 12) |>
    filter(pitch_name != "Pitch Out" & pitch_name != "Other" & 
             pitch_name != "Eephus" & !is.na(pitch_name)) |>
    mutate(Swing = ifelse(description %in% c("hit_into_play", "foul_tip", "swinging_strike",
                                             "foul", "swinging_strike_blocked"), 1, 0)) |>
    mutate(Strike = ifelse(description != "ball" & 
                             description != "blocked_ball" &
                             description != "hit_by_pitch" &
                             description != "pitchout", 1, 0)) |>
    mutate(InZone = ifelse(plate_z < sz_bot - 0.25 |
                             plate_z > sz_top + 0.25 |
                             plate_x < -8.5/12 |
                             plate_x > 8.5/12, 0, 1)) |>
    mutate(Chase = ifelse(InZone == 0 & Swing == 1, 1, NA)) |>
    mutate(Chase = ifelse(InZone == 0 & Swing == 0, 0, Chase)) |>
    mutate(Chase = ifelse(description == "hit_by_pitch", 
                          NA, Chase)) |>
    mutate(Whiff = ifelse(Swing == 0, NA, 0)) |>
    mutate(Whiff = ifelse(Swing == 1, 0, Whiff)) |>
    mutate(Whiff = ifelse(description %in% 
                            c("swinging_strike", "swinging_strike_blocked",
                              "foul_tip"), 1, Whiff)) |>
    mutate(HardHit = ifelse(launch_speed >= 95, 1, 0)) |>
    mutate(HardHit = ifelse(is.na(launch_speed), NA, HardHit)) |>
    mutate(GroundBall = ifelse(bb_type == "ground_ball", 1, 0)) |>
    mutate(GroundBall = ifelse(is.na(launch_speed), NA, GroundBall)) |>
    mutate(IP = ifelse(grepl("out", des), 1/3, 0)) |>
    mutate(IP = ifelse(grepl("double_play", events), 2/3, IP)) |>
    mutate(IP = ifelse(grepl("triple_play", events), 1, IP)) |> 
    mutate(IP = ifelse(!grepl("out", events), 0, IP)) |>
    mutate(PA = ifelse(events %in% c("hit_by_pitch", "force_out", "field_out",
                                     "strikeout", "single", "home_run", "walk",
                                     "grounded_into_double_play", "double", "triple",
                                     "sac_fly", "field_error", "double_play", "fielders_choice",
                                     "catcher_interf", "strikeout_double_play", 
                                     "fielders_choice_out", "other_out", 
                                     "sac_fly_double_play", "sac_bunt"), 1, 0)) |>
    mutate(PA = ifelse(is.na(events), NA, PA)) |>
    mutate(HR = ifelse(events == "home_run", 1, 0)) |>
    mutate(HR = ifelse(PA != 1, NA, HR)) |>
    mutate(HBP = ifelse(events == "hit_by_pitch", 1, 0)) |>
    mutate(HBP = ifelse(PA != 1, NA, HBP)) |>
    mutate(BB = ifelse(events == "walk", 1, 0)) |>
    mutate(BB = ifelse(PA != 1, NA, BB)) |>
    mutate(K = ifelse(grepl("strikeout", events), 1, 0)) |>
    mutate(K = ifelse(PA != 1, NA, K)) |>
    mutate(babip_value = ifelse(grepl("strikeout", events), 0, babip_value)) |>
    mutate(babip_value = ifelse(grepl("ball", description), NA, babip_value)) |>
    mutate(estimated_ba_using_speedangle = ifelse(grepl("strikeout", events), 0, estimated_ba_using_speedangle)) |>
    mutate(estimated_woba_using_speedangle = ifelse(grepl("strikeout", events), 0, estimated_woba_using_speedangle)) |>
    mutate(AB = ifelse(events %in% c("home_run", "field_out", "strikeout",
                                     "single", "double", "force_out",
                                     "fielders_choice", "grounded_into_double_play",
                                     "triple", "field_error", "double_play", "strikeout_double_play",
                                     "fielders_choice_out"), 1, NA)) |>
    mutate(slugging = ifelse(events == "single", 1, 0)) |>
    mutate(slugging = ifelse(events == "double", 2, slugging)) |>
    mutate(slugging = ifelse(events == "triple", 3, slugging)) |>
    mutate(slugging = ifelse(events == "home_run", 4, slugging)) |>
    mutate(slugging = ifelse(is.na(AB), NA, slugging)) |>
    mutate(location_x = 2.5 * (hc_x - 125.42),
           location_y = 2.5 * (198.27 - hc_y))
  
  #Prepare to Save Data ####
  
  Pitchers <- Data
  
  Batters <- Data
  
  #Roster Status ####
  
  Roster_Pitchers <- c("David Bednar",
                       "Ryan Borucki",
                       "Aroldis Chapman",
                       "Roansy Contreras",
                       "Bailey Falter",
                       "Josh Fleming",
                       "Marco Gonzales",
                       "Jared Jones",
                       "Mitch Keller",
                       "Luis L. Ortiz",
                       "Martín Pérez",
                       "Ryder Ryan",
                       "Hunter Stratton")
  
  Roster_Batters <- c("Henry Davis", 
                      "Jason Delay",
                      "Oneil Cruz",
                      "Ke'Bryan Hayes",
                      "Connor Joe",
                      "Rowdy Tellez",
                      "Jared Triolo",
                      "Alika Williams",
                      "Edward Olivares",
                      "Bryan Reynolds",
                      "Jack Suwinski",
                      "Michael A. Taylor",
                      "Andrew McCutchen")
  
  Pitching_Prospects <- c("Paul Skenes",
                          "Anthony Solomento",
                          "Bubba Chandler",
                          "Thomas Harrington",
                          "Braxton Ashcraft",
                          "Mike Burrows",
                          "Zander Mueth",
                          "Hunter Barco",
                          "Michael Kennedy",
                          "Jun-Seok Shim",
                          "Kyle Nicolas",
                          "Patrick Reilly")
  
  Hitting_Prospects <- c("Tony Blanco Jr.",
                         "Bralyn Brazoban",
                         "Omar Alfonzo",
                         "Tres Gonzalez",
                         "Estuar Suero",
                         "Jhonny Severino",
                         "Jesus Castillo",
                         "Enmanuel Terrero",
                         "Jase Bowen",
                         "Garret Forrester",
                         "Yordany De Los Santos",
                         "Shalin Polanco",
                         "Lonnie White Jr.",
                         "Jack Brannigan",
                         "Tsung-Che Cheng",
                         "Mitch Jebb",
                         "Termarr Johnson")
  
  Pitchers$Status <- ifelse(Pitchers$pitcher_name %in% Roster_Pitchers, "Roster", NA)
  Pitchers$Status <- ifelse(Pitchers$pitcher_name %in% Pitching_Prospects, "Prospect", Pitchers$Status)
  
  Batters$Status <- ifelse(Batters$batter_name %in% Roster_Batters, "Roster", NA)
  Batters$Status <- ifelse(Batters$batter_name %in% Hitting_Prospects, "Prospect", Batters$Status)
  
  #Save Data ####
  
  write_csv(Pitchers, "Pitchers.csv")
  write_csv(Batters, "Batters.csv")
  
}
