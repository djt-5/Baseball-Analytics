{
  #Load Data ####
  library(tidyverse)
  Data <- read_csv("Data2023.csv") 
  
  Leaderboard <- Data |>
    summarise(.by = "pitcher_name", N = n()) |>
    filter(N >= 750)
  
  qualified <- as.vector(Leaderboard$pitcher_name)
  
  Data <- Data |>
    filter(pitcher_name %in% qualified)
  
  Data$pitch_name <- ifelse(Data$pitch_name == "4-Seam Fastball", "Four-Seam",
                            Data$pitch_name)
  Data$pitch_name <- ifelse(Data$pitch_name == "Knuckle Curve", "Knuckle-Curve",
                            Data$pitch_name)
  Data$pitch_name <- ifelse(Data$pitch_name == "Pitch Out", "Pitch-Out",
                            Data$pitch_name)
  Data$pitch_name <- ifelse(Data$pitch_name == "Slow Curve", "Slow-Curve",
                            Data$pitch_name)
  
  #Add Release Angles & Tunnels ####
  
  Data <- Data |>
    mutate(x0=release_pos_x, 
           y0=60.5-release_extension,
           z0=release_pos_z) |>
    mutate(vyR=-sqrt(vy0^2+2*ay*(y0-50)),
           tR=(vyR-vy0)/ay,
           vxR=vx0+ax*tR,
           vzR=vz0+az*tR) |>
    mutate(release_angle=atan(vzR/sqrt(vxR^2+vyR^2)),
           release_direction=atan(-vxR/vyR)) |>
    select(-x0, -y0, -z0, -vyR, -tR, -vxR, -vzR) |>
    mutate(yR = 36.83333 - release_extension,
           tR = (-vy0 - (vy0**2-2*ay*(50-yR))**0.5)/ay,
           vxR = vx0 + ax*tR,
           vyR = vy0 + ay*tR,
           vzR = vz0 + az*tR,
           tf = (-vyR-(vyR**2-2*ay*(yR-17/12))**0.5)/ay,
           x_tunnel = release_pos_x + vxR*tf + ax*tf*tf,
           z_tunnel = release_pos_z + vzR*tf + az*tf*tf) |>
    select(-yR, -tR, -vxR, -vyR, -vzR, -tf)
  
  #Calculate Breaks ####

  Data$breakless_plate_x <- ifelse(Data$release_direction > 0,
                                       Data$release_pos_x + 
                                         (60.5 - Data$release_extension)*tan(Data$release_direction),
                                       Data$release_pos_x - 
                                         (60.5 - Data$release_extension)*tan(-Data$release_direction))
  
  Data$breakless_plate_z <- ifelse(Data$release_angle < 0,
                                       Data$release_pos_z - 
                                         (60.5 - Data$release_extension)*tan(-Data$release_angle),
                                       Data$release_pos_z +
                                         (60.5 - Data$release_extension)*tan(Data$release_angle))
  
  Data$breakless_tunnel_x <- ifelse(Data$release_direction > 0,
                                        Data$release_pos_x + 
                                          (36.83333 - Data$release_extension)*tan(Data$release_direction),
                                        Data$release_pos_x - 
                                          (36.83333 - Data$release_extension)*tan(-Data$release_direction))
  
  Data$breakless_tunnel_z <- ifelse(Data$release_angle < 0,
                                        Data$release_pos_z - 
                                          (36.83333 - Data$release_extension)*tan(-Data$release_angle),
                                        Data$release_pos_z +
                                          (36.83333 - Data$release_extension)*tan(Data$release_angle))
  
  Data$break_x <- Data$plate_x - Data$breakless_plate_x
  Data$break_z <- Data$plate_z - Data$breakless_plate_z
  
  Data$pre_tunnel_break_x <- Data$x_tunnel - Data$breakless_tunnel_x
  Data$pre_tunnel_break_z <- Data$z_tunnel - Data$breakless_tunnel_z
  
  Data$post_tunnel_break_x <- Data$pre_tunnel_break_x - Data$break_x
  Data$post_tunnel_break_z <- Data$pre_tunnel_break_z - Data$break_z
  
  Data$posttunnelbreak <- sqrt(Data$post_tunnel_break_x^2 +
                                     Data$post_tunnel_break_z^2)
  
  Data$release_angle <- 57.2958*Data$release_angle
  Data$release_direction <- 57.2958*Data$release_direction
  
  Data <- Data |>
    select(-breakless_plate_x, -breakless_plate_z, -breakless_tunnel_x,
           -breakless_plate_z, -pre_tunnel_break_x, -pre_tunnel_break_z,
           -break_x, -break_z)
  
  # Paste together columns for the listing function (saves time) ####
  
  Data <- Data |>
    mutate(information = paste(pitch_name, x_tunnel, z_tunnel, plate_x, plate_z,
                               release_speed, release_pos_x, release_pos_z)) 

  Pitch_Seq <- Data |>
    arrange(pitcher_name, game_pk, inning, at_bat_number, pitch_number) |>
    mutate(seq_scenario = paste(pitcher_name, batter_name, game_pk, inning, 
                                at_bat_number)) 
  
  Seq_List <- split(Pitch_Seq, Pitch_Seq$seq_scenario)
  
  Seq_List_2 <- lapply(Seq_List, function(x) {
    mutate(x, prev_information = dplyr::lag(information))
  })
  
  Data <- bind_rows(Seq_List_2) |>
    select(-seq_scenario)
  
  # Calculate Pitches Thrown in a Game ####
  
  Pitcher_Arrange <- Data |>
    arrange(pitcher_name, game_pk, pitch_number) |>
    mutate(Scenario = paste(pitcher_name, game_pk))
  
  Pitcher_List <- split(Pitcher_Arrange, Pitcher_Arrange$Scenario)
  
  Pitcher_List_2 <- lapply(Pitcher_List, function(x) {
    mutate(x, pitches_thrown = 1:nrow(x))
  })
  
  Data <- bind_rows(Pitcher_List_2)
  
  #Determining Spin Efficiency ####
  
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
    select(-K, -yR, -tR, -vxR, -vyR, -vzR, -tf, -vxbar, -vybar, -vzbar, -vbar, 
           -adrag, -amagx, -amagy, -amagz, -amag, -Cl, -S, -spinT)
  
  
  #Differential Information ####
  
  Data <- Data |>
    separate(prev_information, into = c("prev_pitch", "prev_x_tunnel", "prev_z_tunnel",
                                        "prev_plate_x", "prev_plate_z", "prev_release_speed",
                                        "prev_release_pos_x", "prev_release_pos_z"),
             sep =" ") 
  
  Data$prev_x_tunnel <- as.numeric(Data$prev_x_tunnel)
  Data$prev_z_tunnel <- as.numeric(Data$prev_z_tunnel)
  Data$prev_plate_x <- as.numeric(Data$prev_plate_x)
  Data$prev_plate_z <- as.numeric(Data$prev_plate_z)
  Data$prev_release_speed <- as.numeric(Data$prev_release_speed)
  Data$prev_release_pos_x <- as.numeric(Data$prev_release_pos_x)
  Data$prev_release_pos_z <- as.numeric(Data$prev_release_pos_z)
  
  Data <- Data |>
    mutate(diffattunnel = sqrt((x_tunnel - prev_x_tunnel)^2 + (z_tunnel - prev_z_tunnel)^2)) |>
    mutate(diffatplate = sqrt((plate_x - prev_plate_x)^2 + (plate_z - prev_plate_z)^2)) |>
    mutate(speeddiff = release_speed - prev_release_speed) |>
    mutate(diffatrelease = sqrt((release_pos_x - prev_release_pos_x)^2 + 
                                  (release_pos_z - prev_release_pos_z)^2)) |>
    mutate(breaktotunnelratio = posttunnelbreak/diffattunnel) |>
    mutate(releasetotunnelratio = diffatrelease/diffattunnel) 
  
  # Response Variables ####

  Data$Swing <- ifelse(Data$description == "hit_into_play" | 
                             Data$description == "foul_tip" |
                             Data$description == "swinging_strike" |
                             Data$description == "foul" |
                             Data$description == "swinging_strike_blocked", 1, 0)
  
  Data$Strike <- ifelse(Data$description != "ball" & 
                              Data$description != "blocked_ball" &
                              Data$description != "hit_by_pitch", 1, 0)
  
  Data$GroundBall <- ifelse(Data$bb_type == "ground_ball", 1, 0)
  Data$GroundBall <- ifelse(is.na(Data$launch_angle),
                                NA,
                                Data$GroundBall)
  
  Data$InZone <- ifelse(Data$plate_x < -0.8308333 | Data$plate_x > 0.8308333 | 
                           Data$plate_z < Data$sz_bot | 
                           Data$plate_z > Data$sz_top, 0, 1)
  
  Data$Chase <- ifelse(Data$InZone == 0 & Data$Swing == 1, 1, NA)
  
  Data$Chase <- ifelse(Data$InZone == 0 & Data$Swing == 0, 0, Data$Chase)
  
  Data$Chase <- ifelse(Data$description == "hit_by_pitch", 
                           NA, Data$Chase)
  
  Data$Whiff <- ifelse(Data$Swing == 0, NA, 0)
  Data$Whiff <- ifelse(Data$Swing == 1, 0, Data$Whiff)
  Data$Whiff <- ifelse(Data$description %in% c("swinging_strike", "swinging_strike_blocked",
                                               "foul_tip"), 1, Data$Whiff)
  
  Data$InZoneWhiff <- ifelse(Data$InZone == 1 & Data$Whiff == 1, 1, NA)
  Data$InZoneWhiff <- ifelse(Data$InZone == 1 & Data$Whiff == 0, 0, Data$InZoneWhiff)
  
  #Last Minute Changes ####
  
  Data <- Data |>
    filter(pitch_name != "Pitch Out" &
             pitch_name != "Other" &
             pitch_name != "Eephus")
  
  Data$delta_run_exp <- -Data$delta_run_exp
  
  index <- c(2,6:9,16,17,23:29,51,52,72,83,85:89,91,92,93,95,98,99,104:111,114:118)
  
  Data <- Data[,index]
  
  write_csv(Data, "Data.csv")
}
