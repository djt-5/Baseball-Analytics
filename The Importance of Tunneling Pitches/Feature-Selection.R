{
  #Load, Add Release Angles & Tunnels ####
  
  library(tidyverse)
  Data2023 <- read_csv("Data2023.csv") |>
    select(pitch_type, release_speed, release_pos_x, release_pos_z, events,
           stand, p_throws, bb_type, plate_x, plate_z, vx0, vy0, vz0, 
           ax, ay, az, sz_top, sz_bot, release_spin_rate,
           release_extension, pfx_x, pfx_z, pitcher_name, game_pk, inning, 
           at_bat_number, pitch_number, batter_name, description, launch_angle) |>
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
  
  Data2023$breakless_plate_x <- ifelse(Data2023$release_direction > 0,
                                   Data2023$release_pos_x + 
                                     (60.5 - Data2023$release_extension)*tan(Data2023$release_direction),
                                   Data2023$release_pos_x - 
                                     (60.5 - Data2023$release_extension)*tan(-Data2023$release_direction))
  
  Data2023$breakless_plate_z <- ifelse(Data2023$release_angle < 0,
                                   Data2023$release_pos_z - 
                                     (60.5 - Data2023$release_extension)*tan(-Data2023$release_angle),
                                   Data2023$release_pos_z +
                                     (60.5 - Data2023$release_extension)*tan(Data2023$release_angle))
  
  Data2023$breakless_tunnel_x <- ifelse(Data2023$release_direction > 0,
                                    Data2023$release_pos_x + 
                                      (36.83333 - Data2023$release_extension)*tan(Data2023$release_direction),
                                    Data2023$release_pos_x - 
                                      (36.83333 - Data2023$release_extension)*tan(-Data2023$release_direction))
  
  Data2023$breakless_tunnel_z <- ifelse(Data2023$release_angle < 0,
                                    Data2023$release_pos_z - 
                                      (36.83333 - Data2023$release_extension)*tan(-Data2023$release_angle),
                                    Data2023$release_pos_z +
                                      (36.83333 - Data2023$release_extension)*tan(Data2023$release_angle))
  
  Data2023$break_x <- Data2023$plate_x - Data2023$breakless_plate_x
  Data2023$break_z <- Data2023$plate_z - Data2023$breakless_plate_z
  
  Data2023$pre_tunnel_break_x <- Data2023$x_tunnel - Data2023$breakless_tunnel_x
  Data2023$pre_tunnel_break_z <- Data2023$z_tunnel - Data2023$breakless_tunnel_z
  
  Data2023$post_tunnel_break_x <- Data2023$break_x - Data2023$pre_tunnel_break_x
  Data2023$post_tunnel_break_z <- Data2023$break_z - Data2023$pre_tunnel_break_z
  
  Data2023$posttunnelbreak <- sqrt(Data2023$post_tunnel_break_x^2 +
                                 Data2023$post_tunnel_break_z^2)
  
  Data2023 <- Data2023 |>
    select(-breakless_plate_x, -breakless_plate_z, -breakless_tunnel_x,
           -breakless_tunnel_z, -break_x, -break_z, -pre_tunnel_break_x,
           -pre_tunnel_break_z, -post_tunnel_break_x, -post_tunnel_break_z)
  
  # Paste together columns for the listing function (saves time) ####
  
  Data2023 <- Data2023 |>
    mutate(information = paste(pitch_type, x_tunnel, z_tunnel, plate_x, plate_z,
                               release_speed, release_pos_x, release_pos_z)) |>
    select(-vx0, -vy0, -vz0, -ax, -ay, -az)

  #Pitch Sequences ####
  
  Pitch_Seq <- Data2023 |>
    arrange(pitcher_name, game_pk, inning, at_bat_number, pitch_number) |>
    mutate(seq_scenario = paste(pitcher_name, batter_name, game_pk, inning, 
                                at_bat_number)) |>
    select(-pitcher_name, -game_pk, -inning, -at_bat_number, -pitch_number, 
           -batter_name)
  
  Seq_List <- split(Pitch_Seq, Pitch_Seq$seq_scenario)
  
  Seq_List_2 <- lapply(Seq_List, function(x) {
    mutate(x, prev_information = dplyr::lag(information))
  })
  
  Data2023 <- bind_rows(Seq_List_2) |>
    select(-seq_scenario)

  #Differential Information ####
  
  fastballs <- c("SI", "FF", "FC")
  secondary <- c("CH", "SL", "KC", "CU", "ST", "FS", "SV", "SC", "KN", "CS")
  
  Data2023 <- Data2023 |>
    separate(prev_information, into = c("prev_pitch", "prev_x_tunnel", "prev_z_tunnel",
                            "prev_plate_x", "prev_plate_z", "prev_release_speed",
                            "prev_release_pos_x", "prev_release_pos_z"),
             sep =" ") 
  
  Data2023$prev_x_tunnel <- as.numeric(Data2023$prev_x_tunnel)
  Data2023$prev_z_tunnel <- as.numeric(Data2023$prev_z_tunnel)
  Data2023$prev_plate_x <- as.numeric(Data2023$prev_plate_x)
  Data2023$prev_plate_z <- as.numeric(Data2023$prev_plate_z)
  Data2023$prev_release_speed <- as.numeric(Data2023$prev_release_speed)
  Data2023$prev_release_pos_x <- as.numeric(Data2023$prev_release_pos_x)
  Data2023$prev_release_pos_z <- as.numeric(Data2023$prev_release_pos_z)
  
  Data2023 <- Data2023 |>
    select(-information) |>
    filter(pitch_type %in% secondary & prev_pitch %in% fastballs) |>
    filter(plate_z > 0 & plate_z <= 5) |>
    filter(prev_plate_z > 0 & prev_plate_z <= 5) |>
    mutate(diffattunnel = sqrt((x_tunnel - prev_x_tunnel)^2 + (z_tunnel - prev_z_tunnel)^2)) |>
    mutate(diffatplate = sqrt((plate_x - prev_plate_x)^2 + (plate_z - prev_plate_z)^2)) |>
    mutate(speeddiff = release_speed - prev_release_speed) |>
    mutate(diffatrelease = sqrt((release_pos_x - prev_release_pos_x)^2 + 
                                  (release_pos_z - prev_release_pos_z)^2)) |>
    mutate(breaktotunnelratio = posttunnelbreak/diffattunnel) |>
    mutate(releasetotunnelratio = diffatrelease/diffattunnel) |>
    filter(!is.na(release_spin_rate) & !is.na(release_extension) & 
             !is.na(pfx_x) & !is.na(pfx_z) & !is.na(release_angle) & 
             !is.na(release_direction) & !is.na(x_tunnel) & !is.na(z_tunnel) & 
             !is.na(posttunnelbreak) & !is.na(prev_x_tunnel) &
             !is.na(prev_z_tunnel) & !is.na(prev_plate_x) & !is.na(prev_plate_z) &
             !is.na(prev_release_speed) & !is.na(prev_release_pos_x) & !is.na(prev_release_pos_z)) |>
    select(-release_pos_x, -release_pos_z, -release_extension, -x_tunnel, -z_tunnel, 
           -prev_x_tunnel, -prev_z_tunnel, -prev_plate_x, -prev_plate_z,
           -prev_release_speed, -prev_release_pos_x, -prev_release_pos_z)
  
    # Response Variables ####
    
    Data2023$Swing <- ifelse(Data2023$description == "hit_into_play" | 
                           Data2023$description == "foul_tip" |
                           Data2023$description == "swinging_strike" |
                           Data2023$description == "foul" |
                           Data2023$description == "swinging_strike_blocked", 1, 0)
    
    Data2023$Strike <- ifelse(Data2023$description != "ball" & 
                            Data2023$description != "blocked_ball" &
                            Data2023$description != "hit_by_pitch", 1, 0)
    
    Data2023$GroundBall <- ifelse(Data2023$bb_type == "ground_ball", 1, 0)
    Data2023$GroundBall <- ifelse(is.na(Data2023$launch_angle),
                              NA,
                              Data2023$GroundBall)
    
    Data2023$Chase <- ifelse((Data2023$plate_x < -0.8308333 | Data2023$plate_x > 0.8308333 | 
                            Data2023$plate_z < Data2023$sz_bot | 
                            Data2023$plate_z > Data2023$sz_top) &
                           Data2023$Swing == 1, 1, NA)
    
    Data2023$Chase <- ifelse((Data2023$plate_x < -0.8308333 | Data2023$plate_x > 0.8308333 | 
                            Data2023$plate_z < Data2023$sz_bot | 
                            Data2023$plate_z > Data2023$sz_top) &
                           Data2023$Swing == 0, 0, Data2023$Chase)
    
    Data2023$Chase <- ifelse(Data2023$description == "hit_by_pitch", 
                             NA, Data2023$Chase)
    
    Data2023$Whiff <- ifelse(Data2023$Swing == 0, NA, 0)
    Data2023$Whiff <- ifelse(Data2023$Swing == 1, 0, Data2023$Whiff)
    Data2023$Whiff <- ifelse(Data2023$description == "swinging_strike" |
                           Data2023$description == "swinging_strike_blocked" |
                           Data2023$description == "foul_tip", 1, Data2023$Whiff)
  
  Data2023 <- Data2023 |>
    select(-Swing, -Strike, -events, -description, -bb_type, -launch_angle) 
  
  write_csv(Data2023, "Tunnel2023.csv")
}
