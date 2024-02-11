{
  #Load, Add Release Angles & Tunnels ####
  
  library(tidyverse)
  Data2023 <- read_csv("Data2023.csv") |>
    select(pitch_type, release_speed, release_pos_x, release_pos_z, 
           stand, p_throws, plate_x, plate_z, vx0, vy0, vz0, 
           ax, ay, az, sz_top, sz_bot, release_spin_rate,
           release_extension, pfx_x, pfx_z, pitcher_name, game_pk, inning, 
           at_bat_number, pitch_number, batter_name, description) |>
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
    select(-yR, -tR, -vxR, -vyR, -vzR, -tf) |>
  #Calculate Breaks ####
    mutate(breakless_plate_x = ifelse(release_direction > 0,
                                release_pos_x + 
                                  (60.5 - release_extension)*tan(release_direction),
                                release_pos_x - 
                                  (60.5 - release_extension)*tan(-release_direction))) |>
    mutate(breakless_plate_z = ifelse(release_angle < 0,
                                release_pos_z - 
                                  (60.5 - release_extension)*tan(-release_angle),
                                release_pos_z +
                                  (60.5 - release_extension)*tan(release_angle))) |>
    mutate(breakless_tunnel_x = ifelse(release_direction > 0,
                                release_pos_x + 
                                  (36.83333 - release_extension)*tan(release_direction),
                                release_pos_x - 
                                  (36.83333 - release_extension)*tan(-release_direction))) |>
    mutate(breakless_tunnel_z = ifelse(release_angle < 0,
                                release_pos_z - 
                                  (36.83333 - release_extension)*tan(-release_angle),
                                release_pos_z +
                                  (36.83333 - release_extension)*tan(release_angle))) |>
    mutate(break_x = plate_x - breakless_plate_x) |>
    mutate(break_z = plate_z - breakless_plate_z) |>
    mutate(pre_tunnel_break_x = x_tunnel - breakless_tunnel_x) |>
    mutate(pre_tunnel_break_z = z_tunnel - breakless_tunnel_z) |>
    mutate(post_tunnel_break_x = break_x - pre_tunnel_break_x) |>
    mutate(post_tunnel_break_z = break_z - pre_tunnel_break_z) |>
    mutate(posttunnelbreak = sqrt(post_tunnel_break_x^2 +
                                     post_tunnel_break_z^2)) |>
    select(-breakless_plate_x, -breakless_plate_z, -breakless_tunnel_x,
           -breakless_tunnel_z, -break_x, -break_z, -pre_tunnel_break_x,
           -pre_tunnel_break_z, -post_tunnel_break_x, -post_tunnel_break_z,
           -vx0, -vy0, -vz0, -ax, -ay, -az, -release_pos_x, -release_pos_z) |>
    filter(!is.na(pitch_type) & !is.na(x_tunnel) & !is.na(z_tunnel) &
             !is.na(plate_x) & !is.na(plate_z) & !is.na(release_speed) & 
             !is.na(release_spin_rate)) |>
  # Paste together columns for the listing function (saves time) #### 
    mutate(information = paste(pitch_type, x_tunnel, z_tunnel, plate_x, plate_z,
                               release_speed))

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

  #Differential Information and Response Variables ####
  
  fastballs <- c("SI", "FF", "FC")
  secondary <- c("CH", "SL", "KC", "CU", "ST", "FS", "SV", "SC", "KN", "CS")
  
  Data2023 <- Data2023 |>
    separate(prev_information, into = c("prev_pitch", "prev_x_tunnel", "prev_z_tunnel",
                            "prev_plate_x", "prev_plate_z", "prev_release_speed"),
             sep =" ") 
  
  Data2023$prev_x_tunnel <- as.numeric(Data2023$prev_x_tunnel)
  Data2023$prev_z_tunnel <- as.numeric(Data2023$prev_z_tunnel)
  Data2023$prev_plate_x <- as.numeric(Data2023$prev_plate_x)
  Data2023$prev_plate_z <- as.numeric(Data2023$prev_plate_z)
  Data2023$prev_release_speed <- as.numeric(Data2023$prev_release_speed)
  
  Data2023 <- Data2023 |>
    select(-information) |>
    filter(pitch_type %in% secondary & prev_pitch %in% fastballs) |>
    filter(plate_z > 0 & plate_z <= 5) |>
    filter(prev_plate_z > 0 & prev_plate_z <= 5) |>
    mutate(diffattunnel = sqrt((x_tunnel - prev_x_tunnel)^2 + (z_tunnel - prev_z_tunnel)^2)) |>
    mutate(speeddiff = release_speed - prev_release_speed) |>
    mutate(breaktotunnelratio = posttunnelbreak/diffattunnel) |>
    select(-release_extension, -x_tunnel, -z_tunnel, -prev_x_tunnel, -prev_z_tunnel,
           -prev_release_speed) |>
    mutate(Swing = ifelse(description == "hit_into_play" | 
                             description == "foul_tip" |
                             description == "swinging_strike" |
                             description == "foul" |
                             description == "swinging_strike_blocked", 1, 0)) |>
    mutate(Strike = ifelse(description != "ball" & 
                              description != "blocked_ball" &
                              description != "hit_by_pitch", 1, 0)) |>
    mutate(InZone = ifelse(plate_x < -0.8308333 | plate_x > 0.8308333 | 
                              plate_z < sz_bot | 
                              plate_z > sz_top, 0, 1)) |>
    mutate(Chase = ifelse(InZone == 0 & Swing == 1, 1, NA)) |>
    mutate(Chase = ifelse(InZone == 0 & Swing == 0, 0, Chase)) |>
    mutate(Chase = ifelse(description == "hit_by_pitch", 
                           NA, Chase)) |>
    mutate(Whiff = ifelse(Swing == 0, NA, 0)) |>
    mutate(Whiff = ifelse(Swing == 1, 0, Whiff)) |>
    mutate(Whiff = ifelse(description %in% 
                             c("swinging_strike", "swinging_strike_blocked",
                               "foul_tip"), 1, Whiff)) |>
    select(-sz_top, -sz_bot, -description, 
           -release_angle, -release_direction, -posttunnelbreak, -diffattunnel, 
           -Swing, -Strike, -InZone)
  
  write_csv(Data2023, "Tunnel2023.csv")
}
