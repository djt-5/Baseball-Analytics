{
  #Load Data ####
  library(tidyverse)
  Data <- read_csv("Data2023.csv") 
  
  Leaderboard <- Data |>
    summarise(.by = "pitcher_name", N = n()) |>
    filter(N >= 750)
  
  qualified <- as.vector(Leaderboard$pitcher_name)
  
  Data <- Data |>
    filter(pitcher_name %in% qualified) |>
    mutate(pfx_x = 12 * pfx_x) |>
    mutate(pfx_z = 12 * pfx_z) |>
    mutate(pitch_name = ifelse(pitch_name == "4-Seam Fastball", "Four-Seam",
                                pitch_name)) |>
    mutate(pitch_name = ifelse(pitch_name == "Knuckle Curve", "Knuckle-Curve",
                                pitch_name)) |>
    mutate(pitch_name = ifelse(pitch_name == "Pitch Out", "Pitch-Out",
                               pitch_name)) |>
    mutate(pitch_name = ifelse(pitch_name == "Slow Curve", "Slow-Curve",
                               pitch_name)) |>
  #Add Release Angles & Tunnels ####
    select(pitch_name, release_speed, release_pos_x, release_pos_z, 
         stand, p_throws, plate_x, plate_z, vx0, vy0, vz0,
         ax, ay, az, sz_top, sz_bot, release_spin_rate, spin_axis,
         release_extension, pfx_x, pfx_z, pitcher_name, game_pk, inning, 
         at_bat_number, pitch_number, batter_name, description, delta_run_exp, 
         game_date, balls, strikes) |>
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
    mutate(post_tunnel_break_x = (break_x - pre_tunnel_break_x)*12) |>
    mutate(post_tunnel_break_z = (pre_tunnel_break_z - break_z)*12) |>
    mutate(posttunnelbreak = sqrt(post_tunnel_break_x^2 +
                                    post_tunnel_break_z^2)) |>
    select(-breakless_plate_x, -breakless_plate_z, -breakless_tunnel_x,
           -breakless_tunnel_z, -break_x, -break_z, -pre_tunnel_break_x,
           -pre_tunnel_break_z) |>
    # Paste together columns for the listing function (saves time) #### 
    mutate(information = paste(pitch_name, x_tunnel, z_tunnel, plate_x, plate_z,
                             release_speed))
  
  #Pitch Sequences ####
  
  Pitch_Seq <- Data |>
    arrange(pitcher_name, game_pk, inning, at_bat_number, pitch_number) |>
    mutate(seq_scenario = paste(pitcher_name, batter_name, game_pk, inning, 
                                at_bat_number)) |>
    select(-inning, -at_bat_number, -batter_name)
  
  Seq_List <- split(Pitch_Seq, Pitch_Seq$seq_scenario)
  
  Seq_List_2 <- lapply(Seq_List, function(x) {
    mutate(x, prev_information = dplyr::lag(information))
  })
  
  Data <- bind_rows(Seq_List_2) |>
    select(-seq_scenario)
  
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
           -adrag, -amagx, -amagy, -amagz, -amag, -Cl, -S, -spinT) |>
  #Differential Information ####
    separate(prev_information, into = c("prev_pitch", "prev_x_tunnel", "prev_z_tunnel",
                                        "prev_plate_x", "prev_plate_z", "prev_release_speed"),
             sep =" ") 
  
  Data$prev_x_tunnel <- as.numeric(Data$prev_x_tunnel)
  Data$prev_z_tunnel <- as.numeric(Data$prev_z_tunnel)
  Data$prev_plate_x <- as.numeric(Data$prev_plate_x)
  Data$prev_plate_z <- as.numeric(Data$prev_plate_z)
  Data$prev_release_speed <- as.numeric(Data$prev_release_speed)
  
  Data <- Data |>
    mutate(diffattunnel = sqrt((x_tunnel - prev_x_tunnel)^2 + (z_tunnel - prev_z_tunnel)^2)) |>
    mutate(speeddiff = release_speed - prev_release_speed) |>
    mutate(breaktotunnelratio = posttunnelbreak/diffattunnel) |>
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
    mutate(InZoneWhiff = ifelse(InZone == 1 & Whiff == 1, 1, NA)) |>
    mutate(InZoneWhiff = ifelse(InZone == 1 & Whiff == 0, 0, InZoneWhiff)) |>
  #Last Minute Changes ####
    filter(pitch_name != "Pitch Out" &
             pitch_name != "Other" &
             pitch_name != "Eephus") |>
    mutate(delta_run_exp = -delta_run_exp) |>
    mutate(spin_eff = ifelse(spin_eff >= 1, 1, spin_eff)) |>
    mutate(release_angle = release_angle*57.29578) |>
    mutate(release_direction = release_direction*57.29578) |>
    mutate(breaktotunnelratio = breaktotunnelratio/12) 
  
  index <- c(1:8,15:22,26:36,38:44,46,47,51,52,53)
  
  Data <- Data[,index]
  
  write_csv(Data, "Data.csv")
}
