server <- function(input, output, session) {
  
  Pitchers <- read_csv("Pitchers.csv")
  Batters <- read_csv("Batters.csv")
  
  PiratesPitchers <- Pitchers |> 
    filter(pitcher_team == "PIT") |>
    filter(!is.na(Status)) 
  
  PiratesBatters <- Batters |> 
    filter(batter_team == "PIT") |>
    filter(!is.na(Status))
  
  #Adding Reactives ####
  
  #Pitchers
  
  observeEvent(
    input$pitcher_status,
    updateSelectInput(session,
                      "pitcher_name", "Select Pitcher",
                      choices = levels(factor(filter(PiratesPitchers,
                                                     Status == isolate(input$pitcher_status))$pitcher_name))))
  observeEvent(
    input$pitcher_name,
    updateCheckboxGroupInput(session,
                             "pitch_name_pitcher", "Pitch Type",
                             choices = levels(factor(filter(PiratesPitchers,
                                                            pitcher_name == isolate(input$pitcher_name))$pitch_name))))
  observeEvent(
    input$pitcher_name,
    updateDateRangeInput(session,
                         "game_date_pitcher", "Select Date Range",
                         start = min(PiratesPitchers$game_date),
                         end = max(PiratesPitchers$game_date)))
  
  #Batters
  
  observeEvent(
    input$batter_status,
    updateSelectInput(session,
                      "batter_name", "Select Batter",
                      choices = levels(factor(filter(PiratesBatters,
                                                     Status == isolate(input$batter_status))$batter_name))))
  observeEvent(
    input$batter_name,
    updateCheckboxGroupInput(session,
                             "pitch_name_batter", "Pitch Type",
                             choices = levels(factor(filter(PiratesBatters,
                                                            batter_name == isolate(input$batter_name))$pitch_name))))
  observeEvent(
    input$batter_name,
    updateDateRangeInput(session,
                         "game_date_batter", "Select Date Range",
                         start = min(PiratesBatters$game_date),
                         end = max(PiratesBatters$game_date)))
  
  #Filtered Datasets ####
  
  PitcherData <- reactive({
    PiratesPitchers |>
      filter(Status == input$pitcher_status) |>
      filter(pitcher_name == input$pitcher_name) |>
      filter(pitch_name %in% input$pitch_name_pitcher) |>
      filter(stand %in% input$stand) |>
      filter(balls %in% input$balls_pitcher) |>
      filter(strikes %in% input$strikes_pitcher) |>  
      filter(game_date >= input$game_date_pitcher[1] &
               game_date <= input$game_date_pitcher[2])
  })
  
  BatterData <- reactive({
    PiratesBatters |>
      filter(Status == input$batter_status) |>
      filter(batter_name == input$batter_name) |>
      filter(pitch_name %in% input$pitch_name_batter) |>
      filter(p_throws %in% input$p_throws) |>
      filter(balls %in% input$balls_batter) |>
      filter(strikes %in% input$strikes_batter) |>
      filter(game_date >= input$game_date_batter[1] &
               game_date <= input$game_date_batter[2])
  })
  
  # Pitcher Summary ####
  
  output$pitcher_percentiles <- renderPlot({
    
    FIP_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                FIP = 3.05 + (((sum(HR, na.rm = TRUE)*13) + (3*(sum(BB, na.rm = TRUE) + sum(HBP, na.rm = TRUE))) - 
                                 (2*sum(K, na.rm = TRUE))) / (sum(IP, na.rm = TRUE)))) |>
      filter(FIP != "NaN" & FIP != "Inf") |>
      mutate(rank = rank(FIP)) |>
      mutate(FIP = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, FIP) |>
      filter(pitcher_name == input$pitcher_name)
    
    WOBA_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                woba_value = mean(woba_value, na.rm = TRUE)) |>
      mutate(rank = rank(woba_value)) |>
      mutate(`WOBA Value` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `WOBA Value`) |>
      filter(pitcher_name == input$pitcher_name)
    
    BA_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                babip_value = mean(babip_value, na.rm = TRUE)) |>
      mutate(rank = rank(babip_value)) |>
      mutate(`Batting Average` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `Batting Average`) |>
      filter(pitcher_name == input$pitcher_name)
    
    xWOBA_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                estimated_woba_using_speedangle = mean(estimated_woba_using_speedangle, na.rm = TRUE)) |>
      mutate(rank = rank(estimated_woba_using_speedangle)) |>
      mutate(`xWOBA Value` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `xWOBA Value`) |>
      filter(pitcher_name == input$pitcher_name)
    
    xBA_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                estimated_ba_using_speedangle = mean(estimated_ba_using_speedangle, na.rm = TRUE)) |>
      mutate(rank = rank(estimated_ba_using_speedangle)) |>
      mutate(`xBA Value` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `xBA Value`) |>
      filter(pitcher_name == input$pitcher_name)
    
    velo_percentile <- Pitchers |>
      filter(pitch_name %in% c("Fastball", "Cutter", "Sinker")) |>
      summarise(.by = "pitcher_name",
                release_speed = mean(release_speed, na.rm = TRUE)) |>
      mutate(rank = rank(release_speed)) |>
      mutate(`Fastball Velocity` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `Fastball Velocity`) |>
      filter(pitcher_name == input$pitcher_name)
    
    spin_percentile <- Pitchers |>
      filter(pitch_name %in% c("Fastball", "Cutter", "Sinker")) |>
      summarise(.by = "pitcher_name", spin = mean(release_spin_rate, na.rm = TRUE)) |>
      mutate(rank = rank(spin)) |>
      mutate(`Fastball Spin` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `Fastball Spin`) |>
      filter(pitcher_name == input$pitcher_name)
    
    exit_velo_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                launch_speed = mean(launch_speed, na.rm = TRUE)) |>
      mutate(rank = rank(launch_speed)) |>
      mutate(`Exit Velo` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `Exit Velo`) |>
      filter(pitcher_name == input$pitcher_name)
    
    chase_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                Chase = mean(Chase, na.rm = TRUE)) |>
      mutate(rank = rank(Chase)) |>
      mutate(`Chase %` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `Chase %`) |>
      filter(pitcher_name == input$pitcher_name)
    
    whiff_percentile <- Pitchers |>
      summarise(.by = "pitcher_name",
                Whiff = mean(Whiff, na.rm = TRUE)) |>
      mutate(rank = rank(Whiff)) |>
      mutate(`Whiff %` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `Whiff %`) |>
      filter(pitcher_name == input$pitcher_name)
    
    strikeout_percentile <- Pitchers |>
      summarise(.by = "pitcher_name", 
                K = mean(K, na.rm = TRUE)) |>
      mutate(rank = rank(K)) |>
      mutate(`K %` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `K %`) |>
      filter(pitcher_name == input$pitcher_name)
    
    walk_percentile <- Pitchers |>
      summarise(.by = "pitcher_name", 
                BB = mean(BB, na.rm = TRUE)) |>
      mutate(rank = rank(BB)) |>
      mutate(`BB %` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `BB %`) |>
      filter(pitcher_name == input$pitcher_name)
    
    hard_hit_percentile <- Pitchers |>
      summarise(.by = "pitcher_name", 
                HardHit = mean(HardHit, na.rm = TRUE)) |>
      mutate(rank = rank(HardHit)) |>
      mutate(`Hard Hit %` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `Hard Hit %`) |>
      filter(pitcher_name == input$pitcher_name)
    
    ground_ball_percentile <- Pitchers |>
      summarise(.by = "pitcher_name", 
                GroundBall = mean(GroundBall, na.rm = TRUE)) |>
      mutate(rank = rank(GroundBall)) |>
      mutate(`GB %` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, `GB %`) |>
      filter(pitcher_name == input$pitcher_name)
    
    extension_percentile <- Pitchers |>
      summarise(.by = "pitcher_name", 
                release_extension = mean(release_extension, na.rm = TRUE)) |>
      mutate(rank = rank(release_extension)) |>
      mutate(Extension = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(pitcher_name, Extension) |>
      filter(pitcher_name == input$pitcher_name)
    
    Percentile_Data <- Pitchers |>
      left_join(FIP_percentile, by = "pitcher_name") |>
      left_join(WOBA_percentile, by = "pitcher_name") |>
      left_join(BA_percentile, by = "pitcher_name") |>
      left_join(xWOBA_percentile, by = "pitcher_name") |>
      left_join(xBA_percentile, by = "pitcher_name") |>
      left_join(velo_percentile, by = "pitcher_name") |>
      left_join(spin_percentile, by = "pitcher_name") |>
      left_join(exit_velo_percentile, by = "pitcher_name") |>
      left_join(chase_percentile, by = "pitcher_name") |>
      left_join(whiff_percentile, by = "pitcher_name") |>
      left_join(strikeout_percentile, by = "pitcher_name") |>
      left_join(walk_percentile, by = "pitcher_name") |>
      left_join(hard_hit_percentile, by = "pitcher_name") |>
      left_join(ground_ball_percentile, by = "pitcher_name") |>
      left_join(extension_percentile, by = "pitcher_name") |>
      select(-pitcher_name) |>
      pivot_longer(cols = c("FIP", 
                            "WOBA Value",
                            "Batting Average",
                            "xWOBA Value",
                            "xBA Value",
                            "Fastball Velocity",
                            "Fastball Spin",
                            "Exit Velo",
                            "Chase %",
                            "Whiff %",
                            "K %",
                            "BB %",
                            "Hard Hit %",
                            "GB %",
                            "Extension"),
                   names_to = "Category", values_to = "Percentile") |>
      filter(!is.na(Percentile)) |>
      mutate(color = ifelse(Percentile >= 50, "red", "blue"))
    
    ggdotchart(Percentile_Data, x = "Category", y = "Percentile",
               color = "color",  
               palette = get_palette(c("#4393C3","#CC0000"), 2),
               sorting = "descending", 
               rotate = TRUE,
               add = "segments",                            
               dot.size = 12,                                 
               label = round(Percentile_Data$Percentile),    
               font.label = list(color = "white", size = 12, 
                                 vjust = 0.5),               
               ggtheme = theme_pubr()) + theme(legend.position="none") + xlab("")
  })
  
  output$pitcher_summary_table <- renderDataTable({
    
    Pitches_Thrown <- PitcherData() |>
      summarise(.by = "pitch_name", `Pitches Thrown` = n())
    
    Batted_Balls <- PitcherData() |>
      filter(!is.na(bb_type)) |>
      summarise(.by = "pitch_name", `Batted Balls` = n())
    
    WOBA <- PitcherData() |>
      summarise(.by = "pitch_name", `WOBA` = mean(woba_value, na.rm = TRUE))
    
    xWOBA <- PitcherData() |>
      summarise(.by = "pitch_name", `xWOBA` = mean(estimated_woba_using_speedangle, na.rm = TRUE))
    
    Velocity <- PitcherData() |>
      summarise(.by = "pitch_name", Velocity = mean(release_speed, na.rm = TRUE))
    
    Exit_Velocity <- PitcherData() |>
      summarise(.by = "pitch_name", `Exit Velo` = mean(launch_speed, na.rm = TRUE))
    
    Launch_Angle <- PitcherData() |>
      summarise(.by = "pitch_name", `Launch Angle` = mean(launch_angle, na.rm = TRUE))
    
    Hard_Hit <- PitcherData() |>
      filter(!is.na(launch_speed)) |>
      mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0)) |>
      summarise(.by = "pitch_name", `Hard Hit %` = mean(hard_hit, na.rm = TRUE))
    
    Strikeout <- PitcherData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(strikeout = ifelse(events == "strikeout", 1, 0)) |>
      summarise(.by = "pitch_name", `K %` = mean(strikeout, na.rm = TRUE))
    
    Walk <- PitcherData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(walk = ifelse(events %in% c("walk", "hit_by_pitch"), 1, 0)) |>
      summarise(.by = "pitch_name", `BB %` = mean(walk, na.rm = TRUE))
    
    Spin_Rate <- PitcherData() |>
      summarise(.by = "pitch_name", `Spin` = mean(release_spin_rate, na.rm = TRUE))
    
    Spin_Axis <- PitcherData() |>
      summarise(.by = "pitch_name", `Spin Axis` = mean(spin_axis, na.rm = TRUE))
    
    Spin_Eff <- PitcherData() |>
      summarise(.by = "pitch_name", `Spin Efficiency` = mean(spin_eff, na.rm = TRUE))
    
    Bauer_Units <- PitcherData() |>
      summarise(.by = "pitch_name", `Bauer Units` = mean(Bauer, na.rm = TRUE))
    
    Horz_Break <- PitcherData() |>
      summarise(.by = "pitch_name", `Horizontal Break` = mean(pfx_x, na.rm = TRUE))
    
    Vert_Break <- PitcherData() |>
      summarise(.by = "pitch_name", `Vertical Break` = mean(pfx_z, na.rm = TRUE))
    
    Groundball <- PitcherData() |>
      filter(!is.na(bb_type)) |>
      mutate(ground_ball = ifelse(bb_type == "ground_ball", 1, 0)) |>
      summarise(.by = "pitch_name", `GB%` = mean(ground_ball, na.rm = TRUE))
    
    Whiff <- PitcherData() |>
      filter(description %in% c("hit_into_play", "swinging_strike", "foul",
                                "swinging_strike_blocked", "foul_tip")) |>
      mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked",
                                               "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(whiff, na.rm = TRUE))
    
    Chase <- PitcherData() |>
      filter(plate_z < sz_bot - 0.25 |
               plate_z > sz_top + 0.25 |
               plate_x < -8.5/12 |
               plate_x > 8.5/12) |>
      filter(description != "bunt_foul_tip" & description != "foul_bunt" & 
               description != "missed_bunt" & description != "hit_by_pitch") |>
      mutate(chase = ifelse(description %in% c("hit_into_play", "swinging_strike", "foul",
                                               "swinging_strike_blocked", "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(chase, na.rm = TRUE))
    
    Pitches_Thrown |>
      left_join(Velocity, by = "pitch_name") |>
      left_join(Batted_Balls, by = "pitch_name") |>
      left_join(WOBA, by = "pitch_name") |>
      left_join(xWOBA, by = "pitch_name") |>
      left_join(Exit_Velocity, by = "pitch_name") |>
      left_join(Launch_Angle, by = "pitch_name") |>
      left_join(Strikeout, by = "pitch_name") |>
      left_join(Walk, by = "pitch_name") |>
      left_join(Whiff, by = "pitch_name") |>
      left_join(Chase, by = "pitch_name") |>
      left_join(Hard_Hit, by = "pitch_name") |>
      left_join(Groundball, by = "pitch_name") |>
      left_join(Velocity, by = "pitch_name") |>
      left_join(Spin_Rate, by = "pitch_name") |>
      left_join(Spin_Axis, by = "pitch_name") |>
      left_join(Spin_Eff, by = "pitch_name") |>
      left_join(Bauer_Units, by = "pitch_name") |>
      left_join(Horz_Break, by = "pitch_name") |>
      left_join(Vert_Break, by = "pitch_name") |>
      arrange(desc(`Pitches Thrown`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      column_to_rownames("Pitch Type") |>
      mutate_if(is.numeric, round, digits = 2) 
  })
  
  # Pitcher Metrics ####
  
  output$release_points <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(release_pos_x) & !is.na(release_pos_z)) |>
      ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name,
                 label = release_speed,
                 label2 = release_spin_rate,
                 label3 = spin_axis,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      scale_x_continuous(limits = c(-60,60)) +
      scale_y_continuous(limits = c(10,100)) +
      ggtitle("Release Points") +
      xlab("Horizontal (Inches)") +
      ylab("Vertical (Inches)") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$release_angles <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(release_direction) & !is.na(release_angle)) |>
      ggplot(aes(x = release_direction, y = release_angle, color = pitch_name,
                 label = release_speed,
                 label2 = release_spin_rate,
                 label3 = spin_axis,
                 label4 = release_pos_x,
                 label5 = release_pos_z,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      scale_x_continuous(limits = c(-10,10)) +
      scale_y_continuous(limits = c(-10,10)) +
      ggtitle("Release Angles") +
      xlab("Release Direction (Degrees)") +
      ylab("Release Angle (Degrees)") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$tunnel_points <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(x_tunnel) & !is.na(z_tunnel)) |>
      mutate(x_tunnel = x_tunnel*12) |>
      mutate(z_tunnel = z_tunnel*12) |>
      ggplot(aes(x = x_tunnel, y = z_tunnel, color = pitch_name,
                 label = release_speed,
                 label2 = release_spin_rate,
                 label3 = spin_axis,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = release_pos_x,
                 label7 = release_pos_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      scale_x_continuous(limits = c(-40,40)) +
      scale_y_continuous(limits = c(0,60)) +
      ggtitle("Tunnel Points") +
      xlab("Horizontal Tunnel (Inches)") +
      ylab("Vertical Tunnel (Inches)") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA)) 
  })
  
  output$pitch_movement <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
      ggplot(aes(x = pfx_x, y = pfx_z, color = pitch_name,
                 label = release_speed,
                 label2 = release_spin_rate,
                 label3 = spin_axis,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = release_pos_x,
                 label7 = release_pos_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      scale_x_continuous(limits = c(-30,30)) +
      scale_y_continuous(limits = c(-30,30)) +
      ggtitle("Pitch Movement") +
      xlab("Horizontal Break (Inches)") +
      ylab("Induced Vertical Break (Inches)") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA)) 
  })
  
  output$pitch_spin <- renderPlot({
    
    PitcherData() |>
      filter(!is.na(release_spin_rate) & !is.na(spin_axis)) |>
      ggplot(aes(x = spin_axis, y = release_spin_rate, color = pitch_name)) +
      geom_point(size = 1, alpha = 0.6) +
      coord_polar() +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      ggtitle("Pitch Spin") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA)) 
  })
  
  output$pitch_speed <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(release_speed) & !is.na(effective_speed)) |>
      ggplot(aes(x = effective_speed, y = release_speed, color = pitch_name,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      ggtitle("Pitch Velocity") +
      xlab("Effective Velocity") +
      ylab("Velocity") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA)) 
  })
  
  #Pitcher K-Zone ####
  
  Left = -8.5/12
  Right = 8.5/12
  Bottom = 18.29/12
  Top = 44.08/12
  Width = (Right - Left) / 3
  Height = (Top - Bottom) / 3
  
  output$kzone_freq_pitcher <- renderPlotly({
    
    PitcherData() |>
      ggplot(aes(x = plate_x, y = plate_z, color = pitch_name,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Pitch Locations") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_speed_pitcher <- renderPlotly({
    
    PitcherData() |>
      ggplot(aes(x = plate_x, y = plate_z, color = release_speed,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Pitch Speed") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_ba_pitcher <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(babip_value)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = babip_value,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Batting Average") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_xba_pitcher <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(estimated_ba_using_speedangle)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = estimated_ba_using_speedangle,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Expected Batting Average") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_swing_pitcher <- renderPlotly({
    
    PitcherData() |>
      ggplot(aes(x = plate_x, y = plate_z, color = Swing,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Swing Decision") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_whiff_pitcher <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(Whiff)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Whiff,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Swings & Misses") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_ev_pitcher <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(launch_speed)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = launch_speed,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Exit Velocity") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_la_pitcher <- renderPlotly({
    
    PitcherData() |>
      filter(!is.na(launch_angle)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = launch_angle,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description,
                 label9 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Launch Angle") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  #Pitcher Time Table ####
  
  output$pitcher_time_table <- renderDataTable({
    
    Pitches_Thrown <- PitcherData() |>
      summarise(.by = "game_date", `Pitches Thrown` = n())
    
    Batted_Balls <- PitcherData() |>
      filter(!is.na(bb_type)) |>
      summarise(.by = "game_date", `Batted Balls` = n())
    
    WOBA <- PitcherData() |>
      summarise(.by = "game_date", `WOBA` = mean(woba_value, na.rm = TRUE))
    
    xWOBA <- PitcherData() |>
      summarise(.by = "game_date", `xWOBA` = mean(estimated_woba_using_speedangle, na.rm = TRUE))
    
    Exit_Velocity <- PitcherData() |>
      summarise(.by = "game_date", `Exit Velo` = mean(launch_speed, na.rm = TRUE))
    
    Launch_Angle <- PitcherData() |>
      summarise(.by = "game_date", `Launch Angle` = mean(launch_angle, na.rm = TRUE))
    
    Hard_Hit <- PitcherData() |>
      filter(!is.na(launch_speed)) |>
      mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0)) |>
      summarise(.by = "game_date", `Hard Hit %` = mean(hard_hit, na.rm = TRUE))
    
    Strikeout <- PitcherData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(strikeout = ifelse(events == "strikeout", 1, 0)) |>
      summarise(.by = "game_date", `K %` = mean(strikeout, na.rm = TRUE))
    
    Walk <- PitcherData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(walk = ifelse(events %in% c("walk", "hit_by_pitch"), 1, 0)) |>
      summarise(.by = "game_date", `BB %` = mean(walk, na.rm = TRUE))
    
    Groundball <- PitcherData() |>
      filter(!is.na(bb_type)) |>
      mutate(ground_ball = ifelse(bb_type == "ground_ball", 1, 0)) |>
      summarise(.by = "game_date", `GB%` = mean(ground_ball, na.rm = TRUE))
    
    Whiff <- PitcherData() |>
      filter(description %in% c("hit_into_play", "swinging_strike", "foul",
                                "swinging_strike_blocked", "foul_tip")) |>
      mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked",
                                               "foul_tip"), 1, 0)) |>
      summarise(.by = "game_date", `Whiff %` = mean(whiff, na.rm = TRUE))
    
    Chase <- PitcherData() |>
      filter(plate_z < sz_bot - 0.25 |
               plate_z > sz_top + 0.25 |
               plate_x < -8.5/12 |
               plate_x > 8.5/12) |>
      filter(description != "bunt_foul_tip" & description != "foul_bunt" & 
               description != "missed_bunt" & description != "hit_by_pitch") |>
      mutate(chase = ifelse(description %in% c("hit_into_play", "swinging_strike", "foul",
                                               "swinging_strike_blocked", "foul_tip"), 1, 0)) |>
      summarise(.by = "game_date", `Chase %` = mean(chase, na.rm = TRUE))
    
    Pitches_Thrown |>
      left_join(Batted_Balls, by = "game_date") |>
      left_join(WOBA, by = "game_date") |>
      left_join(xWOBA, by = "game_date") |>
      left_join(Exit_Velocity, by = "game_date") |>
      left_join(Launch_Angle, by = "game_date") |>
      left_join(Strikeout, by = "game_date") |>
      left_join(Walk, by = "game_date") |>
      left_join(Whiff, by = "game_date") |>
      left_join(Chase, by = "game_date") |>
      left_join(Hard_Hit, by = "game_date") |>
      left_join(Groundball, by = "game_date") |>
      rename(`Game Date` = "game_date") |>
      arrange(desc(`Game Date`)) |>
      column_to_rownames("Game Date") |>
      mutate_if(is.numeric, round, digits = 2) 
  })
  
  # Batter Summary ####
  
  output$batter_percentiles <- renderPlot({
    
    WOBA_percentile <- Batters |>
      summarise(.by = "batter_name",
                woba_value = mean(woba_value, na.rm = TRUE)) |>
      mutate(rank = rank(woba_value)) |>
      mutate(`WOBA Value` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `WOBA Value`) |>
      filter(batter_name == input$batter_name)
    
    BA_percentile <- Batters |>
      summarise(.by = "batter_name",
                babip_value = mean(babip_value, na.rm = TRUE)) |>
      mutate(rank = rank(babip_value)) |>
      mutate(`Batting Average` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `Batting Average`) |>
      filter(batter_name == input$batter_name)
    
    xWOBA_percentile <- Batters |>
      summarise(.by = "batter_name",
                estimated_woba_using_speedangle = mean(estimated_woba_using_speedangle, na.rm = TRUE)) |>
      mutate(rank = rank(estimated_woba_using_speedangle)) |>
      mutate(`xWOBA Value` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `xWOBA Value`) |>
      filter(batter_name == input$batter_name)
    
    xBA_percentile <- Batters |>
      summarise(.by = "batter_name",
                estimated_ba_using_speedangle = mean(estimated_ba_using_speedangle, na.rm = TRUE)) |>
      mutate(rank = rank(estimated_ba_using_speedangle)) |>
      mutate(`xBA Value` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `xBA Value`) |>
      filter(batter_name == input$batter_name)
    
    exit_velo_percentile <- Batters |>
      summarise(.by = "batter_name",
                launch_speed = mean(launch_speed, na.rm = TRUE)) |>
      mutate(rank = rank(launch_speed)) |>
      mutate(`Exit Velo` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `Exit Velo`) |>
      filter(batter_name == input$batter_name)
    
    chase_percentile <- Batters |>
      summarise(.by = "batter_name",
                Chase = mean(Chase, na.rm = TRUE)) |>
      mutate(rank = rank(Chase)) |>
      mutate(`Chase %` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `Chase %`) |>
      filter(batter_name == input$batter_name)
    
    whiff_percentile <- Batters |>
      summarise(.by = "batter_name",
                Whiff = mean(Whiff, na.rm = TRUE)) |>
      mutate(rank = rank(Whiff)) |>
      mutate(`Whiff %` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `Whiff %`) |>
      filter(batter_name == input$batter_name)
    
    strikeout_percentile <- Batters |>
      summarise(.by = "batter_name", 
                K = mean(K, na.rm = TRUE)) |>
      mutate(rank = rank(K)) |>
      mutate(`K %` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `K %`) |>
      filter(batter_name == input$batter_name)
    
    walk_percentile <- Batters |>
      summarise(.by = "batter_name", 
                BB = mean(BB, na.rm = TRUE)) |>
      mutate(rank = rank(BB)) |>
      mutate(`BB %` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `BB %`) |>
      filter(batter_name == input$batter_name)
    
    hard_hit_percentile <- Batters |>
      summarise(.by = "batter_name", 
                HardHit = mean(HardHit, na.rm = TRUE)) |>
      mutate(rank = rank(HardHit)) |>
      mutate(`Hard Hit %` = round(100*((rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `Hard Hit %`) |>
      filter(batter_name == input$batter_name)
    
    ground_ball_percentile <- Batters |>
      summarise(.by = "batter_name", 
                GroundBall = mean(GroundBall, na.rm = TRUE)) |>
      mutate(rank = rank(GroundBall)) |>
      mutate(`GB %` = round(100*(1 - (rank-1)/(max(rank)-1)), digits = 0)) |>
      select(batter_name, `GB %`) |>
      filter(batter_name == input$batter_name)

    Percentile_Data <- Batters |>
      left_join(WOBA_percentile, by = "batter_name") |>
      left_join(BA_percentile, by = "batter_name") |>
      left_join(xWOBA_percentile, by = "batter_name") |>
      left_join(xBA_percentile, by = "batter_name") |>
      left_join(exit_velo_percentile, by = "batter_name") |>
      left_join(chase_percentile, by = "batter_name") |>
      left_join(whiff_percentile, by = "batter_name") |>
      left_join(strikeout_percentile, by = "batter_name") |>
      left_join(walk_percentile, by = "batter_name") |>
      left_join(hard_hit_percentile, by = "batter_name") |>
      left_join(ground_ball_percentile, by = "batter_name") |>
      select(-batter_name) |>
      pivot_longer(cols = c("WOBA Value",
                            "Batting Average",
                            "xWOBA Value",
                            "xBA Value",
                            "Exit Velo",
                            "Chase %",
                            "Whiff %",
                            "K %",
                            "BB %",
                            "Hard Hit %",
                            "GB %"),
                   names_to = "Category", values_to = "Percentile") |>
      filter(!is.na(Percentile)) |>
      mutate(color = ifelse(Percentile >= 50, "red", "blue"))
    
    ggdotchart(Percentile_Data, x = "Category", y = "Percentile",
               color = "color",  
               palette = get_palette(c("#4393C3","#CC0000"), 2),
               sorting = "descending", 
               rotate = TRUE,
               add = "segments",                            
               dot.size = 12,                                 
               label = round(Percentile_Data$Percentile),    
               font.label = list(color = "white", size = 12, 
                                 vjust = 0.5),               
               ggtheme = theme_pubr()) + theme(legend.position="none") + xlab("")
  })
  
  output$batter_summary_table <- renderDataTable({
    
    Pitches_Seen <- BatterData() |>
      summarise(.by = "pitch_name", `Pitches Seen` = n())
    
    Batted_Balls <- BatterData() |>
      filter(!is.na(bb_type)) |>
      summarise(.by = "pitch_name", `Batted Balls` = n())
    
    WOBA <- BatterData() |>
      summarise(.by = "pitch_name", `WOBA` = mean(woba_value, na.rm = TRUE))
    
    xWOBA <- BatterData() |>
      summarise(.by = "pitch_name", `xWOBA` = mean(estimated_woba_using_speedangle, na.rm = TRUE))
    
    Exit_Velocity <- BatterData() |>
      summarise(.by = "pitch_name", `Exit Velo` = mean(launch_speed, na.rm = TRUE))
    
    Launch_Angle <- BatterData() |>
      summarise(.by = "pitch_name", `Launch Angle` = mean(launch_angle, na.rm = TRUE))
    
    Hard_Hit <- BatterData() |>
      filter(!is.na(launch_speed)) |>
      mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0)) |>
      summarise(.by = "pitch_name", `Hard Hit %` = mean(hard_hit, na.rm = TRUE))
    
    Strikeout <- BatterData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(strikeout = ifelse(events == "strikeout", 1, 0)) |>
      summarise(.by = "pitch_name", `K %` = mean(strikeout, na.rm = TRUE))
    
    Walk <- BatterData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(walk = ifelse(events %in% c("walk", "hit_by_pitch"), 1, 0)) |>
      summarise(.by = "pitch_name", `BB %` = mean(walk, na.rm = TRUE))
    
    Groundball <- BatterData() |>
      filter(!is.na(bb_type)) |>
      mutate(ground_ball = ifelse(bb_type == "ground_ball", 1, 0)) |>
      summarise(.by = "pitch_name", `GB%` = mean(ground_ball, na.rm = TRUE))
    
    Whiff <- BatterData() |>
      filter(description %in% c("hit_into_play", "swinging_strike", "foul",
                                "swinging_strike_blocked", "foul_tip")) |>
      mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked",
                                               "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(whiff, na.rm = TRUE))
    
    Chase <- BatterData() |>
      filter(plate_z < sz_bot - 0.25 |
               plate_z > sz_top + 0.25 |
               plate_x < -8.5/12 |
               plate_x > 8.5/12) |>
      filter(description != "bunt_foul_tip" & description != "foul_bunt" & 
               description != "missed_bunt" & description != "hit_by_pitch") |>
      mutate(chase = ifelse(description %in% c("hit_into_play", "swinging_strike", "foul",
                                               "swinging_strike_blocked", "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(chase, na.rm = TRUE))
    
    Pitches_Seen |>
      left_join(Batted_Balls, by = "pitch_name") |>
      left_join(WOBA, by = "pitch_name") |>
      left_join(xWOBA, by = "pitch_name") |>
      left_join(Exit_Velocity, by = "pitch_name") |>
      left_join(Launch_Angle, by = "pitch_name") |>
      left_join(Strikeout, by = "pitch_name") |>
      left_join(Walk, by = "pitch_name") |>
      left_join(Whiff, by = "pitch_name") |>
      left_join(Chase, by = "pitch_name") |>
      left_join(Hard_Hit, by = "pitch_name") |>
      left_join(Groundball, by = "pitch_name") |>
      arrange(desc(`Pitches Seen`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      column_to_rownames("Pitch Type") |>
      mutate_if(is.numeric, round, digits = 2) 
  })
  
  # Batter Metrics ####
  
  output$spray_chart <- renderPlotly({
    
    geom_baseball(league = "MLB") +
      geom_point(data = BatterData(),
                 aes(location_x, location_y,
                     color = bb_type)) +
      scale_colour_manual(values =
                            c(ground_ball = "yellow", line_drive = "red", 
                              fly_ball = "purple", popup = "blue")) +
      ggtitle(paste(input$batter_name, "Spray Chart Spring 2024")) 
    
  })
  
  output$VAA_swingangle <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(vert_appr_angle) & !is.na(attack_angle)) |>
      ggplot(aes(x = attack_angle, y = vert_appr_angle, color = pitch_name,
                 label = launch_speed,
                 label2 = bb_type,
                 label3 = events,
                 label4 = estimated_woba_using_speedangle,
                 label5 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      xlim(c(-50,60)) +
      ylim(c(-15,0)) +
      geom_vline(xintercept = 0) +
      geom_vline(xintercept = 30) +
      ggtitle("Approach Angle vs Attack Angle") +
      xlab("Swing Attack Angle") +
      ylab("Pitch Approach Angle") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  #Batter K-Zone ####
  
  Left = -8.5/12
  Right = 8.5/12
  Bottom = 18.29/12
  Top = 44.08/12
  Width = (Right - Left) / 3
  Height = (Top - Bottom) / 3
  
  output$kzone_freq_batter <- renderPlotly({
    
    BatterData() |>
      ggplot(aes(x = plate_x, y = plate_z, color = pitch_name,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(Changeup = "blue", Fastball = "red",
                                    Slider = "orange", Curveball = "green",
                                    Cutter = "purple",Sinker = "yellow",
                                    Splitter = "pink", Sweeper = "maroon",
                                    Screwball = "black", Knuckleball = "gray",
                                    Forkball = "darkgreen")) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Pitch Locations") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_ba_batter <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(babip_value)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = babip_value,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Batting Average") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_xba_batter <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(estimated_ba_using_speedangle)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = estimated_ba_using_speedangle,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Expected Batting Average") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_swing_batter <- renderPlotly({
    
    BatterData() |>
      ggplot(aes(x = plate_x, y = plate_z, color = Swing,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Swing Decision") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_whiff_batter <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(Whiff)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Whiff,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Swings & Misses") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_ev_batter <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(launch_speed)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = launch_speed,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Exit Velocity") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_la_batter <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(launch_angle)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = launch_angle,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Launch Angle") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_vaa_batter <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(vert_appr_angle)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = vert_appr_angle,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Pitch Approach Angle") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  output$kzone_sa_batter <- renderPlotly({
    
    BatterData() |>
      filter(!is.na(attack_angle)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = attack_angle,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description,
                 label5 = game_date)) +
      geom_point(size = 1) +
      labs(color=NULL) +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom), color = "black") +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top), color = "black") +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height), color = "black") +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height), color = "black") +
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top), color = "black") +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Right), yend = (8.5/12), color = "black") +
      geom_segment(x = (Left), y = (8.5/12), xend = (Left), yend = (4.25/12), color = "black") +
      geom_segment(x = (Right), y = (8.5/12), xend = (Right), yend = (4.25/12), color = "black") +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      geom_segment(x = (Right), y = (4.25/12), xend = (0), yend = (0), color = "black") +
      xlim(-2.5,2.5) +
      ylim(0,4.5) +
      ggtitle("Swing Angle") +
      xlab("Plate Width") +
      ylab("Height") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))
  })
  
  #Batter Time Table ####
  
  output$batter_time_table <- renderDataTable({
    
    Pitches_Seen <- BatterData() |>
      summarise(.by = "game_date", `Pitches Seen` = n())
    
    Batted_Balls <- BatterData() |>
      filter(!is.na(bb_type)) |>
      summarise(.by = "game_date", `Batted Balls` = n())
    
    WOBA <- BatterData() |>
      summarise(.by = "game_date", `WOBA` = mean(woba_value, na.rm = TRUE))
    
    xWOBA <- BatterData() |>
      summarise(.by = "game_date", `xWOBA` = mean(estimated_woba_using_speedangle, na.rm = TRUE))
    
    Exit_Velocity <- BatterData() |>
      summarise(.by = "game_date", `Exit Velo` = mean(launch_speed, na.rm = TRUE))
    
    Launch_Angle <- BatterData() |>
      summarise(.by = "game_date", `Launch Angle` = mean(launch_angle, na.rm = TRUE))
    
    Hard_Hit <- BatterData() |>
      filter(!is.na(launch_speed)) |>
      mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0)) |>
      summarise(.by = "game_date", `Hard Hit %` = mean(hard_hit, na.rm = TRUE))
    
    Strikeout <- BatterData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(strikeout = ifelse(events == "strikeout", 1, 0)) |>
      summarise(.by = "game_date", `K %` = mean(strikeout, na.rm = TRUE))
    
    Walk <- BatterData() |>
      filter(!is.na(events) & !grepl("caught_stealing", events) &
               !grepl("pickoff", events) & !grepl("stolen_base", events) &
               events != "wild_pitch") |>
      mutate(walk = ifelse(events %in% c("walk", "hit_by_pitch"), 1, 0)) |>
      summarise(.by = "game_date", `BB %` = mean(walk, na.rm = TRUE))
    
    Groundball <- BatterData() |>
      filter(!is.na(bb_type)) |>
      mutate(ground_ball = ifelse(bb_type == "ground_ball", 1, 0)) |>
      summarise(.by = "game_date", `GB%` = mean(ground_ball, na.rm = TRUE))
    
    Whiff <- BatterData() |>
      filter(description %in% c("hit_into_play", "swinging_strike", "foul",
                                "swinging_strike_blocked", "foul_tip")) |>
      mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked",
                                               "foul_tip"), 1, 0)) |>
      summarise(.by = "game_date", `Whiff %` = mean(whiff, na.rm = TRUE))
    
    Chase <- BatterData() |>
      filter(plate_z < sz_bot - 0.25 |
               plate_z > sz_top + 0.25 |
               plate_x < -8.5/12 |
               plate_x > 8.5/12) |>
      filter(description != "bunt_foul_tip" & description != "foul_bunt" & 
               description != "missed_bunt" & description != "hit_by_pitch") |>
      mutate(chase = ifelse(description %in% c("hit_into_play", "swinging_strike", "foul",
                                               "swinging_strike_blocked", "foul_tip"), 1, 0)) |>
      summarise(.by = "game_date", `Chase %` = mean(chase, na.rm = TRUE))
    
    Pitches_Seen |>
      left_join(Batted_Balls, by = "game_date") |>
      left_join(WOBA, by = "game_date") |>
      left_join(xWOBA, by = "game_date") |>
      left_join(Exit_Velocity, by = "game_date") |>
      left_join(Launch_Angle, by = "game_date") |>
      left_join(Strikeout, by = "game_date") |>
      left_join(Walk, by = "game_date") |>
      left_join(Whiff, by = "game_date") |>
      left_join(Chase, by = "game_date") |>
      left_join(Hard_Hit, by = "game_date") |>
      left_join(Groundball, by = "game_date") |>
      rename(`Game Date` = "game_date") |>
      arrange(desc(`Game Date`)) |>
      column_to_rownames("Game Date") |>
      mutate_if(is.numeric, round, digits = 2) 
  })
  
}
