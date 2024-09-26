server <- function(input, output, session) {
  
  Data <- read_csv("Data.csv")
  
  #Adding Reactives ####
  
  #Pitchers
  
  observeEvent(
    input$pitcher_team,
    updateSelectInput(session,
                      "pitcher", "Select Pitcher",
                      choices = levels(factor(dplyr::filter(Data,
                                                     pitcher_team == isolate(input$pitcher_team))$pitcher))))
  observeEvent(
    input$pitcher,
    updateCheckboxGroupInput(session,
                             "pitch_name_pitcher", "Pitch Type",
                             choices = levels(factor(dplyr::filter(Data,
                                                            pitcher == isolate(input$pitcher))$pitch_name))))
  
  #Batters
  
  observeEvent(
    input$batter_team,
    updateSelectInput(session,
                      "batter", "Select Batter",
                      choices = levels(factor(dplyr::filter(Data,
                                                     batter_team == isolate(input$batter_team))$batter))))
  observeEvent(
    input$batter,
    updateCheckboxGroupInput(session,
                             "pitch_name_batter", "Pitch Type",
                             choices = levels(factor(dplyr::filter(Data,
                                                            batter == isolate(input$batter))$pitch_name))))
  
  #Filtered Datasets ####
  
  PitcherData <- reactive({
    
    Data |>
      dplyr::filter(pitcher_team == input$pitcher_team) |>
      dplyr::filter(pitcher == input$pitcher) |>
      dplyr::filter(pitch_name %in% input$pitch_name_pitcher) |>
      dplyr::filter(stand %in% input$batter_side) |>
      dplyr::filter(balls %in% input$balls_pitcher) |>
      dplyr::filter(strikes %in% input$strikes_pitcher) |>  
      dplyr::filter(plate_x >= input$slider_plate_x_pitcher[1] &
               plate_x <= input$slider_plate_x_pitcher[2]) |>
      dplyr::filter(plate_z >= input$slider_plate_z_pitcher[1] &
               plate_z <= input$slider_plate_z_pitcher[2]) |>
      dplyr::filter(pfx_x >= input$slider_pfx_x_pitcher[1] &
               pfx_x <= input$slider_pfx_x_pitcher[2]) |>
      dplyr::filter(pfx_z >= input$slider_pfx_z_pitcher[1] &
               pfx_z <= input$slider_pfx_z_pitcher[2]) |>
      dplyr::filter(release_speed >= input$slider_speed_pitcher[1] &
               release_speed <= input$slider_speed_pitcher[2]) |>
      dplyr::filter(release_spin_rate >= input$slider_spin_pitcher[1] &
               release_spin_rate <= input$slider_spin_pitcher[2]) |>
      dplyr::filter(spin_eff >= input$slider_spin_eff_pitcher[1] &
               spin_eff <= input$slider_spin_eff_pitcher[2])
  })
  
  BatterData <- reactive({
    
    Data |>
      dplyr::filter(batter_team == input$batter_team) |>
      dplyr::filter(batter == input$batter) |>
      dplyr::filter(pitch_name %in% input$pitch_name_batter) |>
      dplyr::filter(p_throws %in% input$p_throws) |>
      dplyr::filter(balls %in% input$balls_batter) |>
      dplyr::filter(strikes %in% input$strikes_batter) |>  
      dplyr::filter(plate_x >= input$slider_plate_x_batter[1] &
               plate_x <= input$slider_plate_x_batter[2]) |>
      dplyr::filter(plate_z >= input$slider_plate_z_batter[1] &
               plate_z <= input$slider_plate_z_batter[2]) |>
      dplyr::filter(pfx_x >= input$slider_pfx_x_batter[1] &
               pfx_x <= input$slider_pfx_x_batter[2]) |>
      dplyr::filter(pfx_z >= input$slider_pfx_z_batter[1] &
               pfx_z <= input$slider_pfx_z_batter[2]) |>
      dplyr::filter(release_speed >= input$slider_speed_batter[1] &
               release_speed <= input$slider_speed_batter[2])
  })
  
  # Pitcher Summary ####
  
  output$pitcher_summary_table <- renderDataTable({
    
    Pitches_Thrown <- PitcherData() |>
      summarise(.by = "pitch_name", `Pitches Thrown` = n())
    
    BA <- PitcherData() |>
      summarise(.by = "pitch_name", `Batting Average` = mean(babip_value, na.rm = TRUE))
    
    SLG <- PitcherData() |>
      summarise(.by = "pitch_name", Slugging = mean(slugging, na.rm = TRUE))
    
    Velocity <- PitcherData() |>
      summarise(.by = "pitch_name", Velocity = mean(release_speed, na.rm = TRUE))
    
    Strikeout <- PitcherData() |>
      dplyr::filter(!is.na(events)) |>
      mutate(strikeout = ifelse(events == "strikeout", 1, 0)) |>
      summarise(.by = "pitch_name", `K %` = mean(strikeout, na.rm = TRUE))
    
    Walk <- PitcherData() |>
      dplyr::filter(!is.na(events)) |>
      mutate(walk = ifelse(events %in% c("walk"), 1, 0)) |>
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
    
    Whiff <- PitcherData() |>
      dplyr::filter(description %in% c("hit_into_play", "swinging_strike", "foul",
                                "swinging_strike_blocked", "foul_tip")) |>
      mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked",
                                               "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(whiff, na.rm = TRUE))
    
    Chase <- PitcherData() |>
      dplyr::filter(plate_z < sz_bot - 0.25 |
               plate_z > sz_top + 0.25 |
               plate_x < -8.5/12 |
               plate_x > 8.5/12) |>
      dplyr::filter(description != "foul_bunt" & description != "sac_bunt") |>
      mutate(chase = ifelse(description %in% c("hit_into_play", "swinging_strike", "foul",
                                               "swinging_strike_blocked", "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(chase, na.rm = TRUE))
    
    Pitches_Thrown |>
      left_join(BA, by = "pitch_name") |>
      left_join(SLG, by = "pitch_name") |>
      left_join(Velocity, by = "pitch_name") |>
      left_join(Strikeout, by = "pitch_name") |>
      left_join(Walk, by = "pitch_name") |>
      left_join(Whiff, by = "pitch_name") |>
      left_join(Chase, by = "pitch_name") |>
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
      dplyr::filter(!is.na(release_pos_x) & !is.na(release_pos_z)) |>
      ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name,
                 label = release_speed,
                 label2 = release_spin_rate,
                 label3 = spin_axis,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
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
      dplyr::filter(!is.na(release_direction) & !is.na(release_angle)) |>
      ggplot(aes(x = release_direction, y = release_angle, color = pitch_name,
                 label = release_speed,
                 label2 = release_spin_rate,
                 label3 = spin_axis,
                 label4 = release_pos_x,
                 label5 = release_pos_z,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
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
      dplyr::filter(!is.na(x_tunnel) & !is.na(z_tunnel)) |>
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
                 label8 = description)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
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
      dplyr::filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
      ggplot(aes(x = pfx_x, y = pfx_z, color = pitch_name,
                 label = release_speed,
                 label2 = release_spin_rate,
                 label3 = spin_axis,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = release_pos_x,
                 label7 = release_pos_z,
                 label8 = description)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
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
      dplyr::filter(!is.na(release_spin_rate) & !is.na(spin_axis)) |>
      ggplot(aes(x = spin_axis, y = release_spin_rate, color = pitch_name)) +
      geom_point(size = 1, alpha = 0.6) +
      coord_polar() +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
      ggtitle("Pitch Spin") +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.x = element_text(color="black", size = 13, face = "bold"),
        axis.title.y = element_text(color="black" , size = 13, face = "bold"),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA)) 
  })
  
  output$pitch_speed <- renderPlotly({
    
    PitcherData() |>
      dplyr::filter(!is.na(release_speed) & !is.na(speed_drop)) |>
      ggplot(aes(x = speed_drop, y = release_speed, color = pitch_name,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
      ggtitle("Pitch Velocity") +
      xlab("Speed Drop") +
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
                 label8 = description)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
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
                 label8 = description)) +
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
      dplyr::filter(!is.na(babip_value)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = babip_value,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description)) +
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
  
  output$kzone_slg_pitcher <- renderPlotly({
    
    PitcherData() |>
      dplyr::filter(!is.na(slugging)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = slugging,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description)) +
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
      ggtitle("Slugging") +
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
                 label8 = description)) +
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
      dplyr::filter(!is.na(Whiff)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Whiff,
                 label = release_speed,
                 label2 = release_pos_x,
                 label3 = release_pos_z,
                 label4 = release_angle,
                 label5 = release_direction,
                 label6 = pfx_x,
                 label7 = pfx_z,
                 label8 = description)) +
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
  
  #Pitcher Variable Efficacy ####
  
  #Break
  
  plot_pfx_pitcher <- reactive({
    if(input$response_select_pitcher == "Whiff") {
      PitcherData() |>
        dplyr::filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
        select(pitcher, pfx_x, pfx_z, Whiff) |>
        ggplot(aes(x = pfx_x, y = pfx_z, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_break_pitcher) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select_pitcher == "Chase") {
      PitcherData() |>
        dplyr::filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
        select(pitcher, pfx_x, pfx_z, Chase) |>
        ggplot(aes(x = pfx_x, y = pfx_z, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_break_pitcher) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    }
  })
  
  output$plot_pfx_pitcher <- renderPlot({plot_pfx_pitcher()},height = 600, width = 1000)
  
  #Speed
  
  plot_release_speed_pitcher <- reactive({
    if(input$response_select_pitcher == "Whiff") {
      PitcherData() |>
        dplyr::filter(!is.na(release_speed) & !is.na(speed_drop)) |>
        select(pitcher, release_speed, speed_drop, Whiff) |>
        ggplot(aes(x = release_speed, y = speed_drop, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_speed_pitcher) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select_pitcher == "Chase") {
      PitcherData() |>
        dplyr::filter(!is.na(release_speed)) |>
        select(pitcher, release_speed, Chase) |>
        ggplot(aes(x = release_speed, y = speed_drop, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_speed_pitcher) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    }
  })
  
  output$plot_release_speed_pitcher <- renderPlot({plot_release_speed_pitcher()},height = 600, width = 1000)
  
  #Spin
  
  plot_release_spin_pitcher <- reactive({
    if(input$response_select_pitcher == "Whiff") {
      PitcherData() |>
        dplyr::filter(!is.na(release_spin_rate) & !is.na(spin_eff)) |>
        select(pitcher, release_spin_rate, spin_eff, Whiff) |>
        ggplot(aes(x = release_spin_rate, y = spin_eff, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_spin_pitcher) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select_pitcher == "Chase") {
      PitcherData() |>
        dplyr::filter(!is.na(release_spin_rate) & !is.na(spin_eff)) |>
        select(pitcher, release_spin_rate, spin_eff, Chase) |>
        ggplot(aes(x = release_spin_rate, y = spin_eff, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_spin_pitcher) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    } 
  })
  
  output$plot_release_spin_pitcher <- renderPlot({plot_release_spin_pitcher()},height = 600, width = 1000)
  
  # Batter Summary ####
  
  output$batter_summary_table <- renderDataTable({
    
    Pitches_Seen2 <- BatterData() |>
      summarise(.by = "pitch_name", `Pitches Seen` = n())
    
    BA2 <- BatterData() |>
      summarise(.by = "pitch_name", `Batting Average` = mean(babip_value, na.rm = TRUE))
    
    SLG2 <- BatterData() |>
      summarise(.by = "pitch_name", Slugging = mean(slugging, na.rm = TRUE))
    
    Strikeout2 <- BatterData() |>
      dplyr::filter(!is.na(events)) |>
      mutate(strikeout = ifelse(events == "strikeout", 1, 0)) |>
      summarise(.by = "pitch_name", `K %` = mean(strikeout, na.rm = TRUE))
    
    Walk2 <- BatterData() |>
      dplyr::filter(!is.na(events)) |>
      mutate(walk = ifelse(events %in% c("walk", "hit_by_pitch"), 1, 0)) |>
      summarise(.by = "pitch_name", `BB %` = mean(walk, na.rm = TRUE))
    
    Whiff2 <- BatterData() |>
      dplyr::filter(description %in% c("hit_into_play", "swinging_strike", "foul",
                                "swinging_strike_blocked", "foul_tip")) |>
      mutate(whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked",
                                               "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(whiff, na.rm = TRUE))
    
    Chase2 <- BatterData() |>
      dplyr::filter(plate_z < sz_bot - 0.25 |
               plate_z > sz_top + 0.25 |
               plate_x < -8.5/12 |
               plate_x > 8.5/12) |>
      dplyr::filter(description != "bunt_foul_tip" & description != "foul_bunt" & 
               description != "missed_bunt" & description != "hit_by_pitch") |>
      mutate(chase = ifelse(description %in% c("hit_into_play", "swinging_strike", "foul",
                                               "swinging_strike_blocked", "foul_tip"), 1, 0)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(chase, na.rm = TRUE))
    
    Pitches_Seen2 |>
      left_join(BA2, by = "pitch_name") |>
      left_join(SLG2, by = "pitch_name") |>
      left_join(Strikeout2, by = "pitch_name") |>
      left_join(Walk2, by = "pitch_name") |>
      left_join(Whiff2, by = "pitch_name") |>
      left_join(Chase2, by = "pitch_name") |>
      arrange(desc(`Pitches Seen`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      column_to_rownames("Pitch Type") |>
      mutate_if(is.numeric, round, digits = 2) 
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
                 label4 = description)) +
      geom_point(size = 1, alpha = 0.6) +
      scale_color_manual(values = c(CH = "blue", FF = "red",
                                    SL = "orange", CU = "green",
                                    FC = "purple",SI = "yellow",
                                    KC = "pink")) +
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
      dplyr::filter(!is.na(babip_value)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = babip_value,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description)) +
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
  
  output$kzone_slg_batter <- renderPlotly({
    
    BatterData() |>
      dplyr::filter(!is.na(slugging)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = slugging,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description)) +
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
      ggtitle("Slugging") +
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
                 label4 = description)) +
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
      dplyr::filter(!is.na(Whiff)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Whiff,
                 label = release_speed,
                 label2 = pfx_x,
                 label3 = pfx_z,
                 label4 = description)) +
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
  
  #Batter Variable Efficacy ####
  
  #Break
  
  plot_pfx_batter <- reactive({
    if(input$response_select_batter == "Whiff") {
      BatterData() |>
        dplyr::filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
        select(batter, pfx_x, pfx_z, Whiff) |>
        ggplot(aes(x = pfx_x, y = pfx_z, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_break_batter) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select_batter == "Chase") {
      BatterData() |>
        dplyr::filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
        select(batter, pfx_x, pfx_z, Chase) |>
        ggplot(aes(x = pfx_x, y = pfx_z, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_break_batter) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    }
  })
  
  output$plot_pfx_batter <- renderPlot({plot_pfx_batter()},height = 600, width = 1000)
  
  #Speed
  
  plot_release_speed_batter <- reactive({
    if(input$response_select_batter == "Whiff") {
      BatterData() |>
        dplyr::filter(!is.na(release_speed) & !is.na(speed_drop)) |>
        select(batter, release_speed, speed_drop, Whiff) |>
        ggplot(aes(x = release_speed, y = speed_drop, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_speed_batter) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select_batter == "Chase") {
      BatterData() |>
        dplyr::filter(!is.na(release_speed)) |>
        select(batter, release_speed, Chase) |>
        ggplot(aes(x = release_speed, y = speed_drop, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_speed_batter) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase")  
    }
  })
  
  output$plot_release_speed_batter <- renderPlot({plot_release_speed_batter()},height = 600, width = 1000)
  
}
