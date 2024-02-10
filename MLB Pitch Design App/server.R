server <- function(input, output) {
  
  #Data Info
  
  Data <- read_csv("Data.csv") 
  
  #Reactive Filtering ####
  
  App_Data <- reactive({
    if(input$batter_side != "Both") {
      Data <- Data |>
        filter(stand == input$batter_side)
    }
    
    if(input$pitch_name != "Any") {
      Data <- Data |>
        filter(pitch_name == input$pitch_name)
    }
    
    if(input$previous_pitch != "Any") {
      Data <- Data |>
        filter(prev_pitch == input$previous_pitch) |>
        filter(speeddiff >= input$diffspeed[1] & 
                 speeddiff <= input$diffspeed[2]) |>
        filter(breaktotunnelratio >= input$breaktotunnelratio[1] & 
                 breaktotunnelratio <= input$breaktotunnelratio[2]) |>
        filter(prev_plate_x >= input$prev_plate_x[1] & 
                 prev_plate_x <= input$prev_plate_x[2]) |>
        filter(prev_plate_z >= input$prev_plate_z[1] & 
                 prev_plate_z <= input$prev_plate_z[2]) 
    }
    
    Data |> 
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(plate_x >= input$plate_x[1] & 
               plate_x <= input$plate_x[2]) |>
      filter(plate_z >= input$plate_z[1] & 
               plate_z <= input$plate_z[2]) |>
      filter(pfx_x >= input$pfx_x[1] & 
               pfx_x <= input$pfx_x[2]) |>
      filter(pfx_z >= input$pfx_z[1] & 
               pfx_z <= input$pfx_z[2]) |>
      filter(release_speed >= input$speed[1] & 
               release_speed <= input$speed[2]) |>
      filter(release_spin_rate >= input$spin[1] & 
               release_spin_rate <= input$spin[2]) |>
      filter(posttunnelbreak >= input$PTB[1] & 
               posttunnelbreak <= input$PTB[2]) |>
      filter(spin_eff >= input$spin_eff[1] & 
               spin_eff <= input$spin_eff[2]) |>
      filter(balls >= input$balls[1] &
               balls <= input$balls[2]) |>
      filter(strikes >= input$strikes[1] &
               strikes <= input$strikes[2]) 
  })
  
  #Summary Table ####
  
  Pitch_Filter_Table <- reactive({
    
    Pitches_Thrown <- App_Data() |>
      summarise(.by = "pitch_name", `Pitches Thrown` = n())
    
    RE24 <- App_Data() |>
      summarise(.by = "pitch_name", `Avg Run Value`  = 
                  mean(delta_run_exp, na.rm = TRUE))
    
    Whiff <- App_Data() |>
      summarise(.by = "pitch_name", `Whiff %` = mean(Whiff, na.rm = TRUE))
    
    InZoneWhiff <- App_Data() |>
      summarise(.by = "pitch_name", `In Zone Whiff %` = mean(InZoneWhiff, na.rm = TRUE))
    
    Chase <- App_Data() |>
      summarise(.by = "pitch_name", `Chase %` = mean(Chase, na.rm = TRUE))
    
    Spin_Rate <- App_Data() |>
      summarise(.by = "pitch_name", `Spin Rate (RPM)`  = 
                  mean(release_spin_rate, na.rm = TRUE))
    
    Spin_Axis <- App_Data() |>
      summarise(.by = "pitch_name", `Spin Axis`  = 
                  mean(spin_axis, na.rm = TRUE))
    
    Spin_Eff <- App_Data() |>
      summarise(.by = "pitch_name", `Spin Efficiency`  = mean(spin_eff, na.rm = TRUE))
    
    Horz_Break <- App_Data() |>
      summarise(.by = "pitch_name", `Horizontal Break (In)`  = 
                  mean(pfx_x, na.rm = TRUE))
    
    Vert_Break <- App_Data() |>
      summarise(.by = "pitch_name", `Induced Vertical Break (In)`  = 
                  mean(pfx_z, na.rm = TRUE))
    
    Post_Tunnel_Break <- App_Data() |>
      summarise(.by = "pitch_name", `Post Tunnel Break (In)` =
                  mean(posttunnelbreak, na.rm = TRUE))
    
    Velocity <- App_Data() |>
      summarise(.by = "pitch_name", `Pitch Velo (MPH)`  = 
                  mean(release_speed, na.rm = TRUE))
    
    Speed_Difference <- App_Data() |>
      summarise(.by = "pitch_name", `Speed Difference (MPH)` =
                  mean(speeddiff, na.rm = TRUE))
    
    Break_To_Tunnel_Ratio <- App_Data() |>
      summarise(.by = "pitch_name", `Break to Tunnel Ratio` = 
                  mean(breaktotunnelratio, na.rm = TRUE))
    
    Table <- Pitches_Thrown |>
      left_join(RE24, by = "pitch_name") |>
      left_join(Whiff, by = "pitch_name") |>
      left_join(InZoneWhiff, by = "pitch_name") |>
      left_join(Chase, by = "pitch_name") |>
      left_join(Spin_Rate, by = "pitch_name") |>
      left_join(Spin_Axis, by = "pitch_name") |>
      left_join(Spin_Eff, by = "pitch_name") |>
      left_join(Horz_Break, by = "pitch_name") |>
      left_join(Vert_Break, by = "pitch_name") |>
      left_join(Post_Tunnel_Break, by = "pitch_name") |>
      left_join(Velocity, by = "pitch_name") |>
      left_join(Speed_Difference, by = "pitch_name") |>
      left_join(Break_To_Tunnel_Ratio, by = "pitch_name") |>
      arrange(desc(`Pitches Thrown`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      column_to_rownames("Pitch Type") |>
      mutate_if(is.numeric, round, digits = 3)
    
    return(Table)
    
  })
  
  output$table <- renderDataTable({Pitch_Filter_Table()})
  
  #Count Data ####
  
  count_matrix <- reactive({
    
    App_Data() |>
      filter(!is.na(balls) & !is.na(strikes) & !is.na(pitch_name)) |>
      summarise(.by = c("balls", "strikes", "pitch_name"), 
                `Pitches Thrown` = n(),
                `Whiff %` = mean(Whiff, na.rm = TRUE),
                `Chase %` = mean(Chase, na.rm = TRUE)) |>
      mutate_if(is.numeric, round, digits = 3) |>
      arrange(desc(`Pitches Thrown`), balls, strikes)
  })
  
  output$count_matrix <- renderDataTable({count_matrix()},height = 700)
  
  # Performance ####
  
  seq_whiff <- reactive({
    
    App_Data() |>
      filter(prev_pitch %in% c("Four-Seam", "Sinker", "Cutter")) |>
      filter(pitch_name %in% c("Changeup", "Sweeper", "Slider", "Curveball", "Split-Finger",
                               "Knuckle Curve", "Slurve")) |>
      summarise(.by = c("prev_pitch", "pitch_name"), 
                `Whiff %` = mean(Whiff, na.rm = TRUE)) |>
      ggplot(aes(x = prev_pitch, y = pitch_name, fill = `Whiff %`)) +
      geom_tile() +
      scale_fill_distiller(palette = "RdBu", direction = -1) +
      geom_text(aes(label = round(`Whiff %`, digits = 3))) +
      theme_light()
    
  })
  
  output$seq_whiff <- renderPlot({seq_whiff()}, width = 1100, height = 700)
  
  seq_chase <- reactive({
    
    App_Data() |>
      filter(prev_pitch %in% c("Four-Seam", "Sinker", "Cutter")) |>
      filter(pitch_name %in% c("Changeup", "Sweeper", "Slider", "Curveball", "Split-Finger",
                               "Knuckle Curve", "Slurve")) |>
      summarise(.by = c("prev_pitch", "pitch_name"), 
                `Chase %` = mean(Chase, na.rm = TRUE)) |>
      ggplot(aes(x = prev_pitch, y = pitch_name, fill = `Chase %`)) +
      geom_tile() +
      scale_fill_distiller(palette = "RdBu", direction = -1) +
      geom_text(aes(label = round(`Chase %`, digits = 3))) +
      theme_light()
    
  })
  
  output$seq_chase <- renderPlot({seq_chase()}, width = 1100, height = 700)
  
  #Pitch Profile ####
  
  pitch_release <- reactive({
    
    App_Data() |>
      select(pitcher_name, pitch_name, release_pos_x, release_pos_z) |>
      ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Release Side (Feet)", limits = c(-5,5)) +
      scale_y_continuous("Release Height (Feet)", limits = c(0,8)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_light()
  })
  
  output$pitch_release <- renderPlot({pitch_release()}, height = 700)
  
  pitch_angles <- reactive({
    
    App_Data() |>
      filter(!is.na(release_angle) & !is.na(release_direction)) |>
      select(pitcher_name, pitch_name, release_angle, release_direction) |>
      ggplot(aes(x = release_direction, y = release_angle, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_y_continuous("Release Angle (Degrees)", limits = c(-5,5)) +
      scale_x_continuous("Release Direction (Degrees)", limits = c(-6,6)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_light()
  })
  
  output$pitch_angles <- renderPlot({pitch_angles()}, height = 700)
  
  pitch_spin <- reactive({
    
    App_Data() |>
      filter(!is.na(release_spin_rate) & !is.na(spin_axis)) |>
      select(pitcher_name, pitch_name, release_spin_rate, spin_axis) |>
      ggplot(aes(x = spin_axis, y = release_spin_rate, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      coord_polar() +
      theme_light()
  })
  
  output$pitch_spin <- renderPlot({pitch_spin()}, height = 700)
  
  pitch_velo <- reactive({
    
    App_Data() |>
      select(pitcher_name, pitch_name, release_speed) |>
      ggplot(aes(x=pitch_name, y=release_speed, fill=pitch_name)) + 
      geom_boxplot(alpha=0.3) +
      theme_light()
  })
  
  output$pitch_velo <- renderPlot({pitch_velo()}, height = 700)
  
  pitch_tunnel <- reactive({
    
    App_Data() |>
      filter(!is.na(x_tunnel) & !is.na(z_tunnel)) |>
      select(pitcher_name, pitch_name, x_tunnel, z_tunnel) |>
      ggplot(aes(x = x_tunnel, y = z_tunnel, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horz Tunnel Point (Feet)", limits = c(-4,4)) +
      scale_y_continuous("Vert Tunnel Point (Feet)", limits = c(0,5)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_light()
  })
  
  output$pitch_tunnel <- renderPlot({pitch_tunnel()}, height = 700)
  
  pitch_posttunnelbreak <- reactive({
    
    App_Data() |>
      filter(!is.na(post_tunnel_break_x) & !is.na(post_tunnel_break_z)) |>
      select(pitcher_name, pitch_name, post_tunnel_break_x, post_tunnel_break_z) |>
      ggplot(aes(x = post_tunnel_break_x, y = post_tunnel_break_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horz Post Tunnel Break (Inches)", limits = c(-4,4)) +
      scale_y_continuous("Vert Post Tunnel Break (Inches)", limits = c(-10,0)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_light()
  })
  
  output$pitch_posttunnelbreak <- renderPlot({pitch_posttunnelbreak()}, height = 700)
  
  pitch_break <- reactive({
    
    App_Data() |>
      select(pitcher_name, pitch_name, pfx_z, pfx_x) |>
      ggplot(aes(x = pfx_x, y = pfx_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horizontal Break (Inches)", limits = c(-25,25)) +
      scale_y_continuous("Induced Vertical Break (Inches)", limits = c(-35,25)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_light()
  })
  
  output$pitch_break <- renderPlot({pitch_break()}, height = 700)
  
  # Kzone ####
  
  fastball_whiff <- reactive({
    
    App_Data() |>
      filter(!is.na(Whiff)) |>
      rename(`Whiff On Next Pitch` = Whiff) |>
      ggplot(aes(x = prev_plate_x, y = prev_plate_z, color = `Whiff On Next Pitch`, 
                 alpha = 0.5)) +
      geom_point() +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      scale_x_continuous("Width (Catcher POV)", limits = c(-2.5,2.5)) +
      scale_y_continuous("Height", limits = c(0,4.5)) +
      geom_segment(x = -0.8308333, xend = -0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = 0.8308333, xend = 0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 1.594688, yend = 1.594688,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 3.362442, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      theme(aspect.ratio = 1.4) +
      theme_light()
  })
  
  output$fastball_whiff <- renderPlot({fastball_whiff()}, height = 500)
  
  second_whiff <- reactive({
    
    App_Data() |>
      filter(!is.na(Whiff)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Whiff, alpha = 0.5)) +
      geom_point() +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      scale_x_continuous("Width (Catcher POV)", limits = c(-2.5,2.5)) +
      scale_y_continuous("Height", limits = c(0,4.5)) +
      geom_segment(x = -0.8308333, xend = -0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = 0.8308333, xend = 0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 1.594688, yend = 1.594688,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 3.362442, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      theme(aspect.ratio = 1.4) +
      theme_light()
  })
  
  output$second_whiff <- renderPlot({second_whiff()}, height = 500)
  
  fastball_chase <- reactive({
    
    App_Data() |>
      filter(!is.na(Chase)) |>
      rename(`Chase On Next Pitch` = Chase) |>
      ggplot(aes(x = prev_plate_x, y = prev_plate_z, color = `Chase On Next Pitch`, 
                 alpha = 0.5)) +
      geom_point() +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      scale_x_continuous("Width (Catcher POV)", limits = c(-2.5,2.5)) +
      scale_y_continuous("Height", limits = c(0,4.5)) +
      geom_segment(x = -0.8308333, xend = -0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = 0.8308333, xend = 0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 1.594688, yend = 1.594688,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 3.362442, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      theme(aspect.ratio = 1.4) +
      theme_light()
  })
  
  output$fastball_chase <- renderPlot({fastball_chase()}, height = 500)
  
  second_chase <- reactive({
    
    App_Data() |>
      filter(!is.na(Chase)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Chase, alpha = 0.5)) +
      geom_point() +
      scale_color_distiller(palette = "RdBu", direction = -1) +
      scale_x_continuous("Width (Catcher POV)", limits = c(-2.5,2.5)) +
      scale_y_continuous("Height", limits = c(0,4.5)) +
      geom_segment(x = -0.8308333, xend = -0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = 0.8308333, xend = 0.8308333, y = 1.594688, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 1.594688, yend = 1.594688,
                   color = "black", linewidth = 1.5) +
      geom_segment(x = -0.8308333, xend = 0.8308333, y = 3.362442, yend = 3.362442,
                   color = "black", linewidth = 1.5) +
      theme(aspect.ratio = 1.4) +
      theme_light()
  })
  
  output$second_chase <- renderPlot({second_chase()}, height = 500)
  
  #Variable Efficacy ####
  
  plot_pfx <- reactive({
    if(input$response_select == "Whiff") {
      App_Data() |>
        filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
        select(pitcher_name, pfx_x, pfx_z, Whiff) |>
        ggplot(aes(x = pfx_x, y = pfx_z, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_break) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select == "Chase") {
      App_Data() |>
        filter(!is.na(pfx_x) & !is.na(pfx_z)) |>
        select(pitcher_name, pfx_x, pfx_z, Chase) |>
        ggplot(aes(x = pfx_x, y = pfx_z, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_break) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    }
  })
  
  output$plot_pfx <- renderPlot({plot_pfx()},height = 600, width = 1000)
  
  plot_posttunnelbreak <- reactive({
    if(input$response_select == "Whiff") {
      App_Data() |>
        filter(!is.na(post_tunnel_break_x) & !is.na(post_tunnel_break_z)) |>
        select(pitcher_name, post_tunnel_break_x, post_tunnel_break_z, Whiff) |>
        ggplot(aes(x = post_tunnel_break_x, y = post_tunnel_break_z, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_ptb) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select == "Chase") {
      App_Data() |>
        filter(!is.na(post_tunnel_break_x) & !is.na(post_tunnel_break_z)) |>
        select(pitcher_name, post_tunnel_break_x, post_tunnel_break_z, Chase) |>
        ggplot(aes(x = post_tunnel_break_x, y = post_tunnel_break_z, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_ptb) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    } 
  })
  
  output$plot_posttunnelbreak <- renderPlot({plot_posttunnelbreak()},height = 600, width = 1000)
  
  plot_release_speed <- reactive({
    if(input$response_select == "Whiff") {
      App_Data() |>
        filter(!is.na(release_speed) & !is.na(speeddiff)) |>
        select(pitcher_name, release_speed, speeddiff, Whiff) |>
        ggplot(aes(x = release_speed, y = speeddiff, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_speed) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select == "Chase") {
      App_Data() |>
        filter(!is.na(release_speed) & !is.na(speeddiff)) |>
        select(pitcher_name, release_speed, speeddiff, Chase) |>
        ggplot(aes(x = release_speed, y = speeddiff, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_speed) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    }
  })
  
  output$plot_release_speed <- renderPlot({plot_release_speed()},height = 600, width = 1000)
  
  plot_release_spin <- reactive({
    if(input$response_select == "Whiff") {
      App_Data() |>
        filter(!is.na(release_spin_rate) & !is.na(spin_axis)) |>
        select(pitcher_name, release_spin_rate, spin_axis, Whiff) |>
        ggplot(aes(x = release_spin_rate, y = spin_axis, z = Whiff)) +
        stat_summary_hex(fun = "mean", bins = input$bins_spin) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select == "Chase") {
      App_Data() |>
        filter(!is.na(release_spin_rate) & !is.na(spin_axis)) |>
        select(pitcher_name, release_spin_rate, spin_axis, Chase) |>
        ggplot(aes(x = release_spin_rate, y = spin_axis, z = Chase)) +
        stat_summary_hex(fun = "mean", bins = input$bins_spin) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    } 
  })
  
  output$plot_release_spin <- renderPlot({plot_release_spin()},height = 600, width = 1000)
  
  plot_spin_eff <- reactive({
    if(input$response_select == "Whiff") {
      App_Data() |>
        filter(!is.na(Whiff)) |>
        mutate(spin_eff_cut = cut(spin_eff,seq(0,1.2,input$bins_spin_eff))) |>
        summarise(.by = "spin_eff_cut",
                  N = n(),
                  Whiff = mean(Whiff)) |>
        ggplot(aes(x=spin_eff_cut, fill=Whiff)) +
        geom_col(aes(y = N), width = 1) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select == "Chase") {
      App_Data() |>
        filter(!is.na(Chase)) |>
        mutate(spin_eff_cut = cut(spin_eff,seq(0,1.2,input$bins_spin_eff))) |>
        summarise(.by = "spin_eff_cut",
                  N = n(),
                  Chase = mean(Chase)) |>
        ggplot(aes(x=spin_eff_cut, fill=Chase)) +
        geom_col(aes(y = N), width = 1) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    } 
  })
  
  output$plot_spin_eff <- renderPlot({plot_spin_eff()},height = 600, width = 1000)
  
  plot_breaktotunnelratio <- reactive({
    if(input$response_select == "Whiff") {
      App_Data() |>
        filter(!is.na(Whiff)) |>
        mutate(breaktotunnelratio_cut = cut(breaktotunnelratio,seq(0,40,input$bins_bttr))) |>
        summarise(.by = "breaktotunnelratio_cut",
                  N = n(),
                  Whiff = mean(Whiff)) |>
        ggplot(aes(x=breaktotunnelratio_cut, fill=Whiff)) +
        geom_col(aes(y = N), width = 1) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Whiff") 
    } else if(input$response_select == "Chase") {
      App_Data() |>
        filter(!is.na(Chase)) |>
        mutate(breaktotunnelratio_cut = cut(breaktotunnelratio,seq(0,4,input$bins_bttr))) |>
        summarise(.by = "breaktotunnelratio_cut",
                  N = n(),
                  Chase = mean(Chase)) |>
        ggplot(aes(x=breaktotunnelratio_cut, fill=Chase)) +
        geom_col(aes(y = N), width = 1) +
        scale_fill_distiller(palette = "RdBu", direction = -1, name = "Chase") 
    }
  })
  
  output$plot_breaktotunnelratio <- renderPlot({plot_breaktotunnelratio()},height = 600, width = 1000)
}
