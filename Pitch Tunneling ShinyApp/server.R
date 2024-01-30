server <- function(input, output) {
  
  #Data Info
  
  Data <- read_csv("Data.csv") 
  
  #Reactive Filtering ####
  
  Pitcher_Filtered <- reactive({
    if(input$batter_side != "Both") {
      Data <- Data |>
        filter(stand == input$batter_side)
    }
    Data
  })
  
  # Kzone ####
  
  kzone_whiff <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(prev_pitch == input$fastball_select) |>
      filter(pitch_name == input$secondary_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |> 
      filter(!is.na(Whiff)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Whiff, alpha = 0.5)) +
      geom_point() +
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
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$kzone_whiff <- renderPlot({kzone_whiff()}, height = 500)
  
  kzone_chase <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(prev_pitch == input$fastball_select) |>
      filter(pitch_name == input$secondary_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |> 
      filter(!is.na(Chase)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = Chase, alpha = 0.5)) +
      geom_point() +
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
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$kzone_chase <- renderPlot({kzone_chase()}, height = 500)
  
  kzone_groundball <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(prev_pitch == input$fastball_select) |>
      filter(pitch_name == input$secondary_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |> 
      filter(!is.na(GroundBall)) |>
      ggplot(aes(x = plate_x, y = plate_z, color = GroundBall, alpha = 0.5)) +
      geom_point() +
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
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$kzone_groundball <- renderPlot({kzone_groundball()}, height = 500)
  
  
  #Pitch Info ####
  
  pitch_release <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      select(pitcher_name, pitch_name, release_pos_x, release_pos_z) |>
      ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Release Side", limits = c(-5,5)) +
      scale_y_continuous("Release Height", limits = c(0,8)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$pitch_release <- renderPlot({pitch_release()}, height = 700)
  
  pitch_release_side <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      select(pitcher_name, pitch_name, release_pos_z, release_extension) |>
      ggplot(aes(x = release_extension, y = release_pos_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_y_continuous("Release Height", limits = c(0,8)) +
      scale_x_continuous("Release Extension", limits = c(4,8)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$pitch_release_side <- renderPlot({pitch_release_side()}, height = 700)
  
  pitch_angles <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(!is.na(release_angle) & !is.na(release_direction)) |>
      select(pitcher_name, pitch_name, release_angle, release_direction) |>
      ggplot(aes(x = release_direction, y = release_angle, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_y_continuous("Release Angle", limits = c(-5,5)) +
      scale_x_continuous("Release Direction", limits = c(-6,6)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$pitch_angles <- renderPlot({pitch_angles()}, height = 700)
  
  pitch_spin <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(!is.na(release_spin_rate) & !is.na(spin_axis)) |>
      select(pitcher_name, pitch_name, release_spin_rate, spin_axis) |>
      ggplot(aes(x = spin_axis, y = release_spin_rate, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      coord_polar() +
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$pitch_spin <- renderPlot({pitch_spin()}, height = 700)
  
  pitch_velo <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      select(pitcher_name, pitch_name, release_speed) |>
      ggplot(aes(x=pitch_name, y=release_speed, fill=pitch_name)) + 
      geom_boxplot(alpha=0.3) +
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$pitch_velo <- renderPlot({pitch_velo()}, height = 700)
  
  pitch_tunnel <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(!is.na(x_tunnel) & !is.na(z_tunnel)) |>
      select(pitcher_name, pitch_name, x_tunnel, z_tunnel) |>
      ggplot(aes(x = x_tunnel, y = z_tunnel, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horz Tunnel Point", limits = c(-4,4)) +
      scale_y_continuous("Vert Tunnel Point", limits = c(0,5)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$pitch_tunnel <- renderPlot({pitch_tunnel()}, height = 700)
  
  pitch_posttunnelbreak <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(!is.na(post_tunnel_break_x) & !is.na(post_tunnel_break_z)) |>
      select(pitcher_name, pitch_name, post_tunnel_break_x, post_tunnel_break_z) |>
      ggplot(aes(x = post_tunnel_break_x, y = post_tunnel_break_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horz Post Tunnel Break", limits = c(-0.4,0.4)) +
      scale_y_continuous("Vert Post Tunnel Break", limits = c(-1,0)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme(legend.position="none") +
      theme_bw()
  })
  
  output$pitch_posttunnelbreak <- renderPlot({pitch_posttunnelbreak()}, height = 700)
  
  pitch_break <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      select(pitcher_name, pitch_name, pfx_z, pfx_x) |>
      ggplot(aes(x = pfx_x, y = pfx_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horizontal Break", limits = c(-2,2)) +
      scale_y_continuous("Induced Vertical Break", limits = c(-2,2)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_break <- renderPlot({pitch_break()}, height = 700)
  
  # Performance ####
  
  seq_whiff <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(prev_pitch %in% c("Four-Seam", "Sinker", "Cutter")) |>
      filter(pitch_name %in% c("Changeup", "Sweeper", "Slider", "Curveball", "Split-Finger",
                               "Knuckle Curve", "Slurve")) |>
      summarise(.by = c("prev_pitch", "pitch_name"), 
                `Whiff %` = mean(Whiff, na.rm = TRUE)) |>
      ggplot(aes(x = prev_pitch, y = pitch_name, fill = `Whiff %`)) +
      geom_tile() +
      geom_text(aes(label = round(`Whiff %`, digits = 3))) +
      theme_bw()
    
  })
  
  output$seq_whiff <- renderPlot({seq_whiff()}, width = 1100, height = 700)
  
  seq_chase <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(prev_pitch %in% c("Four-Seam", "Sinker", "Cutter")) |>
      filter(pitch_name %in% c("Changeup", "Sweeper", "Slider", "Curveball", "Split-Finger",
                               "Knuckle Curve", "Slurve")) |>
      summarise(.by = c("prev_pitch", "pitch_name"), 
                `Chase %` = mean(Chase, na.rm = TRUE)) |>
      ggplot(aes(x = prev_pitch, y = pitch_name, fill = `Chase %`)) +
      geom_tile() +
      geom_text(aes(label = round(`Chase %`, digits = 3))) +
      theme_bw()
    
  })
  
  output$seq_chase <- renderPlot({seq_chase()}, width = 1100, height = 700)
  
  seq_groundball <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |>
      filter(prev_pitch %in% c("Four-Seam", "Sinker", "Cutter")) |>
      filter(pitch_name %in% c("Changeup", "Sweeper", "Slider", "Curveball", "Split-Finger",
                               "Knuckle Curve", "Slurve")) |>
      summarise(.by = c("prev_pitch", "pitch_name"), 
                `GroundBall %` = mean(GroundBall, na.rm = TRUE)) |>
      ggplot(aes(x = prev_pitch, y = pitch_name, fill = `GroundBall %`)) +
      geom_tile() +
      geom_text(aes(label = round(`GroundBall %`, digits = 3))) +
      theme_bw()
    
  })
  
  output$seq_groundball <- renderPlot({seq_groundball()}, width = 1100, height = 700)
  
  #Variable Efficacy ####
  
  plot_pfx_x <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(pfx_x)) |>
        select(pitcher_name, pfx_x, Whiff) |>
        ggplot(aes(x = pfx_x, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(pfx_x)) |>
        select(pitcher_name, pfx_x, Chase) |>
        ggplot(aes(x = pfx_x, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(pfx_x)) |>
        select(pitcher_name, pfx_x, GroundBall) |>
        ggplot(aes(x = pfx_x, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_pfx_x <- renderPlot({plot_pfx_x()},height = 700)
  
  plot_pfx_z <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(pfx_z)) |>
        select(pitcher_name, pfx_z, Whiff) |>
        ggplot(aes(x = pfx_z, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(pfx_z)) |>
        select(pitcher_name, pfx_z, Chase) |>
        ggplot(aes(x = pfx_z, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(pfx_z)) |>
        select(pitcher_name, pfx_z, GroundBall) |>
        ggplot(aes(x = pfx_z, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_pfx_z <- renderPlot({plot_pfx_z()},height = 700)
  
  plot_diffattunnel <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffattunnel)) |>
        select(pitcher_name, diffattunnel, Whiff) |>
        ggplot(aes(x = diffattunnel, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffattunnel)) |>
        select(pitcher_name, diffattunnel, Chase) |>
        ggplot(aes(x = diffattunnel, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffattunnel)) |>
        select(pitcher_name, diffattunnel, GroundBall) |>
        ggplot(aes(x = diffattunnel, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_diffattunnel <- renderPlot({plot_diffattunnel()},height = 700)
  
  plot_release_speed <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(release_speed)) |>
        select(pitcher_name, release_speed, Whiff) |>
        ggplot(aes(x = release_speed, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(release_speed)) |>
        select(pitcher_name, release_speed, Chase) |>
        ggplot(aes(x = release_speed, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(release_speed)) |>
        select(pitcher_name, release_speed, GroundBall) |>
        ggplot(aes(x = release_speed, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_release_speed <- renderPlot({plot_release_speed()},height = 700)
  
  plot_speeddiff <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(speeddiff)) |>
        select(pitcher_name, speeddiff, Whiff) |>
        ggplot(aes(x = speeddiff, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(speeddiff)) |>
        select(pitcher_name, speeddiff, Chase) |>
        ggplot(aes(x = speeddiff, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(speeddiff)) |>
        select(pitcher_name, speeddiff, GroundBall) |>
        ggplot(aes(x = speeddiff, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_speeddiff <- renderPlot({plot_speeddiff()},height = 700)
  
  plot_release_spin_rate <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(release_spin_rate)) |>
        select(pitcher_name, release_spin_rate, Whiff) |>
        ggplot(aes(x = release_spin_rate, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(release_spin_rate)) |>
        select(pitcher_name, release_spin_rate, Chase) |>
        ggplot(aes(x = release_spin_rate, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(release_spin_rate)) |>
        select(pitcher_name, release_spin_rate, GroundBall) |>
        ggplot(aes(x = release_spin_rate, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_release_spin_rate <- renderPlot({plot_release_spin_rate()},height = 700)
  
  plot_diffatrelease <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffatrelease)) |>
        select(pitcher_name, diffatrelease, Whiff) |>
        ggplot(aes(x = diffatrelease, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffatrelease)) |>
        select(pitcher_name, diffatrelease, Chase) |>
        ggplot(aes(x = diffatrelease, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffatrelease)) |>
        select(pitcher_name, diffatrelease, GroundBall) |>
        ggplot(aes(x = diffatrelease, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_diffatrelease <- renderPlot({plot_diffatrelease()},height = 700)
  
  plot_diffatplate <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffatplate)) |>
        select(pitcher_name, diffatplate, Whiff) |>
        ggplot(aes(x = diffatplate, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffatplate)) |>
        select(pitcher_name, diffatplate, Chase) |>
        ggplot(aes(x = diffatplate, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(diffatplate)) |>
        select(pitcher_name, diffatplate, GroundBall) |>
        ggplot(aes(x = diffatplate, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_diffatplate <- renderPlot({plot_diffatplate()},height = 700)
  
  plot_posttunnelbreak <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(posttunnelbreak)) |>
        select(pitcher_name, posttunnelbreak, Whiff) |>
        ggplot(aes(x = posttunnelbreak, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(posttunnelbreak)) |>
        select(pitcher_name, posttunnelbreak, Chase) |>
        ggplot(aes(x = posttunnelbreak, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(posttunnelbreak)) |>
        select(pitcher_name, posttunnelbreak, GroundBall) |>
        ggplot(aes(x = posttunnelbreak, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_posttunnelbreak <- renderPlot({plot_posttunnelbreak()},height = 700)
  
  plot_breaktotunnelratio <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(breaktotunnelratio)) |>
        select(pitcher_name, breaktotunnelratio, Whiff) |>
        ggplot(aes(x = breaktotunnelratio, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(!is.na(breaktotunnelratio)) |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        select(pitcher_name, breaktotunnelratio, Chase) |>
        ggplot(aes(x = breaktotunnelratio, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(breaktotunnelratio)) |>
        select(pitcher_name, breaktotunnelratio, GroundBall) |>
        ggplot(aes(x = breaktotunnelratio, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_breaktotunnelratio <- renderPlot({plot_breaktotunnelratio()},height = 700)
  
  plot_releasetotunnelratio <- reactive({
    if(input$response_select == "Whiff") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(releasetotunnelratio)) |>
        select(pitcher_name, releasetotunnelratio, Whiff) |>
        ggplot(aes(x = releasetotunnelratio, y = Whiff)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "Chase") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(releasetotunnelratio)) |>
        select(pitcher_name, releasetotunnelratio, Chase) |>
        ggplot(aes(x = releasetotunnelratio, y = Chase)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } else if(input$response_select == "GroundBall") {
      Pitcher_Filtered() |>
        filter(pitcher_name == input$pitcher_select) |>
        filter(prev_pitch == input$fastball_select) |>
        filter(pitch_name == input$secondary_select) |>
        filter(game_date >= input$date_slider[1] & 
                 game_date <= input$date_slider[2]) |> 
        filter(!is.na(releasetotunnelratio)) |>
        select(pitcher_name, releasetotunnelratio, GroundBall) |>
        ggplot(aes(x = releasetotunnelratio, y = GroundBall)) +
        geom_point() +
        geom_smooth(method="loess") +
        theme_bw()
    } 
  })
  
  output$plot_releasetotunnelratio <- renderPlot({plot_releasetotunnelratio()},height = 700)
  
  #Regression Plots ####
  
  regression_plot <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(prev_pitch == input$fastball_select) |>
      filter(pitch_name == input$secondary_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2]) |> 
      ggplot(aes_string(x=input$x_axis, y=input$y_axis)) +
      geom_point() +
      geom_smooth(method="loess") +
      theme_bw()
  })
  
  output$regression_plot <- renderPlot({regression_plot()},height = 700)
  
  #Data Table ####
  
  Pitch_Filter_Table <- reactive({
    
    Data <- Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(game_date >= input$date_slider[1] & 
               game_date <= input$date_slider[2])
    
    Pitches_Thrown <- Data |>
      summarise(.by = "pitch_name", pitches_thrown = n())  |>
      rename(`Pitches Thrown` = pitches_thrown)
    
    RE24 <- Data |>
      summarise(.by = "pitch_name", `Avg Run Value`  = 
                  mean(delta_run_exp, na.rm = TRUE))
    
    Whiff <- Data |>
      filter(!is.na(Whiff)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(Whiff))
    
    Chase <- Data |>
      filter(!is.na(Chase)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(Chase))
    
    Ground_Ball <- Data |>
      filter(!is.na(GroundBall)) |>
      summarise(.by = "pitch_name", `Ground Ball %` = mean(GroundBall))
    
    Velocity <- Data |> 
      summarise(.by = "pitch_name", `Pitch Velo`  = 
                  mean(release_speed, na.rm = TRUE))
    
    Release_Difference <- Data |>
      summarise(.by = "pitch_name", `Release Difference` =
                  mean(diffatrelease, na.rm = TRUE))
    
    Tunnel_Difference <- Data |>
      summarise(.by = "pitch_name", `Tunnel Difference` =
                  mean(diffattunnel, na.rm = TRUE))
    
    Plate_Difference <- Data |>
      summarise(.by = "pitch_name", `Plate Difference` =
                  mean(diffatplate, na.rm = TRUE))
    
    Speed_Difference <- Data |>
      summarise(.by = "pitch_name", `Speed Difference` =
                  mean(speeddiff, na.rm = TRUE))
    
    Horz_Break <- Data |>
      summarise(.by = "pitch_name", `Horizontal Break`  = 
                  mean(pfx_x, na.rm = TRUE))
    
    Vert_Break <- Data |>
      summarise(.by = "pitch_name", `Induced Vertical Break`  = 
                  mean(pfx_z, na.rm = TRUE))
    
    Post_Tunnel_Break <- Data |>
      summarise(.by = "pitch_name", `Post Tunnel Break` =
                  mean(posttunnelbreak, na.rm = TRUE))
    
    Release_To_Tunnel_Ratio <- Data |>
      summarise(.by = "pitch_name", `Release to Tunnel Ratio` = 
                  mean(releasetotunnelratio, na.rm = TRUE))
    
    Break_To_Tunnel_Ratio <- Data |>
      summarise(.by = "pitch_name", `Break to Tunnel Ratio` = 
                  mean(breaktotunnelratio, na.rm = TRUE))
    
    Spin_Rate <- Data |>
      summarise(.by = "pitch_name", `Spin Rate`  = 
                  mean(release_spin_rate, na.rm = TRUE))
    
    Spin_Axis <- Data |>
      summarise(.by = "pitch_name", `Spin Axis`  = 
                  mean(spin_axis, na.rm = TRUE))
    
    Spin_Eff <- Data |>
      filter(!is.na(spin_eff)) |>
      summarise(.by = "pitch_name", `Spin Efficiency`  = mean(spin_eff))
    
    Table <- Pitches_Thrown |>
      left_join(RE24, by = "pitch_name") |>
      left_join(Whiff, by = "pitch_name") |>
      left_join(Chase, by = "pitch_name") |>
      left_join(Ground_Ball, by = "pitch_name") |>
      left_join(Velocity, by = "pitch_name") |>
      left_join(Release_Difference, by = "pitch_name") |>
      left_join(Tunnel_Difference, by = "pitch_name") |>
      left_join(Plate_Difference, by = "pitch_name") |>
      left_join(Speed_Difference, by = "pitch_name") |>
      left_join(Horz_Break, by = "pitch_name") |>
      left_join(Vert_Break, by = "pitch_name") |>
      left_join(Post_Tunnel_Break, by = "pitch_name") |>
      left_join(Release_To_Tunnel_Ratio, by = "pitch_name") |>
      left_join(Break_To_Tunnel_Ratio, by = "pitch_name") |>
      left_join(Spin_Rate, by = "pitch_name") |>
      left_join(Spin_Axis, by = "pitch_name") |>
      left_join(Spin_Eff, by = "pitch_name") |>
      arrange(desc(`Pitches Thrown`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      column_to_rownames("Pitch Type") |>
      mutate_if(is.numeric, round, digits = 3)
    
    return(Table)
    
  })
  
  output$table <- renderDataTable({Pitch_Filter_Table()})
  
  #Filter Data ####
  
  Filter_Data <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(prev_pitch == input$fastball_select) |>
      filter(pitch_name == input$secondary_select) |> 
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
      filter(diffattunnel >= input$tunnel[1] & 
               diffattunnel <= input$tunnel[2]) |>
      filter(diffatrelease >= input$release[1] & 
               diffatrelease <= input$release[2]) |>
      filter(diffatplate >= input$plate[1] & 
               diffatplate <= input$plate[2]) |>
      filter(speeddiff >= input$diffspeed[1] & 
               speeddiff <= input$diffspeed[2]) |>
      filter(posttunnelbreak >= input$PTB[1] & 
               posttunnelbreak <= input$PTB[2]) |>
      select(pitch_name, Whiff, Chase, GroundBall, release_direction, release_angle, 
             release_spin_rate, spin_axis, ax, ay, az, vx0, vy0, vz0, release_pos_x, 
             release_pos_z, release_extension, x_tunnel, z_tunnel) |>
      mutate(across(where(is.numeric), round, 3))
  })
  
  output$Filter_Data <- renderDataTable({Filter_Data()})
}
