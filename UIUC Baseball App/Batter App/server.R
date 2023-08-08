server <- function(input, output) {
  
  #Data Info
  
  Batter_Data <- read_csv("Batter_Data.csv")
  
  #Reactive Filtering ####
  
  Batter_Filtered <- reactive({
    if(input$batter_side != "Both") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$batter_side)
    }
    
    if(input$pitch_select != "All") {
      Batter_Data <- Batter_Data |>
        filter(pitch_name == input$pitch_select)
    }
    
    if(input$p_throws != "Both") {
      Batter_Data <- Batter_Data |>
        filter(p_throws == input$p_throws)
    }
    
    Batter_Data
  })
  
  # Kzone ####
  
  hit_kzone_obp <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(on_base)) |>
      summarize(.by = c("width","height"), 
                `OBP` = mean(on_base)) |>
      ggplot(aes(x = width, y = height, fill = `OBP`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`OBP`, digits = 3), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_obp <- renderPlot({hit_kzone_obp()}, height = 500)
  
  hit_kzone_slg <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(slugging)) |>
      summarize(.by = c("width","height"), 
                `SLG` = mean(slugging)) |>
      ggplot(aes(x = width, y = height, fill = `SLG`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`SLG`, digits = 3), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_slg <- renderPlot({hit_kzone_slg()}, height = 500)
  
  hit_kzone_freq <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      summarize(.by = c("width","height"), 
                `Pitch Count` = n()) |>
      ggplot(aes(x = width, y = height, fill = `Pitch Count`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = `Pitch Count`), colour = "white") +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_freq <- renderPlot({hit_kzone_freq()}, height = 500)
  
  hit_kzone_exit_velo <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(launch_speed)) |>
      summarize(.by = c("width","height"), 
                `Median Exit Velo` = median(launch_speed)) |>
      ggplot(aes(x = width, y = height, fill = `Median Exit Velo`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Median Exit Velo`, digits = 0), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_exit_velo <- renderPlot({hit_kzone_exit_velo()}, height = 500)
  
  hit_kzone_angle <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(launch_angle)) |>
      summarize(.by = c("width","height"), 
                `Median Launch Angle` = median(launch_angle)) |>
      ggplot(aes(x = width, y = height, fill = `Median Launch Angle`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Median Launch Angle`, digits = 1), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_angle <- renderPlot({hit_kzone_angle()}, height = 500)
  
  hit_kzone_hard_hit <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(Hard_Hit)) |>
      summarize(.by = c("width","height"), 
                `Hard Hit %` = mean(Hard_Hit)) |>
      ggplot(aes(x = width, y = height, fill = `Hard Hit %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Hard Hit %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_hard_hit <- renderPlot({hit_kzone_hard_hit()}, height = 500)
  
  hit_kzone_groundball <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(GroundBall)) |>
      filter(description == "InPlay") |>
      summarize(.by = c("width","height"), 
                `Ground Ball %` = mean(GroundBall)) |>
      ggplot(aes(x = width, y = height, fill = `Ground Ball %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Ground Ball %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_groundball <- renderPlot({hit_kzone_groundball()}, height = 500)
  
  hit_kzone_linedrive <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(LineDrive)) |>
      filter(description == "InPlay") |>
      summarize(.by = c("width","height"), 
                `Line Drive %` = mean(LineDrive)) |>
      ggplot(aes(x = width, y = height, fill = `Line Drive %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Line Drive %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_linedrive <- renderPlot({hit_kzone_linedrive()}, height = 500)
  
  hit_kzone_flyball <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(FlyBall)) |>
      filter(description == "InPlay") |>
      summarize(.by = c("width","height"), 
                `Fly Ball %` = mean(FlyBall)) |>
      ggplot(aes(x = width, y = height, fill = `Fly Ball %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Fly Ball %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_flyball <- renderPlot({hit_kzone_flyball()}, height = 500)
  
  hit_kzone_whiff <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(description == "InPlay" | description == "FoulBall" |
               description == "StrikeSwinging") |>
      filter(!is.na(Whiff)) |>
      summarize(.by = c("width","height"), 
                `Whiff %` = mean(Whiff)) |>
      ggplot(aes(x = width, y = height, fill = `Whiff %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Whiff %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_whiff <- renderPlot({hit_kzone_whiff()}, height = 500)
  
  hit_kzone_chase <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(Chase)) |>
      filter(plate_x < -0.8308333 | plate_x > 0.8308333 | 
               plate_z < 1.5275 | plate_z > 3.7725) |>
      summarize(.by = c("width","height"), 
                `Chase %` = mean(Chase)) |>
      ggplot(aes(x = width, y = height, fill = `Chase %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Chase %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_chase <- renderPlot({hit_kzone_chase()}, height = 500)
  
  hit_kzone_taken <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      summarize(.by = c("width","height"), 
                `Taken %` = mean(Taken)) |>
      ggplot(aes(x = width, y = height, fill = `Taken %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Taken %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_taken <- renderPlot({hit_kzone_taken()}, height = 500)
  
  hit_kzone_pull <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(Spray_Angle)) |>
      summarize(.by = c("width","height"), 
                `Pull %` = mean(pull)) |>
      ggplot(aes(x = width, y = height, fill = `Pull %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Pull %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_pull <- renderPlot({hit_kzone_pull()}, height = 500)
  
  hit_kzone_foul <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_pos_x >= input$kzone_rel_x[1] &
               release_pos_x <= input$kzone_rel_x[2]) |>
      filter(release_pos_z >= input$kzone_rel_z[1] &
               release_pos_z <= input$kzone_rel_z[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(FoulBall)) |>
      summarize(.by = c("width","height"), 
                `Foul Ball %` = mean(FoulBall)) |>
      ggplot(aes(x = width, y = height, fill = `Foul Ball %`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Foul Ball %`, digits = 2), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$hit_kzone_foul <- renderPlot({hit_kzone_foul()}, height = 500)
  
  # Batter Spray Chart ####
  
  Batter_Filtered_Spray <- reactive({
    
    if(input$p_throws != "Both") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$p_throws)
    }
    
    if(input$batter_side != "Both") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$batter_side)
    }
    
    if(input$pitch_select != "All") {
      Batter_Data <- Batter_Data |>
        filter(pitch_name == input$pitch_select)
    }
    
    if(input$bb_type != "All") {
      Batter_Data <- Batter_Data |>
        filter(bb_type %in% input$bb_type)
    }
    
    Batter_Data
  })
  
  hit_spray_chart <- reactive({
    
    Batter_Filtered_Spray() |>
      filter(batter_name == input$batter_select) |>
      filter(events != "Undefined") |>
      filter(!is.na(hc_x)) |>
      filter(!is.na(hc_y)) |>
      filter(launch_speed >= input$spray_exit_velo[1] &
               launch_speed <= input$spray_exit_velo[2]) |>
      filter(launch_angle >= input$spray_launch_angle[1] &
               launch_angle <= input$spray_launch_angle[2]) |>
      ggplot(aes(x = hc_x, y = -hc_y, color = events)) +
      scale_color_manual(values = c("Single" = "blue", "Double" = "green", 
                                    "Triple" = "yellow", "HomeRun" = "red",
                                    "Sacrifice" = "orange", "FieldersChoice" = "pink",
                                    "Out" = "black", "Error" = "purple")) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100,
                 curvature = -0.65, colour = "black") +
      geom_segment(x = 128, xend = 33, y = -208, yend = -100, colour = "black") +
      geom_segment(x = 128, xend = 223, y = -208, yend = -100, colour = "black") +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, 
                 curvature = -0.65, linetype = "dotted", colour = "black") +
      scale_x_continuous(NULL, limits = c(25, 225)) +
      scale_y_continuous(NULL, limits = c(-225, -25)) +
      coord_fixed() +
      geom_point() +
      theme_bw()
    
  })
  
  output$hit_spray <- renderPlot({hit_spray_chart()})
  
  # Pitch Sequencing ####
  
  Seq_Filtered <- reactive({
    
    if(input$batter_side != "Both") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$batter_side)
    }
    
    if(input$p_throws != "Both") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$p_throws)
    }
    
    Batter_Data |>
      filter(!(!is.na(prev_pitch) & count == ("0 - 0"))) |>
      filter(!is.na(prev_pitch)) 
  })
  
  seq_guess <- reactive({
    
    df <- Seq_Filtered() |>
      filter(batter_name == input$batter_select) |>
      summarise(.by = c("prev_pitch", "count"), 
                best_guess = fmode(pitch_name)) 
    
    df2 <- df[!duplicated(df), ]
    
    df2 |>
      ggplot(aes(x = count, y = prev_pitch, fill = best_guess)) +
      geom_tile() +
      scale_x_discrete("Count", limits = unique(Batter_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Batter_Data$pitch_name)) +
      ggtitle("Best Pitch To Guess Based on Probability") +
      theme_bw()
    
  })
  
  output$seq_guess <- renderPlot({seq_guess()}, height = 700)
  
  
  #Timeline ####
  
  hit_timeline_whiff <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      select(pitcher_name, Whiff, BatterPA) |>
      filter(!is.na(Whiff)) |>
      summarise(.by = "BatterPA", `Whiff %` = mean(Whiff)) |>
      ggplot(aes(x = BatterPA, y = `Whiff %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_whiff <- renderPlot({hit_timeline_whiff()},height = 700)
  
  hit_timeline_chase <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(Chase)) |>
      filter(plate_x < -0.8308333 | plate_x > 0.8308333 | 
               plate_z < 1.5275 | plate_z > 3.7725) |>
      select(pitcher_name, Chase, BatterPA) |>
      summarise(.by = "BatterPA", `Chase %` = mean(Chase)) |>
      ggplot(aes(x = BatterPA, y = `Chase %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_chase <- renderPlot({hit_timeline_chase()},height = 700)
  
  hit_timeline_hard_hit <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(Hard_Hit)) |>
      select(pitcher_name, Hard_Hit, BatterPA) |>
      summarise(.by = "BatterPA", `Hard Hit %` = mean(Hard_Hit)) |>
      ggplot(aes(x = BatterPA, y = `Hard Hit %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_hard_hit <- renderPlot({hit_timeline_hard_hit()},height = 700)
  
  hit_timeline_groundball <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(GroundBall)) |>
      select(pitcher_name, GroundBall, BatterPA) |>
      summarise(.by = "BatterPA", `Ground Ball %` = mean(GroundBall)) |>
      ggplot(aes(x = BatterPA, y = `Ground Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_groundball <- renderPlot({hit_timeline_groundball()},height = 700)
  
  hit_timeline_linedrive <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(GroundBall)) |>
      select(pitcher_name, LineDrive, BatterPA) |>
      summarise(.by = "BatterPA", `Line Drive %` = mean(LineDrive)) |>
      ggplot(aes(x = BatterPA, y = `Line Drive %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_linedrive <- renderPlot({hit_timeline_linedrive()},height = 700)
  
  hit_timeline_flyball <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(FlyBall)) |>
      select(pitcher_name, FlyBall, BatterPA) |>
      summarise(.by = "BatterPA", `Fly Ball %` = mean(FlyBall)) |>
      ggplot(aes(x = BatterPA, y = `Fly Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_flyball <- renderPlot({hit_timeline_flyball()},height = 700)
  
  hit_timeline_foulball <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      select(pitcher_name, FoulBall, BatterPA) |>
      filter(!is.na(FoulBall)) |>
      summarise(.by = "BatterPA", `Foul Ball %` = mean(FoulBall)) |>
      ggplot(aes(x = BatterPA, y = `Foul Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_foulball <- renderPlot({hit_timeline_foulball()},height = 700)
  
  hit_timeline_exit_velo <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_speed)) |>
      select(pitcher_name, launch_speed, BatterPA) |>
      summarise(.by = "BatterPA", `Mean Exit Velo` = mean(launch_speed)) |>
      ggplot(aes(x = BatterPA, y = `Mean Exit Velo`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(50,100)) +
      theme_bw()
  })
  
  output$hit_timeline_exit_velo <- renderPlot({hit_timeline_exit_velo()},height = 700)
  
  hit_timeline_angle <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_angle)) |>
      select(pitcher_name, launch_angle, BatterPA) |>
      summarise(.by = "BatterPA", `Mean Launch Angle` = mean(launch_angle)) |>
      ggplot(aes(x = BatterPA, y = `Mean Launch Angle`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(-90,90)) +
      theme_bw()
  })
  
  output$hit_timeline_angle <- renderPlot({hit_timeline_angle()},height = 700)
  
  hit_timeline_obp <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(on_base)) |>
      select(pitcher_name, on_base, BatterPA) |>
      summarise(.by = "BatterPA", `On Base %` = mean(on_base)) |>
      ggplot(aes(x = BatterPA, y = `On Base %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_obp <- renderPlot({hit_timeline_obp()},height = 700)
  
  hit_timeline_slg <- reactive({
    
    Batter_Filtered() |>
      filter(BatterPA <= 6) |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(slugging)) |>
      select(pitcher_name, slugging, BatterPA) |>
      summarise(.by = "BatterPA", `Slugging %` = mean(slugging) + 
                  mean(slugging)) |>
      ggplot(aes(x = BatterPA, y = `Slugging %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,4)) +
      theme_bw()
  })
  
  output$hit_timeline_slg <- renderPlot({hit_timeline_slg()},height = 700)
  
  #Data Table ####
  
  Pitch_Filter_Table <- reactive({
    
    if(input$batter_side != "Both") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$batter_side)
    }
    
    if(input$p_throws != "Both") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$p_throws)
    }
    
    Batter_Data <- Batter_Data |>
      filter(batter_name == input$batter_select)
    
    Pitches_Seen <- Batter_Data |>
      summarise(.by = "pitch_name", Pitches_Seen = n())  |>
      rename(`Pitches Seen` = Pitches_Seen)
    
    Whiff <- Batter_Data |>
      filter(!is.na(Whiff)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(Whiff))
    
    Chase <- Batter_Data |>
      filter(!is.na(Chase)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(Chase))
    
    Foul <- Batter_Data |>
      filter(!is.na(FoulBall)) |>
      summarise(.by = "pitch_name", `Foul Ball %` = mean(FoulBall))
    
    Strike_Looking <- Batter_Data |>
      filter(plate_z > 1.5275 & plate_z < 3.7725 & plate_x > -0.8308333 & 
               plate_x < 0.8308333) |>
      summarise(.by = "pitch_name", `Strike Looking %` = mean(Taken))
    
    Exit_Velo <- Batter_Data |>
      filter(!is.na(launch_speed)) |>
      summarise(.by = "pitch_name", `Mean Exit Velo` = mean(launch_speed))
    
    Hard_Hit <- Batter_Data |>
      filter(!is.na(Hard_Hit)) |>
      summarise(.by = "pitch_name", `Hard Hit %` = mean(Hard_Hit)) 
    
    Line_Drive <- Batter_Data |>
      filter(!is.na(LineDrive)) |>
      summarise(.by = "pitch_name", `Line Drive %` = mean(LineDrive))
    
    Ground_Ball <- Batter_Data |>
      filter(!is.na(GroundBall)) |>
      summarise(.by = "pitch_name", `Ground Ball %` = mean(GroundBall))
    
    Fly_Ball <- Batter_Data |>
      filter(!is.na(FlyBall)) |>
      summarise(.by = "pitch_name", `Fly Ball %` = mean(FlyBall))
    
    OBP <- Batter_Data |>
      filter(!is.na(on_base)) |>
      summarise(.by = "pitch_name", `On Base %`  = mean(on_base))
    
    SLG <- Batter_Data |>
      filter(!is.na(slugging)) |>
      summarise(.by = "pitch_name", `Slugging %`  = mean(slugging))
    
    Table <- Pitches_Seen |>
      left_join(Whiff, by = "pitch_name") |>
      left_join(Chase, by = "pitch_name") |>
      left_join(Foul, by = "pitch_name") |>
      left_join(Strike_Looking, by = "pitch_name") |>
      left_join(Exit_Velo, by = "pitch_name") |>
      left_join(Hard_Hit, by = "pitch_name") |>
      left_join(Ground_Ball, by = "pitch_name") |>
      left_join(Line_Drive, by = "pitch_name") |>
      left_join(Fly_Ball, by = "pitch_name") |>
      left_join(OBP, by = "pitch_name") |>
      left_join(SLG, by = "pitch_name") |>
      arrange(desc(`Pitches Seen`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      mutate_if(is.numeric, round, digits = 3)
    
  })
  
  output$table <- renderDataTable({Pitch_Filter_Table()})
  
  #Linear Model ####
  
  lm_variables <- colnames(Batter_Data)[c(29:33,35:46,58:68,79,189)]
  glm_variables <- colnames(Batter_Data)[c(168:179)]
  
  Filtered_Names <- reactive({
    Batter_Filtered() |>
      filter(batter_name == input$batter_select)
  })
  
  fo_lm <- reactive({
    as.formula(reformulate(input$SelectX_LM, as.character(input$SelectY_LM)))
  })
  
  fo_glm <- reactive({
    as.formula(reformulate(input$SelectX_GLM, as.character(input$SelectY_GLM)))
  })
  
  Linear_Model <- reactive({
    lm(fo_lm(), data = Filtered_Names(), na.action = na.omit)
  })
  
  Generalized_Linear_Model <- reactive({
    lm(fo_glm(), data = Filtered_Names(), na.action = na.omit)
  })
  
  output$LM <- renderPrint(summary(Linear_Model()))
  output$GLM <- renderPrint(summary(Generalized_Linear_Model()))
  
}
  
