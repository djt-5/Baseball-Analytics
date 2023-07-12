server <- function(input, output) {
  
  #Data Info
  
  Batter_Data <- read_csv("Batter_Data.csv")
  
  #Reactive Filtering ####
  
  Batter_Filtered <- reactive({
    if(input$p_throws != "Either") {
      Batter_Data <- Batter_Data |>
        filter(p_throws == input$p_throws)
    }
    
    if(input$pitch_select != "All") {
      Batter_Data <- Batter_Data |>
        filter(pitch_name == input$pitch_select)
    }
    
    Batter_Data
  })
  
  hit_kzone_obp <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_obp <- renderPlot({hit_kzone_obp()}, height = 500)
  
  hit_kzone_slg <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_slg <- renderPlot({hit_kzone_slg()}, height = 500)
  
  hit_kzone_freq <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_freq <- renderPlot({hit_kzone_freq()}, height = 500)
  
  hit_kzone_exit_velo <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(launch_speed)) |>
      summarize(.by = c("width","height"), 
                `Mean Exit Velo` = mean(launch_speed)) |>
      ggplot(aes(x = width, y = height, fill = `Mean Exit Velo`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Mean Exit Velo`, digits = 0), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_exit_velo <- renderPlot({hit_kzone_exit_velo()}, height = 500)
  
  hit_kzone_angle <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(launch_angle)) |>
      summarize(.by = c("width","height"), 
                `Mean Launch Angle` = mean(launch_angle)) |>
      ggplot(aes(x = width, y = height, fill = `Mean Launch Angle`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Mean Launch Angle`, digits = 1), colour = "white")) +
      scale_colour_manual(values=c("white"="white")) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_angle <- renderPlot({hit_kzone_angle()}, height = 500)
  
  hit_kzone_hard_hit <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_hard_hit <- renderPlot({hit_kzone_hard_hit()}, height = 500)
  
  hit_kzone_groundball <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_groundball <- renderPlot({hit_kzone_groundball()}, height = 500)
  
  hit_kzone_linedrive <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_linedrive <- renderPlot({hit_kzone_linedrive()}, height = 500)
  
  hit_kzone_flyball <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_flyball <- renderPlot({hit_kzone_flyball()}, height = 500)
  
  hit_kzone_whiff <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(description == "InPlay" | description == "FoulBall" |
               description == "StrikeSwinging") |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_whiff <- renderPlot({hit_kzone_whiff()}, height = 500)
  
  hit_kzone_chase <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
      filter(HorzBreak >= input$kzone_break_x[1] &
               HorzBreak <= input$kzone_break_x[2]) |>
      filter(VertBreak >= input$kzone_break_y[1] &
               VertBreak <= input$kzone_break_y[2]) |>
      filter(!is.na(Chase)) |>
      filter(plate_x < -17/24 | plate_x > 17/24 | 
               plate_z < 1.65 | plate_z > 3.65) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_chase <- renderPlot({hit_kzone_chase()}, height = 500)
  
  hit_kzone_taken <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_taken <- renderPlot({hit_kzone_taken()}, height = 500)
  
  hit_kzone_pull <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_pull <- renderPlot({hit_kzone_pull()}, height = 500)
  
  hit_kzone_foul <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
                fill = "transparent", color = "red", size = 2.5) +
      theme(aspect.ratio = 1.4)
  })
  
  output$hit_kzone_foul <- renderPlot({hit_kzone_foul()}, height = 500)
  
  Batter_Filtered_Spray <- reactive({
    if(input$p_throws != "Either") {
      Batter_Data <- Batter_Data |>
        filter(p_throws == input$p_throws)
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
  
  # Pitcher Spray Chart ####
  
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
  
  #Timeline ####
  
  hit_timeline_zone <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      select(batter_name, zone, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Zone %` = mean(zone)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Zone %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_zone <- renderPlot({hit_timeline_zone()},height = 700)
  
  hit_timeline_whiff <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      select(batter_name, Whiff, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Whiff %` = mean(Whiff)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Whiff %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_whiff <- renderPlot({hit_timeline_whiff()},height = 700)
  
  hit_timeline_chase <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(Chase)) |>
      filter(plate_x < -17/24 | plate_x > 17/24 | 
               plate_z < 1.65 | plate_z > 3.65) |>
      select(batter_name, Chase, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Chase %` = mean(Chase, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Chase %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_chase <- renderPlot({hit_timeline_chase()},height = 700)
  
  hit_timeline_hard_hit <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_speed)) |>
      select(batter_name, Hard_Hit, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Hard Hit %` = mean(Hard_Hit, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Hard Hit %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_hard_hit <- renderPlot({hit_timeline_hard_hit()},height = 700)
  
  hit_timeline_groundball <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_speed)) |>
      select(batter_name, GroundBall, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Ground Ball %` = mean(GroundBall, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Ground Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_groundball <- renderPlot({hit_timeline_groundball()},height = 700)
  
  hit_timeline_linedrive <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_speed)) |>
      select(batter_name, LineDrive, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Line Drive %` = mean(LineDrive, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Line Drive %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_linedrive <- renderPlot({hit_timeline_linedrive()},height = 700)
  
  hit_timeline_flyball <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_speed)) |>
      select(batter_name, FlyBall, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Fly Ball %` = mean(FlyBall, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Fly Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_flyball <- renderPlot({hit_timeline_flyball()},height = 700)
  
  hit_timeline_foulball <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      select(batter_name, FoulBall, `Plate Appearances`) |>
      filter(!is.na(FoulBall)) |>
      summarise(.by = "Plate Appearances", `Foul Ball %` = mean(FoulBall, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Foul Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_foulball <- renderPlot({hit_timeline_foulball()},height = 700)
  
  hit_timeline_exit_velo <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_speed)) |>
      select(batter_name, launch_speed, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Mean Exit Velo` = mean(launch_speed, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Mean Exit Velo`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(50,100)) +
      theme_bw()
  })
  
  output$hit_timeline_exit_velo <- renderPlot({hit_timeline_exit_velo()},height = 700)
  
  hit_timeline_angle <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(launch_angle)) |>
      select(batter_name, launch_angle, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Mean Launch Angle` = mean(launch_angle, na.rm = TRUE)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Mean Launch Angle`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(-90,90)) +
      theme_bw()
  })
  
  output$hit_timeline_angle <- renderPlot({hit_timeline_angle()},height = 700)
  
  hit_timeline_obp <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(on_base)) |>
      select(batter_name, on_base, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `On Base %` = mean(on_base)) |>
      ggplot(aes(x = `Plate Appearances`, y = `On Base %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_obp <- renderPlot({hit_timeline_obp()},height = 700)
  
  hit_timeline_slg <- reactive({
    
    Batter_Filtered() |>
      filter(batter_name == input$batter_select) |>
      filter(!is.na(slugging)) |>
      select(batter_name, slugging, `Plate Appearances`) |>
      summarise(.by = "Plate Appearances", `Slugging %` = mean(slugging, na.rm = TRUE) + 
                  mean(slugging)) |>
      ggplot(aes(x = `Plate Appearances`, y = `Slugging %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$hit_timeline_slg <- renderPlot({hit_timeline_slg()},height = 700)
  
  #Data Table ####
  
  Hit_Filter_Table <- reactive({
    
    if(input$p_throws != "Either") {
      Batter_Data <- Batter_Data |>
        filter(p_throws == input$p_throws)
    }
    
    Batter_Data <- Batter_Data |>
      filter(batter_name == input$batter_select)
    
    Pitches_Seen <- Batter_Data |>
      filter(batter_name == input$batter_select) |>
      count(pitch_name) |>
      rename(`Pitches Seen` = n)
    
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
      filter(plate_z > 1.65 & plate_z < 3.65 & plate_x > -17/24 & 
               plate_x < 17/24) |>
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
  
  output$table <- renderDataTable({Hit_Filter_Table()})
  
  #Linear Model ####
  
  InputDataset <- reactive({
    
    if(input$model_stand != "Either") {
      Batter_Data <- Batter_Data |>
        filter(stand == input$model_stand)
    }
    
    if(input$model_pitch != "All") {
      Batter_Data <- Batter_Data |>
        filter(pitch_name == input$model_pitch)
    }
    
    if(input$model_p_throws != "Either") {
      Batter_Data <- Batter_Data |>
        filter(p_throws == input$model_p_throws)
    }
    
    Batter_Data
  })
  
  fo <- reactive({
    as.formula(reformulate(input$SelectX, as.character(input$SelectY)))
  })
  
  Linear_Model <- reactive({
    lm(fo(), data = InputDataset(), na.action = na.omit)
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  
}