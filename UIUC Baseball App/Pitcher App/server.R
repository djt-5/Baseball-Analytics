server <- function(input, output) {
  
  #Data Info
  
  Pitcher_Data <- read_csv("Pitcher_Data.csv")
  
  #Reactive Filtering ####
  
  Pitcher_Filtered <- reactive({
    if(input$batter_side != "Both") {
      Pitcher_Data <- Pitcher_Data |>
        filter(stand == input$batter_side)
    }
    
    if(input$pitch_select != "All") {
      Pitcher_Data <- Pitcher_Data |>
        filter(pitch_name == input$pitch_select)
    }
    
    Pitcher_Data
  })
  
  # Kzone ####
  
  pitch_kzone_obp <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_obp <- renderPlot({pitch_kzone_obp()}, height = 500)
  
  pitch_kzone_slg <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_slg <- renderPlot({pitch_kzone_slg()}, height = 500)
  
  pitch_kzone_freq <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_freq <- renderPlot({pitch_kzone_freq()}, height = 500)
  
  pitch_kzone_exit_velo <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
  
  output$pitch_kzone_exit_velo <- renderPlot({pitch_kzone_exit_velo()}, height = 500)
  
  pitch_kzone_angle <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
  
  output$pitch_kzone_angle <- renderPlot({pitch_kzone_angle()}, height = 500)
  
  pitch_kzone_hard_hit <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_hard_hit <- renderPlot({pitch_kzone_hard_hit()}, height = 500)
  
  pitch_kzone_groundball <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_groundball <- renderPlot({pitch_kzone_groundball()}, height = 500)
  
  pitch_kzone_linedrive <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_linedrive <- renderPlot({pitch_kzone_linedrive()}, height = 500)
  
  pitch_kzone_flyball <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_flyball <- renderPlot({pitch_kzone_flyball()}, height = 500)
  
  pitch_kzone_whiff <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
  
  output$pitch_kzone_whiff <- renderPlot({pitch_kzone_whiff()}, height = 500)
  
  pitch_kzone_chase <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(release_speed >= input$kzone_velo_slider[1] &
               release_speed <= input$kzone_velo_slider[2]) |>
      filter(release_spin_rate >= input$kzone_spin_slider[1] &
               release_spin_rate <= input$kzone_spin_slider[2]) |>
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
  
  output$pitch_kzone_chase <- renderPlot({pitch_kzone_chase()}, height = 500)
  
  pitch_kzone_taken <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_taken <- renderPlot({pitch_kzone_taken()}, height = 500)
  
  pitch_kzone_pull <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_pull <- renderPlot({pitch_kzone_pull()}, height = 500)
  
  pitch_kzone_foul <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
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
                fill = "transparent", color = "red", linewidth = 2.5) +
      theme(aspect.ratio = 1.4) 
  })
  
  output$pitch_kzone_foul <- renderPlot({pitch_kzone_foul()}, height = 500)
  
  Pitcher_Filtered_Spray <- reactive({
    if(input$batter_side != "Both") {
      Pitcher_Data <- Pitcher_Data |>
        filter(stand == input$batter_side)
    }
    
    if(input$pitch_select != "All") {
      Pitcher_Data <- Pitcher_Data |>
        filter(pitch_name == input$pitch_select)
    }
    
    if(input$bb_type != "All") {
      Pitcher_Data <- Pitcher_Data |>
        filter(bb_type %in% input$bb_type)
    }
    
    Pitcher_Data
  })
  
  # Pitcher Spray Chart ####
  
  pitch_spray_chart <- reactive({
    
    Pitcher_Filtered_Spray() |>
      filter(pitcher_name == input$pitcher_select) |>
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
  
  output$pitch_spray <- renderPlot({pitch_spray_chart()})
  
  #Pitch Info ####
  
  pitch_release <- reactive({
    
    Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, pitch_name, release_pos_x, release_pos_z) |>
      ggplot(aes(x = release_pos_x, y = release_pos_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Release Side", limits = c(-5,5)*12) +
      scale_y_continuous("Release Height", limits = c(0,7)*12) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_release <- renderPlot({pitch_release()}, height = 700)
  
  pitch_angle <- reactive({
    
    Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, pitch_name, HorzRelAngle, VertRelAngle) |>
      ggplot(aes(x = HorzRelAngle, y = VertRelAngle, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horizontal Release Angle", limits = c(-9,10)) +
      scale_y_continuous("Vertical Release Angle", limits = c(-8,7)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_angle <- renderPlot({pitch_angle()}, height = 700)
  
  pitch_appr <- reactive({
    
    Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, pitch_name, HorzApprAngle, VertApprAngle) |>
      ggplot(aes(x = HorzApprAngle, y = VertApprAngle, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horizontal Approach Angle", limits = c(-9,11)) +
      scale_y_continuous("Vertical Approach Angle", limits = c(-17,1)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_appr <- renderPlot({pitch_appr()}, height = 700)
  
  pitch_pfx <- reactive({
    
    Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, pitch_name, pfx_x, pfx_z) |>
      ggplot(aes(x = pfx_x, y = pfx_z, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horizontal Movement", limits = c(-16,19)) +
      scale_y_continuous("Vertical Movement", limits = c(-13,19)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_pfx <- renderPlot({pitch_pfx()}, height = 700)
  
  pitch_break <- reactive({
    
    Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, pitch_name, VertBreak, HorzBreak) |>
      ggplot(aes(x = HorzBreak, y = VertBreak, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horizontal Break", limits = c(-35,30)) +
      scale_y_continuous("Vertical Break", limits = c(-80,0)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_break <- renderPlot({pitch_break()}, height = 700)
  
  pitch_spin <- reactive({
    
    Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(release_spin_rate)) |>
      select(pitcher_name, pitch_name, release_spin_rate, spin_axis) |>
      ggplot(aes(x = spin_axis, y = release_spin_rate, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Spin Axis (Degrees)", limits = c(0,360)) +
      scale_y_continuous("Release Spin", limits = c(700,3200)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_spin <- renderPlot({pitch_spin()}, height = 700)
  
  pitch_velo <- reactive({

    Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(release_spin_rate)) |>
      select(pitcher_name, pitch_name, release_speed, SpeedDrop) |>
      ggplot(aes(x = release_speed, y = -SpeedDrop, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Release Velo", limits = c(50,100)) +
      scale_y_continuous("Drop in Velo", limits = c(-20,0)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_velo <- renderPlot({pitch_velo()}, height = 700)
  
  pitch_tunnel <- reactive({
    
    Pitcher_Filtered() |> 
      filter(pitcher_name == input$pitcher_select) |>
      ggplot(aes(x = x0, y = z0, color = pitch_name,
                 alpha = 0.5)) +
      geom_point() +
      scale_x_continuous("Horizontal Location after 10 feet", limits = c(-5,5)*12) +
      scale_y_continuous("Vertical Location after 10 feet", limits = c(0,7)*12) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      theme_bw()
  })
  
  output$pitch_tunnel <- renderPlot({pitch_tunnel()}, height = 700)
  
  # Pitch Sequencing ####
  
  Seq_Filtered <- reactive({
    
    if(input$batter_side != "Both") {
      Pitcher_Data <- Pitcher_Data |>
        filter(stand == input$batter_side)
    }
    
    Pitcher_Data |>
      filter(!(!is.na(prev_pitch) & count == ("0 - 0"))) |>
      filter(!is.na(prev_pitch)) 
  })
  
  seq_freq <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      summarise(.by = c("prev_pitch", "count"), 
                most_frequent = fmode(pitch_name)) 
    
    df2 <- df[!duplicated(df), ]
    
    df2 |>
      ggplot(aes(x = count, y = prev_pitch, fill = most_frequent)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Pitch Most Thrown in Situation") +
      theme_bw()
    
  })
  
  output$seq_freq <- renderPlot({seq_freq()}, height = 700)
  
  seq_whiff <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(Whiff != "NaN") |>
      summarise(.by = c("prev_pitch", "pitch_name", "count"), 
                `Whiff %` = mean(Whiff, na.rm = TRUE)) |>
      filter(`Whiff %` != 0)
    
    df2 <- df |>
      summarise(.by = c("prev_pitch", "count"), 
                `Whiff %` = max(`Whiff %`))
    
    df3 <- inner_join(df, df2, by = c("prev_pitch", "count", "Whiff %"))
    
    df4 <- df3[!duplicated(df3), ]
    
    df4 |>
      ggplot(aes(x = count, y = prev_pitch, fill = pitch_name)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      geom_text(aes(label = round(`Whiff %`, digits = 3))) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Best pitch to throw to maximize Whiff % based on count and previous pitch") +
      theme_bw()
      
  })
  
  output$seq_whiff <- renderPlot({seq_whiff()}, width = 1100, height = 700)
  
  seq_chase <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(Chase != "NaN") |>
      summarise(.by = c("prev_pitch", "pitch_name", "count"), 
                `Chase %` = mean(Chase, na.rm = TRUE)) |>
      filter(`Chase %` != 0)
    
    df2 <- df |>
      summarise(.by = c("prev_pitch", "count"), 
                `Chase %` = max(`Chase %`))
    
    df3 <- inner_join(df, df2, by = c("prev_pitch", "count", "Chase %"))
    
    df4 <- df3[!duplicated(df3), ]
    
    df4 |>
      ggplot(aes(x = count, y = prev_pitch, fill = pitch_name)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      geom_text(aes(label = round(`Chase %`, digits = 3))) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Best pitch to throw to maximize Chase % based on count and previous pitch") +
      theme_bw()
    
  })
  
  output$seq_chase <- renderPlot({seq_chase()}, width = 1100, height = 700)
  
  seq_foul <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(FoulBall != "NaN") |>
      summarise(.by = c("prev_pitch", "pitch_name", "count"), 
                `Foul Ball %` = mean(FoulBall, na.rm = TRUE)) |>
      filter(`Foul Ball %` != 0)
    
    df2 <- df |>
      summarise(.by = c("prev_pitch", "count"), 
                `Foul Ball %` = max(`Foul Ball %`))
    
    df3 <- inner_join(df, df2, by = c("prev_pitch", "count", "Foul Ball %"))
    
    df4 <- df3[!duplicated(df3), ]
    
    df4 |>
      ggplot(aes(x = count, y = prev_pitch, fill = pitch_name)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      geom_text(aes(label = round(`Foul Ball %`, digits = 3))) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Best pitch to throw to maximize Foul Ball % based on count and previous pitch") +
      theme_bw()
    
  })
  
  output$seq_foul <- renderPlot({seq_foul()}, width = 1100, height = 700)
  
  seq_flyball <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(FlyBall != "NaN") |>
      summarise(.by = c("prev_pitch", "pitch_name", "count"), 
                `Fly Ball %` = mean(FlyBall, na.rm = TRUE)) |>
      filter(`Fly Ball %` != 1)
    
    df2 <- df |>
      summarise(.by = c("prev_pitch", "count"), 
                `Fly Ball %` = min(`Fly Ball %`))
    
    df3 <- inner_join(df, df2, by = c("prev_pitch", "count", "Fly Ball %"))
    
    df4 <- df3[!duplicated(df3), ]
    
    df4 |>
      ggplot(aes(x = count, y = prev_pitch, fill = pitch_name)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      geom_text(aes(label = round(`Fly Ball %`, digits = 3))) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Best pitch to throw to minimize Fly Ball % based on count and previous pitch") +
      theme_bw()
    
  })
  
  output$seq_flyball <- renderPlot({seq_flyball()}, width = 1100, height = 700)
  
  seq_groundball <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(GroundBall != "NaN") |>
      summarise(.by = c("prev_pitch", "pitch_name", "count"), 
                `Ground Ball %` = mean(GroundBall, na.rm = TRUE)) |>
      filter(`Ground Ball %` != 0)
    
    df2 <- df |>
      summarise(.by = c("prev_pitch", "count"), 
                `Ground Ball %` = max(`Ground Ball %`))
    
    df3 <- inner_join(df, df2, by = c("prev_pitch", "count", "Ground Ball %"))
    
    df4 <- df3[!duplicated(df3), ]
    
    df4 |>
      ggplot(aes(x = count, y = prev_pitch, fill = pitch_name)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      geom_text(aes(label = round(`Ground Ball %`, digits = 3))) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Best pitch to throw to maximize Ground Ball % based on count and previous pitch") +
      theme_bw()
    
  })
  
  output$seq_groundball <- renderPlot({seq_groundball()}, width = 1100, height = 700)
  
  seq_linedrive <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(LineDrive != "NaN") |>
      summarise(.by = c("prev_pitch", "pitch_name", "count"), 
                `Line Drive %` = mean(LineDrive, na.rm = TRUE)) |>
      filter(`Line Drive %` != 1)
    
    df2 <- df |>
      summarise(.by = c("prev_pitch", "count"), 
                `Line Drive %` = min(`Line Drive %`))
    
    df3 <- inner_join(df, df2, by = c("prev_pitch", "count", "Line Drive %"))
    
    df4 <- df3[!duplicated(df3), ]
    
    df4 |>
      ggplot(aes(x = count, y = prev_pitch, fill = pitch_name)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      geom_text(aes(label = round(`Line Drive %`, digits = 3))) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Best pitch to throw to minimize Line Drive % based on count and previous pitch") +
      theme_bw()
    
  })
  
  output$seq_linedrive <- renderPlot({seq_linedrive()}, width = 1100, height = 700)
  
  seq_hardhit <- reactive({
    
    df <- Seq_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(Hard_Hit != "NaN") |>
      summarise(.by = c("prev_pitch", "pitch_name", "count"), 
                `Hard Hit %` = mean(Hard_Hit, na.rm = TRUE)) |>
      filter(`Hard Hit %` != 1)
    
    df2 <- df |>
      summarise(.by = c("prev_pitch", "count"), 
                `Hard Hit %` = min(`Hard Hit %`))
    
    df3 <- inner_join(df, df2, by = c("prev_pitch", "count", "Hard Hit %"))
    
    df4 <- df3[!duplicated(df3), ]
    
    df4 |>
      ggplot(aes(x = count, y = prev_pitch, fill = pitch_name)) +
      geom_tile() +
      scale_fill_manual(values = c("Changeup" = "blue", "Curveball" = "yellow", 
                                    "Sinker" = "pink", "Four-Seam" = "red",
                                    "Slider" = "orange", "Splitter" = "green",
                                    "Cutter" = "purple")) +
      geom_text(aes(label = round(`Hard Hit %`, digits = 3))) +
      scale_x_discrete("Count", limits = unique(Pitcher_Data$count)) +
      scale_y_discrete("Previous Pitch", limits = unique(Pitcher_Data$pitch_name)) +
      ggtitle("Best pitch to throw to minimize Hard Hit % based on count and previous pitch") +
      theme_bw()
    
  })
  
  output$seq_hardhit <- renderPlot({seq_hardhit()}, width = 1100, height = 700)

  
  #Timeline ####
  
  pitch_timeline_arm_height <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(release_pos_z)) |>
      select(pitcher_name, release_pos_z, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Mean Arm Height` = mean(release_pos_z)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Mean Arm Height`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(3,7)*12) +
      theme_bw()
  })
  
  output$pitch_timeline_arm_height <- renderPlot({pitch_timeline_arm_height()},height = 700)
  
  pitch_timeline_velo <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, release_speed, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Mean Velo` = mean(release_speed)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Mean Velo`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(50,100)) +
      theme_bw()
  })
  
  output$pitch_timeline_velo <- renderPlot({pitch_timeline_velo()},height = 700)
  
  pitch_timeline_strike <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, Strike, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Strike %` = mean(Strike)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Strike %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_strike <- renderPlot({pitch_timeline_strike()},height = 700)
  
  pitch_timeline_whiff <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, Whiff, Pitches_Thrown) |>
      filter(!is.na(Whiff)) |>
      summarise(.by = "Pitches_Thrown", `Whiff %` = mean(Whiff)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Whiff %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_whiff <- renderPlot({pitch_timeline_whiff()},height = 700)
  
  pitch_timeline_chase <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(Chase)) |>
      filter(plate_x < -0.8308333 | plate_x > 0.8308333 | 
               plate_z < 1.5275 | plate_z > 3.7725) |>
      select(pitcher_name, Chase, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Chase %` = mean(Chase)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Chase %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_chase <- renderPlot({pitch_timeline_chase()},height = 700)
  
  pitch_timeline_hard_hit <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(launch_speed)) |>
      select(pitcher_name, Hard_Hit, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Hard Hit %` = mean(Hard_Hit)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Hard Hit %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_hard_hit <- renderPlot({pitch_timeline_hard_hit()},height = 700)
  
  pitch_timeline_groundball <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(launch_speed)) |>
      select(pitcher_name, GroundBall, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Ground Ball %` = mean(GroundBall)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Ground Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_groundball <- renderPlot({pitch_timeline_groundball()},height = 700)
  
  pitch_timeline_linedrive <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(launch_speed)) |>
      select(pitcher_name, LineDrive, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Line Drive %` = mean(LineDrive)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Line Drive %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_linedrive <- renderPlot({pitch_timeline_linedrive()},height = 700)
  
  pitch_timeline_flyball <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(launch_speed)) |>
      select(pitcher_name, FlyBall, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Fly Ball %` = mean(FlyBall)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Fly Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_flyball <- renderPlot({pitch_timeline_flyball()},height = 700)
  
  pitch_timeline_foulball <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      select(pitcher_name, FoulBall, Pitches_Thrown) |>
      filter(!is.na(FoulBall)) |>
      summarise(.by = "Pitches_Thrown", `Foul Ball %` = mean(FoulBall)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Foul Ball %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_foulball <- renderPlot({pitch_timeline_foulball()},height = 700)
  
  pitch_timeline_exit_velo <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(launch_speed)) |>
      select(pitcher_name, launch_speed, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Mean Exit Velo` = mean(launch_speed)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Mean Exit Velo`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(50,100)) +
      theme_bw()
  })
  
  output$pitch_timeline_exit_velo <- renderPlot({pitch_timeline_exit_velo()},height = 700)
  
  pitch_timeline_angle <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(launch_angle)) |>
      select(pitcher_name, launch_angle, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Mean Launch Angle` = mean(launch_angle)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Mean Launch Angle`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(-90,90)) +
      theme_bw()
  })
  
  output$pitch_timeline_angle <- renderPlot({pitch_timeline_angle()},height = 700)
  
  pitch_timeline_obp <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(on_base)) |>
      select(pitcher_name, on_base, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `On Base %` = mean(on_base)) |>
      ggplot(aes(x = Pitches_Thrown, y = `On Base %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
  })
  
  output$pitch_timeline_obp <- renderPlot({pitch_timeline_obp()},height = 700)
  
  pitch_timeline_slg <- reactive({
    
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select) |>
      filter(!is.na(slugging)) |>
      select(pitcher_name, slugging, Pitches_Thrown) |>
      summarise(.by = "Pitches_Thrown", `Slugging %` = mean(slugging) + 
                  mean(slugging)) |>
      ggplot(aes(x = Pitches_Thrown, y = `Slugging %`)) +
      geom_point() +
      geom_smooth(method="lm" ) +
      scale_y_continuous(limits = c(0,4)) +
      theme_bw()
  })
  
  output$pitch_timeline_slg <- renderPlot({pitch_timeline_slg()},height = 700)
  
  #Data Table ####
  
  Pitch_Filter_Table <- reactive({
    
    if(input$batter_side != "Both") {
      Pitcher_Data <- Pitcher_Data |>
        filter(stand == input$batter_side)
    }
    
    Pitcher_Data <- Pitcher_Data |>
      filter(pitcher_name == input$pitcher_select)
    
    Pitches_Seen <- Pitcher_Data |>
      summarise(.by = "pitch_name", Pitches_Thrown = n())  |>
      rename(`Pitches Thrown` = Pitches_Thrown)
    
    Whiff <- Pitcher_Data |>
      filter(!is.na(Whiff)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(Whiff))
    
    Chase <- Pitcher_Data |>
      filter(!is.na(Chase)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(Chase))
    
    Foul <- Pitcher_Data |>
      filter(!is.na(FoulBall)) |>
      summarise(.by = "pitch_name", `Foul Ball %` = mean(FoulBall))
    
    Strike_Looking <- Pitcher_Data |>
      filter(plate_z > 1.5275 & plate_z < 3.7725 & plate_x > -0.8308333 & 
               plate_x < 0.8308333) |>
      summarise(.by = "pitch_name", `Strike Looking %` = mean(Taken))
    
    Exit_Velo <- Pitcher_Data |>
      filter(!is.na(launch_speed)) |>
      summarise(.by = "pitch_name", `Mean Exit Velo` = mean(launch_speed))
    
    Hard_Hit <- Pitcher_Data |>
      filter(!is.na(Hard_Hit)) |>
      summarise(.by = "pitch_name", `Hard Hit %` = mean(Hard_Hit)) 
    
    Line_Drive <- Pitcher_Data |>
      filter(!is.na(LineDrive)) |>
      summarise(.by = "pitch_name", `Line Drive %` = mean(LineDrive))
    
    Ground_Ball <- Pitcher_Data |>
      filter(!is.na(GroundBall)) |>
      summarise(.by = "pitch_name", `Ground Ball %` = mean(GroundBall))
    
    Fly_Ball <- Pitcher_Data |>
      filter(!is.na(FlyBall)) |>
      summarise(.by = "pitch_name", `Fly Ball %` = mean(FlyBall))
    
    OBP <- Pitcher_Data |>
      filter(!is.na(on_base)) |>
      summarise(.by = "pitch_name", `On Base %`  = mean(on_base))
    
    SLG <- Pitcher_Data |>
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
      arrange(desc(`Pitches Thrown`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      mutate_if(is.numeric, round, digits = 3)
    
  })
  
  output$table <- renderDataTable({Pitch_Filter_Table()})
  
  #Linear Model ####
  
  lm_variables <- colnames(Pitcher_Data)[c(29:33,35:46,58:60,62,79,188)]
  glm_variables <- colnames(Pitcher_Data)[c(168:179)]
  
  Filtered_Names <- reactive({
    Pitcher_Filtered() |>
      filter(pitcher_name == input$pitcher_select)
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
  
