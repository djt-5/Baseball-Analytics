server <- function(input, output, session) {
  
  Data <- read_csv("Baserunning-Data.csv")
  model <- read_rds("naivebayes.RDS")
  
  ReactiveData <- reactive({
    
    Data |>
      filter(sprint_speed >= input$sprint_speed[1] &
               sprint_speed <= input$sprint_speed[2]) |>
      filter(arm_strength >= input$arm_strength[1] &
                      arm_strength <= input$arm_strength[2]) |>
      filter(swing %in% input$swing) |>
      filter(plate_z >= input$plate_z[1] &
               plate_z <= input$plate_z[2]) |>
      filter(plate_x >= input$plate_x[1] &
               plate_x <= input$plate_x[2]) |>
      filter(pop_time >= input$pop_time[1] &
               pop_time <= input$pop_time[2]) |>
      filter(release_speed >= input$release_speed[1] &
               release_speed <= input$release_speed[2]) |>
      filter(lead >= input$lead[1] &
              lead <= input$lead[2])
  })
  
  output$sprint_speed_barplot <- renderPlot({
    ReactiveData() |>
      mutate(sprint_cut = cut(sprint_speed,seq(0,100,1))) |>
      summarise(.by = "sprint_cut",
                N = n(),
                safe = mean(safe)) |>
      ggplot(aes(x=sprint_cut, fill=safe)) +
      xlab("Sprint Speed") +
      geom_col(aes(y = N), width = 1) +
      scale_fill_distiller(palette = "RdBu", direction = -1, name = "safe") 
  })
  
  output$arm_strength_barplot <- renderPlot({
    ReactiveData() |>
      mutate(arm_cut = cut(arm_strength,seq(0,100,1))) |>
      summarise(.by = "arm_cut",
                N = n(),
                safe = mean(safe)) |>
      ggplot(aes(x=arm_cut, fill=safe)) +
      xlab("Arm Strength") +
      geom_col(aes(y = N), width = 1) +
      scale_fill_distiller(palette = "RdBu", direction = -1, name = "safe") 
  })
  
  output$pop_time_barplot <- renderPlot({
    ReactiveData() |>
      mutate(pop_cut = cut(pop_time,seq(0,40,0.03))) |>
      summarise(.by = "pop_cut",
                N = n(),
                safe = mean(safe)) |>
      ggplot(aes(x=pop_cut, fill=safe)) +
      xlab("Pop Time") +
      geom_col(aes(y = N), width = 1) +
      scale_fill_distiller(palette = "RdBu", direction = -1, name = "safe") 
  })
  
  output$release_speed_barplot <- renderPlot({
    ReactiveData() |>
      mutate(velo_cut = cut(release_speed,seq(0,100,1))) |>
      summarise(.by = "velo_cut",
                N = n(),
                safe = mean(safe)) |>
      ggplot(aes(x=velo_cut, fill=safe)) +
      xlab("Release Speed") +
      geom_col(aes(y = N), width = 1) +
      scale_fill_distiller(palette = "RdBu", direction = -1, name = "safe") 
  })
  
  output$lead_barplot <- renderPlot({
    ReactiveData() |>
      mutate(lead_cut = cut(lead,seq(0,100,1))) |>
      summarise(.by = "lead_cut",
                N = n(),
                safe = mean(safe)) |>
      ggplot(aes(x=lead_cut, fill=safe)) +
      xlab("Runner Lead") +
      geom_col(aes(y = N), width = 1) +
      scale_fill_distiller(palette = "RdBu", direction = -1, name = "safe") 
  })
  
  Left = -8.5/12
  Right = 8.5/12
  Bottom = 18.29/12
  Top = 44.08/12
  Width = (Right - Left) / 3
  Height = (Top - Bottom) / 3
  
  output$kzone_plot <- renderPlot({
    
    ReactiveData() |>
      mutate(safe = ifelse(safe == 1, "safe", "out")) |>
      ggplot(aes(x = plate_x, y = plate_z, color = safe)) +
      geom_point(size = 1, alpha = 0.3) +
      scale_color_manual(values = c(safe = "red", out = "blue")) +
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
  
  output$data_table <- renderDT({
    Reactive <- ReactiveData()
    data.frame(`Safe %` = mean(Reactive$safe),
               `Run Value` = mean(Reactive$delta_run_exp),
               `Release Speed` = mean(Reactive$release_speed),
               `Plate X` = mean(Reactive$plate_x),
               `Plate Z` = mean(Reactive$plate_z),
               `Pop Time` = mean(Reactive$pop_time),
               `Exchange Time` = mean(Reactive$exchange_time),
               `Arm Strength` = mean(Reactive$arm_strength),
               Lead = mean(Reactive$lead),
               `Sprint Speed` = mean(Reactive$sprint_speed),
               `Swing %` = mean(Reactive$swing)
               ) |>
      mutate_if(is.numeric, round, digits = 3)
  })
    
    test_data <- eventReactive(input$action, {
        data.frame(release_speed = input$bayes_velocity,
        plate_x = input$bayes_plate_x,
        plate_z = input$bayes_plate_z,
        pop_time = input$bayes_pop,
        arm_strength = input$bayes_arm,
        lead = input$bayes_lead,
        sprint_speed = input$bayes_sprint,
        swing = input$bayes_swing)
    })
    
    prediction <- eventReactive(input$action, {
      pred <- predict(model, newdata = test_data())
      pred
    })
    
    output$bayes_pred <- renderText({
      safe_out <- prediction()
      decode <- function(x){
        case_when(x == 1 ~ paste0("The runner is likely to be SAFE"),
                  x == 0 ~ "The runner is likely to be OUT"
        )}
      decode(safe_out)
    })
   
}
  
  