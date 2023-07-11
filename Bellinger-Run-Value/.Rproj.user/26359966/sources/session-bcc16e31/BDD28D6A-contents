library(shiny)
library(tidyverse)
Belli_Data <- read_csv("Belli_Data.csv") 

server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    
    Timeline_Data <- Belli_Data |>
      filter(Bases %in% input$bases) |>
      filter(outs_when_up %in% input$outs) |>
      filter(p_throws %in% input$p_throws) 
    
    Input_Data <- Timeline_Data |>
      filter(game_year %in% input$year)
    
    #Timeline Output
    
    if(input$p_throws == "R") {
      timeline <- Timeline_Data |>
        summarise(.by = "game_year", `Average Run Value` = mean(delta_run_exp)) |>
        ggplot(aes(x = game_year, y = `Average Run Value`)) +
        geom_bar(stat="identity", color = "blue") +
        scale_x_discrete(limits = 2017:2023) +
        ggtitle(paste0("The RE24 over time of Cody Bellinger with ", 
                       input$bases, " and ", input$outs, " out against a Righty")) +
        theme_bw()
    } else {
      timeline <- Timeline_Data |>
        summarise(.by = "game_year", `Average Run Value` = mean(delta_run_exp)) |>
        ggplot(aes(x = game_year, y = `Average Run Value`)) +
        geom_bar(stat="identity", color = "blue") +
        scale_x_discrete(limits = 2017:2023) +
        ggtitle(paste0("The RE24 over time of Cody Bellinger with ", 
                       input$bases, " and ", input$outs, " out against a Lefty")) +
        theme_bw()
    }
    
    
    output$timeline <- renderPlot(timeline)
    
    #Strike Zone Output
    
    k_zone <- Input_Data |>
      filter(game_year %in% input$year) |>
      summarize(.by = c("width","height"), 
                `Average Run Value` = mean(delta_run_exp)) |>
      ggplot(aes(x = width, y = height, fill = `Average Run Value`)) +
      geom_tile() +
      scale_x_discrete(limits = c("Left Out Of Zone", "Left", "Middle", "Right",
                                  "Right Out Of Zone")) +
      scale_y_discrete(limits = c("Low Out Of Zone", "Low", "Middle", "High",
                                  "High Out Of Zone")) +
      geom_text(aes(label = round(`Average Run Value`, digits = 2))) +
      geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
                fill = "transparent", color = "red", size = 1.5) +
      theme(aspect.ratio = 1.4)
    
    output$k_zone <- renderPlot(k_zone)
    
    #Analysis Table Output
    
    Pitches_Seen <- Input_Data |>
      summarise(.by = "pitch_name", `Pitches Seen` = n()) 
    
    Outcomes <- Input_Data |>
      filter(!is.na(events)) |>
      summarise(.by = "pitch_name", `Total Outcomes` = n())
    
    Whiff <- Input_Data |>
      filter(!is.na(whiff)) |>
      summarise(.by = "pitch_name", `Whiff %` = mean(whiff))
    
    Chase <- Input_Data |>
      filter(!is.na(chase)) |>
      summarise(.by = "pitch_name", `Chase %` = mean(chase))
    
    Strike_Looking <- Input_Data |>
      filter(plate_z > 1.65 & plate_z < 3.65 & plate_x > -0.7083333 & 
               plate_x < 0.7083333) |>
      summarise(.by = "pitch_name", `Strike Looking %` = mean(called_strike))
    
    Exit_Velo <- Input_Data |>
      filter(!is.na(launch_speed)) |>
      summarise(.by = "pitch_name", `Median Exit Velo` = median(launch_speed))
    
    Hard_Hit <- Input_Data |>
      filter(!is.na(hard_hit)) |>
      summarise(.by = "pitch_name", `Hard Hit %` = mean(hard_hit)) 
    
    Line_Drive <- Input_Data |>
      filter(!is.na(events)) |>
      summarise(.by = "pitch_name", `Line Drive %` = mean(line_drive))
    
    Ground_Ball <- Input_Data |>
      filter(!is.na(events)) |>
      summarise(.by = "pitch_name", `Ground Ball %` = mean(ground_ball))
    
    Fly_Ball <- Input_Data |>
      filter(!is.na(events)) |>
      summarise(.by = "pitch_name", `Fly Ball %` = mean(fly_ball))
    
    Most_Common_Outcome <- Input_Data |>
      filter(!is.na(events)) |>
      summarise(.by = "pitch_name", `Most Common Outcome` = 
                  tail(names(sort(table(bb_type))), 1))
    
    Run_Value <- Input_Data |>
      summarise(.by = "pitch_name", `Average Run Value` = mean(delta_run_exp))
    
    Table <- Pitches_Seen |>
      left_join(Outcomes, by = "pitch_name") |>
      left_join(Whiff, by = "pitch_name") |>
      left_join(Chase, by = "pitch_name") |>
      left_join(Strike_Looking, by = "pitch_name") |>
      left_join(Exit_Velo, by = "pitch_name") |>
      left_join(Hard_Hit, by = "pitch_name") |>
      left_join(Ground_Ball, by = "pitch_name") |>
      left_join(Line_Drive, by = "pitch_name") |>
      left_join(Fly_Ball, by = "pitch_name") |>
      left_join(Most_Common_Outcome, by = "pitch_name") |>
      left_join(Run_Value, by = "pitch_name") |>
      arrange(desc(`Average Run Value`)) |>
      rename(`Pitch Type` = "pitch_name") |>
      mutate_if(is.numeric, round, digits = 2)
    
    output$data_table <- renderDataTable(Table)
  })
}