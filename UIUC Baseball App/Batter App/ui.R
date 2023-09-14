library(tidyverse)
library(shiny)
library(shinydashboard)
library(collapse)


Batter_Data <- read_csv("Batter_Data.csv")

lm_variables <- c("release_speed", "VertRelAngle", "HorzRelAngle", 
                  "release_spin_rate", "spin_axis", "release_pos_z",  
                  "release_pos_x", "release_extension", "VertBreak", 
                  "InducedVertBreak", "HorzBreak", "plate_z", "plate_x", 
                  "ZoneSpeed", "VertApprAngle", "HorzApprAngle", "ZoneTime",
                  "pfx_x", "pfx_z", "x0", "z0", "SpeedDrop", "Pitches_Thrown", 
                  "balls", "strikes")

glm_variables <- c("Swing", "Strike", "Hard_Hit", "GroundBall", "LineDrive",
                   "FlyBall", "FoulBall", "Chase", "Whiff", "Taken", "on_base", 
                   "slugging")

ui <- dashboardPage(skin = "blue", 
    dashboardHeader(title = "UIUC Batting Savant"),
    dashboardSidebar(
      selectInput("batter_select", "Batter's Name", 
                  choices = unique(Batter_Data$batter_name)),
      selectInput("pitch_select", "Pitch Type",
                  choices = c("All", unique(Batter_Data$pitch_name))),
      selectInput("p_throws", "Pitcher Throws",
                  choices = c("Both", "Left", "Right")),
      selectInput("balls", "Balls", choices = c("All", 0:3)),
      selectInput("strikes", "Strikes", choices = c("All", 0:2)),
      selectInput("batter_side", "Batter Side",
                  choices = c("Both", "Left", "Right")),
      sidebarMenu(
        menuItem("Strike Zone", tabName = "k_zone", icon = icon("th")),
        menuItem("Spray Chart", tabName = "spray", icon = icon("arrows-alt")),
        menuItem("Pitch Guessing", tabName = "guess", icon = icon("arrow-right")),
        menuItem("Game Timeline", tabName = "timeline", icon = icon("clock")),
        menuItem("Summary Table", tabName = "table", icon = icon("table")),
        menuItem("Regression Models", tabName = "models", icon = icon("computer"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "k_zone",
          tabBox(
            title = "Strike Zone",
            id = "kzone_box", height = "800px",
            tabPanel("Frequency", plotOutput('bat_kzone_freq')),
            tabPanel("BIP", plotOutput('bat_kzone_bip')),
            tabPanel("On Base %", plotOutput('bat_kzone_obp')),
            tabPanel("Slugging %", plotOutput('bat_kzone_slg')),
            tabPanel("Exit Velo", plotOutput('bat_kzone_exit_velo')),
            tabPanel("Launch Angle", plotOutput('bat_kzone_angle')),
            tabPanel("Hard Hit %", plotOutput('bat_kzone_hard_hit')),
            tabPanel("Ground Ball %", plotOutput('bat_kzone_groundball')),
            tabPanel("Line Drive %", plotOutput('bat_kzone_linedrive')),
            tabPanel("Fly Ball %", plotOutput('bat_kzone_flyball')),
            tabPanel("Whiff %", plotOutput('bat_kzone_whiff')),
            tabPanel("Chase %", plotOutput('bat_kzone_chase')),
            tabPanel("Foul Ball %", plotOutput('bat_kzone_foul')),
            tabPanel("Taken Pitch %", plotOutput('bat_kzone_taken')),
            tabPanel("Pull %", plotOutput('bat_kzone_pull'))
          ),
          box(
            title = "Adjust",
            id = "kzone_input", height = "500px",
            sliderInput('kzone_velo_slider', "Pitch Velo", min = 60, max = 100, 
                        value = c(60,100)),
            sliderInput('kzone_spin_slider', "Spin Rate", min = 940, max = 3000, 
                        value = c(940,3000)),
            sliderInput('kzone_break_x', "Horizontal Break", min = -40, max = 40, 
                        value = c(-40,40)),
            sliderInput('kzone_break_y', "Vertical Break", min = -80, max = 0, 
                        value = c(-80,0))
          )
      ),
      tabItem(tabName = "spray",
        box(
          title = "Spray Chart",
          id = "spray_box", height = "500px",
          plotOutput('bat_spray')
        ),
        box(
          title = "Adjust",
          id = "spray_input", height = "500px",
          sliderInput('spray_exit_velo', "Exit Velocity", min = 0, max = 120, 
                      value = c(0,120)),
          sliderInput('spray_launch_angle', "Launch Angle", min = -90, max = 90, 
                      value = c(-90,90)),
          radioButtons('bb_type', "Batted Ball Type", 
                       choices = c("All","GroundBall", "LineDrive", "FlyBall",
                                   "Popup"))
        )
      ),
      tabItem(tabName = "guess",
              box(
                title = "Pitch Guessing",height = "800px", width = "1000px",
                plotOutput('guess_plot'))
      ),
      tabItem(tabName = "timeline",
        tabBox(
          title = "Game Timeline by Plate Appearance",
          id = "timeline_box", height = "800px", width = "1000px",
          tabPanel("Exit Velo", plotOutput('bat_timeline_exit_velo')),
          tabPanel("Launch Angle", plotOutput('bat_timeline_angle')),
          tabPanel("Whiffs", plotOutput('bat_timeline_whiff')),
          tabPanel("Chases", plotOutput('bat_timeline_chase')),
          tabPanel("Foul Balls", plotOutput('bat_timeline_foulball')),
          tabPanel("Hard Hits", plotOutput('bat_timeline_hard_hit')),
          tabPanel("Ground Balls", plotOutput('bat_timeline_groundball')),
          tabPanel("Line Drives", plotOutput('bat_timeline_linedrive')),
          tabPanel("Fly Balls", plotOutput('bat_timeline_flyball')),
          tabPanel("On Base %", plotOutput('bat_timeline_obp')),
          tabPanel("Slugging %", plotOutput('bat_timeline_slg'))
        )
      ),
      tabItem(tabName = "table",
        box(
          title = "Summary Table",
          id = "sum_table", height = "800px", width = "1000px",
          dataTableOutput('table')
        )
      ),
      tabItem(tabName = "models",
        tabBox(
          title = "Regression Models for Guiding",
          id = "regression_models", height = "800px", width = "1000px",
          tabPanel("Logistic Regression Model", 
                   selectInput("SelectY_GLM", label = "Select variable to predict:", 
                               choices = glm_variables),
                   selectInput("SelectX_GLM", label = "Select variables:",
                               choices = lm_variables,
                               multiple = TRUE
                   ),
                   verbatimTextOutput('GLM')),
          tabPanel("Linear Regression Model", 
                   selectInput("SelectY_LM", label = "Select variable to predict:", 
                               choices = lm_variables),
                   selectInput("SelectX_LM", label = "Select variables:",
                               choices = lm_variables,
                               multiple = TRUE
                   ),
                   verbatimTextOutput('LM'))
        )
      )
    )
  )
)


