library(tidyverse)
library(shiny)
library(shinydashboard)
library(collapse)


Pitcher_Data <- read_csv("Pitcher_Data.csv")

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

ui <- dashboardPage(skin = "yellow", 
    dashboardHeader(title = "UIUC Pitching Savant"),
    dashboardSidebar(
      selectInput("pitcher_select", "Pitcher's Name", 
                  choices = unique(Pitcher_Data$pitcher_name)),
      selectInput("pitch_select", "Pitch Type",
                  choices = c("All", unique(Pitcher_Data$pitch_name))),
      selectInput("batter_side", "Batter Side",
                  choices = c("Both", "Left", "Right")),
      selectInput("balls", "Balls", choices = c("All", 0:3)),
      selectInput("strikes", "Strikes", choices = c("All", 0:2)),
      sidebarMenu(
        menuItem("Summary Table", tabName = "table", icon = icon("table")),
        menuItem("Strike Zone", tabName = "k_zone", icon = icon("th")),
        menuItem("Spray Chart", tabName = "spray", icon = icon("arrows-alt")),
        menuItem("Pitch Metrics", tabName = "pitch", icon = icon("baseball")),
        menuItem("Pitch Sequencing", tabName = "seq", icon = icon("arrow-right")),
        menuItem("Game Timeline", tabName = "timeline", icon = icon("clock")),
        menuItem("Regression Models", tabName = "models", icon = icon("computer"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "k_zone",
          tabBox(
            title = "Strike Zone",
            id = "kzone_box", height = "800px",
            tabPanel("Frequency", plotOutput('pitch_kzone_freq')),
            tabPanel("BIP", plotOutput('pitch_kzone_bip')),
            tabPanel("On Base %", plotOutput('pitch_kzone_obp')),
            tabPanel("Slugging %", plotOutput('pitch_kzone_slg')),
            tabPanel("Exit Velo", plotOutput('pitch_kzone_exit_velo')),
            tabPanel("Launch Angle", plotOutput('pitch_kzone_angle')),
            tabPanel("Hard Hit %", plotOutput('pitch_kzone_hard_hit')),
            tabPanel("Ground Ball %", plotOutput('pitch_kzone_groundball')),
            tabPanel("Line Drive %", plotOutput('pitch_kzone_linedrive')),
            tabPanel("Fly Ball %", plotOutput('pitch_kzone_flyball')),
            tabPanel("Whiff %", plotOutput('pitch_kzone_whiff')),
            tabPanel("Chase %", plotOutput('pitch_kzone_chase')),
            tabPanel("Foul Ball %", plotOutput('pitch_kzone_foul')),
            tabPanel("Taken Pitch %", plotOutput('pitch_kzone_taken')),
            tabPanel("Pull %", plotOutput('pitch_kzone_pull'))
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
          plotOutput('pitch_spray')
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
      tabItem(tabName = "pitch",
        tabBox(
          title = "Pitch Metrics",
          id = "release_graph", height = "800px", width = "1000px",
          tabPanel("Release Point", plotOutput('pitch_release')),
          tabPanel("Release Angle", plotOutput('pitch_angle')),
          tabPanel("Approach Angle", plotOutput('pitch_appr')),
          tabPanel("Pitch Tunnel", plotOutput('pitch_tunnel')),
          tabPanel("Movement", plotOutput('pitch_pfx')),
          tabPanel("Break", plotOutput('pitch_break')),
          tabPanel("Spin", plotOutput('pitch_spin')),
          tabPanel("Velo", plotOutput('pitch_velo'))
        )
      ),
      tabItem(tabName = "seq",
              tabBox(
                title = "Pitch Sequencing",
                id = "pitch_seq", height = "800px", width = "1000px",
                tabPanel("Frequency", plotOutput('seq_freq')),
                tabPanel("Whiff", plotOutput('seq_whiff')),
                tabPanel("Chase", plotOutput('seq_chase')),
                tabPanel("Foul Ball", plotOutput('seq_foul')),
                tabPanel("Ground Ball", plotOutput('seq_groundball')),
                tabPanel("Fly Ball", plotOutput('seq_flyball')),
                tabPanel("Line Drive", plotOutput('seq_linedrive')),
                tabPanel("Hard Hit", plotOutput('seq_hardhit'))
              )
      ),
      tabItem(tabName = "timeline",
        tabBox(
          title = "Game Timeline by Pitch",
          id = "timeline_box", height = "800px", width = "1000px",
          tabPanel("Arm Height", plotOutput('pitch_timeline_arm_height')),
          tabPanel("Pitch Velocity", plotOutput('pitch_timeline_velo')),
          tabPanel("Exit Velo", plotOutput('pitch_timeline_exit_velo')),
          tabPanel("Launch Angle", plotOutput('pitch_timeline_angle')),
          tabPanel("Strikes", plotOutput('pitch_timeline_strike')),
          tabPanel("Whiffs", plotOutput('pitch_timeline_whiff')),
          tabPanel("Chases", plotOutput('pitch_timeline_chase')),
          tabPanel("Foul Balls", plotOutput('pitch_timeline_foulball')),
          tabPanel("Hard Hits", plotOutput('pitch_timeline_hard_hit')),
          tabPanel("Ground Balls", plotOutput('pitch_timeline_groundball')),
          tabPanel("Line Drives", plotOutput('pitch_timeline_linedrive')),
          tabPanel("Fly Balls", plotOutput('pitch_timeline_flyball')),
          tabPanel("On Base %", plotOutput('pitch_timeline_obp')),
          tabPanel("Slugging %", plotOutput('pitch_timeline_slg'))
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


