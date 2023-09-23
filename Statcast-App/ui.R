{
  library(tidyverse)
  library(shiny)
  library(shinydashboard)
  library(collapse)
  library(DT)
  
  
  Pitcher_Data_Cubs <- read_csv("Pitcher_Data_Cubs.csv")
  
  lm_variables <- c("release_speed", "release_pos_x", "release_pos_z", 
                    "release_pos_y", "pfx_x", "pfx_z", "plate_x", "plate_z", 
                    "vx0", "vy0", "vz0", "ax", "ay", "az", "effective_speed", 
                    "release_spin_rate", "release_extension", "spin_axis", 
                    "delta_run_exp", "pitches_thrown", "spin_eff", "yR", "tR", 
                    "vxR", "vyR", "vzR", "dv0", "tf", "vxbar", "vybar", "vzbar",
                    "vbar", "adrag", "Cd", "amagx", "amagy", "amagz", "amag", "Mx",
                    "Mz", "Cl", "S", "spinT", "spinTX", "spinTY", "spinTZ",
                    "spin_eff", "phi_dummy", "phi", "theta", "count")
  
  glm_variables <- c("Swing", "Strike", "Hard_Hit", "GroundBall", "LineDrive",
                     "FlyBall", "FoulBall", "Chase", "Whiff", "Taken", "pull")
  
  ui <- dashboardPage(skin = "blue", 
    dashboardHeader(title = "Cubs Pitch Dashboard"),
    dashboardSidebar(
      selectInput("pitcher_select", "Pitcher's Name", 
                  choices = unique(Pitcher_Data_Cubs$pitcher_name)),
      selectInput("pitch_select", "Pitch Type",
                  choices = c("All", unique(Pitcher_Data_Cubs$pitch_name))),
      selectInput("batter_side", "Batter Side",
                  choices = c("Both", "L", "R")),
      selectInput("balls", "Balls", choices = c("All", 0:3)),
      selectInput("strikes", "Strikes", choices = c("All", 0:2)),
      sidebarMenu(
        menuItem("Strike Zone", tabName = "k_zone", icon = icon("th")),
        menuItem("Spray Chart", tabName = "spray", icon = icon("arrows-alt")),
        menuItem("Pitch Metrics", tabName = "pitch", icon = icon("baseball")),
        menuItem("Pitch Timeline", tabName = "timeline", icon = icon("clock")),
        menuItem("Pitch Sequencing", tabName = "seq", icon = icon("arrow-right")),
        menuItem("Summary Table", tabName = "table", icon = icon("table")),
        menuItem("Regression Models", tabName = "models", icon = icon("computer"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "k_zone",
                tabBox(
                  title = "Strike Zone",
                  id = "kzone_box", height = "850px",
                  tabPanel("Frequency", plotOutput('pitch_kzone_freq')), 
                  tabPanel("BIP", plotOutput('pitch_kzone_bip')), 
                  tabPanel("Run Value", plotOutput('pitch_kzone_re24')),
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
                  id = "kzone_input", height = "600px",
                  sliderInput('kzone_velo_slider', "Pitch Velo", min = 60, max = 105, 
                              value = c(60,105)),
                  sliderInput('kzone_spin_slider', "Spin Rate", min = 350, max = 3600, 
                              value = c(350,3600)),
                  sliderInput('kzone_spin_eff_slider', "Spin Efficiency", min = 0, max = 1,
                              value = c(0,1)),
                  sliderInput('kzone_break_x', "Horizontal Movement", min = -2, max = 2, 
                              value = c(-2,2)),
                  sliderInput('kzone_break_y', "Vertical Movement", min = -2, max = 2, 
                              value = c(-2,2))
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
                               choices = c("All","ground_ball", "line_drive", "fly_ball",
                                           "popup"))
                )
        ),
        tabItem(tabName = "pitch",
                tabBox(
                  title = "Pitch Metrics",
                  id = "release_graph", height = "800px", width = "1000px",
                  tabPanel("Release Point (Catcher's View)", plotOutput('pitch_release')),
                  tabPanel("Release Point (Side View)", plotOutput('pitch_release_side')),
                  tabPanel("Short Term Acceleration", plotOutput('pitch_accel')),
                  tabPanel("Spin", plotOutput('pitch_spin')),
                  tabPanel("Velo", plotOutput('pitch_velo')),
                  tabPanel("Break", plotOutput('pitch_break'))
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
                  tabPanel("Slugging %", plotOutput('pitch_timeline_slg')),
                  tabPanel("Spin Efficiency", plotOutput('pitch_timeline_eff')),
                  tabPanel("Run Value", plotOutput('pitch_timeline_re24'))
                )
        ),
        tabItem(tabName = "table",
                box(
                  style='width:1200px;overflow-x: scroll;height:800px;overflow-y: scroll;',
                  width = "1250px", height = "800px",
                  title = "Summary Table",
                  id = "sum_table",
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
}
