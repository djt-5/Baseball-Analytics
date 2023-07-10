library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(lattice)
library(caret)

Pitch_Names <- c("Changeup", "Curveball", "Sinker", "Four-Seam", "Slider",
                 "Cutter", "Splitter")

Pitcher_Names <- c("Alex Vera", "Andrew Pogue", "Ben Plumley", "Brady Banker", 
                   "Calvin Shepherd", "Cole Kirschsieper", "Crayton Burnett", 
                   "Gabe Helder", "Gavin Schmitt", "Jack Crowder", "Jack Wenninger", 
                   "Jake Swartz", "Joe Glassey",  "John Lundgren", "Joseph Glassey", 
                    "Julius Sanchez", "Korey Bunselmeyer", "Logan Tabeling", 
                   "O'Ryan Hara", "Payton Hutchings", "Riley Gowens", "Ryan Kutt", 
                   "TJ Constertina", "Tommy Green", "Tommy White", "Ty Rybarczyk")


Model_Names <- sort(c("Whiff", "Chase", "Bearing", "release_speed", "VertRelAngle", 
                      "HorzRelAngle", "release_spin_rate",
                      "spin_axis", "Tilt", "release_pos_z", "release_pos_x", 
                      "release_extension", "VertBreak", "InducedVertBreak", "HorzBreak", 
                      "plate_z", "plate_x", "ZoneSpeed", "VertApprAngle", "HorzApprAngle", 
                      "ZoneTime", "launch_speed", "launch_angle", "Direction", "HitSpinRate", 
                      "PositionAt110X", "PositionAt110Y", "PositionAt110Z",
                      "hit_distance_sc", "pfx_x", "pfx_z", "x0", "y0", "z0", "vx0", 
                      "vy0", "vz0", "ax", "ay", "az", "effective_speed", "SpeedDrop", 
                      "HitSpinAxis", "PitchTrajectoryXc0", "PitchTrajectoryXc1", 
                      "PitchTrajectoryXc2", "PitchTrajectoryYc0", "PitchTrajectoryYc1", 
                      "PitchTrajectoryYc2", "PitchTrajectoryZc0", "PitchTrajectoryZc1", 
                      "PitchTrajectoryZc2"))

ui <- dashboardPage(skin = "yellow", 
    dashboardHeader(title = "UIUC Baseball Scouting Reports"),
    dashboardSidebar(
      selectInput("pitcher_select", "Pitcher's Name", 
                  choices = Pitcher_Names),
      selectInput("pitch_select", "Pitch Type",
                  choices = c("All", Pitch_Names)),
      selectInput("batter_side", "Batter Side",
                  choices = c("Both", "Left", "Right")),
      sidebarMenu(
        menuItem("Strike Zone", tabName = "k_zone", icon = icon("th")),
        menuItem("Spray Chart", tabName = "spray", icon = icon("arrows-alt")),
        menuItem("Pitch Metrics", tabName = "pitch", icon = icon("baseball")),
        menuItem("Game Timeline", tabName = "timeline", icon = icon("clock")),
        menuItem("Summary Table", tabName = "table", icon = icon("table")),
        menuItem("Regression Model", tabName = "model", icon = icon("computer"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "k_zone",
          tabBox(
            title = "Strike Zone",
            id = "kzone_box", height = "800px",
            tabPanel("Frequency", plotOutput('pitch_kzone_freq')),
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
            tabPanel("Taken Pitch %", plotOutput('pitch_kzone_taken')),
            tabPanel("Pull %", plotOutput('pitch_kzone_pull'))
          ),
          box(
            title = "Adjust",
            id = "kzone_input", height = "500px",
            sliderInput('kzone_velo_slider', "Pitch Velo", min = 50, max = 100, 
                        value = c(50,100)),
            sliderInput('kzone_spin_slider', "Spin Rate", min = 500, max = 3000, 
                        value = c(500,3000)),
            sliderInput('kzone_break_x', "Horizontal Break", min = -40, max = 40, 
                        value = c(-40,40)),
            sliderInput('kzone_break_y', "Vertical Break", min = -40, max = 0, 
                        value = c(-40,0))
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
          tabPanel("Release Points", plotOutput('pitch_release')),
          tabPanel("Movement", plotOutput('pitch_pfx')),
          tabPanel("Break", plotOutput('pitch_break')),
          tabPanel("Spin", plotOutput('pitch_spin')),
          tabPanel("Velo", plotOutput('pitch_velo'))
        )
      ),
      tabItem(tabName = "timeline",
        tabBox(
          title = "Game Timeline by Pitch",
          id = "timeline_box", height = "800px", width = "1000px",
          tabPanel("Arm Height", plotOutput('pitch_timeline_arm_height')),
          tabPanel("Pitch Velocity", plotOutput('pitch_timeline_velo')),
          tabPanel("Strike %", plotOutput('pitch_timeline_strike')),
          tabPanel("Whiff %", plotOutput('pitch_timeline_whiff')),
          tabPanel("Chase %", plotOutput('pitch_timeline_chase')),
          tabPanel("Hard Hit %", plotOutput('pitch_timeline_hard_hit')),
          tabPanel("Ground Ball %", plotOutput('pitch_timeline_groundball')),
          tabPanel("Line Drive %", plotOutput('pitch_timeline_linedrive')),
          tabPanel("Fly Ball %", plotOutput('pitch_timeline_flyball')),
          tabPanel("Exit Velo", plotOutput('pitch_timeline_exit_velo')),
          tabPanel("Launch Angle", plotOutput('pitch_timeline_angle')),
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
      tabItem(tabName = "model",
        fluidRow(
          box(
          selectInput("SelectX", label = "Select variables:",
                      choices = Model_Names,
                      multiple = TRUE
          ),
          solidHeader = TRUE,
          width = "3",
          status = "primary",
          title = "X variable"
        ),
          box(
          selectInput("SelectY", label = "Select variable to predict:", 
                      choices = Model_Names),
          solidHeader = TRUE,
          width = "3",
          status = "primary",
          title = "Y variable"
        ),
          box(height = "250px",
            selectInput("model_pitch", label = "Pitch Type",
                        choices = c("All", Pitch_Names)),
            selectInput("model_stand", label = "Batter Side", 
                        choices = c("Both", "Left", "Right")),
            selectInput("model_p_throws", label = "Pitcher Throws", 
                        choices = c("Both", "Left", "Right"))
        )),
        fluidRow(
          box(title = "Linear Regression Model (With Data for ALL Pitchers)",
              width = "700px",
              verbatimTextOutput("Model"))
        )
      )
    )
  )
)


