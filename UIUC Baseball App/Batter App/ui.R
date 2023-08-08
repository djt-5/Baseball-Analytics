library(tidyverse)
library(shiny)
library(shinydashboard)
library(collapse)

Batter_Data <- read_csv("Batter_Data.csv")
lm_variables <- colnames(Batter_Data)[c(29:33,35:46,58:68,79,189)]
glm_variables <- colnames(Batter_Data)[c(168:179)]

ui <- dashboardPage(skin = "yellow", 
    dashboardHeader(title = "UIUC Batting Savant"),
    dashboardSidebar(
      selectInput("batter_select", "Batter's Name", 
                  choices = unique(Batter_Data$batter_name)),
      selectInput("pitch_select", "Pitch Type",
                  choices = c("All", unique(Batter_Data$pitch_name))),
      selectInput("p_throws", "Pitcher Throws",
                  choices = c("Both", "Left", "Right")),
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
            tabPanel("Frequency", plotOutput('hit_kzone_freq')),
            tabPanel("On Base %", plotOutput('hit_kzone_obp')),
            tabPanel("Slugging %", plotOutput('hit_kzone_slg')),
            tabPanel("Exit Velo", plotOutput('hit_kzone_exit_velo')),
            tabPanel("Launch Angle", plotOutput('hit_kzone_angle')),
            tabPanel("Hard Hit %", plotOutput('hit_kzone_hard_hit')),
            tabPanel("Ground Ball %", plotOutput('hit_kzone_groundball')),
            tabPanel("Line Drive %", plotOutput('hit_kzone_linedrive')),
            tabPanel("Fly Ball %", plotOutput('hit_kzone_flyball')),
            tabPanel("Whiff %", plotOutput('hit_kzone_whiff')),
            tabPanel("Chase %", plotOutput('hit_kzone_chase')),
            tabPanel("Foul Ball %", plotOutput('hit_kzone_foul')),
            tabPanel("Taken Pitch %", plotOutput('hit_kzone_taken')),
            tabPanel("Pull %", plotOutput('hit_kzone_pull'))
          ),
          box(
            title = "Adjust",
            id = "kzone_input", height = "560px",
            sliderInput('kzone_velo_slider', "Pitch Velo", min = 60, max = 100, 
                        value = c(60,100)),
            sliderInput('kzone_rel_x', "Release Side", min = -55, max = 45, 
                        value = c(-55,45)),
            sliderInput('kzone_rel_z', "Release Height", min = 40, max = 95, 
                        value = c(40,95)),
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
          plotOutput('hit_spray')
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
                title = "Guess Help",
                id = "guess_help", height = "800px", width = "900px",
                plotOutput("seq_guess")
              )
      ),
      tabItem(tabName = "timeline",
        tabBox(
          title = "Game Timeline by Plate Appearance",
          id = "timeline_box", height = "800px", width = "1000px",
          tabPanel("Whiff %", plotOutput('hit_timeline_whiff')),
          tabPanel("Chase %", plotOutput('hit_timeline_chase')),
          tabPanel("Foul Ball %", plotOutput('hit_timeline_foulball')),
          tabPanel("Hard Hit %", plotOutput('hit_timeline_hard_hit')),
          tabPanel("Ground Ball %", plotOutput('hit_timeline_groundball')),
          tabPanel("Line Drive %", plotOutput('hit_timeline_linedrive')),
          tabPanel("Fly Ball %", plotOutput('hit_timeline_flyball')),
          tabPanel("Exit Velo", plotOutput('hit_timeline_exit_velo')),
          tabPanel("Launch Angle", plotOutput('hit_timeline_angle')),
          tabPanel("On Base %", plotOutput('hit_timeline_obp')),
          tabPanel("Slugging %", plotOutput('hit_timeline_slg'))
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
          tabPanel("Generalized Linear Model", 
                   selectInput("SelectY_GLM", label = "Select variable to predict:", 
                               choices = glm_variables),
                   selectInput("SelectX_GLM", label = "Select variables:",
                               choices = lm_variables,
                               multiple = TRUE
                   ),
                   verbatimTextOutput('GLM')),
          tabPanel("1:1 Linear Model", 
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


