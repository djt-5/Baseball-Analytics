{
  library(rsample)
  library(klaR)
  library(e1071) 
  library(naivebayes)
  library(caret)
  library(shiny)
  library(shinythemes)
  library(DT)
  library(tidyverse)
  
  Data <- read_csv("Baserunning-Data.csv")
  
  ui <- navbarPage("Shiny Savant", theme = shinytheme("sandstone"),
                              sidebarLayout(
                                sidebarPanel(
                                  sliderInput("sprint_speed", "Sprint Speed", min = 22, max = 32, value = c(22,32), step = 0.1),
                                  sliderInput("arm_strength", "Catcher Arm Strength", min = 65, max = 90, value = c(65,90), step = 0.1),
                                  checkboxGroupInput("swing", label = "Batter Swing", 
                                                     choices = 0:1),
                                  sliderInput("plate_z", "Vertical Pitch Location", min = -2, max = 6, value = c(-2,6), step = 0.1),
                                  sliderInput("plate_x", "Horizontal Pitch Location", min = -4, max = 4, value = c(-4,4), step = 0.1),
                                  sliderInput("pop_time", "Catcher Pop Time", min = 1.8, max = 2.3, value = c(1.8,2.3), step = 0.01),
                                  sliderInput("release_speed", "Pitch Speed", min = 65, max = 102, value = c(65,102), step = 0.1),
                                  sliderInput("lead", "Baserunner Lead", min = 9, max = 15, value = c(9,15), step = 0.05),
                                  width = 3
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Sprint Speed",
                                             plotOutput('sprint_speed_barplot')
                                             ),
                                    tabPanel("Catcher Arm",
                                             plotOutput('arm_strength_barplot')
                                             ),
                                    tabPanel("Strike Zone",
                                             plotOutput('kzone_plot', width = 400)
                                    ),
                                    tabPanel("Catcher Pop Time",
                                             plotOutput('pop_time_barplot')
                                             ),
                                    tabPanel("Release Speed",
                                             plotOutput('release_speed_barplot')
                                    ),
                                    tabPanel("Runner Lead",
                                             plotOutput('lead_barplot')
                                    ),
                                    tabPanel("Data Table",
                                             DTOutput('data_table')
                                    ),
                                    tabPanel("Bayesian Predictor",
                                             sliderInput('bayes_sprint', "Sprint Speed", min = 22, max = 32, value = 22, step = 0.1),
                                             sliderInput('bayes_arm', "Arm Strength", min = 65, max = 90, value = 65, step = 0.1),
                                             sliderInput('bayes_plate_z', "Vertical Location", min = -2, max = 6, value = -2, step = 0.05),
                                             sliderInput('bayes_plate_x', "Horizontal Location", min = -4, max = 4, value = -4, step = 0.05),
                                             sliderInput('bayes_pop', "Pop Time", min = 1.8, max = 2.3, value = 1.8, step = 0.01),
                                             sliderInput('bayes_velocity', "Pitch Velo", min = 65, max = 102, value = 65, step = 0.1),
                                             sliderInput('bayes_lead', "Runner Lead", min = 9, max = 15, value = 9, step = 0.1),
                                             sliderInput('bayes_swing', "Swing 1/0", min = 0, max = 1, value = 0, step = 1),
                                             actionButton(inputId = "action", label = "Check the Result!"),
                                             verbatimTextOutput('bayes_pred')
                                    )
                                    )
                                  )
                                )
                     
  )
}
