{
  library(ggpubr)
  library(DT)
  library(sportyR)
  library(baseballr)
  library(plotly)
  library(shiny)
  library(shinythemes)
  library(tidyverse)
  
  Data <- read_csv("Data.csv")
  
  ui <- navbarPage("Shiny Savant", theme = shinytheme("sandstone"),
                   tabsetPanel(
                     tabPanel("Pitchers",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("pitcher_team", label = "Pitcher Team",
                                              choices = levels(as.factor(Data$pitcher_team))),
                                  selectInput("pitcher", label = "Select Pitcher",
                                              choices = levels(as.factor(Data$pitcher))),
                                  checkboxGroupInput("pitch_name_pitcher", label = "Pitch Type", 
                                                     choices = levels(as.factor(Data$pitch_name))),
                                  checkboxGroupInput("batter_side", label = "Batter Side",
                                                     choices = c("L","R"), selected = c("L","R")),
                                  checkboxGroupInput("balls_pitcher", label = "Balls", 
                                                     choices = c(0:3), selected = 0:3),
                                  checkboxGroupInput("strikes_pitcher", label = "Strikes", 
                                                     choices = c(0:2), selected = 0:2),
                                  width = 3
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Summary",
                                             dataTableOutput('pitcher_summary_table')),
                                    tabPanel("Metrics",
                                             fluidRow(
                                               column(width = 6, plotlyOutput('release_points')),
                                               column(width = 6, plotlyOutput('release_angles'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('tunnel_points')),
                                               column(width = 6, plotlyOutput('pitch_movement'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotOutput('pitch_spin')),
                                               column(width = 6, plotlyOutput('pitch_speed'))
                                             )),
                                    tabPanel("K-Zone",
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_freq_pitcher')),
                                               column(width = 6, plotlyOutput('kzone_speed_pitcher'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_ba_pitcher')),
                                               column(width = 6, plotlyOutput('kzone_slg_pitcher'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_swing_pitcher')),
                                               column(width = 6, plotlyOutput('kzone_whiff_pitcher'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br()),
                                    tabPanel("Variable Efficacy",
                                             tabsetPanel(
                                               tabPanel("Response", selectInput("response_select_pitcher", "Response Variable", 
                                                                                choices = c("Whiff", "Chase"))),
                                               tabPanel("Break", 
                                                        sliderInput("bins_break_pitcher", "Number of Bins:", min = 1, max = 30, value = 10),
                                                        plotOutput('plot_pfx_pitcher')),
                                               tabPanel("Speed", 
                                                        sliderInput("bins_speed_pitcher", "Number of Bins:", min = 1, max = 30, value = 10),
                                                        plotOutput('plot_release_speed_pitcher')),
                                               tabPanel("Spin", 
                                                        sliderInput("bins_spin_pitcher", "Number of Bins:", min = 1, max = 30, value = 10),
                                                        plotOutput('plot_release_spin_pitcher'))
                                             )
                                    ),
                                    tabPanel("Extra Inputs",
                                             sliderInput('slider_plate_x_pitcher', "K-Zone Horizontal", min = -4, max = 4, 
                                                         value = c(-4,4), step = 0.1),
                                             sliderInput('slider_plate_z_pitcher', "K-Zone Vertical", min = -2, max = 5, 
                                                         value = c(-2,5), step = 0.1),
                                             sliderInput('slider_pfx_x_pitcher', "Horizontal Break", min = -25, max = 25, 
                                                         value = c(-25,25), step = 1),
                                             sliderInput('slider_pfx_z_pitcher', "Vertical Break", min = -35, max = 25, 
                                                         value = c(-35,25), step = 1),
                                             sliderInput('slider_speed_pitcher', "Release Speed", min = 60, max = 105, 
                                                         value = c(60,105)),
                                             sliderInput('slider_spin_pitcher', "Release Spin Rate", min = 500, max = 3600, 
                                                         value = c(500,3600)),
                                             sliderInput('slider_spin_eff_pitcher', "Spin Efficiency", min = 0, max = 1, 
                                                         value = c(0,1), step = 0.01)
                                      )
                                    )
                                  )
                                )
                              ),
                     tabPanel("Batters",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("batter_team", label = "Batter Team",
                                              choices = levels(as.factor(Data$batter_team))),
                                  selectInput("batter", label = "Select Batter",
                                              choices = levels(as.factor(Data$batter))),
                                  checkboxGroupInput("pitch_name_batter", label = "Pitch Type", 
                                                     choices = levels(as.factor(Data$pitch_name))),
                                  checkboxGroupInput("p_throws", label = "Pitcher Throws",
                                                     choices = c("L","R"), selected = c("L","R")),
                                  checkboxGroupInput("balls_batter", label = "Balls", 
                                                     choices = c(0:3), selected = 0:3),
                                  checkboxGroupInput("strikes_batter", label = "Strikes", 
                                                     choices = c(0:2), selected = 0:2),
                                  width = 3
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Summary",
                                             dataTableOutput('batter_summary_table')),
                                    tabPanel("K-Zone",
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_freq_batter'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_ba_batter')),
                                               column(width = 6, plotlyOutput('kzone_slg_batter'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_swing_batter')),
                                               column(width = 6, plotlyOutput('kzone_whiff_batter'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br()),
                                    tabPanel("Variable Efficacy",
                                             tabsetPanel(
                                               tabPanel("Response", selectInput("response_select_batter", "Response Variable", 
                                                                                choices = c("Whiff", "Chase"))),
                                               tabPanel("Speed", 
                                                        sliderInput("bins_speed_batter", "Number of Bins:", min = 1, max = 30, value = 10),
                                                        plotOutput('plot_release_speed_batter')),
                                               tabPanel("Break", 
                                                        sliderInput("bins_break_batter", "Number of Bins:", min = 1, max = 30, value = 10),
                                                        plotOutput('plot_pfx_batter'))
                                             )
                                    ),
                                    tabPanel("Extra Inputs",
                                             sliderInput('slider_plate_x_batter', "K-Zone Horizontal", min = -4, max = 4, 
                                                         value = c(-4,4), step = 0.1),
                                             sliderInput('slider_plate_z_batter', "K-Zone Vertical", min = -2, max = 5, 
                                                         value = c(-2,5), step = 0.1),
                                             sliderInput('slider_pfx_x_batter', "Horizontal Break", min = -25, max = 25, 
                                                         value = c(-25,25), step = 1),
                                             sliderInput('slider_pfx_z_batter', "Vertical Break", min = -35, max = 25, 
                                                         value = c(-35,25), step = 1),
                                             sliderInput('slider_speed_batter', "Release Speed", min = 60, max = 105, 
                                                         value = c(60,105))
                                    )
            )
          )
        )
      )
    )
  )
}
