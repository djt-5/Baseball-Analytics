{
  library(TTR)
  library(collapse)
  library(DT)
  library(data.table)
  library(ggpubr)
  library(sportyR)
  library(baseballr)
  library(lubridate)
  library(tidyverse)
  library(plotly)
  library(shiny)
  library(shinythemes)
  
  Pitchers <- read_csv("Pitchers.csv")
  Batters <- read_csv("Batters.csv")
  
  PiratesPitchers <- Pitchers |> 
    filter(pitcher_team == "PIT") |>
    filter(!is.na(Status)) 
  
  PiratesBatters <- Batters |> 
    filter(batter_team == "PIT") |>
    filter(!is.na(Status))
  
  ui <- navbarPage("Pirates Spring Training", theme = shinytheme("sandstone"),
                   tabsetPanel(
                     tabPanel("Pitchers",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("pitcher_status", label = "Roster Status",
                                              choices = levels(as.factor(PiratesPitchers$Status))),
                                  selectInput("pitcher_name", label = "Select Pitcher",
                                              choices = levels(as.factor(PiratesPitchers$pitcher_name))),
                                  checkboxGroupInput("pitch_name_pitcher", label = "Pitch Type", 
                                                     choices = levels(as.factor(PiratesPitchers$pitch_name))),
                                  checkboxGroupInput("stand", label = "Batter Side",
                                                     choices = c("L","R"), selected = c("L","R")),
                                  checkboxGroupInput("balls_pitcher", label = "Balls", 
                                                     choices = c(0:3), selected = 0:3),
                                  checkboxGroupInput("strikes_pitcher", label = "Strikes", 
                                                     choices = c(0:2), selected = 0:2),
                                  dateRangeInput("game_date_pitcher", label = "Select Date Range",
                                                 start = min(PiratesPitchers$game_date),
                                                 end = max(PiratesPitchers$game_date),
                                                 min = min(PiratesPitchers$game_date),
                                                 max = max(PiratesPitchers$game_date),
                                                 format = "yyyy-mm-dd",
                                                 separator = "to"),
                                  width = 3
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Summary",
                                             plotOutput('pitcher_percentiles'),
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
                                               column(width = 6, plotlyOutput('kzone_xba_pitcher'))
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
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_ev_pitcher')),
                                               column(width = 6, plotlyOutput('kzone_la_pitcher'))
                                             )),
                                    tabPanel("Time Table",
                                             dataTableOutput('pitcher_time_table')
                                      )
                                    )
                                  )
                                )
                              ),
                     tabPanel("Batters",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("batter_status", label = "Roster Status",
                                              choices = levels(as.factor(PiratesBatters$Status))),
                                  selectInput("batter_name", label = "Select Batter",
                                              choices = levels(as.factor(PiratesBatters$batter_name))),
                                  checkboxGroupInput("pitch_name_batter", label = "Pitch Type", 
                                                     choices = levels(as.factor(PiratesBatters$pitch_name))),
                                  checkboxGroupInput("p_throws", label = "Pitcher Throws",
                                                     choices = c("L","R"), selected = c("L","R")),
                                  checkboxGroupInput("balls_batter", label = "Balls", 
                                                     choices = c(0:3), selected = 0:3),
                                  checkboxGroupInput("strikes_batter", label = "Strikes", 
                                                     choices = c(0:2), selected = 0:2),
                                  dateRangeInput("game_date_batter", label = "Select Date Range",
                                                 start = min(PiratesBatters$game_date),
                                                 end = max(PiratesBatters$game_date),
                                                 min = min(PiratesBatters$game_date),
                                                 max = max(PiratesBatters$game_date),
                                                 format = "yyyy-mm-dd",
                                                 separator = "to"),
                                  width = 3
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel("Summary",
                                             plotOutput('batter_percentiles'),
                                             dataTableOutput('batter_summary_table')),
                                    tabPanel("Metrics",
                                             plotlyOutput('spray_chart'),
                                             plotlyOutput('VAA_swingangle')),
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
                                               column(width = 6, plotlyOutput('kzone_xba_batter'))
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
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_ev_batter')),
                                               column(width = 6, plotlyOutput('kzone_la_batter'))
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             fluidRow(
                                               column(width = 6, plotlyOutput('kzone_vaa_batter')),
                                               column(width = 6, plotlyOutput('kzone_sa_batter'))
                                               )),
                                    tabPanel("Time Table",
                                             dataTableOutput('batter_time_table')
                                    )
            )
          )
        )
      )
    )
  )
}
