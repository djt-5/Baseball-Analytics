{
  library(tidyverse)
  library(shiny)
  library(shinydashboard)
  library(collapse)
  library(DT)
  library(hexbin)
  
  
  Data <- read_csv("Data.csv")
  
  ui <- dashboardPage(skin = "blue", 
    dashboardHeader(title = "Pitch Design"),
    dashboardSidebar(
      selectInput("pitcher_select", "Pitcher's Name", 
                  choices = unique(Data$pitcher_name)),
      selectInput("previous_pitch", "Previous Pitch",
                  choices = c("Any","Four-Seam", "Sinker", "Cutter")),
      selectInput("pitch_name", "Pitch Name",
                  choices = c("Any","Changeup", "Sweeper", "Slider", "Curveball", 
                              "Split-Finger", "Knuckle Curve", "Slurve")),
      selectInput("batter_side", "Batter Side",
                  choices = c("Both", "L", "R")),
      sidebarMenu(
        menuItem("Summary Table", tabName = "table", icon = icon("table")),
        menuItem("Count Data", tabName = "count", icon = icon("chart-simple")),
        menuItem("Performance", tabName = "perform", icon = icon("chart-simple")),
        menuItem("Strike Zone", tabName = "k_zone", icon = icon("th")),
        menuItem("Variable Efficacy", tabName = "var_eff", icon = icon("star")),
        menuItem("Pitch Profile", tabName = "pitch", icon = icon("baseball")),
        menuItem("Data Filters", tabName = "filters", icon = icon("computer"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "table",
                box(
                  style='width:1200px;overflow-x: scroll;height:800px;overflow-y: scroll;',
                  width = "1250px", height = "800px",
                  title = "Summary Table",
                  id = "sum_table",
                  dataTableOutput('table')
                )
        ),
        tabItem(tabName = "count",
                box(
                  title = "Count Matrix", height = "800px", width = "1250px",
                  id = "count_matrix",
                  dataTableOutput('count_matrix')
                )
        ),
        tabItem(tabName = "perform",
                tabBox(
                  title = "Performance",
                  id = "perform", height = "800px", width = "1000px",
                  tabPanel("Whiffs", plotOutput('seq_whiff')),
                  tabPanel("Chases", plotOutput('seq_chase'))
                )
        ),
        tabItem(tabName = "pitch",
                tabBox(
                  title = "Pitch Profile",
                  id = "release_graph", height = "800px", width = "1000px",
                  tabPanel("Release Point (Catcher's View)", plotOutput('pitch_release')),
                  tabPanel("Release Angles", plotOutput('pitch_angles')),
                  tabPanel("Spin", plotOutput('pitch_spin')),
                  tabPanel("Velo", plotOutput('pitch_velo')),
                  tabPanel("Tunnel Points", plotOutput('pitch_tunnel')),
                  tabPanel("Post Tunnel Break", plotOutput('pitch_posttunnelbreak')),
                  tabPanel("Total Break", plotOutput('pitch_break'))
                )
        ),
        tabItem(tabName = "k_zone",
                tabBox(
                  title = "Previous Pitch Strike Zone",
                  id = "kzone_box_1", height = "800px",
                  tabPanel("Whiffs", plotOutput('fastball_whiff')),
                  tabPanel("Chases", plotOutput('fastball_chase'))
                ),
                tabBox(
                  title = "Current Pitch Strike Zone",
                  id = "kzone_box_2", height = "800px",
                  tabPanel("Whiffs", plotOutput('second_whiff')),
                  tabPanel("Chases", plotOutput('second_chase'))
                )
        ),
        tabItem(tabName = "var_eff",
                tabBox(
                  title = "Variable Efficacy",
                  id = "var_eff", height = "700px", width = "1200px",
                  tabPanel("Response", selectInput("response_select", "Response Variable", 
                                                   choices = c("Whiff", "Chase"))),
                  tabPanel("Break", 
                           sliderInput("bins_break", "Number of Bins:", min = 1, max = 30, value = 10),
                           plotOutput('plot_pfx')),
                  tabPanel("Post Tunnel Break", 
                           sliderInput("bins_ptb", "Number of Bins:", min = 1, max = 30, value = 10),
                           plotOutput('plot_posttunnelbreak')),
                  tabPanel("Speed", 
                           sliderInput("bins_speed", "Number of Bins:", min = 1, max = 30, value = 10),
                           plotOutput('plot_release_speed')),
                  tabPanel("Spin", 
                           sliderInput("bins_spin", "Number of Bins:", min = 1, max = 30, value = 10),
                           plotOutput('plot_release_spin')),
                  tabPanel("Spin Efficiency", 
                           sliderInput("bins_spin_eff", "Bin Break:", min = 0.01, max = 0.2, value = 0.1, step = 0.01),
                           plotOutput('plot_spin_eff')),
                  tabPanel("Break to Tunnel Ratio", 
                           sliderInput("bins_bttr", "Bin Break:", min = 0.01, max = 0.1, value = 0.05, step = 0.001),
                           plotOutput('plot_breaktotunnelratio'))
                )
        ),
        tabItem(tabName = "filters",
                box(
                  height = "1000px",
                  id = "filter_sliders",
                  sliderInput('plate_x', "K-Zone Horizontal", min = -4, max = 4, 
                              value = c(-4,4), step = 0.1),
                  sliderInput('plate_z', "K-Zone Vertical", min = -2, max = 5, 
                              value = c(-2,5), step = 0.1),
                  sliderInput('pfx_x', "Horizontal Break", min = -25, max = 25, 
                              value = c(-25,25), step = 1),
                  sliderInput('pfx_z', "Vertical Break", min = -35, max = 25, 
                              value = c(-35,25), step = 1),
                  sliderInput('speed', "Release Speed", min = 60, max = 105, 
                              value = c(60,105)),
                  sliderInput('spin', "Release Spin Rate", min = 500, max = 3600, 
                              value = c(500,3600)),
                  sliderInput('spin_eff', "Spin Efficiency", min = 0, max = 1, 
                              value = c(0,1), step = 0.01),
                  sliderInput('PTB', "Post Tunnel Break", min = 0, max = 12, 
                              value = c(0,12), step = 0.5),
                  sliderInput('date_slider', "Game Date", min = min(Data$game_date), 
                              max = max(Data$game_date), 
                              value = c(min(Data$game_date),max(Data$game_date)))
                  
                ),
                box(
                  height = "1000px",
                  id = "filter_sliders_2",
                  sliderInput('prev_plate_x', "Previous K-Zone Horizontal", min = -4, max = 4, 
                              value = c(-4,4), step = 0.1),
                  sliderInput('prev_plate_z', "Previous K-Zone Vertical", min = -2, max = 5, 
                              value = c(-2,5), step = 0.1),
                  sliderInput('diffspeed', "Speed Difference", min = -35, max = 0, 
                              value = c(-35,0)),
                  sliderInput('breaktotunnelratio', "Break to Tunnel Ratio", min = 0, max = 15,
                              value = c(0,15), step = 0.1),
                  sliderInput('balls', "Balls", min = 0, max = 3, 
                              value = c(0,3)),
                  sliderInput('strikes', "Strikes", min = 0, max = 2, 
                              value = c(0,2))
                )
        )
      )
    )
  )
}
