{
  library(tidyverse)
  library(shiny)
  library(shinydashboard)
  library(collapse)
  library(DT)
  
  
  Data <- read_csv("Data.csv")
  
  ui <- dashboardPage(skin = "blue", 
    dashboardHeader(title = "Tunnel Dashboard"),
    dashboardSidebar(
      selectInput("pitcher_select", "Pitcher's Name", 
                  choices = unique(Data$pitcher_name)),
      selectInput("fastball_select", "Fastball",
                  choices = c("Four-Seam", "Sinker", "Cutter")),
      selectInput("secondary_select", "Secondary Pitch",
                  choices = c("Changeup", "Sweeper", "Slider", "Curveball", "Split-Finger",
                              "Knuckle Curve", "Slurve")),
      selectInput("batter_side", "Batter Side",
                  choices = c("Both", "L", "R")),
      sidebarMenu(
        menuItem("Strike Zone", tabName = "k_zone", icon = icon("th")),
        menuItem("Pitch Metrics", tabName = "pitch", icon = icon("baseball")),
        menuItem("Performance", tabName = "perform", icon = icon("chart-simple")),
        menuItem("Variable Efficacy", tabName = "var_eff", icon = icon("star")),
        menuItem("Regressions", tabName = "regress", icon = icon("clock")),
        menuItem("Summary Table", tabName = "table", icon = icon("table")),
        menuItem("Data Filters", tabName = "filters", icon = icon("computer"))
      ),
      sliderInput('date_slider', "Timeline", min = min(Data$game_date), max = max(Data$game_date), 
                  value = c(min(Data$game_date),max(Data$game_date)))
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "k_zone",
                tabBox(
                  title = "Strike Zone",
                  id = "kzone_box", height = "850px", 
                  tabPanel("Whiffs", plotOutput('kzone_whiff')),
                  tabPanel("Chases", plotOutput('kzone_chase')),
                  tabPanel("Ground Balls", plotOutput('kzone_groundball')),
                )
        ),
        tabItem(tabName = "pitch",
                tabBox(
                  title = "Pitch Metrics",
                  id = "release_graph", height = "800px", width = "1000px",
                  tabPanel("Release Point (Catcher's View)", plotOutput('pitch_release')),
                  tabPanel("Release Point (Side View)", plotOutput('pitch_release_side')),
                  tabPanel("Release Angles", plotOutput('pitch_angles')),
                  tabPanel("Spin", plotOutput('pitch_spin')),
                  tabPanel("Velo", plotOutput('pitch_velo')),
                  tabPanel("Tunnel Points", plotOutput('pitch_tunnel')),
                  tabPanel("Post Tunnel Break", plotOutput('pitch_posttunnelbreak')),
                  tabPanel("Total Break", plotOutput('pitch_break'))
                )
        ),
        tabItem(tabName = "perform",
                tabBox(
                  title = "Performance",
                  id = "perform", height = "800px", width = "1000px",
                  tabPanel("Whiffs", plotOutput('seq_whiff')),
                  tabPanel("Chases", plotOutput('seq_chase')),
                  tabPanel("Ground Balls", plotOutput('seq_groundball'))
                )
        ),
        tabItem(tabName = "var_eff",
                tabBox(
                  title = "Variable Efficacy",
                  id = "var_eff", height = "800px", width = "1000px",
                  tabPanel("Response", selectInput("response_select", "Response Variable", 
                                                   choices = c("Whiff", "Chase", "GroundBall"))),
                  tabPanel("Horz Break", plotOutput('plot_pfx_x')),
                  tabPanel("Vert Break", plotOutput('plot_pfx_z')),
                  tabPanel("Tunnel Difference", plotOutput('plot_diffattunnel')),
                  tabPanel("Speed", plotOutput('plot_release_speed')),
                  tabPanel("Speed Diff", plotOutput('plot_speeddiff')),
                  tabPanel("Spin", plotOutput('plot_release_spin_rate')),
                  tabPanel("Release Difference", plotOutput('plot_diffatrelease')),
                  tabPanel("Plate Difference", plotOutput('plot_diffatplate')),
                  tabPanel("Post Tunnel Break", plotOutput('plot_posttunnelbreak')),
                  tabPanel("Break to Tunnel Ratio", plotOutput('plot_breaktotunnelratio')),
                  tabPanel("Release to Tunnel Ratio", plotOutput('plot_releasetotunnelratio'))
                )
        ),
        tabItem(tabName = "regress",
                box(
                  title = "Regression Plots",
                  id = "regression_box",
                  plotOutput('regression_plot')
                ),
                box(
                  title = "Inputs",
                  id = "regression_inputs",
                  selectInput("x_axis", "X Axis", choices = c("release_speed",
                    "pfx_x", "pfx_z","release_spin_rate", "spin_axis", "delta_run_exp",
                    "release_angle", "release_direction", "posttunnelbreak", "pitches_thrown",
                    "spin_eff", "diffattunnel", "diffatplate", "speeddiff", "diffatrelease",
                    "breaktotunnelratio", "releasetotunnelratio")),
                  selectInput("y_axis", "Y Axis", choices = c("release_speed",
                    "pfx_x", "pfx_z","release_spin_rate", "spin_axis", "delta_run_exp",
                    "release_angle", "release_direction", "posttunnelbreak", "pitches_thrown",
                    "spin_eff", "diffattunnel", "diffatplate", "speeddiff", "diffatrelease",
                    "breaktotunnelratio", "releasetotunnelratio"))
                ),
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
        tabItem(tabName = "filters",
                box(
                  style='width:1200px;overflow-x: scroll;height:800px;overflow-y: scroll;',
                  width = "300px", height = "600px",
                  title = "Data",
                  id = "filter_table",
                  dataTableOutput('Filter_Data')
                ),
                box(
                  height = "700px",
                  title = "Filters",
                  id = "filter_sliders",
                  sliderInput('plate_x', "K-Zone Horizontal", min = -4, max = 4, 
                              value = c(-4,4), step = 0.1),
                  sliderInput('plate_z', "K-Zone Vertical", min = 0, max = 5, 
                              value = c(0,5), step = 0.1),
                  sliderInput('pfx_x', "Horizontal Break", min = -2, max = 2, 
                              value = c(-2,2), step = 0.1),
                  sliderInput('pfx_z', "Vertical Break", min = -2, max = 2, 
                              value = c(-2,2), step = 0.1),
                  sliderInput('speed', "Release Speed", min = 65, max = 105, 
                              value = c(65,105)),
                  sliderInput('spin', "Release Spin Rate", min = 1000, max = 3500, 
                              value = c(1000,3500))
                  
                ),
                box(
                  height = "700px",
                  id = "filter_sliders_2",
                  sliderInput('tunnel', "Tunnel Difference", min = 0, max = 4, 
                              value = c(0,4), step = 0.1),
                  sliderInput('release', "Release Difference", min = 0, max = 5, 
                              value = c(0,5), step = 0.1),
                  sliderInput('plate', "Plate Difference", min = 0, max = 8, 
                              value = c(0,8), step = 0.1),
                  sliderInput('diffspeed', "Speed Difference", min = -35, max = 0, 
                              value = c(-35,0)),
                  sliderInput('PTB', "Post Tunnel Break", min = 0, max = 1, 
                              value = c(0,1), step = 0.05)
                )
        )
      )
    )
  )
}
