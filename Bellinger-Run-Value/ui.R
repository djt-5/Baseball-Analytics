library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel("Cody Bellinger's RE24"),
  sidebarLayout(
    sidebarPanel(
      h3("Welcome"),
      p("This Shiny App is an in depth look at the year-to-year performance of Cody Bellinger in terms of his RE24. Since winning MVP in 2019, Bellinger has struggled with the Dodgers. This app is a fun tool to research as to why that is, and what he has done better at since joining the Cubs."),
      br(),
      h3("What is RE24?"),
      p("So what is RE24? It's is a statistic that takes into account the Run Expectancy of 24 possible Base/Out states (8 base states x 3 out states). For example, after 1st and 2nd with 1 out, a team scores on average 0.908 runs. If Bellinger strikes out, the situation then calls for 0.343 runs. Bellinger's Run Value on the last pitch of the strikeout would be -0.565. This statistic tells you everything you need to know about how a batter affects the run scoring of the game, which is basically what every hitting statistic is trying to paint a picture of. Enjoy!"),
      br(),
      selectInput("bases", "Base State", choices = c("No Runners", "1st", "2nd",
                                                     "3rd", "1st & 2nd",
                                                     "1st & 3rd", "2nd & 3rd", 
                                                     "Bases Loaded"), 
                  selected = NULL, multiple = FALSE),
      br(),
      selectInput("outs", "Outs When Batting", choices = 0:2, 
                  selected = NULL, multiple = FALSE),
      br(),
      selectInput("p_throws", "Pitcher Throws", choices = c("R", "L"), 
                  selected = NULL, multiple = FALSE),
      br(),
      actionButton("submit", "Explore")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Timeline", 
                 plotOutput('timeline')),
        tabPanel("Analysis",
                 selectInput("year", "Year", choices = 2017:2023),
                 plotOutput('k_zone'),
                 dataTableOutput('data_table')
        )))))
