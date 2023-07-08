library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

scip <- read_csv("scip2023.csv")
source("plot_bip_rate2.R")
teams <- unique(sc_ip$Team)

ui <- fluidPage(
  titlePanel("BABIP and Spray Angle"),
  sidebarLayout(
    sidebarPanel(
      br(),
      sliderInput("la",
                  "Select Range of Launch Angle:",
                  min = -80, max = 80,
                  c(-80, 80)),
      sliderInput("ls",
                  "Select Range of Launch Speed:",
                  min = 40, max = 120,
                  c(40, 120)),
      radioButtons("keep_hr",
                   "Include Home Runs?",
                   choices = c("yes", "no"),
                   inline = TRUE),
      radioButtons("type",
                    "Choose Y Variable:",
                    choices = c("BABIP", "HA", "Z"),
                    inline = TRUE),
      checkboxGroupInput("teams",
                         "Select Teams:",
                         choices = teams,
                         selected = teams[1],
                         inline = TRUE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Graph",
      plotOutput("plot")),
      tabPanel("Explanation",
               hr(),
               p("Have Statcast data
                  for all balls in play during the
                 2018, 2019, 2021 and 2022 seasons."),
               p("One selects a range of values of
              launch angle and exit velocity values."),
              p("Ground balls correspond to  launch
                angles less than 10 degrees, line drives
                10-25 degrees, fly balls 25-50 degrees, and
                pop ups greater than 50 degrees"),
              p("One decides if one wishes to include home
                runs."),
              p("One selects one of three possible measures:"),
              p("- BABIP = batting average for subset of
                batted balls"),
              p("- HA = H - E(H) where E(H) is the expected
                number of hits given values of the launch
                angle and exit velocity"),
              p("- Z = HA / sqrt(E(H)), the standardized
                value of hits added"),
              p("One selects one or more seasons of interest."),
              hr(),
              p("Graph displays the measure plotted against the
                adjusted spray angle for each season.")
      ))
      )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    plot_bip_rate(scip,
                  input$la,
                  input$ls,
                  input$type,
                  input$teams,
                  input$keep_hr)
})
}

# Run the application
shinyApp(ui = ui, server = server)
