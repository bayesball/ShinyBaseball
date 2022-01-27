library(dplyr)
library(ggplot2)

history_plot <- function(br_data,
                         seasons,
                         measure,
                         smoothing_span){
  br_data %>%
    filter(Year >= seasons[1],
           Year <= seasons[2]) -> br_subset

  ggplot(br_subset,
         aes_string("Year", measure)) +
    geom_point(size = 2, color = "blue") +
    geom_smooth(se = FALSE,
                method = "loess",
                formula = "y ~ x",
                span = smoothing_span,
                color = "red") +
    xlab("Season") +
    labs(title = paste("History of", measure, "Per Team Game"),
         subtitle = paste("Seasons:",
                seasons[1], "to", seasons[2])) +
    theme(text = element_text(size = 15),
          plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0),
          plot.subtitle = element_text(colour = "blue",
                                       size = 16,
                                       hjust = 0.5,
                                       vjust = 0.8,
                                       angle = 0))
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("Baseball Reference Batting Averages"),
  column(3,
  sliderInput("seasons", "Select Range of Seasons:",
              1871, 2021,
              value = c(1871, 2021), sep = ""),
  selectInput("measure",
               "Select BR Measure:",
              names(BR_batting_league_avgs[, -c(1, 3,  5)]),
               selected = "R"),
  sliderInput("span", "Select Span of Smoother:",
              0, 1,
              value = 0.75)
  ),
  column(9,
         plotOutput("plot1",
                    height = '500px'))
)

server <- function(input, output, session) {
  options(warn=-1)

  output$plot1 <- renderPlot({
   history_plot(BR_batting_league_avgs,
                input$seasons,
                input$measure,
                input$span)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
