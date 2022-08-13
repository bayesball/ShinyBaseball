library(shiny)

library(LearnBayes)
library(dplyr)
library(janitor)
library(ggplot2)

predict_hr2 <- function(prior, y_n, PA){

  # find beta shape parameters
  a_b <- beta.select(list(x = prior[1],
                          prob = 0.25),
                     list(x = prior[2],
                          prob = 0.75))

  # update with data
  a_b_post <- a_b +
    c(y_n[1], y_n[2] - y_n[1])

  # compute posterior predictive distribution
  tibble(HR = 0:PA) %>%
    mutate(Prob = pbetap(a_b_post, PA, HR)) -> S

  # compute mean of posterior predictive
  S %>%
    summarize(Mean = sum(HR * Prob)) -> SU

  # compute probability that total is 62 or more
  S %>%
    filter(HR + y_n[1] >= 62) %>%
    summarize(P = sum(Prob)) %>%
    pull() -> y62

  # find 90% prediction interval

  p_out <- discint(as.matrix(S), .9)
  p_interval <- range(p_out$set) + y_n[1]
  p_prob <- round(p_out$prob, 3)
  p_text <- paste("Prob(", p_interval[1],
                  " <= HR <= ",
                  p_interval[2], ") = ",
                  p_prob, sep = "")

  # plot results
  S1 <- filter(S, Prob >= .000001)
  ggplot(S1, aes(HR, Prob)) +
    geom_segment(mapping = aes(xend = HR,
                               yend = 0),
                 size = 3) +
    geom_segment(data = filter(S1, HR >= 62 - y_n[1]),
                 mapping = aes(xend = HR,
                               yend = 0),
                 size = 3, color = "red") +
    ggtitle(paste("Mean Prediction:", y_n[1],
                  "+", round(SU$Mean, 1),
                  "=", round(SU$Mean + y_n[1], 1),
                  "\n Prob(62+ home runs) = ",
                  round(y62, 2),
                  "\n", p_text)) +
    xlab("Future Home Runs") +
    ylab("Probability") +
    theme(text=element_text(size=20)) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 24,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4("Predicting Aaron Judge's HR Count"),
      sliderInput("HR_Rate",
                  "Select Quartiles of Prior (< 2022) of Judge's True HR Rate Per PA:",
                  min = 0.01, max = 0.2,
                  value = c(.05, .09)),
      tableOutput("out1"),
      numericInput("PA",
                   "Observed PA:",
                   value = 479),
      numericInput("HR",
                   "Observed HR:",
                   value = 45),
      numericInput("Games",
                  "Future Games:",
                  value = 50),
      tableOutput("out2")
    )),
    column(8,
          plotOutput("plot1", height = "600px")
           ))
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
     y_n <- c(input$HR, input$PA)
     prior <- input$HR_Rate
     PA <- round(input$Games * 4.3)
     predict_hr2(prior, y_n, PA)
  })
  output$out1 <- renderTable({
      a_b <- beta.select(list(x = input$HR_Rate[1],
                              prob = 0.25),
                         list(x = input$HR_Rate[2],
                              prob = 0.75))
     data.frame("Type" = c("Prior", "Posterior"),
                "Beta a" = c(a_b[1],
                             a_b[1] + input$HR),
                "Beta b" = c(a_b[2],
                             a_b[2] + input$PA -
                               input$HR)
     )
  }, digits = 2)
  output$out2 <- renderTable({
    data.frame("Games" = input$Games,
               "Avg PA" = 4.3,
               "PA" =
                 round(input$Games * 4.3)
    )
  }, digits = 1)
}

shinyApp(ui = ui, server = server)
