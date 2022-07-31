library(shiny)

library(LearnBayes)
library(dplyr)
library(janitor)
library(ggplot2)

predict_hr <- function(prior, y_n, mu_sigma,
                       iterations = 1000){

  # find beta shape parameters
  a_b <- beta.select(list(x = prior[1],
                          prob = 0.25),
                     list(x = prior[2],
                          prob = 0.75))

  # simulate from posterior predictive distribution
  ns <- round(rnorm(iterations,
                    mu_sigma[1], mu_sigma[2]))
  ns <- pmax(ns, 1)
  p <- rbeta(iterations, a_b[1] + y_n[1],
                   a_b[2] + y_n[2] - y_n[1])
  ds <- data.frame(
    ys = rbinom(iterations, size = ns, prob = p)
    )

  # tabulate simulated draws of ys
  ds %>%
    tabyl(ys)  %>%
    mutate(y_all = ys + y_n[1]) -> S

  # compute mean of posterior predictive
  S %>%
    summarize(Mean = sum(ys * percent) +
                y_n[1]) -> SU

  # compute probability that total is 62 or more
  S %>%
    filter(y_all >= 62) %>%
    summarize(P = sum(percent)) %>%
    pull() -> y62

  # plot results
  ggplot(S, aes(ys, percent)) +
    geom_segment(mapping = aes(xend = ys,
                               yend = 0),
                 size = 3) +
    geom_segment(data = filter(S, y_all >= 62),
                 mapping = aes(xend = ys,
                               yend = 0),
                 size = 3, color = "red") +
    ggtitle(paste("Mean Prediction:", y_n[1],
                  "+", round(SU$Mean - y_n[1], 1),
                  "=", round(SU$Mean, 1),
                  "\n P(62+ home runs) = ",
                  y62)) +
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
                  "Select Quartiles of Judge's True HR Rate Per PA:",
                  min = 0.01, max = 0.2,
                  value = c(.05, .09)),
      tableOutput("out1"),
      numericInput("PA",
                   "Observed PA:",
                   value = 441),
      numericInput("HR",
                   "Observed HR:",
                   value = 42),
      numericInput("PA1",
                  "Future PA:",
                  value = 254),
      numericInput("SD1",
                  "Standard Deviation of Future PA:",
                  value = 15),
    )),
    column(8,
          plotOutput("plot1", height = "600px")
           ))
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
     y_n <- c(input$HR, input$PA)
     prior <- input$HR_Rate
     mu_sigma <- c(input$PA1, input$SD1)
     predict_hr(prior, y_n, mu_sigma)
  })
  output$out1 <- renderTable({
      a_b <- beta.select(list(x = input$HR_Rate[1],
                              prob = 0.25),
                         list(x = input$HR_Rate[2],
                              prob = 0.75))
     data.frame("Beta a" = a_b[1],
                "Beta b" = a_b[2])
  }, digits = 2)
}

shinyApp(ui = ui, server = server)
