library(shiny)
library(ggplot2)
library(dplyr)
library(LearnBayes)

post_pred_simulation <- function(beta_ab,
                                 N,
                                 iter = 500,
                                 myfunc = max,
                                 observed,
                                 label = "Max",
                                 the_title = ""){
  ppsim <- function(beta_ab, N, myfunc = max){
    p <- rbeta(1, beta_ab[1], beta_ab[2])
    y <- rbinom(N, size = 1, prob = p)
    streaks <- rle(y)
    ofers <- streaks$lengths[streaks$values == 0]
    myfunc(ofers)
  }
  ofer_m1 <- replicate(iter,
                    ppsim(beta_ab, N, myfunc))
  tail_prob <- mean(ofer_m1 >= observed)
  the_subtitle <- paste("Tail Probability =", tail_prob)
  ggplot(data = data.frame(Stat = ofer_m1),
         aes(Stat)) +
    geom_histogram(bins = 15,
                   color = "white",
                   fill = "tan") +
    geom_vline(xintercept = observed,
               color = "red",
               size = 2) +
    labs(title = the_title,
         subtitle = the_subtitle) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 16, hjust = 0.5,
                                    vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 16, hjust = 0.5,
                                       vjust = 0.8, angle = 0),
          text = element_text(size = 18)) +
    xlab(label) +
    ylab("Count") +
    annotate(geom = "text", x = observed * 1.1, y = 100,
             label = "Obs", color = "red", size = 5)
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "darkly"),
  fluidRow(
    column(4, wellPanel(
      sliderInput("qbeta",
                  h6("90% Bounds for Hit Probability p:"),
                  min = .15,
                  max = .35,
                  value = c(.2, .3)),
      sliderInput("N",
                  h6("Number of At-Bats AB:"),
                  min = 100,
                  max = 600,
                  value = 400),
      radioButtons("stat",
                   h6("Streaky Measure:"),
                   choices = c("Maximum Ofer Length",
                               "Sum of Squared Ofer Lengths"),
                   inline = TRUE),
      textInput("Ofer",
                  h6("Observed Value of Measure:"),
                  value = "")
    )),
    column(8, wellPanel(
      h4(id="big-heading",
         "Predictive Distribution of Streaky Measure",
         align = "center"),
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           plotOutput("plot1",
                                      height = "405px")
                  ),
          tabPanel("Description",
                   p(''),
                   p('This app illustrates predictive checking of a streaky measure
                     for a Beta/Bernoulli model.'),
                   p('Assume y_1, ..., y_N
                      are independent Bernoulli outcomes with probability
                      of success p.  Assume p has a Beta distribution with
                      shape parameters a and b.'),
                   p("The ofers are the at-bats between successes
                     in the binary sequence."),
                   p('Interested in the predictive distribution of the
                     maximum length of an ofer or the sum of squared ofer lengths
                     among the Bernoulli outcomes.'),
                   h5('Using the App'),
                   p("One inputs the limits for a 90% central probability interval for
                     the hitting probability p.  (The values of the beta
                     shape parameters are found that match this input.) Also one inputs the number of
                     at-bats N, the type of streaky measure, and the observed value
                     of the streaky measure."),
                   p("Histogram displays the simulated predictive distribution of the
                     streaky measure.  The observed value of the
                     streaky measure is
                     displayed as a vertical line.  The tail probability is the
                     probability that the predictive probability is at least as
                     large as the observed value.")))
    ))
  ))

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    options(warn=-1)
    q1 <- list(p = .05,
               x = input$qbeta[1])
    q2 <- list(p = .95,
               x = input$qbeta[2])
    beta_ab <- beta.select(q1, q2)
    the_title <- paste("90% Interval for p: (", input$qbeta[1],
                       ", ", input$qbeta[2], "), N = ",
                       input$N,
                       "\nBeta(", round(beta_ab[1], 1),
                       ", ", round(beta_ab[2], 1),
                       ") Prior",
                       sep = "")
    cluster <- function(ofers){
      sum(ofers ^ 2)
    }
    if(input$stat == "Maximum Ofer Length"){
      post_pred_simulation(beta_ab,
                         input$N,
                         iter = 500,
                         myfunc = max,
                         as.numeric(input$Ofer),
                         label = "Maximum Ofer Length",
                         the_title = the_title)} else {
      post_pred_simulation(beta_ab,
                           input$N,
                           iter = 500,
                           myfunc = cluster,
                           as.numeric(input$Ofer),
                           label = "Sum of Squared Ofer Lengths",
                           the_title = the_title)}
  }, res = 96)

}

shinyApp(ui = ui, server = server)
