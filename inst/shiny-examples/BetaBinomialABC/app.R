library(shiny)
library(ggplot2)
d <- fg2020batting

ui <- basicPage(
  column(4,
         h3("Approximate Bayesian Calculation"),
         h4("The Model:"),
         p('Observe y_1, ..., y_N,
                      where y_i is binomial(n_i, p_i)'),
         p('Assume p_1, ..., p_N
              are independent beta(K eta, K (1 - eta)'),
         p('eta is beta(8, 192)'),
         p('log K is logistic(5, 0.5)'),
         hr(),
         h4("ABC:"),
         p('Simulate from full model,
           compute Stat = sd(y / n)'),
         p('Look at distribution of (log K, Stat)'),
         p('Observe Stat = 0.018')
  ),
  column(8,
        plotOutput("plot",
                      brush = "plot_brush",
                      height = "300px",
                      width = "500px"),
        plotOutput("hist",
                   height = "300px",
                   width = "500px")
))

server <- function(input, output) {
  one_sim <- function(d){
    eta <- rbeta(1, 8, 192)
    logK <- rlogis(1, 5, 0.5)
    K <- exp(logK)
    N <- dim(d)[1]
    p <- rbeta(N, K * eta, K * (1 - eta))
    ys <- rbinom(N, size = d$PA, prob = p)
    stat <- sd(ys / d$PA)
    c(logK, stat)
  }
  out <- replicate(10000, one_sim(d))
  out_df <- data.frame(logK = out[1, ],
                       Stat = out[2, ])
  output$plot <- renderPlot({
    ggplot() +
      geom_point(data = out_df,
                 mapping = aes(logK, Stat)) +
      ggtitle("Joint Posterior of (logK, stat)")
  }, res = 96)

  output$hist <- renderPlot({
    req(input$plot_brush)
    df_new <- brushedPoints(out_df,
                            input$plot_brush)
    stat_limits <- round(range(df_new$Stat), 3)
    interval <- round(quantile(df_new$logK,
                      c(.05, .95)), 2)
    l_int <- paste("P(", interval[1], "< logK <",
                   interval[2], ") = 0.90")
    mytitle <- paste('Distribution of [logK |', stat_limits[1],
                     '< Stat <', stat_limits[2], ']')
    ggplot() +
      geom_histogram(data = df_new,
                 mapping = aes(logK)) +
      labs(title = mytitle,
           caption = l_int)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
