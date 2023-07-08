# app to compute brushed home run rates
# currently live at https://bayesball.shinyapps.io/HomeRunLaunchVariables/

library(shiny)
library(LearnBayes)

predict_win_pct <- function(prior, y1){
  require(ggplot2)
  require(dplyr)

  # prior is mean and sd of normal prior on mu
  mu <- prior[1]
  tau <- prior[2]

  # sampling variance of y1 and y2
  V1 <- 0.5 * 0.5 / 162
  V2 <- V1

  # basic calculations
  B <- V1 / (V1 + tau ^ 2)
  mu1 <- (1 - B) * y1 + B * mu
  mu2 <- mu1 - y1

  tau1 <- sqrt(V1 * (1 - B))
  tau2 <- sqrt(V2 + V1 * (1 - B))

  # posterior of mu given y1
  post <- c(mu1, tau1)

  # predictive of y2 given y1
  pred <- c(mu1, tau2)

  # predictive of y2 - y1 given y1
  pred2 <- c(mu2, tau2)

  # P(y2 < y1 | y1)
  prob <- pnorm(y1, mu1, tau2)

  # prior/posterior calculations

  xmin <- min(c(mu - 3 * tau, mu1 - 3 * tau1))
  xmax <- max(c(mu + 3 * tau, mu1 + 3 * tau1))
  x <- seq(xmin, xmax, length.out = 200)
  prior <- dnorm(x, mu, tau)
  post <- dnorm(x, mu1, tau1)
  df1 <- data.frame(MU = x, Density = prior, Type = "Prior")
  df2 <- data.frame(MU = x, Density = post, Type = "Posterior")
  df12 <- rbind(df1, df2)

  # predictive of y2 - y1 calculations

  xlimits <- c(mu2 - 3 * tau2,
               mu2 + 3 * tau2)
  x <- seq(xlimits[1], xlimits[2], length.out = 200)
  pred <- dnorm(x, mu2, tau2)
  df <- data.frame(Change = x,
                   Density = pred)

  # both plots together

  df12 %>%
    mutate(Variable = MU,
           GType = "True Win Fraction") %>%
    select(Variable, Density, Type, GType) -> df12
  df %>%
    mutate(Variable = Change,
           GType = "Change in Next Season's Win Fraction",
           Type = "Predictive") %>%
    select(Variable, Density, Type, GType) -> df

  df_all <- rbind(df12, df)
  df_all$GType <- factor(df_all$GType,
                         levels = c("True Win Fraction",
                                    "Change in Next Season's Win Fraction"))
  df_all$Type <- factor(df_all$Type,
                        levels = c("Prior",
                                   "Posterior", "Predictive"))
  plot4 <- ggplot(data = df_all,
                  aes(Variable, Density, color = Type)) +
    geom_line(linewidth = 1.5) +
    theme(text = element_text(size = 18)) +
    facet_wrap(~ GType, ncol = 1, scales = "free")

  df_out <- data.frame(Type = c("Prior", "Observed","Posterior",
                                "Predictive", "Change"),
                       Mean = round(c(mu, y1, mu1, mu1, mu2), 3),
                       StanDev = round(c(tau, NA, tau1, tau2, tau2), 3),
                       Prob_Negative = c(NA, NA, NA, NA,
                                         round(prob, 3)))

  # output
  list(both_plots = plot4,
       the_table = df_out)
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4("Predicting Next Season's Win Percentage"),
      hr(),
      sliderInput("q", "(Prior) Select Quartiles of p:",
                  min = .4, max = .6,
                  value = c(.466, .534)),
      sliderInput("y", "Select Observed Win Fraction y1:",
                  min = .5, max = .65,
                  value = .585),
      hr(),
      p("MODEL:"),
      p("y1, y2 ~ N(p, V)"),
      p("Prior p ~ N(mu, tau)"),
      p("Observe y1"),
      p("Interested in: "),
      p("- Posterior p | y1"),
      p("- Pred. Dist. of Change y2 - y1 | y1")
    )),
    column(8,
            hr(),
            plotOutput("plot1",
                       height = "450px"),
           tableOutput("data")
    )
))

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    norm_par <- normal.select(list(p = .25, x = input$q[1]),
                              list(p = .75, x = input$q[2]))
    predict_win_pct(c(norm_par$mu, norm_par$sigma),
                    input$y)$both_plots
  }, res = 96)

  output$data <- renderTable({
    norm_par <- normal.select(list(p = .25, x = input$q[1]),
                              list(p = .75, x = input$q[2]))
    predict_win_pct(c(norm_par$mu, norm_par$sigma),
                    input$y)$the_table

  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE)
}

shinyApp(ui = ui, server = server)
