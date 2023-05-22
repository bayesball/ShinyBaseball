library(dplyr)
library(readr)

# app is live at https://bayesball.shinyapps.io/wOBA_multilevel_comparison/

# data is read from a Github respository

fg_batting <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/fgbatting_complete.csv")
fg_batting %>%
  filter(inducted == "Y") -> fg_batting_hof
minPA <- 5000
fg_batting_hof %>%
  group_by(key_bbref) %>%
  summarize(PA = sum(PA),
            Name = first(Name)) %>%
  filter(PA >= minPA) %>%
  pull(Name) -> player_list

Player_List <- unique(player_list)

get_player_id <- function(player_name){
  fg_batting_hof %>%
    filter(Name == player_name) %>%
    summarize(playerid = first(key_bbref)) %>%
    pull(playerid)
}

woba_player_function <- function(player_id, fg_batting){

  # player_id is value of bbref_id variable in fg_data

  # uses LearnBayes package to simulate from
  # multilevel model

  library(dplyr)
  library(LearnBayes)

  # some data work

  fg_batting %>%
    filter(key_bbref == player_id) -> d1
  player_name <- d1$Name[1]
  d1 %>%
    mutate(Player = player_name) %>%
    select(Player, Season, Age, PA, wOBA) -> d

  # model description -- definition of logpost of hyperparameters

  qlogpost <- function (theta, data) {
    y <- data[, 1]
    sigma2 <- data[, 2]
    x <- data[, 3]
    tau <- exp(theta[4])
    mu <- theta[1] + theta[2] * x + theta[3] * x ^ 2
    logf = function(mu, tau, y, sigma2){
      dnorm(y, mu, sqrt(sigma2 +  tau ^ 2), log = TRUE)
    }
    sum(logf(mu, tau, y, sigma2)) + log(tau)
  }

  # fit using Laplace approximation

  sigma <- 0.51 # approximation to sigma from retro data
  d$var <- sigma ^ 2 / d$PA

  fit <- laplace(qlogpost, c(0, 0, 0, 0),
                 as.matrix(d[, c("wOBA", "var", "Age")]))

  # compute posterior means of probs

  post_means <- function(d, fit){
    mu <- fit$mode[1] + fit$mode[2] * d$Age +
      fit$mode[3] * d$Age ^ 2
    tau <- exp(fit$mode[4])
    est <- (d$wOBA / d$var + mu / tau ^ 2) /
      (1 / d$var + 1 / tau ^ 2)
    shrinkage <- 100 * (1 / tau ^ 2) /
      (1 / d$var + 1 / tau ^ 2)
    list(est = est, shrinkage = shrinkage)
  }

  out <- post_means(d, fit)
  pmeans <- out$est
  d$Shrinkage <- out$shrinkage

  # quadratic fit

  d1 <- select(d, Age) %>%
    mutate(wOBA = fit$mode[1] + fit$mode[2] * Age +
             fit$mode[3] * Age ^ 2,
           Type = "Quadratic Fit") -> d1

  d2 <- select(d, Age, wOBA) %>%
    mutate(Type = "Observed")

  d3 <- data.frame(Age = d$Age,
                   post_means = pmeans) %>%
    mutate(wOBA = post_means,
           Type = "Multilevel") %>%
    select(Age, wOBA, Type)

  d123 <- rbind(d1, d2, d3) %>%
    mutate(Player = player_name)

  # simulate from posterior of hyperparameters
  sim_h <- rmnorm(5000, fit$mode, fit$var)

  # simulate from posterior of mu_j
  sim_mu_j <- function(j){
    lp <- sim_h[, 1] + sim_h[, 2] * d$Age[j] +
      sim_h[, 3] * d$Age[j] ^ 2
    tau <- exp(sim_h[, 4])
    woba_mean <- (d$wOBA[j] / d$var[j] + lp / tau ^ 2) /
      (1 / d$var[j] + 1 / tau ^ 2)
    woba_var <- 1 / (1 / d$var[j] + 1 / tau ^ 2)
    rnorm(5000, woba_mean, sqrt(woba_var))
  }

  # collect simulations from max p
  indices <- 1:dim(d)[1]
  out <- sapply(indices, sim_mu_j)

  max_mu <- apply(out, 1, max)

  mu_quantile <- apply(out, 2, quantile, c(.25, .5, .75))

  newdf <- data.frame(Max_wOBA = max_mu) %>%
    mutate(Player = player_name)

  newdf2 <- data.frame(t(mu_quantile))
  names(newdf2) <- c("Q25", "Q50", "Q75")
  newdf2$Age <- d$Age
  newdf2$Player <- player_name

  list(d_plot1 = d123,
       d_plot2 = d,
       d_plot3 = newdf,
       d_plot4 = newdf2)
}

comparison_plots <- function(out1, out2, type_number){

  require(ggplot2)

  increasefont <- function(Size = 18){
    theme(text = element_text(size = Size))
  }
  centertitle <- function (Color = "blue"){
    theme(plot.title = element_text(colour = Color, size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  }
  if(type_number == 1){
    the_plot <- ggplot(rbind(out1$d_plot1,
                             out2$d_plot1), aes(Age, wOBA, color = Type)) +
      geom_point(size = 3) +
      geom_line(linewidth = 0.8,
                linetype = 2) +
      increasefont() +
      xlab("Age") +
      ylab("wOBA Estimate") +
      facet_wrap(~ Player, ncol = 1)
  }

  if(type_number == 2){
    the_plot <- ggplot(rbind(out1$d_plot2,
                             out2$d_plot2),
                       aes(Age, Shrinkage, color = Player)) +
      geom_point(size = 3) +
      geom_line(linewidth = 0.8,
                linetype = 2) +
      increasefont() +
      centertitle() +
      ggtitle("Shrinkage of MLM Estimates") +
      ylim(0, 100) +
      ylab("Shrinkage (Pct)")
  }

  if(type_number == 3){
    the_plot <-  ggplot(rbind(out1$d_plot3,
                              out2$d_plot3),
                        aes(Max_wOBA, color = Player)) +
      geom_density(linewidth = 1.5) +
      increasefont() +
      centertitle() +
      ylab("Density") +
      ggtitle("Posterior of Maximum wOBA")
  }

  if(type_number == 4){
    the_plot <- ggplot(rbind(out1$d_plot4,
                             out2$d_plot4), aes(x = Age, y = Q50)) +
      geom_errorbar(aes(ymin = Q25, ymax = Q75),
                    color = "blue") +
      geom_point(color = "red", size = 3) +
      geom_line() +
      increasefont() +
      centertitle() +
      ylab("Expected wOBA") +
      ggtitle("50% Posterior Intervals of E(wOBA)") +
      facet_wrap(~ Player, ncol = 1)
  }

  the_plot
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("wOBA Multilevel Comparison"),
  column(3,
  hr(),
  selectInput("player_name_1",
               "Select First Hall of Fame Player:",
               Player_List,
               "Mickey Mantle"),
  selectInput("player_name_2",
              "Select Second Hall of Fame Player:",
               Player_List,
               "Willie Mays")
  ),
  column(9,
         tabsetPanel(type = "tabs",
            tabPanel("Estimates",
                     plotOutput("plot1",
                     height = '550px')),
            tabPanel("Shrinkage",
                     plotOutput("plot2",
                     height = '450px')),
            tabPanel("Posteriors",
                     plotOutput("plot4",
                     height = '550px')),
            tabPanel("Max wOBA",
                     plotOutput("plot3",
                     height = '450px'))
         )
  )
)
server <- function(input, output, session) {
  options(warn=-1)

  output$plot1 <- renderPlot({
     pid1 <- get_player_id(input$player_name_1)
     out1 <- woba_player_function(pid1, fg_batting_hof)
     pid2 <- get_player_id(input$player_name_2)
     out2 <- woba_player_function(pid2, fg_batting_hof)
     comparison_plots(out1, out2, 1)
  }, res = 96)

  output$plot2 <- renderPlot({
    pid1 <- get_player_id(input$player_name_1)
    out1 <- woba_player_function(pid1, fg_batting_hof)
    pid2 <- get_player_id(input$player_name_2)
    out2 <- woba_player_function(pid2, fg_batting_hof)
    comparison_plots(out1, out2, 2)
  }, res = 96)

  output$plot3 <- renderPlot({
    pid1 <- get_player_id(input$player_name_1)
    out1 <- woba_player_function(pid1, fg_batting_hof)
    pid2 <- get_player_id(input$player_name_2)
    out2 <- woba_player_function(pid2, fg_batting_hof)
    comparison_plots(out1, out2, 3)
  }, res = 96)

  output$plot4 <- renderPlot({
    pid1 <- get_player_id(input$player_name_1)
    out1 <- woba_player_function(pid1, fg_batting_hof)
    pid2 <- get_player_id(input$player_name_2)
    out2 <- woba_player_function(pid2, fg_batting_hof)
    comparison_plots(out1, out2, 4)
  }, res = 96)

}

shinyApp(ui = ui, server = server)
