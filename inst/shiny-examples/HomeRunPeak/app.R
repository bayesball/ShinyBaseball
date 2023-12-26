library(dplyr)
library(shiny)

collect_data <- function(minHR = 500){
  
  require(Lahman)
  require(dplyr)
  Batting |>
    group_by(playerID) |>
    summarize(HR = sum(HR),
              AB = sum(AB),
              SO = sum(SO)) |>
    arrange(desc(HR)) |>
    filter(HR >= minHR) -> top
  
  People |>
    filter(playerID %in% top$playerID) |>
    mutate(
      mlb_birthyear = if_else(birthMonth >= 7,
                              birthYear + 1, birthYear),
      Player = paste(nameFirst, nameLast)
    ) |>
    select(playerID, Player, mlb_birthyear) -> player_info
  
  inner_join(top, player_info, by = "playerID") ->
    top
  
  Batting |>
    filter(playerID %in% top$playerID) |>
    group_by(playerID, yearID) |>
    summarize(HR = sum(HR),
              AB = sum(AB),
              SO = sum(SO),
              .groups = "drop") -> season_data
  
  inner_join(season_data,
             select(top, playerID, mlb_birthyear),
             by = "playerID") |>
    mutate(Age = yearID - mlb_birthyear) -> season_data
  list(top = top,
       season_data = season_data)
}
graph_results <- function(S, name, type = "mfit"){
  
  # graph to display two estimates at rates
  S$d |>
    mutate(Type = "Observed", Rate = HR / AB) -> S1
  S$d |>
    mutate(Type = "Multilevel", Rate = Estimates) -> S2
  S12 <- rbind(S1, S2)
  
  p1 <- ggplot(S12) +
    geom_smooth(aes(Age, HR / AB),
                method = "lm",
                formula = "y ~ x + I(x^2)",
                se = FALSE,
                color = "black") +
    geom_point(aes(Age, Rate, color = Type),
               size = 3) +
    ylab("Rate") +
    theme(text=element_text(size=18)) +
    ggtitle(paste(name, "HR Rate Estimates")) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  
  # post of peak rate
  
  N <- dim(S$post)[2] - 4
  sim_peak <- apply(S$post[, 1:N], 1, max)
  
  p2 <- ggplot(data.frame(Peak = sim_peak)) +
    geom_histogram(aes(Peak),
                   color = "white",
                   fill = "tan",
                   bins = 20) +
    theme(text=element_text(size=18)) +
    ggtitle(paste(name, "Posterior of Peak HR Rate")) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ylab("Density")
  
  
  # post of age when peak rate achieved
  
  age_peak <- data.frame(Age = S$d$Age[apply(S$post[, 1:N],
                                             1, which.max)])
  age_peak |>
    group_by(Age) |>
    count() -> D
  
  p3 <- ggplot(D,  aes(Age, n)) +
    geom_segment(aes(xend = Age, yend = 0),
                 linewidth = 3,
                 lineend = "butt",
                 color="red") +
    theme(text=element_text(size=18)) +
    scale_x_continuous(breaks=min(S$d$Age):max(S$d$Age)) +
    ggtitle(paste(name, "Posterior of Peak Age")) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ylab("Density")
  
  list(p1 = p1, p2 = p2, p3 = p3)
}
graph_results0 <- function(S, name, type = "mfit"){
  
  # graph to display two estimates at rates
  S$d |>
    mutate(Type = "Observed", Rate = HR / (AB - SO)) -> S1
  S$d |>
    mutate(Type = "Multilevel", Rate = Estimates) -> S2
  S12 <- rbind(S1, S2)
  
  if(type %in% c("rates", "fit")){
    d <- S1
    color_set <- c("blue")
  }
  if(type == "mfit"){
    d <- S12
    color_set <- c("red", "blue")
  }
  p1 <- ggplot(d)
  
  if(type %in% c("fit", "mfit")){
    p1 <- p1 + geom_smooth(aes(Age, HR / (AB - SO)),
                           method = "lm",
                           formula = "y ~ x + I(x^2)",
                           se = FALSE,
                           color = "black")
  }
  p1 <- p1 +
    geom_point(aes(Age, Rate, color = Type),
               size = 3) +
    ylab("Rate") +
    theme(text=element_text(size=18)) +
    scale_colour_manual(values = color_set) +
    ggtitle(paste(name, "HR Rate Estimates")) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  
  
  p1
}
hier_fit <- function(d, iter1 = 5000, iter2 = 5000,
                     df = 1){
  require(runjags)
  require(ggplot2)
  require(dplyr)
  require(coda)
  
  # t(df) modeling at 2nd stage
  modelString <-"
  model {
  ## sampling
  for (i in 1:N){
    y[i] ~ dbin(p[i], n[i])
    logit(p[i]) <- theta[i]
    theta[i] ~ dt(beta0 + beta1 * x[i] +
                 beta2 * x[i] * x[i], tau, df)
  }
  ## priors
  beta0 ~ dnorm(0, 0.01)
  beta1 ~ dnorm(0, 0.01)
  beta2 ~ dnorm(0, 0.01)
  tau <- 1 / (sigma * sigma)
  sigma ~ dt(0, 1, 1) T(0, )
 }
 "
  
  # HR / (AB - SO) is the home rate
  data <- list(N = dim(d)[1],
               y = d$HR,
               x = d$Age - 30,
               n = d$AB - d$SO,
               df = df)
  
  posterior <- run.jags(modelString,
                        n.chains = 1,
                        data = data,
                        monitor = c("p", "sigma",
                                    "beta0", "beta1", "beta2"),
                        adapt = 1000,
                        burnin = iter1,
                        sample = iter2)
  
  # matrix of posterior draws
  posterior %>% as.mcmc() %>% as.data.frame() -> post2
  
  # add posterior means of p to dataset
  d$Estimates <- apply(post2[1:data$N], 2, mean)
  
  # add posterior means at peak and age at peak
  N <- dim(post2)[2] - 4
  sim_peak <- apply(post2[, 1:N], 1, max)
  age_peak <- d$Age[apply(post2[, 1:N], 1, which.max)]
  
  list(d = d, post = post2,
       peak = mean(sim_peak),
       age_peak= mean(age_peak))
}


# set minAB value
minAB <- 200
out <- collect_data(minHR = minAB)

ui <- fluidPage(
    theme = shinythemes::shinytheme("slate"),
    h3(paste("Multilevel HR Rate Trajectories of the",
             minAB, "HR Club")),
    column(
      3,
      selectInput("player",
                  paste("Select Hitter from",
                        minAB, "HR Club:"),
                  choices =
                    out$top$Player,
                  selected = "Mickey Mantle"
      ),
      radioButtons("type", "Choose what to display:",
                   c("Rates" = "rates",
                     "Add Fit" = "fit",
                     "Multilevel" = "mfit")),
      radioButtons("iter", "Select # of MCMC iterations:",
                  c(1000, 2500, 5000)),
      sliderInput("df", "Select df of t prior at 2nd stage:",
                  1, 30, 1),
      hr(),
      p("Inplay HR rate is defined as"),
      p("Rate = HR / (AB - SO)")
    ),
    column(
      9,
      tabsetPanel(type = "tabs",
            tabPanel("HR Rates",
                 plotOutput("plot1",
                    height = "500px")
            ),
            tabPanel("Peak Rate",
                 plotOutput("plot2",
                    height = "500px")
            ),
            tabPanel("Peak Age",
                 plotOutput("plot3",
                     height = "500px")
            )
      ),
      tableOutput("data")
    )
  )

  server <- function(input, output, session) {

    output$plot1 <- renderPlot(
      {
        out$top |>
          filter(Player == input$player) |>
          pull(playerID) -> playerid

        out2 <- hier_fit(filter(out$season_data,
                              playerID == playerid),
                         iter1 = as.numeric(input$iter),
                         iter2 = as.numeric(input$iter),
                         df = input$df)
        name <- filter(out$top,
                       playerID == playerid) |>
          pull(Player)
        graph_results0(out2, name, input$type)
      },
      res = 96
    )

    output$plot2 <- renderPlot(
      {
        out$top |>
          filter(Player == input$player) |>
          pull(playerID) -> playerid

        out2 <- hier_fit(filter(out$season_data,
                                playerID == playerid),
                         iter1 = as.numeric(input$iter),
                         iter2 = as.numeric(input$iter),
                         df = input$df)
        name <- filter(out$top,
                       playerID == playerid) |>
          pull(Player)
        graph_results(out2, name)$p2
      },
      res = 96
    )

    output$plot3 <- renderPlot(
      {
        out$top |>
          filter(Player == input$player) |>
          pull(playerID) -> playerid

        out2 <- hier_fit(filter(out$season_data,
                                playerID == playerid),
                         iter1 = as.numeric(input$iter),
                         iter2 = as.numeric(input$iter),
                         df = input$df)
        name <- filter(out$top,
                       playerID == playerid) |>
          pull(Player)
        graph_results(out2, name)$p3
      },
      res = 96
    )

    output$data <- renderTable({
      out$top |>
        filter(Player == input$player) |>
        pull(playerID) -> playerid

      out2 <- hier_fit(filter(out$season_data,
                              playerID == playerid),
                       iter1 = as.numeric(input$iter),
                       iter2 = as.numeric(input$iter),
                       df = input$df)

      data.frame(Type = c("Peak", "Age at Peak"),
                 Post_Mean = c(out2$peak, out2$age_peak))
    }, digits = 3, width = '75%', align = 'c',
    bordered = TRUE)

  }

  shinyApp(ui = ui, server = server)
