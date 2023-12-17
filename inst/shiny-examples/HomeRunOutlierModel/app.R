library(dplyr)
library(shiny)


collect_data <- function(minHR = 500){

  require(Lahman)
  require(dplyr)
  Batting |>
    group_by(playerID) |>
    summarize(HR = sum(HR),
              AB = sum(AB)) |>
    arrange(desc(HR)) |>
    filter(HR >= minHR) -> top500

  People |>
    filter(playerID %in% top500$playerID) |>
    mutate(
      mlb_birthyear = if_else(birthMonth >= 7,
                              birthYear + 1, birthYear),
      Player = paste(nameFirst, nameLast)
    ) |>
    select(playerID, Player, mlb_birthyear) -> player_info

  inner_join(top500, player_info, by = "playerID") ->
    top500

  Batting |>
    filter(playerID %in% top500$playerID) |>
    group_by(playerID, yearID) |>
    summarize(HR = sum(HR),
              AB = sum(AB),
              .groups = "drop") -> season_data

  inner_join(season_data,
             select(top500, playerID, mlb_birthyear),
             by = "playerID") |>
    mutate(Age = yearID - mlb_birthyear) -> season_data
  list(top500 = top500,
       season_data = season_data)
}

gibbs_outlier <- function(df, v = 5, iter = 1000,
                          minAB = 100){
  require(dplyr)
  df |>
    filter(AB >= minAB) |>
    mutate(AgeD = Age - 30,
           Rate = HR / AB) -> df
  dev <- function(y, t, lambda){
    -2 * (t * (y - lambda - y * log(y / lambda)))
  }
  n <- dim(df)[1]
  gam <- rep(1, n)
  RATE <- matrix(0, iter, n)
  GAM <- matrix(0, iter, n)
  for(j in 1:iter){
    fit <- glm(HR ~ offset(log(AB)) + AgeD + I(AgeD ^ 2),
               family = "poisson",
               data = df,
               weights = gam)
    fit_lam <- fit$fitted.values / df$AB
    gam <- rgamma(n,
                  shape = (v + 1) / 2,
                  rate = (dev(df$Rate, df$AB, fit_lam) + v) / 2)
    #   E_count <- exp(X %*% fit$coefficients + log(df$AB))
    RATE[j, ] <- fit_lam
    GAM[j, ] <- gam
  }
  df$Weight <- apply(GAM, 2, mean)
  df$m_rate <- apply(RATE, 2, mean)
  fit2 <- glm(HR ~ offset(log(AB)) + AgeD + I(AgeD ^ 2),
              family = "poisson",
              data = df)
  df$r_fit <- fit2$fitted.values / df$AB

  S <- list(Peak = max(df$m_rate),
            Career = sum(df$m_rate * df$AB),
            Obs_Peak = max(df$HR / df$AB),
            Obs_Career = sum(df$HR))
  list(df = df, S = S)
}

construct_plot <- function(out2, name){
  require(ggplot2)
  ggplot(out2) +
    geom_point(aes(Age, 100 * Rate, size = Weight),
               color = "red") +
    geom_line(aes(Age, 100 * m_rate),
              color = "blue",
              linewidth = 2) +
    labs(title = paste(name, "Home Run Rate Trajectory"),
         subtitle = "HR Rate = 100 x HR / AB") +
    ylab("HR Rate") +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(plot.subtitle = element_text(colour = "red", size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
}

out <- collect_data(minHR = 400)

ui <- fluidPage(
    theme = shinythemes::shinytheme("slate"),
    h2("Home Run Trajectories of the 400 HR Club"),
    column(
      3,
      selectInput("player",
                  "Select Hitter from 400 HR Club:",
                  choices =
                    out$top500$Player
      ),
      sliderInput("minAB", "Select Mininum Season At-Bats:",
                  0, 200,
                  value = 100,
                  step = 10),
      sliderInput("iter", "Select Number of Gibbs Sampling Iterations:",
                  100, 2000,
                  value = 1000,
                  step = 100),
      hr(),
      p("Blue curve represents Poisson outlier random effects
        quadratic model fit using the Gibbs sampler methodology
        from Albert (1992)."),
      p("Table gives Observed and Model-Based estimates of
        home run Peak and Career Ability measures")
    ),
    column(
      9,
      plotOutput("plot1",
                 height = "500px"
      ),
      tableOutput("data")
    )
  )

  server <- function(input, output, session) {

    output$plot1 <- renderPlot(
      {
        out$top500 |>
          filter(Player == input$player) |>
          pull(playerID) -> playerid

        out2 <- gibbs_outlier(filter(out$season_data,
                              playerID == playerid),
                              iter = input$iter,
                              minAB = input$minAB)
        name <- filter(out$top500,
                       playerID == playerid) |>
          pull(Player)
        construct_plot(out2$df, name)
      },
      res = 96
    )

    output$data <- renderTable({
      out$top500 |>
        filter(Player == input$player) |>
        pull(playerID) -> playerid

      out2 <- gibbs_outlier(filter(out$season_data,
                                   playerID == playerid),
                            iter = input$iter,
                            minAB = input$minAB)

      data.frame(Estimate = c("Observed", "Model Based"),
                 Peak = 100 * c(out2$S$Obs_Peak, out2$S$Peak),
                 Career = c(out2$S$Obs_Career, out2$S$Career))
    }, digits = 3, width = '75%', align = 'c',
    bordered = TRUE)

  }

  shinyApp(ui = ui, server = server)
