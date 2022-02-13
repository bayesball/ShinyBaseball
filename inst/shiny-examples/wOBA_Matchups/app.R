library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(Lahman)
library(LearnBayes)

# read in data work
all_batter_pitcher_36 <-
  read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/all_batter_pitcher_36.csv")
fg_guts <-
  read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/fg_guts.csv")
filter(all_batter_pitcher_36, Type == "Batter") %>%
     select(BAT_ID, Name, Type) %>%
     distinct() -> batter_ids
names(batter_ids)[1] <- "retroID"
filter(all_batter_pitcher_36, Type == "Pitcher") %>%
     select(PIT_ID, Name, Type) %>%
     distinct() -> pitcher_ids
names(pitcher_ids)[1] <- "retroID"
names_batter_pitcher_36 <- rbind(batter_ids,
                                 pitcher_ids)

# sort players by last name
getlastname <- function(name){
  unlist(strsplit(name, " "))[2]
}
names_batter_pitcher_36$Last_Name <-
  sapply(names_batter_pitcher_36$Name, getlastname)
names_batter_pitcher_36 %>%
  arrange(Type, Last_Name) %>%
  select(retroID, Name, Type) ->
  names_batter_pitcher_36

# general function
general_p_b_plot <- function(dall, dn, type,
                             fgwts, name){
  # pitchers against single batter or
  # batters against single pitcher

  # assuming sigma is unknown
  normnormexch3 <- function (theta, data){
    y <- data[, 1]
    n <- data[, 2]
    mu <- theta[1]
    tau <- exp(theta[2])
    sigma <- exp(theta[3])
    logf <- function(mu, tau, sigma, y, n){
      dnorm(y, mu, sqrt(sigma ^ 2 / n + tau ^ 2),
            log = TRUE)
    }
    sum(logf(mu, tau, sigma, y, n)) +
      log(tau) + log(sigma)
  }
  # fitting function
  fit.model3 <- function(ybar, n){
    fit <- laplace(normnormexch3,
                   c(0, 0, 0),
                   cbind(ybar, n))$mode
    mu <- fit[1]
    tau <- exp(fit[2])
    sigma <- exp(fit[3])
    Estimate <- (ybar / (sigma ^ 2 / n) + mu / tau ^ 2) /
      (1 / (sigma ^ 2 / n) + 1 / tau ^ 2)
    list(mu = mu, tau = tau, sigma = sigma,
         Estimate = Estimate)
  }
  # extract retroID
  retro.id <- dn %>%
    filter(Type == type, Name == name) %>%
    pull(retroID)
  # only look at subset of full dataset
  dall %>%
    filter(Type == type) -> d

  # add fg weights
  if(type == "Batter"){
    d %>%
      filter(BAT_ID == retro.id) %>%
      inner_join(fgwts, by = "Season") %>%
      mutate(WT = wBB * (EVENT_CD %in% 14:15) +
               w1B * (EVENT_CD == 20) +
               w2B * (EVENT_CD == 21) +
               w3B * (EVENT_CD == 22) +
               wHR * (EVENT_CD == 23)) %>%
      group_by(PIT_ID) %>%
      summarize(PA = n(),
                wOBA = mean(WT),
                .groups = "drop") -> S
  }
  # compute PA and wOBA for each opposing player
  if(type == "Pitcher"){
    d %>%
      filter(PIT_ID == retro.id) %>%
      inner_join(fgwts, by = "Season") %>%
      mutate(WT = wBB * (EVENT_CD %in% 14:15) +
               w1B * (EVENT_CD == 20) +
               w2B * (EVENT_CD == 21) +
               w3B * (EVENT_CD == 22) +
               wHR * (EVENT_CD == 23)) %>%
      group_by(BAT_ID) %>%
      summarize(PA = n(),
                wOBA = mean(WT),
                .groups = "drop") -> S
  }
  # assuming sigma is unknown
  the_fit <- fit.model3(S$wOBA, S$PA)

  S$MLM_Est <- the_fit$Estimate
  fit_out <- data.frame(mu = the_fit$mu,
                        tau = the_fit$tau,
                        sigma = the_fit$sigma)

  # create tables for all players
  if(type == "Batter"){
    S %>%
      inner_join(select(Master, retroID,
                        nameFirst, nameLast),
                 by = c("PIT_ID" = "retroID")) %>%
      mutate(Name = paste(nameFirst, nameLast),
             wOBA = round(wOBA, 3)) %>%
      select(Name, wOBA, PA, MLM_Est) -> S
  }
  if(type == "Pitcher"){
    S %>%
      inner_join(select(Master, retroID,
                        nameFirst, nameLast),
                 by = c("BAT_ID" = "retroID")) %>%
      mutate(Name = paste(nameFirst, nameLast),
             wOBA = round(wOBA, 3)) %>%
      select(Name, wOBA, PA, MLM_Est) -> S
  }

  nametitle <- ifelse(type == "Batter", "Pitchers",
                      "Batters")

  S %>%
    mutate(Estimate = wOBA, Type = "Raw") %>%
    select(PA, Estimate, Type) -> S1
  S %>%
    mutate(Estimate = MLM_Est, Type = "Multilevel") %>%
    select(PA, Estimate, Type) -> S2
  S12 <- rbind(S1, S2)

  compare_plot <- ggplot(S12, aes(PA, Estimate,
                                  color = Type)) +
    geom_point(size=1) +
    labs(title = paste("Two wOBA Estimates of", nametitle,
                       "Against", name)) +
    theme(text = element_text(size = 16)) +
    theme(plot.title = element_text(colour = "blue", size = 16,
                                    hjust = 0.5,
                                    vjust = 0.8, angle = 0))

  the_plot <- ggplot(S, aes(PA, MLM_Est)) +
    geom_point(size=1.5, color = "chocolate") +
    geom_hline(aes(yintercept = sum(PA * wOBA) / sum(PA)),
               color="black", size=1,
               linetype="dashed") +
    ylab("Multilevel Estimate") +
    labs(title = paste("Smoothed wOBA of", nametitle,
                       "Against", name)) +
    theme(text = element_text(size = 16)) +
    theme(plot.title = element_text(colour = "blue", size = 16,
                                    hjust = 0.5,
                                    vjust = 0.8, angle = 0))

  list(the_plot = the_plot,
       compare_plot = compare_plot,
       S = S,
       fit_out = fit_out)
}

# user interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("wOBA Pitcher and Batter Matchups: 1960-2021"),
  column(3,
  radioButtons("type",
               "Matchups Against:",
                c("Batter", "Pitcher"),
                   inline = TRUE),
  selectInput("player",
               "Select Player:",
               batter_ids$Name),
  radioButtons("plottype",
               "Plot Type:",
               c("Comparison", "Multilevel"),
               inline = TRUE),
  hr(), hr(),
  tableOutput("the_fit")
  ),
  column(9,
         plotOutput("plot1",
                    brush = "plot_brush",
                    height = '400px'),
        tableOutput("selectedStats")
         ),
)
# server function
server <- function(input, output, session) {
   observeEvent(input$type, {
    updateSelectInput(inputId = "player",
                      choices =
            filter(names_batter_pitcher_36,
                              Type == input$type)$Name)
  })

 output$plot1 <- renderPlot({
 out <- general_p_b_plot(all_batter_pitcher_36,
                         names_batter_pitcher_36,
                         input$type,
                         fg_guts,
                         input$player)
   if(input$plottype == "Multilevel"){out$the_plot} else {
         out$compare_plot}
  }, res = 96)

  output$the_fit <- renderTable({
    general_p_b_plot(all_batter_pitcher_36,
                     names_batter_pitcher_36,
                     input$type,
                     fg_guts,
                     input$player)$fit_out
  },
  digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Multilevel Fit:",
  caption.placement = "top")

  output$selectedStats <- renderTable({
    req(input$plot_brush)
    out <- general_p_b_plot(all_batter_pitcher_36,
                            names_batter_pitcher_36,
                            input$type,
                            fg_guts,
                            input$player)
    brushedPoints(out$S,
                  input$plot_brush)
  },
  digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Selected Players:",
  caption.placement = "top")
}

shinyApp(ui = ui, server = server)
