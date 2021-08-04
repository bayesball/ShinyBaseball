library(shiny)
library(ggplot2)
library(dplyr)
library(LearnBayes)
library(Lahman)

pp_markov_simulation <- function(beta1_ab, beta2_ab, rho,
                                 player_name,
                                 retrodata,
                                 iter = 500,
                                 myfunc = max,
                                 label = "Max",
                                 the_title = ""){
  streak_data_retro <- function(playerid, retrodata){
    require(lubridate)
    require(dplyr)
    require(purrr)
    retrodata %>%
      filter(BAT_ID == playerid,
             AB_FL == TRUE) %>%
      mutate(Date = ymd(substr(GAME_ID, 4, 11)),
             Game_No = as.numeric(substr(GAME_ID, 12, 12)),
             Hit = ifelse(EVENT_CD %in% c(20:23),
                          1, 0)) %>%
      arrange(Date, Game_No, INN_CT) %>%
      mutate(game_id = paste(Date, Game_No))  %>%
      select(game_id, Hit) -> d

    dG <- data.frame(game_id = unique(d$game_id)) %>%
      mutate(Game = row_number())

    inner_join(d, dG, by = "game_id") %>%
      select(Game, Hit)
  }
  ppsim_S <- function(beta1_ab, beta2_ab, rho,
                      d, myfunc = max){
    markov_switching <- function(G, p1, p2, rho){
      states <- rep(0, G)
      states[1] <- sample(2, size = 1)
      for(j in 2:G){
        states[j] <- ifelse(runif(1) < rho,
                            states[j - 1],
                            3 - states[j - 1])
      }
      probs <- p1 * (states == 1) + p2 * (states == 2)
      data.frame(Game = 1:G,
                 Probability = probs)
    }
    dG <- markov_switching(max(d$Game),
                           rbeta(1, beta1_ab[1], beta1_ab[2]),
                           rbeta(1, beta2_ab[1], beta2_ab[2]),
                           rho)
    d <- inner_join(d, dG, by = "Game")
    y <- rbinom(nrow(d), size = 1, prob = d$Probability)
    streaks <- rle(y)
    ofers <- streaks$lengths[streaks$values == 0]
    myfunc(ofers)
  }

  the_names <- unlist(strsplit(player_name, " "))
  player_id <- Master %>%
    filter(nameLast == the_names[2],
           nameFirst == the_names[1]) %>%
    pull(retroID)

  d <- streak_data_retro(player_id, retrodata)

  ofer_m1 <- replicate(iter, ppsim_S(beta1_ab, beta2_ab,
                                     rho,
                                     d, myfunc))
  obs_streaks <- rle(d$Hit)
  ofers <- obs_streaks$lengths[obs_streaks$values == 0]
  observed <- myfunc(ofers)

  tail_prob <- mean(ofer_m1 >= observed)
  the_subtitle <- paste("Observed Stat = ", observed,
                        ", Tail Probability = ", tail_prob,
                        sep = "")
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
                                       size = 14, hjust = 0.5,
                                       vjust = 0.8, angle = 0),
          text = element_text(size = 18)) +
    xlab(label) +
    ylab("Count") +
    annotate(geom = "text", x = observed * 1.2, y = 100,
             label = "Obs", color = "red", size = 5)
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "darkly"),
  fluidRow(
    column(4, wellPanel(
      textInput("pname",
                h6("Input 2019 Player Name:"),
                value = "Rhys Hoskins"),
      sliderInput("qbeta2",
                  h6("90% Bounds for COLD Probability pC:"),
                  min = .05,
                  max = .35,
                  value = c(.15, .2)),
      sliderInput("qbeta1",
                  h6("90% Bounds for HOT Probability pH:"),
                  min = .25,
                  max = .55,
                  value = c(.35, .4)),
      sliderInput("rho",
                  h6("Staying Probability rho:"),
                  min = .5,
                  max = .99,
                  value = .9),
      radioButtons("stat",
                   h6("Streaky Measure:"),
                   choices = c("Maximum Ofer Length",
                               "Sum of Squared Ofer Lengths"),
                   inline = TRUE)
    )),
    column(8, wellPanel(
      h4(id="big-heading",
         "Predictive Distribution of Streaky Measure",
         align = "center"),
      h4("Markov Switching Model", align = "center"),
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           plotOutput("plot1",
                                      height = "425px")
                  ),
          tabPanel("Description",
                   p(''),
                   p('This app illustrates predictive checking of a streaky measure
                     for a Markov Switching model.'),
                   p('Assume y_1, ..., y_N
                      are independent Bernoulli outcomes.  For each game,
                     the batter is either in a hot state with hitting probability
                     pH or a cold state with hitting probability pC.  The batter
                     moves between the hot and cold states across games by a
                     Markov Chain with staying probability 0.9.  The probabilities
                     pC and pH have independent beta priors.'),
                   p("The ofers are the at-bats between successes
                     in the binary sequence."),
                   p('Interested in the predictive distribution of the
                     maximum length of an ofer or the sum of squared ofer lengths
                     among the Bernoulli outcomes.'),
                   h5('Using the App'),
                   p("One inputs the name of the player of interest, the limits
                     for a 90% central probability interval for
                     the hitting probabilities pH and pC, and the staying
                     probabilty rho.  Also one inputs the
                     type of streaky measure."),
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
    q1a <- list(p = .05,
               x = input$qbeta1[1])
    q1b <- list(p = .95,
               x = input$qbeta1[2])
    beta1_ab <- beta.select(q1a, q1b)
    q2a <- list(p = .05,
                x = input$qbeta2[1])
    q2b <- list(p = .95,
                x = input$qbeta2[2])
    beta2_ab <- beta.select(q2a, q2b)
    the_title <- paste("2019 ", input$pname,
                       "\npC in (",
                       input$qbeta2[1],
                       ", ", input$qbeta2[2],
                       "), pH in (",
                       input$qbeta1[1],
                       ", ", input$qbeta1[2], "), rho = ",
                       input$rho, sep = "")
    cluster <- function(ofers){
      sum(ofers ^ 2)
    }
    if(input$stat == "Maximum Ofer Length"){
      pp_markov_simulation(beta1_ab, beta2_ab, input$rho,
                         input$pname,
                         retro2019,
                         iter = 500,
                         myfunc = max,
                         label = "Maximum Ofer Length",
                         the_title = the_title)} else {
      pp_markov_simulation(beta1_ab, beta2_ab, input$rho,
                           input$pname,
                           retro2019,
                           iter = 500,
                           myfunc = cluster,
                           label = "Sum of Squared Ofer Lengths",
                           the_title = the_title)}
  }, res = 96)

}

shinyApp(ui = ui, server = server)
