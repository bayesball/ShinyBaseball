library(shiny)

data_work <- function(){
  require(readr)
  require(dplyr)
  require(lubridate)

  sc_2021 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast2021.csv")
  sc_2022 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2022.csv")
  sc_old <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/SC_BB_mini.csv")

  names(sc_old)[2] <- "Game_Date"

  hits <- c("single", "double", "triple",
            "home_run")
  sc_2021 %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% hits, 1, 0)) %>%
    select(game_year, Game_Date, launch_angle,
           launch_speed, events, HR, H) -> sc_2021
  sc_2022 %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% hits, 1, 0))  %>%
    select(game_year, Game_Date, launch_angle,
           launch_speed, events, HR, H) ->
    sc_2022
  sc <- rbind(sc_old, sc_2021, sc_2022)

  sc %>%
    mutate(Season = year(Game_Date))
}

predict_home_runs_2 <- function(fit1, scip2){
  # given gam fit contained in fit1
  # and new dataset scip2
  # finds predictions
  # add observed home run rate
  observed <- mean(scip2$HR)
  scip2 <- filter(scip2,
                  is.na(launch_angle) == FALSE,
                  is.na(launch_angle) == FALSE)
  lp2 <- predict(fit1,  scip2)
  prob2 <- exp(lp2) / (1 + exp(lp2))
  prob2 <- prob2[is.na(prob2) == FALSE]
  N <- length(prob2)
  one_sim <- function(){
    sum(runif(N) < prob2)
  }
  data.frame(Predicted =
               replicate(500, one_sim()) / N) %>%
    mutate(Actual = observed)
}

gam_many_seasons_A <- function(scip,
                               ball_season,
                               pseasons,
                               Month = 4){
  require(lubridate)
  require(mgcv)

  # filters statcast "ball" dataset according to
  # season and month
  scip1 <- filter(scip,
                  Season == ball_season,
                  month(Game_Date) == Month)

  # what is home run rate during ball season?
  TARGET <- sum(scip1$HR, na.rm = TRUE) /
    length(scip1$HR)

  # fits gam model using data from ball season
  fit1 <- gam(HR ~ s(launch_angle, launch_speed),
              data = scip1,
              family = binomial)

  # implements predictions for each season in
  # vector pseasons
  OUT <- NULL
  ACTUAL <- NULL
  for(season in pseasons){
    scip2 <- filter(scip,
                    Season == season,
                    month(Game_Date) == Month)
    out <- predict_home_runs_2(fit1, scip2)
    out$Season <- paste(season, "Season")
    OUT <- rbind(OUT, out)
    ACTUAL <- c(ACTUAL, out$Actual[1])
  }

  s_month <- c("April", "May", "June", "July",
               "August", "September")[Month - 3]

  list(OUT = OUT,
       ball_season = ball_season,
       pseasons = pseasons,
       month = s_month,
       ACTUAL = ACTUAL,
       TARGET = TARGET)
}

gam_many_seasons_B <- function(results,
                               nbins = 25,
                               adjust_x = 0.0018,
                               YLIM = 3 * c(0, 200)){

  require(ggplot2)
  loc_y = YLIM[2] * 0.85

  loc <- data.frame(Season =
                      paste(results$pseasons, "Season"),
                    x = results$ACTUAL,
                    y = loc_y,
                    lab = "Observed")

  xlo <- min(c(results$OUT$Predicted,
               results$ACTUAL,
               results$TARGET))
  xhi <- max(c(results$OUT$Predicted,
               results$ACTUAL,
               results$TARGET))

  ggplot(results$OUT, aes(Predicted)) +
 #   geom_histogram(bins = nbins,
 #                 color = "white",
 #                 fill = "red") +
    geom_density(size = 1.5,
                 color = "red") +
    geom_vline(aes(xintercept = Actual),
               size = 2) +
    facet_wrap(~ Season, ncol = 1) +
    labs(title = paste("Predicting In-Play HR Rate in",
                       results$month,
                       "\n Using",
                       results$ball_season,
                       "GAM Ball Model"),
         subtitle = "Red is prediction distribution, Black Line is observed rate") +
    geom_text(data = loc,
              aes(x = x + adjust_x, y = y,
                  label = lab),
              size = 5) +
    theme(plot.subtitle = element_text(colour = "black",
                                       size = 14,
                                       hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
    ) + ylab("") +
    xlab("Predicted Home Run Rate") +
    geom_vline(aes(xintercept = results$TARGET),
               linetype = "twodash",
               size = 2,
               color = "blue") +
    annotate(geom = "text",
             x = results$TARGET + adjust_x * .7,
             y = loc_y,
             label = results$ball_season,
             size = 5,
             color = "blue") +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0)) +
    theme(text=element_text(size=18)) +
    xlim(xlo, xhi + .005 / 2) +
    ylim(YLIM[1], YLIM[2])
}

# collect all statcast data from Github

scip <- data_work()

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4("Predicting In-Play Home Run Rates"),
      radioButtons("b_season",
                   "Select Ball (Model) Season:",
                   choices = c("2015", "2016", "2017",
                     "2018", "2019", "2021", "2022"),
                   selected = "2018",
                   inline = TRUE),
      checkboxGroupInput("p_season",
                         "Select Prediction Seasons:",
                   choices = c("2015", "2016", "2017",
                    "2018", "2019", "2021", "2022"),
                   selected = c("2019", "2021"),
                   inline = TRUE),
      sliderInput("month", "Select Month:",
                  min = 4, max = 9,
                  value = 4),
      actionButton("goButton", "RUN")
    )),
    column(8,
           tabsetPanel(type = "tabs",
                       tabPanel("Graph",
           shinycssloaders::withSpinner(
             plotOutput("plot1", height = "520px")
           )),
           tabPanel("Explanation",
                    p('This Shiny app illustrates prediction of home run
                      rates.  One first selects a Model Season -- a
                      model is fit predicting home run rates for a
                      given month using a smooth
                      function of the
                      launch variables for that season.
                      Next one selects future seasons to predict,
                      and a particular month of interest.  Pressing
                      the RUN button will implement the prediction.'),
                    img(src="example.png", alt="Baseball",
                        width="400", height="120",
                        style="float:center"),
                    p('In this example, the Model Season is 2016,
                      the month of interest is April, and one is predicting home
                      run rates for April of 2019.'),
                    p('The graph shows the home run rate in April
                      2016 (blue line) and the observed home run rate in April
                      2019 (black line).  The red curve displays the prediction
                      distribution for the 2019 rate from the
                      2016 model.'),
                    p("The prediction distribution is above the
                      2016 rate -- this indicates that the batters
                      are hitting at higher exit velocities and
                      better launch angles in 2019.  But the 2019
                      rate is higher than the prediction distribution.
                      This indicates that the baseball has more carry
                      in 2019 than in 2016.")
           ),
           tabPanel("Reference",
                    hr(),
                    p("Shiny app to accompany FanGraphs article:"),
                    p("Home Runs and Drag:  An Early Look at the 2022 Season"),
                    p("by Jim Albert and Alan Nathan"),
                    hr(),
                    p("Contact Jim Albert at albertcb1@gmail.com for
                      information about the Shiny app or methodology.")
           )
           ))
      )
)

server <- function(input, output, session) {
  predict_plot_work <- eventReactive(input$goButton, {
    results <- gam_many_seasons_A(scip,
                                  input$b_season,
                                  as.numeric(
                                    input$p_season
                                  ),
                                  Month = input$month)

    gam_many_seasons_B(results)
  })
  output$plot1 <- renderPlot({
    predict_plot_work()
  })
}

shinyApp(ui = ui, server = server)
