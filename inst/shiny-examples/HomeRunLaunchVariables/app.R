# app to compute brushed home run rates
# currently live at https://bayesball.shinyapps.io/HomeRunLaunchVariables/

library(shiny)
library(lubridate)
library(mgcv)

# GAM fits are read from local file

load("allfits.Rdata") # list of 18 fits
df <- data.frame(j = 1:18,
                 Season = rep(c(2019, 2021, 2022), each = 6),
                 Month = rep(4:9, 3))

month_table <- data.frame(Month = c("April", "May", "June",
                                    "July", "August",
                                    "September"),
                          n_Month = 4:9)

# data is read from Github repository

data_work <- function(){
  require(readr)
  require(dplyr)
  require(lubridate)

  sc_2021 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast2021.csv")
  sc_2022 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2022.csv")
  sc_2023 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2023.csv")
  sc_2022 %>%
  mutate(HR = ifelse(events == "home_run", 1, 0),
         game_date = Game_Date)  %>%
  select(game_year, game_date, launch_angle,
         launch_speed, HR) ->
  scip_2022
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
  sc_2023 %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% hits, 1, 0))  %>%
    select(game_year, Game_Date, launch_angle,
           launch_speed, events, HR, H) ->
    sc_2023
  sc <- rbind(sc_old, sc_2021, sc_2022, sc_2023)

  sc %>%
    mutate(Season = year(Game_Date))
}

subset_data <- function(scip, LA, LS, day_lo, day_hi){
  require(lubridate)
  require(dplyr)
  scip %>%
    filter(launch_angle >= LA[1],
           launch_angle <= LA[2],
           launch_speed >= LS[1],
           launch_speed <= LS[2],
           Game_Date >= ymd(day_lo),
           Game_Date <= ymd(day_hi)) %>%
    mutate(HR = ifelse(HR == 1, "YES", "NO"))
}

make_plot <- function(scip2, date1, date2){
  require(ggplot2)

  ggplot(scip2, aes(launch_angle, launch_speed,
                    color = HR)) +
    geom_jitter() +
    xlab("Launch Angle") +
    ylab("Exit Velocity") +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "white", size = 18,
                      hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "white", size = 16,
                      hjust = 0.5, vjust = 0.8, angle = 0)) +
    scale_color_manual(values = c("burlywood", "red")) +
    labs(title = "Home Runs and Launch Variables",
         subtitle = paste(date1, "to", date2)) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))
}

# read in statcast dataset
scip <- data_work()
last_date <- max(scip$Game_Date)

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4("Brushing Home Run Rates on Balls in Play in Statcast Era"),
      hr(),
      dateRangeInput("daterange", "Select Date Range:",
                     start = "2023-04-01",
                     end   = last_date),
      sliderInput("rX", "Select Range of Launch Angle:",
                  min = 0, max = 50,
                  value = c(20, 40)),
      sliderInput("rY", "Selext Range of Exit Velocity:",
                  min = 80, max = 120,
                  value = c(95, 110)),
      hr(),
      radioButtons("month",
                   label = "Select Month (Ball Model Prediction):",
                   choices = month_table$Month,
                   selected = "April",
                   inline = TRUE)
    )),
    column(8,
            plotOutput("plot1a",
                       brush = brushOpts("plot_brush",
                               fill = "#0000ff"),
                    height = "400px"),
           tableOutput("data"),
           tableOutput("data2")
      )
      )
)

server <- function(input, output, session) {
  output$plot1a <- renderPlot({
    scip2 <- subset_data(scip, input$rX, input$rY,
                         input$daterange[1],
                         input$daterange[2])
    make_plot(scip2,
              input$daterange[1],
              input$daterange[2])
  }, res = 96)

  output$data <- renderTable({
    req(input$plot_brush)

    scip2 <- subset_data(scip, input$rX, input$rY,
                         input$daterange[1],
                         input$daterange[2])
    sc1 <- brushedPoints(scip2,
                         input$plot_brush)

    la <- paste("(", min(sc1$launch_angle), ", ",
                     max(sc1$launch_angle), ")", sep = "")
    ls <- paste("(", min(sc1$launch_speed), ", ",
                max(sc1$launch_speed), ")", sep = "")

    hr <- sum(sc1$events == "home_run")
    bip <- nrow(sc1)

    data.frame(Launch_Angle = la,
               Exit_Velocity = ls,
               Balls_in_Play = bip,
               HR = hr,
               HR_Rate = hr / bip)

  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE)

  output$data2 <- renderTable({
    req(input$plot_brush)

    scip2 <- subset_data(scip, input$rX, input$rY,
                         input$daterange[1],
                         input$daterange[2])
    sc1 <- brushedPoints(scip2,
                         input$plot_brush)

    la <- paste("(", min(sc1$launch_angle), ", ",
                max(sc1$launch_angle), ")", sep = "")
    ls <- paste("(", min(sc1$launch_speed), ", ",
                max(sc1$launch_speed), ")", sep = "")

    month_table %>%
      filter(Month == input$month) %>%
      pull(n_Month) -> i_month

    df %>%
      filter(Season == 2019, Month == i_month) %>%
      pull(j) -> jj1
    sc1$Prob1 <- predict(fit[[jj1]], sc1, type = "response")
    df %>%
      filter(Season == 2021, Month == i_month) %>%
      pull(j) -> jj2
    sc1$Prob2 <- predict(fit[[jj2]], sc1, type = "response")
    df %>%
      filter(Season == 2022, Month == i_month) %>%
      pull(j) -> jj3
    sc1$Prob3 <- predict(fit[[jj3]], sc1, type = "response")

    hr <- sum(sc1$events == "home_run")
    bip <- nrow(sc1)
    e_hr1 <- sum(sc1$Prob1)
    z1 <- (hr - e_hr1) / sqrt(e_hr1)

    e_hr2 <- sum(sc1$Prob2)
    z2 <- (hr - e_hr2) / sqrt(e_hr2)

    e_hr3 <- sum(sc1$Prob3)
    z3 <- (hr - e_hr3) / sqrt(e_hr3)

    data.frame(Ball_Model = paste(input$month,
                             c(2019, 2021, 2022)),
               Observed_Rate = hr / bip,
               Predicted_Rate =  c(e_hr1, e_hr2, e_hr3) / bip,
               Z_Stat = c(z1, z2, z3))

  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE)
}

shinyApp(ui = ui, server = server)
