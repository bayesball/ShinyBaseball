# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(xtable)

# turn off warnings
options(warn=-1)

# data setup
current_date <- ymd(substr(Sys.time(), 1, 10))
date_2021_hi <- current_date - 1

# read in statcast dataset
sc_2021 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast2021.csv")
sc_2022 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2022.csv")
scip <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/SC_BB_mini.csv")

# adjust 2021 date if necessary
date_2021_hi <- min(date_2021_hi,
                 max(ymd(sc_2021$Game_Date)))
date_2021_lo <- ymd("2021-04-01")

# create in-play 2021 dataset
sc_2021 %>%
 mutate(HR = ifelse(events == "home_run", 1, 0),
        game_date = Game_Date)  %>%
  select(game_year, game_date, launch_angle,
         launch_speed, HR) ->
 scip_2021

sc_2022 %>%
  mutate(HR = ifelse(events == "home_run", 1, 0),
         game_date = Game_Date)  %>%
  select(game_year, game_date, launch_angle,
         launch_speed, HR) ->
  scip_2022

# merge two datasets
rbind(scip[, c(1:4, 6)], scip_2021, scip_2022)  %>%
  filter(game_year %in% 2015:2022) -> scip

# want HR variable to be character
scip$HR <- ifelse(scip$HR == 1,
                  "YES", "NO")

# create sample of 10,000 values to graph
sc_10000 <- sample_n(scip, size = 10000)

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4(id="big-heading", "Home Run Rates in
         Statcast Era"),
      dateInput("date_lo",
                label = h6("Choose Starting Date:"),
                value = date_2021_lo),
      dateInput("date_hi",
                label = h6("Choose Ending Date:"),
                value = date_2021_hi),
      sliderInput("LA",
                  h6("Choose Range of Launch Angle:"),
                  min = 15,
                  max = 50,
                  value = c(15, 50)),
      sliderInput("EV",
                  h6("Choose Range of Exit Velocity:"),
                  min = 90,
                  max = 120,
                  value = c(90, 120))
#     actionButton("goButton", "Update Table")
    )),
    column(8,
      tabsetPanel(type = "tabs",
      tabPanel("Table of Rates",
         plotOutput("plot1",
                 height = "330px"),
         tableOutput("table1")
      ),
      tabPanel("Graph of Rates",
          plotOutput("plot2",
                     height = "500px")
      ),
      tabPanel("Information",
               br(), br(),
               p("This Shiny app explores patterns
      of hard-hit ball and home run rates over the Statcast era.  One selects a
      range of days and chooses a rectangular region of values of
      the launch angle and exit velocity.  For each of the
      Statcast seasons, the Table of Rates tab displays the count and rate of batted
      balls in play
      and the count and rate of home runs from the selected region.
       By choosing the Graph of Rates tab, one sees a graph of the batted ball
        rates and HR rates plotted against season."),
      p("This app will reflect current 2021 data as the season
        progresses."),
      p("All data is made available through Baseball Savant through the
        website http://baseballsavant.mlb.com.  The data was scraped using
        the baseballr package written by Bill Petti.  Information about Shiny
        apps can be found at https://shiny.rstudio.com/"),
      p("Any questions?  Contact Jim Albert at albert@bgsu.edu")
  )
      )
      )
    ))

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    md1 <- paste(month(input$date_lo), "-",
                 day(input$date_lo), sep = "")
    md2 <- paste(month(input$date_hi), "-",
                 day(input$date_hi), sep = "")
    scip %>%
      mutate(date1 = ymd(paste(game_year, "-", md1,
                               sep = "")),
             date2 = ymd(paste(game_year, "-", md2,
                               sep = ""))) %>%
      filter(game_date >= date1,
             game_date <= date2) -> scipR

#    scnew <- sample_n(scipR, size = 10000)

    md1 <- paste(month(input$date_lo), "-",
                 day(input$date_lo), sep = "")
    md2 <- paste(month(input$date_hi), "-",
                 day(input$date_hi), sep = "")
    the_title <- paste("Batted Balls from ", md1, " to ",
                       md2, sep = "")

    rect <- data.frame(x = c(input$LA[1], input$LA[2],
          input$LA[2], input$LA[1], input$LA[1]),
          y = c(input$EV[1], input$EV[1], input$EV[2],
                input$EV[2], input$EV[1]))

    ggplot() +
   geom_point(data = sc_10000,
               mapping = aes(launch_angle,
                             launch_speed,
                             color = HR),
              size = 1, alpha = 0.4
             ) +
    geom_path(data = rect, aes(x, y),
              color = "black", size = 1.5) +
      xlim(15, 50) + ylim(90, 120) +
      ggtitle(the_title) +
      xlab("Launch Angle (degrees)") +
      ylab("Exit Velocity (mph)") +
      theme(plot.title =
              element_text(colour = "white", size = 18,
               hjust = 0.5, vjust = 0.8, angle = 0)) +
      theme(text=element_text(size=18)) +
      scale_color_manual(values = c("orange", "blue")) +
      theme(plot.background = element_rect(fill =
                                  "coral3"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white")) +
      theme(
        panel.background = element_rect(fill = "white",
                                        colour = "grey")) +
      theme(legend.background = element_rect(
                               fill="coral3",
                            size=0.5, linetype="solid",
                              colour ="darkblue"))
  }, res = 96)

  output$table1 <- renderTable({

      md1 <- paste(month(input$date_lo), "-",
                 day(input$date_lo), sep = "")
      md2 <- paste(month(input$date_hi), "-",
                 day(input$date_hi), sep = "")

      scip %>%
        mutate(date1 = ymd(paste(game_year, "-", md1,
                                 sep = "")),
               date2 = ymd(paste(game_year, "-", md2,
                                 sep = ""))) %>%
       filter(game_date >= date1,
              game_date <= date2) -> scipR

      scipR %>%
        group_by(game_year) %>%
        summarize(N = n()) -> S1

      sc1 <- filter(scipR,
                    launch_angle >= input$LA[1],
                    launch_angle <= input$LA[2],
                    launch_speed >= input$EV[1],
                    launch_speed <= input$EV[2])
 #     sc1 <- brushedPoints(scipR,
 #                          input$plot_brush)
      la_lo <- input$LA[1]
      la_hi <- input$LA[2]
      ls_lo <- input$EV[1]
      ls_hi <- input$EV[2]

      label <- paste(round(la_lo, 1), "< LA <",
                     round(la_hi, 1),
                     ", ",
                     round(ls_lo, 1),
                     "< LS <",
                     round(ls_hi, 1), sep="")

      sc1 %>%
        group_by(game_year) %>%
        summarize(BIP = n(),
                  HR = sum(HR == "YES",
                           na.rm = TRUE)) %>%
        inner_join(S1, by = "game_year") %>%
        mutate(LA_lo = as.character(round(la_lo, 1)),
               LA_hi = as.character(round(la_hi, 1)),
               LS_lo = as.character(round(ls_lo, 1)),
               LS_hi = as.character(round(ls_hi, 1)),
               Rate = 100 * BIP / N,
               HR_Rate = as.character(
                    round(100 * HR / BIP, 1)),
               Season = as.character(game_year)) %>%
        select(Season,
               LA_lo, LA_hi, LS_lo, LS_hi,
               BIP, Rate, HR, HR_Rate)
  }, digits = 2)

  output$plot2 <- renderPlot({

    md1 <- paste(month(input$date_lo), "-",
                 day(input$date_lo), sep = "")
    md2 <- paste(month(input$date_hi), "-",
                 day(input$date_hi), sep = "")

    scip %>%
      mutate(date1 = ymd(paste(game_year, "-", md1,
                               sep = "")),
             date2 = ymd(paste(game_year, "-", md2,
                               sep = ""))) %>%
      filter(game_date >= date1,
             game_date <= date2) -> scipR

    scipR %>%
      group_by(game_year) %>%
      summarize(N = n()) -> S1

    sc1 <- filter(scipR,
                  launch_angle >= input$LA[1],
                  launch_angle <= input$LA[2],
                  launch_speed >= input$EV[1],
                  launch_speed <= input$EV[2])
 #   sc1 <- brushedPoints(scipR,
 #                        input$plot_brush)
    la_lo <- input$LA[1]
    la_hi <- input$LA[2]
    ls_lo <- input$EV[1]
    ls_hi <- input$EV[2]

    label <- paste(round(la_lo, 1), "≤ Launch Angle ≤",
                   round(la_hi, 1),
                   ", ",
                   round(ls_lo, 1),
                   "≤ Exit Velocity ≤",
                   round(ls_hi, 1), sep=" ")
    label2 <- paste("Dates: ", md1, " to ",
                       md2, sep = "")

    sc1 %>%
      group_by(game_year) %>%
      summarize(BIP = n(),
                HR = sum(HR == "YES",
                         na.rm = TRUE)) %>%
      inner_join(S1, by = "game_year") %>%
      mutate(Rate = round(100 * BIP / N, 2),
             HR_Rate = round(100 * HR / BIP, 1),
             Season = game_year) -> out
      out1 <- data.frame(Season = as.numeric(out$Season),
                         Rate = out$Rate,
                         Type = "Balls in Play Rate")
      out2 <- data.frame(Season = as.numeric(out$Season),
                         Rate = out$HR_Rate,
                         Type = "Home Run Rate")
      ggplot(rbind(out1, out2),
             aes(Season, Rate)) +
        geom_point(color = "red", size = 4) +
        facet_wrap(~ Type, ncol = 1,
                   scales = "free_y") +
        labs(title = label2,
             subtitle = label) +
        ylab("Rate (Pct)") +
        theme(plot.title =
                element_text(colour = "blue", size = 18,
                 hjust = 0.5, vjust = 0.8, angle = 0),
              plot.subtitle =
                element_text(colour = "blue", size = 18,
                             hjust = 0.5, vjust = 0.8, angle = 0)) +
        theme(text=element_text(size=18)) +
        theme(strip.text.x = element_text(size=16, face="bold"),
              strip.background = element_rect(colour="red",
                                              fill="#CCCCFF"))
  })
}

shinyApp(ui = ui, server = server)
