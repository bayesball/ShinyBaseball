# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# turn off warnings
options(warn=-1)

# data setup
current_date <- ymd(substr(Sys.time(), 1, 10))
date_2021_hi <- current_date - 1

# read in statcast dataset
sc_2021 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast2021.csv")
sc_2022 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2022.csv")
sc_2023 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2023.csv")
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
sc_2023 %>%
  mutate(HR = ifelse(events == "home_run", 1, 0),
         game_date = Game_Date)  %>%
  select(game_year, game_date, launch_angle,
         launch_speed, HR) ->
  scip_2023

# merge two datasets
rbind(scip[, c(1:4, 6)], scip_2021,
      scip_2022, scip_2023)  %>%
  filter(game_year %in% 2015:2023) -> scip

# want HR variable to be character
scip$HR <- ifelse(scip$HR == 1,
                  "YES", "NO")

bin_work <- function(scip, la_lo = 15, la_hi = 45,
                     ls_lo = 90, ls_hi = 115, width = 5,
                     season1 = 2019, season2 = 2021){

  scip %>% filter(game_year == season1) %>%
    summarize(N1 = n()) %>% pull() -> N1
  scip %>% filter(game_year == season2) %>%
    summarize(N1 = n()) %>% pull() -> N2

  # left endpoint is not included, right endpoint is included
  scip %>%
    filter(launch_angle > la_lo,
           launch_angle <= la_hi,
           launch_speed > ls_lo,
           launch_speed <= ls_hi) -> scip2

  la_breaks <- seq(la_lo, la_hi, by = width)
  la_center <- la_breaks[-length(la_breaks)] +
    diff(la_breaks[1:2]) / 2
  ls_breaks <- seq(ls_lo, ls_hi, by = width)
  ls_center <- ls_breaks[-length(ls_breaks)] +
    diff(ls_breaks[1:2]) / 2

  scip2$la_c <- cut(scip2$launch_angle,
                    breaks = la_breaks)
  scip2$ls_c <- cut(scip2$launch_speed,
                    breaks = ls_breaks)
  ######### season 1
  scip2 %>%
    filter(game_year == season1) %>%
    group_by(la_c, ls_c,
             .drop = FALSE) %>%
    summarize(N = N1,
              BIP = n(),
              HR = sum(HR == "YES", na.rm = TRUE),
              .groups = "drop") %>%
    select(la_c, ls_c, N, BIP, HR) -> OUT1

  E <- expand.grid(LS = ls_center,
                   LA = la_center)
  OUT1$LS <- E$LS
  OUT1$LA <- E$LA
  OUT1 %>% select(LA, LS, N, BIP, HR) -> OUT1

  ######### season 2
  scip2 %>%
    filter(game_year == season2) %>%
    group_by(la_c, ls_c,
             .drop = FALSE) %>%
    summarize(N = N2,
              BIP = n(),
              HR = sum(HR == "YES", na.rm = TRUE),
              .groups = "drop") %>%
    select(la_c, ls_c, N, BIP, HR) -> OUT2

  E <- expand.grid(LS = ls_center,
                   LA = la_center)
  OUT2$LS <- E$LS
  OUT2$LA <- E$LA
  OUT2 %>% select(LA, LS, N, BIP, HR) -> OUT2
  ############

  OUT <- cbind(OUT1, OUT2)
  OUT <- OUT[, c(1:5, 8:10)]
  names(OUT) <- c("LA", "LS", "N1", "BIP1", "HR1",
                  "N2", "BIP2", "HR2")
  OUT
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4(id="big-heading", "Compare Home Run Rates"),
      dateInput("date_lo",
                label = "Select Starting Date:",
                value = date_2021_lo),
      dateInput("date_hi",
                label = "Select Ending Date:",
                value = date_2021_hi),
      radioButtons("year1",
                   label = "Select First Season:",
                   choices = c("2015", "2016", "2017",
                             "2018", "2019", "2020",
                             "2021", "2022", "2023"),
                           selected = "2019",
                           inline = TRUE),
      radioButtons("year2",
                   label = "Select Second Season:",
                   choices = c("2015", "2016", "2017",
                               "2018", "2019", "2020",
                               "2021", "2022", "2023"),
                   selected = "2021",
                   inline = TRUE),
      radioButtons("stat",
                   label = "Select Statistic:",
                   choices = c("difference in PCT", "Z"),
                   selected = "difference in PCT",
                   inline = TRUE)
    )),
    column(8,
      tabsetPanel(type = "tabs",
      tabPanel("Batted Ball Rates",
         plotOutput("plot1",
                 height = "450px")
      ),
      tabPanel("Home Run Rates",
          plotOutput("plot2",
                     height = "450px")
      ),
      tabPanel("Information",
               br(), br(),
               p("This Shiny app explores patterns
      of hard-hit ball and home run rates over the Statcast era.  One selects a
      range of days and two Statcast seasons to compare.
      The region of
      launch angle and exit velocity values is divided into thirty bins and the
      number of balls in play and the home run count is recorded for each bin for
      each season.
       By choosing the Batted Ball Rates tab, one compares the percentages of
      batted balls in each bin for the two seasons.  By selecting the Home Run
      Rates tab, one compares the percentages of home runs among balls put into play in each bin for the
      two seasons.  One can gauge the significance of the difference in
      percentages by a Z statistic.  Values of Z larger than 2 in
      absolute value are significant.
      The Data tab shows the data with the total number of balls in
      play, the count of balls in play and home runs for each bin for
      each season."),
      p("This app will reflect current 2021 data as the season
        progresses."),
      p("All data is made available through Baseball Savant through the
        website http://baseballsavant.mlb.com.  The data was scraped using
        the baseballr package written by Bill Petti.  Information about Shiny
        apps can be found at https://shiny.rstudio.com/"),
      p("Any questions?  Contact Jim Albert at albert@bgsu.edu")
  ),
  tabPanel("Data",
        tableOutput("table1")
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

    the_title <- paste("Batted Balls from ", md1, " to ",
                       md2, sep = "")
    if(input$stat == "difference in PCT"){
      the_subtitle <- paste("BB Rate % (",
                            input$year2, ") - BB Rate % (",
                            input$year1, ")", sep = "")
    } else {
      the_subtitle <- paste("Z Score comparing ",
                            input$year2, " and ",
                            input$year1, " Rates", sep = "")
    }

    out <- bin_work(scipR,
                  season1 = as.numeric(input$year1),
                  season2 = as.numeric(input$year2))

    z_score <- function(y1, n1, y2, n2){
      p1 <- y1 / n1
      p2 <- y2 / n2
      p <- (y1 + y2) / (n1 + n2)
      (p2 - p1) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
    }
    out %>%
      mutate(Diff2 =
          round(100 * (HR2 / BIP2 - HR1 / BIP1), 1),
        Diff1 =
          round(100 * (BIP2 / N2 - BIP1 / N1), 2),
        Z2 = round(z_score(HR1, BIP1, HR2, BIP2), 2),
        Z1 = round(z_score(BIP1, N1, BIP2, N2), 2)) -> out

    if(input$stat == "Z"){
      out %>%
        filter(is.na(Z1) == FALSE) -> out
    }
    if(input$stat == "difference in PCT"){
      out %>%
        filter(is.na(Diff1) == FALSE) -> out
    }

    if(input$stat == "difference in PCT"){
      p1 <- ggplot(out, aes(LA, LS, label = Diff1)) +
        geom_label(aes(fill = Diff1 > 0),
                   size = 4)
    } else {
      p1 <- ggplot(out, aes(LA, LS, label = Z1)) +
        geom_label(aes(fill = Z1 > 0),
                   size = 4)
    }

    p1 +
      theme(text=element_text(size=18)) +
      xlim(15, 45) + ylim(90, 115) +
      labs(title = the_title,
           subtitle = the_subtitle) +
      xlab("Launch Angle (degrees)") +
      ylab("Exit Velocity (mph)") +
      theme(plot.title =
              element_text(colour = "blue", size = 18,
               hjust = 0.5, vjust = 0.8, angle = 0),
            plot.subtitle =
              element_text(colour = "blue", size = 18,
               hjust = 0.5, vjust = 0.8, angle = 0)) +
      theme(text=element_text(size=18))
  }, res = 96)

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

    the_title <- paste("Home Runs from ", md1, " to ",
                       md2, sep = "")
    if(input$stat == "difference in PCT"){
      the_subtitle <- paste("HR Rate % (",
                          input$year2, ") - HR Rate % (",
                          input$year1, ")", sep = "")
    } else {
      the_subtitle <- paste("Z Score comparing ",
                            input$year2, " and ",
                            input$year1, " Rates", sep = "")
    }

    out <- bin_work(scipR,
                    season1 = as.numeric(input$year1),
                    season2 = as.numeric(input$year2))

    z_score <- function(y1, n1, y2, n2){
      p1 <- y1 / n1
      p2 <- y2 / n2
      p <- (y1 + y2) / (n1 + n2)
      (p2 - p1) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
    }
    out %>%
      mutate(Diff2 =
               round(100 * (HR2 / BIP2 - HR1 / BIP1), 1),
             Diff1 =
               round(100 * (BIP2 / N2 - BIP1 / N1), 1),
             Z2 = round(z_score(HR1, BIP1, HR2, BIP2), 2),
             Z1 = round(z_score(BIP1, N1, BIP2, N2), 2)) -> out

    if(input$stat == "Z"){
    out %>%
      filter(is.na(Z2) == FALSE) -> out
    }
    if(input$stat == "difference in PCT"){
      out %>%
        filter(is.na(Diff2) == FALSE) -> out
    }

    if(input$stat == "difference in PCT"){
      p1 <- ggplot(out, aes(LA, LS, label = Diff2)) +
        geom_label(aes(fill = Diff2 > 0),
                   size = 4)
    } else {
      p1 <- ggplot(out, aes(LA, LS, label = Z2)) +
        geom_label(aes(fill = Z2 > 0),
                   size = 4)
    }

    p1 +
      theme(text=element_text(size=18)) +
      xlim(15, 45) + ylim(90, 115) +
      labs(title = the_title,
           subtitle = the_subtitle) +
      xlab("Launch Angle (degrees)") +
      ylab("Exit Velocity (mph)") +
      theme(plot.title =
              element_text(colour = "blue", size = 18,
                           hjust = 0.5, vjust = 0.8, angle = 0),
            plot.subtitle =
              element_text(colour = "blue", size = 18,
                           hjust = 0.5, vjust = 0.8, angle = 0)) +
      theme(text=element_text(size=18))
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

    out <- bin_work(scipR,
                    season1 = as.numeric(input$year1),
                    season2 = as.numeric(input$year2))
    names(out)[3:8] <- c("N_y1", "BIP_y1", "HR_y1",
                         "N_y2", "BIP_y2", "HR_y2")
    out

  })

}

shinyApp(ui = ui, server = server)
