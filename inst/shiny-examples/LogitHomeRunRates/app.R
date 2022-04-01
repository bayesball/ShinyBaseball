# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(gridExtra)

# data is read from Github repository

# turn off warnings
options(warn=-1)

data_work <- function(){
  require(readr)
  require(dplyr)
  require(lubridate)

  sc_2021 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast2021.csv")
  sc_old <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/SC_BB_mini.csv")

  names(sc_old)[2] <- "Game_Date"

  hits <- c("single", "double", "triple",
            "home_run")
  sc_2021 %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% hits, 1, 0)) %>%
    select(game_year, Game_Date, launch_angle,
           launch_speed, events, HR, H) -> sc_2021

  sc <- rbind(sc_old, sc_2021)

  sc %>%
    mutate(Season = year(Game_Date))
}

logit_work <- function(sc, LA_breaks, LS_breaks,
                       season1, season2){
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(lubridate)
  library(stringr)

  # some helper functions

  increasefont <- function (){
    theme(text = element_text(size = 16))
  }
  centertitle <- function (){
    theme(plot.title = element_text(
      colour = "white", size = 14,
      face = "bold",
      hjust = 0.5, vjust = 0.8, angle = 0),
      plot.subtitle = element_text(
        colour = "white", size = 12,
        face = "bold",
        hjust = 0.5, vjust = 0.8, angle = 0))
  }

  sc %>%
    mutate(LA = cut(launch_angle,
                    LA_breaks),
           LS = cut(launch_speed,
                    LS_breaks)) -> sc

  sc %>%
    filter(is.na(LA) == FALSE,
           is.na(LS) == FALSE) %>%
    group_by(Season, LA, LS) %>%
    summarize(N = n(),
              HR = sum(HR),
              .groups = "drop") -> S

  sc %>%
    group_by(Season) %>%
    summarize(IP = n()) -> S1

  inner_join(S, S1, by = "Season") -> S

  convert_string <- function(y){
    y1 <- gsub("[,(]", " ", y)
    y2 <- gsub("[][]", "", y1)
    y3 <- gsub("^ ", "", y2)
    mean(as.numeric(str_split(y3, " ")[[1]]))
  }

  S$la <- sapply(S$LA, convert_string)
  S$ls <- sapply(S$LS, convert_string)

  ###################### compare logits work

  S %>%
    mutate(p_inplay = N / IP,
           p_hr = HR / N,
           logit_inplay = log(p_inplay) -
             log(1 - p_inplay),
           logit_hr = log(p_hr) -
             log(1 - p_hr)) -> S

  S %>%
    filter(Season == season1) %>%
    select(Season, la, ls,
           IP, N, HR,
           p_inplay, p_hr,
           logit_inplay, logit_hr) -> S1
  S %>%
    filter(Season == season2) %>%
    select(Season, la, ls,
           IP, N, HR,
           p_inplay, p_hr,
           logit_inplay, logit_hr) -> S2

  S12 <- inner_join(S1, S2,
                    by = c("la", "ls")) %>%
    mutate(diff_inplay = logit_inplay.y -
             logit_inplay.x,
           diff_hr = logit_hr.y -
             logit_hr.x,
           Z_inplay = diff_inplay /
             sqrt(1 / IP.x / p_inplay.x / (1 - p_inplay.x) +
                  1 / IP.y / p_inplay.y / (1 - p_inplay.y)),
           Z_hr = diff_hr /
             sqrt(1 / N.x / p_hr.x / (1 - p_hr.x) +
                  1 / N.y / p_hr.y / (1 - p_hr.y)))

  the_title = paste("Logit(", season2,
                    ") Minus Logit(", season1,
                    ")", sep = "")
  the_title2 = paste("Z Score comparing ", season2,
                    " and ", season1,
                    sep = "")

  xlim_lo <- min(LA_breaks) - diff(LA_breaks)[1] / 4
  xlim_hi <- max(LA_breaks) + diff(LA_breaks)[1] / 4
  ylim_lo <- min(LS_breaks) - diff(LS_breaks)[1] / 4
  ylim_hi <- max(LS_breaks) + diff(LS_breaks)[1] / 4

  plot5 <- ggplot(S1, aes(la, ls,
                  label = N)) +
    geom_label(size = 6,
               fill = "red",
               color = "white") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    labs(title = paste("In-Play Counts:", season1),
         subtitle = paste("Total In-Play =", S1$IP[1])) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))

  plot6 <- ggplot(S1, aes(la, ls,
                          label = HR)) +
    geom_label(size = 6,
               fill = "red",
               color = "white") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    ggtitle(paste("Home Run Counts:", season1)) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))

  plot7 <- ggplot(S2, aes(la, ls,
                          label = N)) +
    geom_label(size = 6,
               fill = "red",
               color = "white") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    labs(title = paste("In-Play Counts:", season2),
         subtitle = paste("Total In-Play =", S2$IP[1])) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))

  plot8 <- ggplot(S2, aes(la, ls,
                          label = HR)) +
    geom_label(size = 6,
               fill = "red",
               color = "white") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    ggtitle(paste("Home Run Counts:", season2)) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))

  plot1 <- ggplot(S12, aes(la, ls,
                           label = round(diff_inplay, 2))) +
    geom_label(size = 6,
               aes(fill = diff_inplay > 0),
               color = "white") +
    theme(legend.position = "none") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    ggtitle(paste("In-Play Rates:", the_title)) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))

  plot2 <- ggplot(S12, aes(la, ls,
                           label = round(diff_hr, 2))) +
    geom_label(size = 6,
               aes(fill = diff_hr > 0),
               color = "white") +
    theme(legend.position = "none") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    ggtitle(paste("Home Run Rates:", the_title)) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))

  plot3 <- ggplot(S12, aes(la, ls,
                           label = round(Z_inplay, 2))) +
    geom_label(size = 6,
               aes(fill = Z_inplay > 0),
               color = "white") +
    theme(legend.position = "none") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    ggtitle(paste("In-Play Rates:", the_title2)) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))

  plot4 <- ggplot(S12, aes(la, ls,
                           label = round(Z_hr, 2))) +
    geom_label(size = 6,
               aes(fill = Z_hr > 0),
               color = "white") +
    theme(legend.position = "none") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    ggtitle(paste("Home Run Rates:", the_title2)) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    geom_vline(xintercept = LA_breaks,
               color = "blue") +
    geom_hline(yintercept = LS_breaks,
               color = "blue") +
    scale_fill_manual(values =
                        c("darkorange2",
                          "dodgerblue")) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "grey"))


 # M_inplay <- apply(matrix(round(S12$diff_inplay, 2),
 #                          length(LS_breaks) - 1,
 #                          length(LA_breaks) - 1),
 #                   2, rev)
 # dimnames(M_inplay)[[1]] <-
 #  (rev(LS_breaks) - diff(LS_breaks)[1] / 2)[-1]
 # dimnames(M_inplay)[[2]] <-
 #   (LA_breaks - diff(LA_breaks)[1] / 2)[-1]

  #M_hr <- apply(matrix(round(S12$diff_hr, 2),
  #                     length(LS_breaks) - 1,
  #                     length(LA_breaks) - 1),
  #              2, rev)
  #dimnames(M_hr)[[1]] <-
  #  (rev(LS_breaks) - diff(LS_breaks)[1] / 2)[-1]
  #dimnames(M_hr)[[2]] <-
  #  (LA_breaks - diff(LA_breaks)[1] / 2)[-1]

  list(S = S12,
       plot1 = plot1,
       plot2 = plot2,
       plot3 = plot3,
       plot4 = plot4,
       plot5 = plot5,
       plot6 = plot6,
       plot7 = plot7,
       plot8 = plot8)
}

# read in statcast dataset
sc <- data_work()

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      radioButtons("year1",
                   label = "Select First Season:",
                   choices = c("2015", "2016", "2017",
                             "2018", "2019", "2020",
                             "2021"),
                           selected = "2019",
                           inline = TRUE),
      radioButtons("year2",
                   label = "Select Second Season:",
                   choices = c("2015", "2016", "2017",
                               "2018", "2019", "2020",
                               "2021"),
                   selected = "2021",
                   inline = TRUE),
      sliderInput("rX", "Range of Launch Angle:",
                  min = 0, max = 50,
                  value = c(20, 40)),
      numericInput("nX",
                  "Number of Groups for Launch Angle:",
                  value = 4, step = 1),
      sliderInput("rY", "Range of Launch Speed:",
                  min = 80, max = 120,
                  value = c(95, 110)),
      numericInput("nY",
                  "Number of Groups for Launch Speed:",
                   value = 3, step = 1),
      downloadButton("downloadData", "Download Rates")
    )),
    column(8,
           tabsetPanel(type = "tabs",
                       tabPanel("First Season",
                                plotOutput("plot1a",
                                           height = "670px")
                       ),
                       tabPanel("Second Season",
                                plotOutput("plot1b",
                                           height = "670px")
                       ),
                       tabPanel("Difference in Logits",
                                plotOutput("plot1",
                                           height = "670px")
                       ),
                       tabPanel("Z-Score",
                         plotOutput("plot2",
                         height = "670px")
           ))
      )
      )
)

server <- function(input, output, session) {
  output$plot1a <- renderPlot({
    step_LA <- diff(input$rX) / input$nX
    step_LS <- diff(input$rY) / input$nY
    LA_breaks <- seq(input$rX[1], input$rX[2],
                     by = step_LA)
    LS_breaks <- seq(input$rY[1], input$rY[2],
                     by = step_LS)
    out1 <- logit_work(sc, LA_breaks, LS_breaks,
                       as.numeric(input$year1),
                       as.numeric(input$year2))
    grid.arrange(out1$plot5,
                 out1$plot6)
  }, res = 96)

  output$plot1b <- renderPlot({
    step_LA <- diff(input$rX) / input$nX
    step_LS <- diff(input$rY) / input$nY
    LA_breaks <- seq(input$rX[1], input$rX[2],
                     by = step_LA)
    LS_breaks <- seq(input$rY[1], input$rY[2],
                     by = step_LS)
    out1 <- logit_work(sc, LA_breaks, LS_breaks,
                       as.numeric(input$year1),
                       as.numeric(input$year2))
    grid.arrange(out1$plot7,
                 out1$plot8)
  }, res = 96)

  output$plot1 <- renderPlot({
    step_LA <- diff(input$rX) / input$nX
    step_LS <- diff(input$rY) / input$nY
    LA_breaks <- seq(input$rX[1], input$rX[2],
                     by = step_LA)
    LS_breaks <- seq(input$rY[1], input$rY[2],
                     by = step_LS)
    out1 <- logit_work(sc, LA_breaks, LS_breaks,
                       as.numeric(input$year1),
                       as.numeric(input$year2))
    grid.arrange(out1$plot1,
                 out1$plot2)
  }, res = 96)

  output$plot2 <- renderPlot({
    step_LA <- diff(input$rX) / input$nX
    step_LS <- diff(input$rY) / input$nY
    LA_breaks <- seq(input$rX[1], input$rX[2],
                     by = step_LA)
    LS_breaks <- seq(input$rY[1], input$rY[2],
                     by = step_LS)
    out1 <- logit_work(sc, LA_breaks, LS_breaks,
                       as.numeric(input$year1),
                       as.numeric(input$year2))
    grid.arrange(out1$plot3,
                 out1$plot4)
  }, res = 96)

  output$downloadData <- downloadHandler(
    filename = "rates_output.csv",
    content = function(file) {
      step_LA <- diff(input$rX) / input$nX
      step_LS <- diff(input$rY) / input$nY
      LA_breaks <- seq(input$rX[1], input$rX[2],
                       by = step_LA)
      LS_breaks <- seq(input$rY[1], input$rY[2],
                       by = step_LS)
      out <- logit_work(sc, LA_breaks, LS_breaks,
                         as.numeric(input$year1),
                         as.numeric(input$year2))
      write.csv(out$S, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
