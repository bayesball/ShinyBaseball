# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# turn off warnings
options(warn=-1)

binning_hits <- function(scip,
                    LA_breaks = seq(-10, 50, by = 10),
                    LS_breaks = seq(60, 110, by = 10),
                    digits = 1){
  require(tidyverse)
  BIP <- nrow(scip)

  scip %>% filter(launch_speed > min(LS_breaks),
                  launch_speed < max(LS_breaks),
                  launch_angle > min(LA_breaks),
                  launch_angle < max(LA_breaks)) %>%
    mutate(ctheta = cut(launch_angle,
                        LA_breaks),
           cv0 = cut(launch_speed,
                     LS_breaks)) -> scip
  # find counts and HRs in each bin
  scip %>%
    group_by(ctheta, cv0) %>%
    summarize(N = n(),
              HR = sum(HR),
              H = sum(H),
              H_noHR = H - HR,
              PH = round(100 * H / N, digits),
              PHR = round(100 * HR / N, digits),
              PH_noHR = round(100 * H_noHR / N, digits),
              .groups = "drop") -> S

  convert_string <- function(y){
    y1 <- gsub("[,(]", " ", y)
    y2 <- gsub("[][]", "", y1)
    y3 <- gsub("^ ", "", y2)
    mean(as.numeric(str_split(y3, " ")[[1]]))
  }

  S$theta <- sapply(S$ctheta, convert_string)
  S$v0 <- sapply(S$cv0, convert_string)

  S
}

# plot hit rates with percentages

pct_plot <- function(S, title = "",
                     type = "H"){

  ggpart <- ggplot(S, aes(theta, v0)) +
    xlim(-10, 50) + ylim(60, 110) +
    increasefont() +
    centertitle() +
    xlab("Launch Angle (degrees)") +
    ylab("Exit Velocity (mph)") +
    geom_hline(yintercept = seq(60, 110, by = 10),
               linetype = "dashed") +
    geom_vline(xintercept = seq(-10, 50, by = 10),
               linetype = "dashed") +
    scale_color_manual(values = c("black", "red")) +
    theme(legend.position = "none") +
    theme(text = element_text(size = 18),
          plot.title = element_text(colour = "blue",
                size = 18,
                hjust = 0.5, vjust = 0.8, angle = 0))

  if(type == "H"){
    p7 <- ggpart +
      geom_text(aes(label = PH,
                    color = PH >= 50),
                size = 6, nudge_y = 0) +
      ggtitle(paste("Hit Percentages", title))
  }
  if(type == "HR"){
    p7 <- ggpart +
      geom_text(aes(label = PHR,
                    color = PHR >= 50),
                size = 6, nudge_y = 0) +
      ggtitle(paste("Home Run Percentages", title))
  }
  if(type == "H_noHR"){
    p7 <- ggpart +
      geom_text(aes(label = PH_noHR,
                    color = PH_noHR >= 50),
                size = 6, nudge_y = 0) +
      ggtitle(paste("Hit (not HR) Percentages", title))
  }
  p7
}

# computes z-scores for two sets of home run rates
compute_z_stat <- function(B1, B2, type){
  if(type == "H"){
    p1 <- B1$H / B1$N
    p2 <- B2$H / B2$N
    p <- (B1$H + B2$H) / (B1$N + B2$N)
    N1 <- B1$N
    N2 <- B2$N
  }
  if(type == "HR"){
    p1 <- B1$HR / B1$N
    p2 <- B2$HR / B2$N
    p <- (B1$HR + B2$HR) / (B1$N + B2$N)
    N1 <- B1$N
    N2 <- B2$N
  }
  if(type == "H no HR"){
    p1 <- (B1$H - B1$HR) / B1$N
    p2 <- (B2$H - B2$HR) / B2$N
    p <- (B1$H + B2$H - B1$HR - B2$HR) / (B1$N + B2$N)
    N1 <- B1$N
    N2 <- B2$N
  }
  B <- select(B1, theta, v0)
  B$Z <- round((p2 - p1) / sqrt(p * (1 - p) *
                                  (1 / N1 + 1 / N2)), 1)
  B
}

# plots the z-scores
z_plot <- function(S, title = ""){
  S$sign <- S$Z > 0
  ggplot(S, aes(theta, v0, label=Z, color = sign)) +
    geom_text(size=6) +
    xlim(-10, 50) + ylim(60, 110) +
    increasefont() +
    ggtitle(paste("Z Stat", title)) +
    centertitle() +
    xlab("Launch Angle (degrees)") +
    ylab("Exit Velocity (mph)") +
    geom_hline(yintercept = seq(60, 110, by = 10),
               linetype = "dashed") +
    geom_vline(xintercept = seq(-10, 50, by = 10),
               linetype = "dashed") +
    scale_color_manual(values = c("red", "blue")) +
    theme(legend.position = "none") +
    theme(text = element_text(size = 18),
          plot.title = element_text(colour = "blue",
                    size = 18,
            hjust = 0.5, vjust = 0.8, angle = 0))
}

# plots change in percentages in Hit rates
two_p_plot <- function(B1, B2, title = "",
                       type = "H"){
  S <- select(B1, theta, v0)
  if(type == "H"){
    S$change <- round(B2$PH - B1$PH, 2)
  }
  if(type == "HR"){
    S$change <- round(B2$PHR - B1$PHR, 3)
  }
  if(type == "H_noHR"){
    S$change <- round(B2$PH_noHR - B1$PH_noHR, 2)
  }
  S$sign <- S$change > 0
  ggplot(S, aes(theta, v0, label=change, color = sign)) +
    geom_text(size=6) +
    xlim(-10, 50) + ylim(60, 110) +
    increasefont() +
    ggtitle(paste("Change in", type,
                  "Percentage", title)) +
    centertitle() +
    xlab("Launch Angle (degrees)") +
    ylab("Exit Velocity (mph)") +
    geom_hline(yintercept = seq(60, 110, by = 10),
               linetype = "dashed") +
    geom_vline(xintercept = seq(-10, 50, by = 10),
               linetype = "dashed") +
    scale_color_manual(values = c("red", "blue")) +
    theme(legend.position = "none") +
    theme(text = element_text(size = 18),
          plot.title = element_text(colour = "blue",
                        size = 18,
               hjust = 0.5, vjust = 0.8, angle = 0))
}

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

# read in statcast dataset
sc <- data_work()

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  h3("In-Play Hit and Home Run Rates"),
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
      radioButtons("type",
                   label = "Select In-Play Event:",
                   choices = c("H", "HR", "H no HR"),
                   selected = "H",
                   inline = TRUE),
      radioButtons("round",
                   label = "Round Percentages?",
                   choices = c("Yes", "No"),
                   selected = "Yes",
                   inline = TRUE),
      downloadButton("downloadData", "Download Rates")
    )),
    column(8,
           tabsetPanel(type = "tabs",
                tabPanel("Pct Season 1",
                     plotOutput("plot1",
                        height = "450px")
                        ),
                tabPanel("Pct Season 2",
                     plotOutput("plot2",
                        height = "450px")
                        ),
                tabPanel("Difference in Pcts",
                     plotOutput("plot3",
                        height = "450px")
                        ),
                tabPanel("Z-Score",
                     plotOutput("plot4",
                        height = "450px")
                        )
                )
      )
      )
)

server <- function(input, output, session) {

  output$plot1 <- renderPlot({
    digits <- ifelse(input$round == "Yes", 0, 1)
    out1 <- binning_hits(filter(sc,
                        Season == input$year1),
                        digits = digits)
    if(input$type == "H"){
       the_plot <- pct_plot(out1, title = input$year1,
                type = "H")
    }
    if(input$type == "HR"){
      the_plot <- pct_plot(out1, title = input$year1,
               type = "HR")
    }
    if(input$type == "H no HR"){
      the_plot <- pct_plot(out1, title = input$year1,
               type = "H_noHR")
    }
    the_plot
  }, res = 96)

  output$plot2 <- renderPlot({
    digits <- ifelse(input$round == "Yes", 0, 1)
    out2 <- binning_hits(filter(sc,
                          Season == input$year2),
                         digits = digits)
    if(input$type == "H"){
      the_plot <- pct_plot(out2, title = input$year2,
                           type = "H")
    }
    if(input$type == "HR"){
      the_plot <- pct_plot(out2, title = input$year2,
                           type = "HR")
    }
    if(input$type == "H no HR"){
      the_plot <- pct_plot(out2, title = input$year2,
                           type = "H_noHR")
    }
    the_plot
  }, res = 96)

  output$plot3 <- renderPlot({
    out1 <- binning_hits(filter(sc,
                                Season == input$year1))
    out2 <- binning_hits(filter(sc,
                                Season == input$year2))
    the_title <- paste(input$year2, "vs", input$year1)
    if(input$type == "H"){
      the_plot <- two_p_plot(out1, out2, the_title,
                           type = "H")
    }
    if(input$type == "HR"){
      the_plot <- two_p_plot(out1, out2, the_title,
                           type = "HR")
    }
    if(input$type == "H no HR"){
      the_plot <- two_p_plot(out1, out2, the_title,
                           type = "H_noHR")
    }
    the_plot
  }, res = 96)

  output$plot4 <- renderPlot({
    out1 <- binning_hits(filter(sc, Season == input$year1))
    out2 <- binning_hits(filter(sc, Season == input$year2))
    B <- compute_z_stat(out1, out2, type = input$type)
    the_title <- paste(input$type,
                       input$year2, "vs", input$year1)
    z_plot(B, the_title)
  }, res = 96)

  output$downloadData <- downloadHandler(
    filename = "rates_output.csv",
    content = function(file) {
      out1 <- binning_hits(filter(sc,
                                  Season == input$year1))
      out2 <- binning_hits(filter(sc,
                                  Season == input$year2))
      out12 <- inner_join(out1, out2,
                  by = c("ctheta" = "ctheta",
                        "cv0"="cv0"))
      write.csv(out12[, 1:18], file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
