# shiny app to visualize rates over the zone for a hitter

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# turn off warnings

# source in two functions
bin_FF_locations_B <- function(sc, plateX, plateZ){
  # inputs:
  # ------------------------------------
  # sc - statcast data with variables plate_x,
  # plate_z, description, p_throws, type, events
  # plateX = c(plateX_lo, plateX_hi, width_X)
  # plateZ = c(plateZ_lo, plateZ_hi, width_Z)

  require(dplyr)

  sc %>% filter(p_throws %in% c("L", "R")) -> sc
  # compute number of FF pitches thrown to each side
  NT <- sc %>%
    group_by(p_throws) %>%
    summarize(N = n())

  swing_situations <- c("hit_into_play",
                        "foul", "swinging_strike",
                        "swinging_strike_blocked",
                        "missed_bunt",
                        "hit_into_play_no_out", "foul_bunt",
                        "foul_tip", "hit_into_play_score")

  miss_situations <- c("swinging_strike",
                       "swinging_strike_blocked")

  hits <- c("single",
            "double", "triple", "home_run")

  # define Swing, Miss, InPlay, Hit, and HR variables
  sc %>%
    mutate(Swing = ifelse(description %in%
                            swing_situations, 1, 0),
           Miss = ifelse(description %in%
                           miss_situations, 1, 0),
           InPlay = ifelse(type == "X", 1, 0),
           Hit = ifelse(events %in% hits, 1, 0),
           HR = ifelse(events == "home_run", 1, 0)) -> sc

  plate_x_lo <- plateX[1]
  plate_x_hi <- plateX[2]
  width_x <- plateX[3]
  plate_z_lo <- plateZ[1]
  plate_z_hi <- plateZ[2]
  width_z <- plateZ[3]

  # NOTE: left endpoint is not included but
  # right endpoint is included

  # focus on pitches thrown within zone
  sc %>%
    filter(plate_x > plate_x_lo,
           plate_x <= plate_x_hi,
           plate_z > plate_z_lo,
           plate_z <= plate_z_hi) -> sc2

  # set up breakpoints for bins
  px_breaks <- seq(plate_x_lo, plate_x_hi,
                   by = width_x)
  pz_breaks <- seq(plate_z_lo, plate_z_hi,
                   by = width_z)

  # bin the values of plate_x and plate_z
  sc2$px_c <- cut(sc2$plate_x,
                  breaks = px_breaks)
  sc2$pz_c <- cut(sc2$plate_z,
                  breaks = pz_breaks)
  sc2 %>%
    filter(is.na(px_c) == FALSE,
           is.na(pz_c) == FALSE) -> sc2

  # function to extract midpoint of a bin interval
  myf <- function(y){
    mean(as.numeric(unlist(strsplit(
      gsub("\\(|\\]", "", as.character(y)),
      ","))))
  }

  # work on pitches thrown to right-handed batters
  # compute counts in bins
  sc2 %>%
    filter(p_throws == "R") %>%
    group_by(px_c, pz_c,
             .drop = FALSE) %>%
    summarize(NT = NT$N[NT$p_throws == "R"],
              N = n(),
              Swing = sum(Swing, na.rm = TRUE),
              Miss = sum(Miss, na.rm = TRUE),
              InPlay = sum(InPlay, na.rm = TRUE),
              Hit = sum(Hit, na.rm = TRUE),
              HR = sum(HR, na.rm = TRUE),
              .groups = "drop")  ->  OUT_R
  # add bin midpoints
  OUT_R$PX <- sapply(OUT_R$px_c, myf)
  OUT_R$PZ <- sapply(OUT_R$pz_c, myf)
  OUT_R$p_throws <- "Right"

  # now left-handed batters
  sc2 %>%
    filter(p_throws == "L") %>%
    group_by(px_c, pz_c,
             .drop = FALSE) %>%
    summarize(NT = NT$N[NT$p_throws == "L"],
              N = n(),
              Swing = sum(Swing, na.rm = TRUE),
              Miss = sum(Miss, na.rm = TRUE),
              InPlay = sum(InPlay, na.rm = TRUE),
              Hit = sum(Hit, na.rm = TRUE),
              HR = sum(HR, na.rm = TRUE),
              .groups = "drop") -> OUT_L
  # add bin midpoints
  OUT_L$PX <- sapply(OUT_L$px_c, myf)
  OUT_L$PZ <- sapply(OUT_L$pz_c, myf)
  OUT_L$p_throws <- "Left"

  # combine left and right data frames
  OUT <- rbind(OUT_L, OUT_R)
  # compute five rates
  OUT %>%
    mutate(P1 = 100 * N / NT,     # location rate
           P2 = 100 * Swing / N,  # swing rate
           P3 = 100 * Miss / Swing, # miss rate
           P4 = ifelse(InPlay > 0,
                       Hit / InPlay, 0),      # BABIP
           P5 = ifelse(InPlay > 0,
                       100 * HR / InPlay, 0)) ->  # HR rate
    OUT

  OUT %>% select(p_throws, PX, PZ, NT, N,
                 Swing, Miss, InPlay, Hit, HR,
                 P1, P2, P3, P4, P5)
}

plot_rates_B <- function(out,
                         title = "",
                         subtitle = "",
                         digits  = 0,
                         label_size = 5){
  # inputs:
  # - data frame with four variables PX, PZ, p_throws, PCT
  # - title and subtitle of graph
  # - number of digits to right of decimal in output

  out$PCT <- round(out$PCT, digits)
  out$Sign <- ifelse(out$PCT > 0, "pos", "neg")
  out$p_throws <- factor(out$p_throws,
                         levels = c("Right", "Left"),
                         labels = c("Right-Handed Pitchers",
                                    "Left-Handed Pitchers"))
  if(mean(out$PCT >= 0) == 1){
    p1 <- ggplot() +
      geom_label(data = filter(out, PX > 0),
                 aes(PX, PZ, label = PCT),
                 size = label_size,
                 fill = "salmon",
                 color = "black") +
      geom_label(data = filter(out, PX < 0),
                 aes(PX, PZ, label = PCT),
                 size = label_size,
                 fill = "salmon",
                 color = "black")} else {
                   p1 <- ggplot() +
                     geom_label(data = filter(out, PX > 0),
                                aes(PX, PZ, label = PCT,
                                    fill = Sign),
                                size = label_size,
                                color = "black") +
                     geom_label(data = filter(out, PX < 0),
                                aes(PX, PZ, label = PCT,
                                    fill = Sign),
                                size = label_size,
                                color = "black")
                 }
  p1 +
    facet_wrap(~ p_throws, ncol = 2) +
    #    coord_fixed() +
    xlim(-1, 1) + ylim(1.5, 3.5) +
    labs(title = title, subtitle = subtitle) +
    theme(text=element_text(size=16)) +
    theme(plot.title = element_text(colour = "white",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "white",
                                       size = 14,
                                       hjust = 0.5, vjust = 0.8, angle = 0),
          strip.text = element_text(
            size = 14, color = "brown",
            face = "bold")
    ) +
    theme(plot.background = element_rect(fill = "black"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                      colour = "#6D9EC1")) +
    theme(legend.background =
            element_rect(fill="bisque",
                         colour ="#6D9EC1"))
}

# read in chadwick dataset
#chadwick <- read_csv("data/chadwick.csv")

# read in statcast datasets
#FF <- read_csv("data/FF_15_20.csv")

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "cyborg"),
  fluidRow(
    column(4, wellPanel(
      h4(id="big-heading", "Batter Four-Seam Rates Over the Zone"),
      textInput("name", "Batter Name:",
                value = "Mike Trout"),
      checkboxGroupInput(
        "year",
        "Select Seasons",
        choices = c("2015", "2016", "2017",
                    "2018", "2019", "2020"),
        selected = c("2015", "2016", "2017",
                     "2018", "2019", "2020"),
        inline = TRUE),
      radioButtons("type",
                   label = "Select Type of Rate:",
                   choices = c("location",
                               "swing",
                               "miss",
                               "hit",
                               "HR"),
                   selected = "location",
                   inline = TRUE),
      downloadButton("downloadData", "Download Rates")
    )),
    column(8,
           tabsetPanel(type = "tabs",
                        tabPanel("Rates",
                         plotOutput("plot1",
                              height = "500px")
                        ),
                       tabPanel("Overall",
                                plotOutput("plot1b",
                                           height = "500px")
                       ),
                       tabPanel("Residuals",
                         plotOutput("plot2",
                              height = "500px")
                        ),
                       tabPanel("Z-Scores",
                         plotOutput("plot3",
                             height = "500px")
                       )
                      )
           )
  )
)

server <- function(input, output, session) {

  get_id <- function(st){
    names <- str_to_lower(unlist(str_split(str_squish(st), " ")))
    if(length(names) == 3){
      names <- c(paste(names[1], names[2]), names[3])
    }
    chadwick %>%
      mutate(fname = str_to_lower(name_first),
             lname = str_to_lower(name_last),
             Name = paste(name_first,
                          name_last)) %>%
      filter(fname == names[1],
             lname == names[2]) %>%
      select(key_mlbam, Name)
  }

  z_score2 <- function(y, n, p){
    se <- ifelse(n > 0,
                 sqrt(p * (1 - p) / n),
                 0)
    ifelse(n > 0, (y / n - p) / se, 0)
  }

  add_zone <- function(Color = "red"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    geom_path(aes(.data$x, .data$y), data=kZone,
              lwd=0.5, col=Color,
              linetype = "dashed")
  }

  output$plot1 <- renderPlot({
    pid <- get_id(input$name) %>%
      pull(key_mlbam)

    if(length(pid) > 0){

    df <- filter(FF_15_20, Season %in% as.numeric(input$year),
                 batter == pid)

    if(nrow(df) > 0){

    out <- bin_FF_locations_B(df, c(-0.85, 0.85, 0.425),
                              c(1.6, 3.5, 0.475))
    if(input$type == "location"){
      out$PCT <- out$P1
      p <- plot_rates_B(out,  input$name,
            paste("Four-Seam Location Percentages", "\n",
                  paste(input$year, collapse = " ")),
            digits = 1) +
        add_zone("black")
    }
    if(input$type == "swing"){
      out$PCT <- out$P2
      p <- plot_rates_B(out, input$name,
             paste("Four-Seam Swing Percentages", "\n",
                   paste(input$year, collapse = " ")),
            digits = 0) +
        add_zone("black")
    }
    if(input$type == "miss"){
      out$PCT <- out$P3
      p <- plot_rates_B(out, input$name,
            paste("Four-Seam Miss Percentages", "\n",
            paste(input$year, collapse = " ")),
           digits = 0) +
        add_zone("black")
    }
    if(input$type == "hit"){
      out$PCT <- out$P4
      p <- plot_rates_B(out, input$name,
          paste("Four-Seam Hit Avgs", "\n",
          paste(input$year, collapse = " ")),
                      digits = 3,
                      label_size = 4) +
        add_zone("black")
    }
    if(input$type == "HR"){
      out$PCT <- out$P5
      p <- plot_rates_B(out, input$name,
        paste("Four-Seam HR Percentages", "\n",
        paste(input$year, collapse = " ")),
                      digits = 1) +
        add_zone("black")
    }
    p
    }}
  }, res = 96)

  output$plot1b <- renderPlot({
    pid <- get_id(input$name) %>%
      pull(key_mlbam)

    if(length(pid) > 0){

      df <- filter(FF_15_20, Season %in% as.numeric(input$year),
                   batter == pid)

      if(nrow(df) > 0){

        dfnew <- filter(FF_15_20, Season %in%
                          as.numeric(input$year))
        out_all <- bin_FF_locations_B(dfnew,
                                      c(-0.85, 0.85, 0.425),
                                      c(1.6, 3.5, 0.475))

        if(input$type == "location"){
          out_all$PCT <- out_all$P1
          p <- plot_rates_B(out_all,  "Overall",
                            paste("Location Percentages", "\n",
                                  paste(input$year, collapse = " ")),
                            digits = 1) +
            add_zone("black")
        }
        if(input$type == "swing"){
          out_all$PCT <- out_all$P2
          p <- plot_rates_B(out_all, "Overall",
                            paste("Swing Percentages", "\n",
                                  paste(input$year, collapse = " ")),
                            digits = 0) +
            add_zone("black")
        }
        if(input$type == "miss"){
          out_all$PCT <- out_all$P3
          p <- plot_rates_B(out_all, "Overall",
                            paste("Miss Percentages", "\n",
                                  paste(input$year, collapse = " ")),
                            digits = 0) +
            add_zone("black")
        }
        if(input$type == "hit"){
          out_all$PCT <- out_all$P4
          p <- plot_rates_B(out_all, "Overall",
                            paste("Hit Avgs", "\n",
                                  paste(input$year, collapse = " ")),
                            digits = 3,
                            label_size = 4) +
            add_zone("black")
        }
        if(input$type == "HR"){
          out_all$PCT <- out_all$P5
          p <- plot_rates_B(out_all, "Overall",
                            paste("HR Percentages", "\n",
                                  paste(input$year, collapse = " ")),
                            digits = 1) +
            add_zone("black")
        }
        p
      }}
  }, res = 96)

  output$plot2 <- renderPlot({
    pid <- get_id(input$name) %>%
      pull(key_mlbam)

    if(length(pid) > 0){

      df <- filter(FF_15_20, Season %in% as.numeric(input$year),
                   batter == pid)

      if(nrow(df) > 0){

        out <- bin_FF_locations_B(df,
                          c(-0.85, 0.85, 0.425),
                          c(1.6, 3.5, 0.475))

        dfnew <- filter(FF_15_20, Season %in%
                          as.numeric(input$year))
        out_all <- bin_FF_locations_B(dfnew,
                          c(-0.85, 0.85, 0.425),
                         c(1.6, 3.5, 0.475))

        if(input$type == "location"){
          out$PCT <- out$P1 - out_all$P1
          p <- plot_rates_B(out,  input$name,
                          paste("Residuals of Location Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 1) +
            add_zone("black")
        }
        if(input$type == "swing"){
          out$PCT <- out$P2 - out_all$P2
          p <- plot_rates_B(out, input$name,
                          paste("Residuals of Swing Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 0) +
            add_zone("black")
        }
        if(input$type == "miss"){
          out$PCT <- out$P3 - out_all$P3
          p <- plot_rates_B(out, input$name,
                          paste("Residuals of Miss Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 0) +
            add_zone("black")
        }
        if(input$type == "hit"){
          out$PCT <- out$P4 - out_all$P4
          p <- plot_rates_B(out, input$name,
                          paste("Residuals of Hit Avgs", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 3,
                          label_size = 4) +
            add_zone("black")
        }
        if(input$type == "HR"){
          out$PCT <- out$P5 - out_all$P5
          p <- plot_rates_B(out, input$name,
                          paste("Residuals of HR Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 1) +
            add_zone("black")
        }
        p
      }}
  }, res = 96)

  output$plot3 <- renderPlot({
    pid <- get_id(input$name) %>%
      pull(key_mlbam)

    if(length(pid) > 0){

      df <- filter(FF_15_20, Season %in% as.numeric(input$year),
                   batter == pid)

      if(nrow(df) > 0){

        out <- bin_FF_locations_B(df,
                                c(-0.85, 0.85, 0.425),
                                c(1.6, 3.5, 0.475))

        dfnew <- filter(FF_15_20, Season %in%
                          as.numeric(input$year))
        out_all <- bin_FF_locations_B(dfnew,
                                c(-0.85, 0.85, 0.425),
                                c(1.6, 3.5, 0.475))

        if(input$type == "location"){
          out$PCT <- z_score2(out$N, out$NT,
                              out_all$P1 / 100)
          p <- plot_rates_B(out,  input$name,
                          paste("Z-Scores of Location Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 1) +
            add_zone("black")
        }
        if(input$type == "swing"){
          out$PCT <- z_score2(out$Swing, out$N,
                              out_all$P2 / 100)
          p <- plot_rates_B(out, input$name,
                          paste("Z-Scores of Swing Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 1) +
            add_zone("black")
        }
        if(input$type == "miss"){
          out$PCT <- z_score2(out$Miss, out$Swing,
                              out_all$P3 / 100)
          p <- plot_rates_B(out, input$name,
                          paste("Z-Scores of Miss Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 1) +
            add_zone("black")
        }
        if(input$type == "hit"){
          out$PCT <- z_score2(out$Hit, out$InPlay,
                              out_all$P4)
          p <- plot_rates_B(out, input$name,
                          paste("Z-Scores of Hit Avgs", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 1) +
            add_zone("black")
        }
        if(input$type == "HR"){
          out$PCT <- z_score2(out$HR, out$InPlay,
                              out_all$P5 / 100)
          p <- plot_rates_B(out, input$name,
                          paste("Z-Scores of HR Percentages", "\n",
                                paste(input$year, collapse = " ")),
                          digits = 1) +
            add_zone("black")
        }
        p
      }}
  }, res = 96)
  output$downloadData <- downloadHandler(
    filename = "batter_rates_output.csv",
    content = function(file) {
      pid <- get_id(input$name)
      df <- filter(FF_15_20, Season %in% as.numeric(input$year),
                   batter == pid$key_mlbam)
      out <- bin_FF_locations_B(df, c(-0.85, 0.85, 0.425),
                                c(1.6, 3.5, 0.475))
      out$Name <- pid$Name
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
