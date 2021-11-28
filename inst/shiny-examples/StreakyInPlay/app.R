# expected hits version
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# turn off warnings
options(warn=-1)

#scip <- read_csv("sc2021_ip3.csv")
#chadwick <- read_csv("chadwick.csv")

##############################################
setup_data <- function(sc, pid){
  sc %>%
    filter(batter == pid)  %>%
    arrange(Game_Date, game_pk, at_bat_number)
}
#############################################
moving_average_plot <- function(d,
                                width,
                                name,
                                type = "BA"){
  require(RcppRoll)

  d %>%
    filter(is.na(estimated_ba) == FALSE) -> d

  d %>%
    mutate(Name = name,
           Width = width,
           Type = type,
           IP_number = 1:n()) -> d

  if(type == "BA"){
    d$Outcome <- as.numeric(d$estimated_ba)
  }
  if(type == "WOBA"){
    d$Outcome <- as.numeric(d$estimated_woba)
  }

  d$Roll_BA <-
    roll_mean(d$Outcome,
              n = width,
              align = "center", fill = NA)

  d$Roll_IP_Number <-
    roll_mean(d$IP_number,
              n = width,
              align = "center", fill = NA)

  d$AVG <- mean(d$Outcome,
                na.rm = TRUE)

  d$BLUE <- round(sum(abs(d$Roll_BA - d$AVG),
                    na.rm = TRUE), 2)

  subtitle <- paste("Width = ", width,
                    ", BLUE = ", d$BLUE, sep = "")

  the_title <- paste(name, type)

  d <- select(d,
              Name, Width, Type,
              Game_Date, game_pk, events,
              IP_number, Outcome,
              Roll_IP_Number, Roll_BA, AVG, BLUE)

  plot1 <- ggplot(d, aes(x = Roll_IP_Number,
                         ymax = Roll_BA,
                         ymin = AVG)) +
    geom_ribbon(fill = "blue") +
    ylab("Rolling BA") +
    theme(text = element_text(size = 18)) +
    labs(title = the_title,
         subtitle = subtitle) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(color = "red",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
  if(type == "BA"){
    plot1 <- plot1 + ylab("Rolling BA")
  }
  if(type == "WOBA"){
    plot1 <- plot1 + ylab("Rolling WOBA")
  }
  list(plot1 = plot1, S = d)
}
#############################################
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
#############################################

moving_average_sim <- function(d, width,
                               ITER = 500,
                               name = "",
                               type = "BA"){
  require(RcppRoll)

  d %>%
    filter(is.na(estimated_ba) == FALSE) -> d

  d %>%
    select(Game_Date, game_pk, events,
           estimated_ba,
           estimated_woba) %>%
    mutate(IP_number = 1:n()) -> d

  if(type == "BA"){
    d$Outcome <- d$estimated_ba
  }
  if(type == "WOBA"){
    d$Outcome <- d$estimated_woba
  }
  d$Roll_IP_Number <-
    roll_mean(d$IP_number,
              n = width,
              align = "center", fill = NA)
  d$AVG <- mean(d$Outcome,
                na.rm = TRUE)

  one_sim <- function(){
    mixed_d <- slice(d, sample(1:n()))
    mixed_roll <- roll_mean(mixed_d$Outcome,
                            n = width,
                            align = "center", fill = NA)
    sum(abs(mixed_roll - d$AVG),  na.rm = TRUE)
  }
  Simulated <- replicate(ITER, one_sim())
  d$Roll_BA <-
    roll_mean(d$Outcome,
              n = width,
              align = "center", fill = NA)
  Observed <- sum(abs(d$Roll_BA - d$AVG),
                  na.rm = TRUE)
  P_Value <- mean(Simulated >= Observed)
  d_out <- data.frame(Simulated = Simulated,
                      Observed = Observed)

  the_title <- paste("Simulated BLUE", name, type)
  subtitle <- paste("Width = ", width,
                    ", Obs BLUE = ",
                    round(Observed, 2),
                    ", Tail-Prob = ", P_Value,
                    sep = "")

  ggplot(d_out, aes(Simulated)) +
    geom_histogram(bins = 10,
                   fill = "plum4",
                   color = "white") +
    geom_vline(aes(xintercept = Observed),
               size = 2, color = "red") +
    theme(text = element_text(size = 18)) +
    labs(title = the_title,
         subtitle = subtitle) +
    xlab("Simulated BLUE Measure") +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(color = "red",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  h2("Streakiness in In-Play Batting Performance"),
  fluidRow(
    column(4, wellPanel(
      textInput("player_name", "2021 Batter Name:",
                value = "Bryce Harper"),
      radioButtons("type", "Select Measure:",
                   c("BA", "WOBA"),
                   "BA", inline = TRUE),
      sliderInput("width", "Width for Moving Average:",
                  min = 1, max = 50,
                  value = 20,
                  animate = FALSE),
      downloadButton("downloadData", "Download Data")
      )),
    column(8,
           tabsetPanel(type = "tabs",
              tabPanel("Observed",
                   plotOutput("plot1",
                       height = "500px")
              ),
              tabPanel("Simulated",
                   plotOutput("plot2",
                       height = "500px")
              ),
              tabPanel("Description",
                       hr(),
                       p("This app displays moving averages
                         of in-play batting data for any
                         2021 batter of interest."),
                       p("One inputs the batter player,
                         the measure (either BA
                         or wOBA) and the width
                         for the moving average.  These measures
                         are estimated values of BA and wOBA
                         based on the
                         launch angle and exit velocity
                         measurements."),
                       p("The Observed tab displays a graph of
                         the moving average against the in-play
                         number.  The shaded region shows the
                         deviations of the moving average from
                         the overall average.  The BLUE statistic
                         is the area of the shaded region and
                         measures the streakiness of the
                         hitting data."),
                       p("The Simulation tab shows results of
                         a simulation to assess the significance
                         of the observed streakiness.  One
                         randomly permutes the measure values,
                         finds all the moving averages, and
                         computes the BLUE statistic.
                         One repeats this exercise 500 times and
                         collects the values of BLUE.  A
                         histogram of the BLUE values is shown.
                         and the observed BLUE is shown as a
                         vertical line.  The tail probability is
                         the probability the simulated BLUE is
                         at least as large as the observed value.
                         A small tail probablity indicates there
                         is more streakiness in the data than
                         one would anticipate by chance.")
              )
           )
          )
          )
)

server <- function(input, output, session) {

  output$plot1 <- renderPlot({
    id_info <- get_id(input$player_name)
    d <- setup_data(sc2021_ip3,
                    id_info$key_mlbam)
    moving_average_plot(d,
                        width = input$width,
                        name = id_info$Name,
                        type = input$type)$plot1
  }, res = 96)

  output$plot2 <- renderPlot({
    id_info <- get_id(input$player_name)
    d <- setup_data(sc2021_ip3,
                    id_info$key_mlbam)
    moving_average_sim(d,
                       width = input$width,
                       name = id_info$Name,
                       type = input$type)
  }, res = 96)

  output$downloadData <- downloadHandler(
    filename = "streak_output.csv",
    content = function(file) {
      id_info <- get_id(input$player_name)
      d <- setup_data(sc2021_ip3,
                      id_info$key_mlbam)
      out <- moving_average_plot(d,
                          width = input$width,
                          name = id_info$Name,
                          type = input$type)
      write.csv(out$S, file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)
