library(shiny)

# function that does all of the work

eba_work <- function(sc, LA_breaks, LS_breaks,
                     type = "Rate"){

  require(dplyr)
  require(ggplot2)
  require(stringr)

  # some helper functions in graphing

  increasefont <- function (){
    theme(text = element_text(size = 16))
  }
  centertitle <- function (){
    theme(plot.title = element_text(
      colour = "white", size = 18,
      hjust = 0.5, vjust = 0.8, angle = 0))
  }

  # create bins by using the LA and LS cutpoints

  sc %>%
    mutate(LA = cut(launch_angle,
                    LA_breaks),
           LS = cut(launch_speed,
                    LS_breaks)) -> sc

  # compute AVG in each bin

  sc %>%
    filter(is.na(LA) == FALSE,
           is.na(LS) == FALSE) %>%
    group_by(LA, LS) %>%
    summarize(IP = n(),
              HR = sum(HR),
              EHR = HR / IP,
              .groups = "drop") -> S

  # define bin midpoints

  convert_string <- function(y){
    y1 <- gsub("[,(]", " ", y)
    y2 <- gsub("[][]", "", y1)
    y3 <- gsub("^ ", "", y2)
    mean(as.numeric(str_split(y3, " ")[[1]]))
  }

  S$la <- sapply(S$LA, convert_string)
  S$ls <- sapply(S$LS, convert_string)

  # x and y limits in graph

  xlim_lo <- min(LA_breaks) - diff(LA_breaks)[1] / 4
  xlim_hi <- max(LA_breaks) + diff(LA_breaks)[1] / 4
  ylim_lo <- min(LS_breaks) - diff(LS_breaks)[1] / 4
  ylim_hi <- max(LS_breaks) + diff(LS_breaks)[1] / 4

  # select the plotting variable

  if(type == "Rate"){
    S$FVar <- S$EHR
  }
  if(type == "HR") {
    S$FVar <- S$HR
  }
  if(type == "In-Play"){
    S$FVar <- S$IP
  }

  # define the graph title

  the_title <- ifelse(type == "Rate",
                      "Home Run Rate",
               ifelse(type == "In-Play",
                      "Balls In Play", "Home Runs"))

  legend_title <- ifelse(type == "Rate",
                      "Rate",
                      ifelse(type == "In-Play",
                             "IP", "HR"))

  # the plot

  ggplot(S, aes(la, ls)) +
    geom_tile(aes(fill = FVar)) +
    scale_fill_distiller(palette = "RdGy") +
    xlim(xlim_lo, xlim_hi) +
    ylim(ylim_lo, ylim_hi) +
    centertitle() +
    increasefont() +
    xlab("Launch Angle") +
    ylab("Launch Speed") +
    ggtitle(the_title) +
    theme(plot.background = element_rect(fill = "grey25"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "bisque",
                                colour = "grey")) +
    guides(fill=guide_legend(title=legend_title))
}

# data is read from Github repository

data_work <- function(){
  require(readr)
  require(dplyr)
  sc_2021 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast2021.csv")
  sc_2021 %>%
    filter(!events %in% c("sac_bunt", "sac_fly")) %>%
    mutate(HR = ifelse(events == "home_run", 1, 0)) %>%
    select(game_year, Game_Date, launch_angle,
           launch_speed, events, HR)
}

# read in statcast dataset
sc <- data_work()

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4("Expected Home Runs"),
      radioButtons("type", "Display:",
                   c("In-Play", "HR", "Rate"),
                   inline = TRUE),
      sliderInput("rX", "Range of Launch Angle:",
                  min = 10, max = 50,
                  value = c(10, 50)),
      sliderInput("sX",
                  "Step Size for Launch Angle:",
                  min = 0.5, max = 10,
                  value = 2),
      sliderInput("rY", "Range of Launch Speed:",
                  min = 90, max = 115,
                  value = c(90, 115)),
      sliderInput("sY",
                  "Step Size for Launch Speed:",
                  min = 0.5, max = 10,
                  value = 2)
    )),
    column(8,
           plotOutput("plot1", height = "520px")
           )
      )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    LA_breaks <- seq(input$rX[1], input$rX[2],
                     by = input$sX)
    LS_breaks <- seq(input$rY[1], input$rY[2],
                     by = input$sY)
    eba_work(sc, LA_breaks, LS_breaks,
             type = input$type)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
