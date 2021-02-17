library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
#library(knitr)
library(readr)

#sc_pitcher_2019 <- read_delim("https://raw.githubusercontent.com/bayesball/ShinyBaseball/main/data/sc_pitcher_2019.txt", delim = " ")
#sc <- read_csv("sc_pitcher_2019.csv")
#chadwick <- read_csv("chadwick.csv")

# some preliminary work on dataset
  called_ball <- c("blocked ball", "ball",
                       "called_strike")
  in_play <- c("hit_into_play",
             "hit_into_play_no_out",
             "hit_into_play_score")
  foul <- c("foul", "foul_bunt",
          "foul_tip")
  miss <- c("swinging_strike",
          "swinging_strike_blocked",
          "missed_bunt")
  hit <- c("single", "double",
         "triple", "home_run")
  out <- c("double_play", "field_error", "field_out",
         "fielders_choice", "fielders_choice_out",
         "force_out", "grounded_into_double_play",
         "other_out")

data_work <- function(sc){
  sc %>%
  mutate(Called = ifelse(description ==
                        "called_strike", "strike",
                  ifelse(description %in% called_ball,
                         "ball", NA)),
         Swing = ifelse(description %in% miss,
                        "miss",
                 ifelse(description %in% foul,
                        "foul",
                 ifelse(description %in% in_play,
                        "in-play", NA))),
         InPlay = ifelse(events %in% hit, "hit",
                  ifelse(events %in% out, "out", NA)))
}

# user interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h3(id="big-heading", "Pitch Count"),
  tags$style(HTML("#big-heading{color: blue;}")),
#  fileInput("file1", "Read in Statcast CSV File",
#            accept = ".csv"),
# checkboxInput("header", "Header", TRUE),
  textInput("name", "Pitcher Name:",
            value = ""),
  tableOutput("table"),
  checkboxGroupInput("pitch_type", "Pitch Type:",
               c("CH", "CU", "EP", "FC",
                 "FF", "FO",
                 "FS", "FT", "KC", "KN", "SI", "SL"),
               selected = "FF",
               inline = TRUE),
  checkboxGroupInput("count", "Count:",
               c("All", "0-0", "1-0", "0-1", "2-0",
                  "1-1", "0-2", "3-0", "2-1", "1-2",
                 "3-1", "2-2", "3-2"),
               selected = "All",
               inline = TRUE)
  )),
  column(8,
         plotOutput("plot",
              height = '440px')
         )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=60*1024^2)
  the_data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    d <- read.csv(file$datapath, header = input$header)
    data_work(d)
 })
  output$table <- renderTable({
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
    nice_table <- function(d){
      d %>%
        group_by(pitch_type) %>%
        summarize(N = n()) %>%
        filter(is.na(pitch_type) == FALSE) %>%
        pivot_wider(
          names_from = pitch_type,
          values_from = N
        )
    }
    pid <- get_id(input$name)$key_mlbam
    req(length(pid) > 0)
 #   sc_pitcher_2019 <- the_data()
    nice_table(filter(sc_pitcher_2019,
                      pitcher == pid))
  })
  output$plot <- renderPlot({
    req(length(input$count) > 0)
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
    add_zone <- function(color = "black"){
      topKzone <- 3.5
      botKzone <- 1.6
      inKzone <- -0.85
      outKzone <- 0.85
      kZone <- data.frame(
        x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
        y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
      )
      geom_path(aes(.data$x, .data$y),
                data=kZone, lwd = 1, color = color)
    }
    centertitle <- function(){
      theme(plot.title = element_text(
        colour = "white", size = 14,
        hjust = 0.5, vjust = 0.8, angle = 0),
        plot.subtitle = element_text(
          colour = "white", size = 14,
          hjust = 0.5, vjust = 0.8, angle = 0))
    }
    pid <- get_id(input$name)$key_mlbam
    req(length(pid) > 0)

#    sc_pitcher_2019 <- the_data()
    all_counts <- NULL
    for(j in 1:length(input$count)){
      all_counts <- paste(all_counts, input$count[j])
    }
    if("All" %in% input$count){
       all_counts <- "All"
    }
    all_pitch_types <- NULL
    for(j in 1:length(input$pitch_type)){
      all_pitch_types <- paste(all_pitch_types,
                               input$pitch_type[j])
    }
    subtitle1 <- paste("Pitch Type: ",
          all_pitch_types,
          sep="")
    subtitle2 <- paste("Count: ",
                      all_counts)
    subtitle <- paste(subtitle1, "\n", subtitle2)
    th1 <- theme(plot.background =
                   element_rect(fill = "deepskyblue4"),
                 axis.text = element_text(colour = "white"),
                 axis.title = element_text(colour = "white"))

    if("All" %in% input$count){
      sc_pitcher_2019 <- filter(sc_pitcher_2019,
                      pitch_type %in% input$pitch_type)
    }
    if(("All" %in% input$count) == FALSE){
      sc_pitcher_2019 <- filter(sc_pitcher_2019,
                            Count %in% input$count,
                            pitch_type %in% input$pitch_type)
    }

   ggplot() +
      geom_point(data = filter(sc_pitcher_2019,
              pitcher == get_id(input$name)$key_mlbam),
                 aes(plate_x, plate_z,
                     color = pitch_type),
              size = 0.8) +
      add_zone() +
      centertitle() + th1 +
      coord_equal() +
      xlim(-2.5, 2.5) +
      ylim(0, 5) +
      labs(title = get_id(input$name)$Name,
           subtitle = subtitle)
}, res = 96)
}

shinyApp(ui = ui, server = server)
