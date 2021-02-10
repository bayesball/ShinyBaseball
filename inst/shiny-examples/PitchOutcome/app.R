library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(knitr)
# library(readr)
#sc_pitcher_2019 <- read_delim("https://raw.githubusercontent.com/bayesball/ShinyBaseball/main/data/sc_pitcher_2019.txt", delim = " ")
#sc <- read_csv("sc_pitcher_2019.csv")
#chadwick <- read_delim("chadwick.txt", delim = " ")

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
sc_pitcher_2019 %>%
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
                  ifelse(events %in% out, "out", NA))) ->
  sc_pitcher_2019

# user interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h3(id="big-heading", "Pitch Outcome"),
  tags$style(HTML("#big-heading{color: blue;}")),
#  fileInput("file1", "Read in Statcast CSV File",
#            accept = ".csv"),
#  checkboxInput("header", "Header", TRUE),
  textInput("name", "Pitcher Name:",
            value = "Aaron Nola"),
  h5("Pitch Distribution:"),
  tableOutput("table"),
  radioButtons("pitch_type", "Pitch Type:",
               c("All", "CH", "CU", "EP", "FC",
                  "FF", "FO",
                 "FS", "FT", "KC", "KN", "SI", "SL"),
               inline = TRUE),
  radioButtons("pitches", "Pitches to Display:",
             c("All", "Called", "Swung", "In-Play"),
             inline = FALSE),
  hr(),
  )),
  column(8,
         plotOutput("plot",
            brush = brushOpts("plot_brush",
                        fill = "#0000ff"),
              height = '440px'),
         tableOutput("data")
         )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=60*1024^2)
#  the_data <- reactive({
#    file <- input$file1
#    ext <- tools::file_ext(file$datapath)
#    req(file)
#   validate(need(ext == "csv", "Please upload a csv file"))
#   read.csv(file$datapath, header = input$header)
# })
  output$table <- renderTable({
    get_id <- function(st){
      st2 <- str_to_lower(str_squish(st))
      names <- unlist(str_split(st2, " "))
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
    nice_table(filter(sc_pitcher_2019,
                      pitcher == pid))
  })
  output$plot <- renderPlot({
    get_id <- function(st){
      st2 <- str_to_lower(str_squish(st))
      names <- unlist(str_split(st2, " "))
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

#    sc <- the_data()
    subtitle <- paste("Pitch Type: ",
                      input$pitch_type,
                      ", Pitches: ", input$pitches,
                      sep="")
    th1 <- theme(plot.background =
                   element_rect(fill = "deepskyblue4"),
                 axis.text = element_text(colour = "white"),
                 axis.title = element_text(colour = "white"))

    if(input$pitches == "Called"){
      sc_pitcher_2019 <- filter(sc_pitcher_2019,
                                is.na(Called) == FALSE) %>%
         mutate(Outcome = Called)
    }
    if(input$pitches == "Swung"){
      sc_pitcher_2019 <- filter(sc_pitcher_2019,
                                is.na(Swing) == FALSE) %>%
        mutate(Outcome = Swing)
    }
    if(input$pitches == "In-Play"){
      sc_pitcher_2019 <- filter(sc_pitcher_2019,
                                is.na(InPlay) == FALSE) %>%
        mutate(Outcome = InPlay)
    }
    if(input$pitches == "All"){
      sc_pitcher_2019 <- mutate(sc_pitcher_2019,
                                Outcome = type)
    }
    if(input$pitch_type == "All"){
      PT <- c("CH", "CU", "EP", "FC", "FF", "FO",
              "FS", "FT", "KC", "KN", "SI", "SL")
    }
    if(input$pitch_type != "All"){
      PT <- input$pitch_type
    }
   p <-  ggplot() +
      geom_point(data = filter(sc_pitcher_2019,
              pitcher == get_id(input$name)$key_mlbam,
              pitch_type %in% PT),
                 aes(plate_x, plate_z,
                     color = Outcome),
              size = 0.8) +
      add_zone() +
      centertitle() + th1 +
      coord_equal() +
      xlim(-2.5, 2.5) +
      ylim(0, 5) +
      labs(title = get_id(input$name)$Name,
           subtitle = subtitle)
     if(input$pitches %in% c("Swung", "In-Play")){
       p <- p + annotate(geom = "text", x = 2, y = 4.7,
                label = "BRUSH",
                color = "deepskyblue4", size = 5)
     }
     p
}, res = 96)

  output$data <- renderTable({
    get_id <- function(st){
      st2 <- str_to_lower(str_squish(st))
      names <- unlist(str_split(st2, " "))
      chadwick %>%
        mutate(fname = str_to_lower(name_first),
               lname = str_to_lower(name_last),
               Name = paste(name_first,
                            name_last)) %>%
        filter(fname == names[1],
               lname == names[2]) %>%
        select(key_mlbam, Name)
    }
    req(input$pitches == "In-Play" |
          input$pitches == "Swung")
    req(input$plot_brush)

    ptypes <- c("CH", "CU", "EP", "FC",
                "FF", "FO",
                "FS", "FT", "KC", "KN", "SI", "SL")
    if(input$pitch_type == "All"){
      PT <- ptypes
       } else {
      PT <- input$pitch_type
      }
    if(input$pitches == "In-Play"){
    sc1 <- brushedPoints(filter(sc_pitcher_2019,
                  pitcher == get_id(input$name)$key_mlbam,
                  pitch_type %in% PT,
                          type == "X"),
                            input$plot_brush)}
    if(input$pitches == "Swung"){
    sc1 <- brushedPoints(filter(sc_pitcher_2019,
                pitcher == get_id(input$name)$key_mlbam,
                                  pitch_type %in% PT,
                          description %in% c(in_play, miss, foul)),
                           input$plot_brush)}
    if(input$pitches == "In-Play"){
    S <- data.frame(Name = get_id(input$name)$Name,
               BIP = nrow(sc1),
               H_Rate = sum(sc1$events %in% hit) /
                     nrow(sc1),
               xBA = mean(sc1$estimated_ba_using_speedangle,
                          na.rm = TRUE))
    }
    if(input$pitches == "Swung"){
      S <- data.frame(Name = get_id(input$name)$Name,
                      Swings = nrow(sc1),
                      Miss_Rate =
                    sum(sc1$description %in% miss) /
                        nrow(sc1))
    }
    S
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats")
}

shinyApp(ui = ui, server = server)
