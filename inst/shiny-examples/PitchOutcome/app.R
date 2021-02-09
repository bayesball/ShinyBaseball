library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)

#sc <- read_csv("sc_pitcher_2019.csv")
#chadwick <- read_delim("chadwick.txt", delim = " ")
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

# read in data

ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h4(id="big-heading", "Pitcher Location App"),
  tags$style(HTML("#big-heading{color: blue;}")),
#  fileInput("file1", "Read in Statcast CSV File",
#            accept = ".csv"),
#  checkboxInput("header", "Header", TRUE),
  textInput("name", "Pitcher Name:",
            value = "Aaron Nola"),
  radioButtons("pitch_type", "Pitch_Type:",
               c("All", "CH", "CU", "FC", "FF",
                 "FS", "FT", "KC", "SI", "SL"),
               inline = TRUE),
  radioButtons("pitches", "Pitches:",
             c("All", "Called", "Swung", "In-Play"),
             inline = TRUE)
  )),
  column(8,
         plotOutput("plot",
            brush = brushOpts("plot_brush",
                        fill = "#0000ff"),
              width = '455px'),
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
  output$plot <- renderPlot({
    fix_name <- function(st){
      str_to_title(str_squish(st))
    }
    get_id <- function(st){
      st2 <- str_to_title(str_squish(st))
      names <- unlist(str_split(st2, " "))
      chadwick %>%
        filter(name_first == names[1],
               name_last == names[2]) %>%
        pull(key_mlbam)
    }
    add_zone <- function(color = "red"){
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
    pid <- get_id(input$name)
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
      sc_pitcher_2019 <- mutate(sc_pitcher_2019, Outcome = type)
    }
    if(input$pitch_type == "All"){
      PT <- c("CH", "CU", "EP", "FC", "FF", "FO",
              "FS", "FT", "KC", "KN", "SI", "SL")
    }
    if(input$pitch_type != "All"){
      PT <- input$pitch_type
    }
    ggplot() +
      geom_point(data = filter(sc_pitcher_2019,
              pitcher == get_id(input$name),
              pitch_type %in% PT),
                 aes(plate_x, plate_z,
                     color = Outcome),
              size = 0.8) +
      add_zone() +
      centertitle() + th1 +
      coord_equal() +
      xlim(-2.5, 2.5) +
      ylim(0, 5) +
      labs(title = fix_name(input$name),
           subtitle = subtitle)
}, res = 96)

  output$data <- renderTable({
    correctinput <- function(st){
      str_to_title(str_squish(st))
    }
    get_id <- function(st){
      st2 <- str_to_title(str_squish(st))
      names <- unlist(str_split(st2, " "))
      chadwick %>%
        filter(name_first == names[1],
               name_last == names[2]) %>%
        pull(key_mlbam)
    }
    req(input$pitches == "In-Play")
    req(input$plot_brush)
    hit <- c("single", "double",
             "triple", "home_run")

    ptypes <- c("CH", "CU", "FC", "FF",
                "FS", "FT", "KC", "SI", "SL")
    if(input$pitch_type == "All"){
      PT <- ptypes
       } else {
      PT <- input$pitch_type
      }

    sc1 <- brushedPoints(filter(sc_pitcher_2019,
                          pitcher == get_id(input$name),
                          pitch_type %in% PT,
                          type == "X"),
                            input$plot_brush)
    data.frame(Name = correctinput(input$name),
               BIP = nrow(sc1),
               H_Rate = sum(sc1$events %in% hit) /
                     nrow(sc1),
               xBA = mean(sc1$estimated_ba_using_speedangle,
                          na.rm = TRUE))
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats")
}

shinyApp(ui = ui, server = server)
