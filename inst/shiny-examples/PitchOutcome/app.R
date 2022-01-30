library(shiny)

print_table_1 <- function(sc_data, name,
                         pitch_type, pitches){
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(tidyr)
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
  pid <- get_id(name)$key_mlbam
  req(length(pid) > 0)
  nice_table(filter(sc_data, pitcher == pid))
}

print_table_2 <- function(sc_data, name,
                          pitch_type, pitches,
                          plot_brush){
  library(ggplot2)
  library(dplyr)
  library(stringr)
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
  req(pitches == "In-Play" |
        pitches == "Swung")
  req(plot_brush)

  in_play <- c("hit_into_play",
               "hit_into_play_no_out",
               "hit_into_play_score")
  foul <- c("foul", "foul_bunt",
            "foul_tip")
  miss <- c("swinging_strike",
            "swinging_strike_blocked",
            "missed_bunt")

  ptypes <- c("CH", "CU", "EP", "FC",
              "FF", "FO",
              "FS", "FT", "KC", "KN", "SI", "SL")
  if(pitch_type == "All"){
    PT <- ptypes
  } else {
    PT <- pitch_type
  }

  scnew <- filter(sc_data,
                    pitcher == get_id(name)$key_mlbam)

  if(pitches == "In-Play"){
    sc1 <- brushedPoints(filter(scnew,
                                pitch_type %in% PT,
                                type == "X"),
                         plot_brush)}
  if(pitches == "Swung"){
    sc1 <- brushedPoints(filter(scnew,
                                pitch_type %in% PT,
                                description %in%
                                  c(in_play, miss, foul)),
                         plot_brush)}
  if(pitches == "In-Play"){
    S <- data.frame(Name = get_id(name)$Name,
                    BIP = nrow(sc1),
                    H_Rate = sum(sc1$events %in%
                                   c("single", "double",
                                     "triple", "home_run")) /
                      nrow(sc1),
                    xBA = mean(sc1$estimated_ba_using_speedangle,
                               na.rm = TRUE))
  }
  if(pitches == "Swung"){
    S <- data.frame(Name = get_id(name)$Name,
                    Swings = nrow(sc1),
                    Miss_Rate =
                      sum(sc1$description %in%
                            c("swinging_strike",
                              "swinging_strike_blocked",
                              "missed_bunt")) /
                      nrow(sc1))
  }
  S
}

construct_plot <- function(sc_data, name,
                           pitch_type, pitches){
  library(ggplot2)
  library(dplyr)
  library(stringr)
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
  pid <- get_id(name)$key_mlbam
  req(length(pid) > 0)

  subtitle <- paste("Pitch Type: ",
                    pitch_type,
                    ", Pitches: ",
                    pitches,
                    sep="")
  th1 <- theme(plot.background =
                 element_rect(fill = "deepskyblue4"),
               axis.text = element_text(colour = "white"),
               axis.title = element_text(colour = "white"))

  if(pitches == "Called"){
    sc_data <- filter(sc_data,
                              is.na(Called) == FALSE) %>%
      mutate(Outcome = Called)
  }
  if(pitches == "Swung"){
    sc_data <- filter(sc_data,
                              is.na(Swing) == FALSE) %>%
      mutate(Outcome = Swing)
  }
  if(pitches == "In-Play"){
    sc_data <- filter(sc_data,
                              is.na(InPlay) == FALSE) %>%
      mutate(Outcome = InPlay)
  }
  if(pitches == "All"){
    sc_data <- mutate(sc_data,
                              Outcome = type)
  }
  if(pitch_type == "All"){
    PT <- c("CH", "CU", "EP", "FC", "FF", "FO",
            "FS", "FT", "KC", "KN", "SI", "SL")
  }
  if(pitch_type != "All"){
    PT <- pitch_type
  }
  scnew <- filter(sc_data,
                    pitcher == get_id(name)$key_mlbam,
                    pitch_type %in% PT)

  Type <- "Pitcher"

  p <-  ggplot() +
    geom_point(data = scnew,
               aes(plate_x, plate_z,
                   color = Outcome),
               size = 0.8) +
    add_zone() +
    centertitle() + th1 +
    coord_equal() +
    xlim(-2.5, 2.5) +
    ylim(0, 5) +
    labs(title = paste("2019", Type,
                       get_id(name)$Name),
         subtitle = subtitle)
  if(pitches %in% c("Swung", "In-Play")){
    p <- p + annotate(geom = "text", x = 2, y = 4.7,
                      label = "BRUSH",
                      color = "deepskyblue4", size = 5)
  }
  p
}
# user interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h3(id="big-heading", "Pitch Outcome"),
  tags$style(HTML("#big-heading{color: blue;}")),
  radioButtons("player_type", "Player Type:",
               c("Pitcher", "Batter"),
               inline = TRUE),
  selectInput("name", "Player Name:",
            choices = unique(sc_pitcher_2019b$Name)),
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
         tableOutput("data"),
         h5("Pitch Distribution:"),
         tableOutput("table")
         )
)

server <- function(input, output, session) {

  output$table <- renderTable({
    print_table_1(sc_pitcher_2019b,
                  input$name,
                  input$pitch_type, input$pitches)
  })
  output$plot <- renderPlot({
    construct_plot(sc_pitcher_2019b,
                   input$name,
                   input$pitch_type, input$pitches)
}, res = 96)

  output$data <- renderTable({
    print_table_2(sc_pitcher_2019b,
                  input$name,
                  input$pitch_type, input$pitches,
                  input$plot_brush)
    }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats",
  caption.placement = "top")
}


shinyApp(ui = ui, server = server)
