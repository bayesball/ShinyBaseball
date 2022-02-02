library(shiny)

# data is dataset sc_pitcher_2019b in data
# folder of package

construct_table <- function(sc_data,
                            name, pitch_type){
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
  nice_table(filter(sc_data,
                    pitcher == pid))
}

construct_plot <- function(sc_data,
                           name, sel_pitch_type, count){
  library(ggplot2)
  library(dplyr)
  library(stringr)
  req(length(count) > 0)
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

  th1 <- theme(plot.background =
                 element_rect(fill = "deepskyblue4"),
               axis.text = element_text(colour = "white"),
               axis.title = element_text(colour = "white"))

  sc_new <- filter(sc_data,
                   Count %in% count,
                   pitch_type %in% sel_pitch_type,
                   pitcher == get_id(name)$key_mlbam)

  ggplot() +
    geom_point(data = sc_new,
               aes(plate_x, plate_z),
               size = 0.8, color = "red") +
    add_zone() +
    centertitle() + th1 +
    coord_equal() +
    xlim(-2.5, 2.5) +
    ylim(0, 5) +
    labs(title = paste("2019", get_id(name)$Name)) +
    facet_grid(Count ~ pitch_type)
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h3(id="big-heading", "Pitch Type Count"),
  tags$style(HTML("#big-heading{color: blue;}")),
  selectInput("name", "Pitcher Name:",
            choices = unique(sc_pitcher_2019b$Name)),
  tableOutput("table"),
  checkboxGroupInput("pitch_type", "Pitch Type:",
               c("CH", "CU", "EP", "FC",
                 "FF", "FO",
                 "FS", "FT", "KC", "KN", "SI", "SL"),
               selected = "FF",
               inline = TRUE),
  checkboxGroupInput("count", "Count:",
               c("0-0", "1-0", "0-1", "2-0",
                  "1-1", "0-2", "3-0", "2-1", "1-2",
                 "3-1", "2-2", "3-2"),
               selected = "0-0",
               inline = TRUE)
  )),
  column(8,
         plotOutput("plot",
              height = '540px')
         )
)

server <- function(input, output, session) {

  output$table <- renderTable({
    construct_table(sc_pitcher_2019b,
                    input$name, input$pitch_type)
  })
  output$plot <- renderPlot({
    construct_plot(sc_pitcher_2019b,
                    input$name, input$pitch_type,
                    input$count)
}, res = 96)
}

shinyApp(ui = ui, server = server)
