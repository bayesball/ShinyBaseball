library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(ShinyBaseball)
library(tidyr)
plot_spray_compare <- function(sc_ip,
                               pname1,
                               pname2,
                               type = "All"){
  require(ggthemes)
  require(tidyverse)

  sc_ip %>%
    mutate(Type = ifelse(launch_angle < 10, "Ground ball",
                         ifelse(launch_angle < 25, "Line drive",
                                ifelse(launch_angle < 50, "Fly ball",
                                       "Pop up")))) ->
    sc_ip

  sc_ip %>% mutate(
    location_x = hc_x - 125.42,
    location_y = 198.27 - hc_y,
    spray_angle = atan(location_x / location_y)
  ) -> sc_ip

  sc_ip %>%
    mutate(phi1 = ifelse(stand == "L",
                         -spray_angle, spray_angle),
           adj_location_x = ifelse(stand == "L",
                                   - location_x, location_x)) ->
    sc_ip

  scnew <- filter(sc_ip, player_name == pname1 |
                    player_name == pname2)

  if(type == "Fly ball"){
    scnew <- filter(scnew, Type == "Fly ball")
  }
  if(type == "Ground ball"){
    scnew <- filter(scnew, Type == "Ground ball")
  }
  if(type == "Line drive"){
    scnew <- filter(scnew, Type == "Line drive")
  }
  if(type == "Pop up"){
    scnew <- filter(scnew, Type == "Pop up")
  }
  p <- ggplot() +
    geom_polygon(data=data.frame(
      x = c(0, -125, -125, 0, 0),
      y = c(0, 125, 200, 200, 0)),
      aes(x, y), fill="beige")  +
    geom_path(data = data.frame(x = c(-100, 0, 100),
                                y = c(100, 0, 100)),
              aes(x, y), color="black", size=1.5) +
    facet_wrap(~ player_name) +
    ggtitle(paste(type, "Locations")) +
    scale_colour_manual(values =
                c("blue", "brown","red", "green")) +
    annotate(geom="text", x=-85, y=190,
             label="PULL", size=6,
             color="black") +
    xlab("Adjusted X Location") +
    ylab("Y Location") +
    theme_fivethirtyeight() +
    theme(
      plot.title = element_text(
        colour = "red",
        size = 18,
        hjust = 0.5,
        vjust = 0.8,
        angle = 0
      ),
      strip.text = element_text(
        size = 18,
        color = "blue"
      )
    ) +
    coord_fixed()

  if(type == "All"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y,
                            color=Type),
                        size = 0.75)
  }
  if(type == "Fly ball"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y),
                        color = "blue",
                        size = 0.75)
  }
  if(type == "Ground ball"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y),
                        color = "brown",
                        size = 0.75)
  }
  if(type == "Line drive"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y),
                        color = "red",
                        size = 0.75)
  }
  if(type == "Pop up"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y),
                        color = "green",
                        size = 0.75)
  }
  p
}
nice_table <- function(d){
  d %>%
    group_by(player_name, Type) %>%
    summarize(N = n()) %>%
    filter(is.na(Type) == FALSE) %>%
    pivot_wider(
      names_from = Type,
      values_from = N
    )
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h3(id="big-heading", "Spray Compare"),
  tags$style(HTML("#big-heading{color: blue;}")),
  textInput("name1", "First Batter Name:",
            value = "Mike Trout"),
  textInput("name2", "Second Batter Name:",
            value = "George Springer"),
  radioButtons("type", "Batted Ball Type:",
               c("All",
                 "Fly ball", "Ground ball",
                 "Line drive", "Pop up"),
               inline = FALSE)
  )),
  column(8,
         plotOutput("plot",
              width = '100%',
              height = "400px"),
         h5("Batted Ball Distribution:"),
         tableOutput("table")
         )
)

server <- function(input, output, session) {
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
    pid1 <- get_id(input$name1)$key_mlbam
    pid2 <- get_id(input$name2)$key_mlbam
    req(length(pid1) > 0 & length(pid2) > 0)
    two_names <- c(get_id(input$name1)$Name,
                   get_id(input$name2)$Name)
    sc2019_ip %>%
      filter(player_name %in% two_names,
             is.na(launch_angle) == FALSE) %>%
      mutate(Type = ifelse(launch_angle < 10,
                           "Ground ball",
            ifelse(launch_angle < 25, "Line drive",
            ifelse(launch_angle < 50, "Fly ball",
                      "Pop up")))) ->
      scnew
    nice_table(scnew)
  })
  output$plot <- renderPlot({
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
    pid1 <- get_id(input$name1)$key_mlbam
    pid2 <- get_id(input$name2)$key_mlbam
    req(length(pid1) > 0 & length(pid2) > 0)
    plot_spray_compare(sc2019_ip,
               get_id(input$name1)$Name,
               get_id(input$name2)$Name,
               input$type)
  })
}

shinyApp(ui = ui, server = server)
