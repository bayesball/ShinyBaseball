library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(sportyR)

# scip2019_new <- read_csv("scip2019_new.csv")
# data is dataset scip2019_new in data folder of package

plot_spray <- function(sc_ip,
                       pname,
                       bb_type = "All",
                       season = 2019){
  require(ggthemes)

  sc_ip %>%
    filter(is.na(launch_angle) == FALSE) %>%
    mutate(BB_Type = ifelse(launch_angle < 10, "Ground ball",
                            ifelse(launch_angle < 25, "Line drive",
                            ifelse(launch_angle < 50, "Fly ball",
                                          "Pop up")))) ->
    sc_ip

  # add factor of 2.4 to conform to new package

  sc_ip %>% mutate(
    location_x = 2.5 *(hc_x - 125.42),
    location_y = 2.5 * (198.27 - hc_y),
    spray_angle = atan(location_x / location_y)
  ) -> sc_ip

  sc_ip %>%
    mutate(phi1 = ifelse(stand == "L",
                         - spray_angle, spray_angle),
          adj_location_x = ifelse(stand == "L",
                                   - location_x, location_x)) ->
    sc_ip

  scnew <- filter(sc_ip, player_name == pname)

  if(bb_type %in% c("Fly ball", "Ground ball",
                    "Line drive", "Pop up")){
    #   scnew %>% filter(BB_Type == bb_type) -> scnew
    scnew <- scnew[scnew$BB_Type == bb_type, ]
  }

  scnew %>% mutate(H = as.logical(H)) -> scnew

  hits <- sum(scnew$H)
  BIP <- nrow(scnew)
  hit_rate <- round(hits / BIP, 3)

  p <- geom_baseball(league = "MLB") +
    ylim(-10, 400)

  p <- p +
    #  geom_polygon(data=data.frame(
    #     x = c(0, -125, -125, 0, 0),
    #     y = c(0, 125, 200, 200, 0)),
    #    aes(x, y), fill="beige") +
    #  geom_path(data = data.frame(x = c(-100, 0, 100),
    #                              y = c(100, 0, 100)),
    #            aes(x, y), color="black", size=1) +
    ggtitle(paste(season, pname, bb_type, "Locations")) +
    annotate(geom="text", x=-100, y=190,
             label="PULL", size=6,
             color="white") +
    xlab("Adjusted X Location") +
    ylab("Y Location") +
    #   theme_fivethirtyeight() +
    theme(
      plot.title = element_text(
        colour = "blue",
        size = 20,
        hjust = 0.5,
        vjust = 0.8,
        angle = 0
      ),
      plot.subtitle = element_text(
        colour = "red",
        size = 16,
        hjust = 0.5,
        vjust = 0.8,
        angle = 0
      )
    ) +
    #   coord_fixed() +
    labs(subtitle = paste("BIP Hit Rate =",
                          hits, "/", BIP,
                          "=", hit_rate))

  if(bb_type == "All"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y,
                            color=BB_Type)) +
      scale_colour_manual(values =
                            c("blue", "brown",
                              "red", "green"))
  }
  if(bb_type == "Fly ball"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y,
                            color = H)) +
      scale_colour_manual(values =
                            c("yellow", "red"))
  }
  if(bb_type == "Ground ball"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y,
                            color = H)) +
      scale_colour_manual(values =
                            c("yellow", "red"))
  }
  if(bb_type == "Line drive"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y,
                            color = H)) +
      scale_colour_manual(values =
                            c("yellow", "red"))
  }
  if(bb_type == "Pop up"){
    p <- p + geom_point(data = scnew,
                        aes(adj_location_x, location_y,
                            color = H)) +
      scale_colour_manual(values =
                            c("yellow", "red"))
  }
  p
}


ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
    h3(id="big-heading", "Spray Chart"),
    tags$style(HTML("#big-heading{color: blue;}")),
    textInput("name", "Batter Name:",
              value = "Mike Trout"),
    radioButtons("type", "Batted Ball Type:",
                 c("All",
                   "Fly ball", "Ground ball",
                   "Line drive", "Pop up"),
                 inline = FALSE)
  )),
  column(8,
         plotOutput("plot",
                    width = '100%',
                    height = "500px"),
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
    nice_table <- function(d){
      d %>%
        group_by(Type) %>%
        summarize(N = n()) %>%
        filter(is.na(Type) == FALSE) %>%
        pivot_wider(
          names_from = Type,
          values_from = N
        )
    }
    pid <- get_id(input$name)$key_mlbam
    req(length(pid) > 0)
    scip2019_new %>%
      filter(player_name ==
               get_id(input$name)$Name,
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
    pid <- get_id(input$name)$key_mlbam
    req(length(pid) > 0)
    plot_spray(scip2019_new,
               get_id(input$name)$Name,
               input$type)
  })
}

shinyApp(ui = ui, server = server)
