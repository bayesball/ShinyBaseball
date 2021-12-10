library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# read in data from Github site
sc_ip <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/scip_ip_2021c.csv")
chadwick <- read.table("https://raw.githubusercontent.com/bayesball/ShinyBaseball/main/data/chadwick.txt",
                       header = TRUE)

# remove a few cases with missing data
sc_ip <- sc_ip[complete.cases(sc_ip), ]

# collect a list of batters with at least 200 BIP
sc_ip %>%
  group_by(batter) %>%
  summarize(N = n()) %>%
  filter(N >= 200)  %>%
  inner_join(chadwick, c("batter" = "key_mlbam")) %>%
  mutate(Name = paste(name_first, name_last)) %>%
  arrange(name_last) -> S1

# making sure chadwick only contains the
# batters with at least 200 BIP
chadwick <- inner_join(chadwick, S1[, 1:2],
                       by = c("key_mlbam" ="batter"))

# function extracts the Statcast id from the player name
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

# function to plot the field locations of balls in play
plot_locations <- function(new_data, input){
  new_data$Hit <- ifelse(new_data$events %in%
                         c("single", "double",
                           "triple", "home_run"),
                        "Yes", "No")
  ggplot() +
    geom_point(data = new_data,
               mapping = aes(location_x, location_y),
               color = "olivedrab") +
    geom_point(data =
                 brushedPoints(new_data,
                               input$plot_brush),
               mapping = aes(location_x, location_y),
               color = "red") +
    coord_fixed() +
    ggtitle("Batted Ball Locations") +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    geom_segment(aes(x = 0, xend = -300,
                     y = 0, yend = 300),
                 color = "blue", lwd = 1) +
    geom_segment(aes(x = 0, xend = 300,
                     y = 0, yend = 300),
                 color = "blue", lwd = 1) +
    geom_path(data = data.frame(
      x = c(0, -63.6, 0, 63.6, 0),
      y = c(0, 63.6, 127.2, 63.6, 0)),
      mapping = aes(x, y),
      color = "blue", lwd = 1) +
    scale_color_manual(values = c("orange", "brown"))
}
# function to plot the zone locations of the BIP pitches
plot_zone <- function(new_data, input){
  add_zone <- function (Color = "red"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(x = c(inKzone, inKzone, outKzone, outKzone,
                            inKzone),
                      y = c(botKzone, topKzone, topKzone, botKzone,
                            botKzone))
    geom_path(aes(.data$x, .data$y),
            data = kZone, lwd = 1, col = Color)
  }
  new_data$Hit <- ifelse(new_data$events %in%
                           c("single", "double",
                             "triple", "home_run"),
                         "Yes", "No")
  ggplot() +
    geom_point(data = new_data,
               mapping = aes(plate_x, plate_z),
               color = "olivedrab") +
    geom_point(data =
                 brushedPoints(new_data,
                               input$plot_brush),
               mapping = aes(plate_x, plate_z),
               color = "red") +
    coord_fixed() +
    ggtitle("Zone Locations") +
    add_zone("blue") +
    theme(plot.title = element_text(colour = "blue",
                             size = 18,
                hjust = 0.5, vjust = 0.8, angle = 0)) +
    scale_color_manual(values = c("orange", "brown"))
}
# constructs scatterplot of the launch variables
plot_launch_variables <- function(new_data, input){
  ggplot() +
    geom_point(data = new_data,
             mapping = aes(launch_angle,
                           launch_speed),
             color = "olivedrab") +
    geom_point(data =
               brushedPoints(new_data,
                             input$plot_brush),
             mapping = aes(launch_angle,
                           launch_speed),
             color = "red") +
    geom_vline(xintercept = c(10, 25, 50),
               color = "blue") +
    ggtitle("Launch Variables") +
    theme(plot.title = element_text(colour = "blue",
                          size = 18,
              hjust = 0.5, vjust = 0.8, angle = 0))
}
# displays average stats for BIP in the selected region
display_stats <- function(sc1){
  Measure <- c("BIP", "H", "H Rate", "Mean LA", "Mean LS",
               "xBA")
  Value <-  c(nrow(sc1),
              sum(sc1$events %in%
                    c("single", "double", "triple", "home_run")),
              sum(sc1$events %in%
                    c("single", "double", "triple", "home_run")) /
                nrow(sc1),
              mean(sc1$launch_angle, na.rm = TRUE),
              mean(sc1$launch_speed, na.rm = TRUE),
              mean(sc1$estimated_ba_using_speedangle,
                   na.rm = TRUE))
  data.frame(Measure, Value)
}
# Shiny part
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  h2(id="big-heading", "Zone Locations, Batted Ball Locations, and Launch Variables"),
  tags$style(HTML("#big-heading{color: white;}")),
  column(6,
         selectInput("player_name",
                     "Select 2021 Batter (at Least 200 BIP):",
             S1$Name,
             selected = "Bryce Harper"),
         h4("Brush any plot to see relationships."),
         plotOutput("plot3",
            brush =
            brushOpts("plot_brush",
                      fill = "auto"),
            height = '330px'),
         tableOutput("data")
  ),
  column(6,
         plotOutput("plot2",
              brush =
              brushOpts("plot_brush",
                      fill = "auto"),
              height = '330px'),
         plotOutput("plot1",
              brush =
              brushOpts("plot_brush",
                      fill = "auto"),
              height = '330px')
         )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    id_info <- get_id(input$player_name)
    new_data <- filter(sc_ip,
                       batter == id_info$key_mlbam)
    plot_locations(new_data, input)
  }, res = 96)

  output$plot2 <- renderPlot({
    id_info <- get_id(input$player_name)
    new_data <- filter(sc_ip,
                       batter == id_info$key_mlbam)
    plot_zone(new_data, input)
  }, res = 96)

  output$plot3 <- renderPlot({
    id_info <- get_id(input$player_name)
    new_data <- filter(sc_ip,
                       batter == id_info$key_mlbam)
    plot_launch_variables(new_data, input)
  }, res = 96)

  output$data <- renderTable({
    req(input$plot_brush)
    id_info <- get_id(input$player_name)
    new_data <- filter(sc_ip,
                       batter == id_info$key_mlbam)
    sc1 <- brushedPoints(new_data,
                         input$plot_brush)
    display_stats(sc1)
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE)
}

shinyApp(ui = ui, server = server)
