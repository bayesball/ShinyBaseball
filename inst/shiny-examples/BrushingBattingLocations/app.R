library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)

sc_ip <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/scip_ip_2021b.csv")
chadwick <- read.table("https://raw.githubusercontent.com/bayesball/ShinyBaseball/main/data/chadwick.txt",
                           header = TRUE)
sc_ip %>% 
  group_by(batter) %>% 
  summarize(N = n()) %>% 
  filter(N >= 200)  %>% 
  inner_join(chadwick, c("batter" = "key_mlbam")) %>% 
  mutate(Name = paste(name_first, name_last)) %>% 
  arrange(name_last) -> S1

draw_field_plot <- function(sc_ip,
                            title = "",
                            subtitle = ""){
  require(scico)
  draw_circle_segment <- function(radius,
                                  theta1, theta2){
    theta <- seq(theta1, theta2, length.out = 50)
    df <- data.frame(x = radius * cos(theta),
                     y = radius * sin(theta))
    geom_line(data = df, aes(x, y),
              color = "black")
  }
  plot1 <- ggplot() +
    geom_point(data = sc_ip,
               aes(location_x, location_y, 
                   color = Outcome)) +
    coord_fixed(ratio = 1) +
    geom_segment(aes(x = 0, xend = -300,
                     y = 0, yend = 300), 
                 color = "black") +
    geom_segment(aes(x = 0, xend = 300,
                     y = 0, yend = 300), 
                 color = "black") +
    geom_path(data = data.frame(
      x = c(0, -63.6, 0, 63.6, 0),
      y = c(0, 63.6, 127.2, 63.6, 0)),
      mapping = aes(x, y)) +
    draw_circle_segment(420, pi / 2 * .5,
                        pi / 2 * 1.5) +
    labs(title = title, subtitle = subtitle) +
    theme(plot.title = element_text(colour = "white", 
                                    size = 18, 
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "white",
                                       size = 16, 
                                       hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(plot.background = element_rect(fill = "coral4"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white")) +
    theme(
      panel.background = element_rect(fill = "beige",
                                      colour = "grey"))
  
  if(is.numeric(sc_ip$Outcome) == TRUE) {
    plot1 <- plot1 +
      #       scale_color_distiller(palette = "Spectral")
      scico::scale_color_scico(palette = "vik")
  }
  plot1
}

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

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  h2(id="big-heading", "Brushing Batted Ball Locations"),
  tags$style(HTML("#big-heading{color: white;}")),
  column(3, wellPanel(
 # textInput("player_name", "Batter Name:",
  #          value = "Bryce Harper"),
 selectInput("player_name", "Batter Name:",
             S1$Name,
             selected = "Bryce Harper"),
  sliderInput("LA", "Range of Launch Angle:",
              min = -20, max = 80,
              value = c(-20, 80)),
  sliderInput("LS", "Range of Launch Speed:",
              min = 50, max = 110,
              value = c(50, 110)),
  radioButtons("measure", "Measure:",
               c("Hit", "Single","Double", "Triple",
                 "Home Run",
                 "Expected BA", "Expected wOBA"),
               inline = FALSE)
  )),
  column(9,
         plotOutput("plot", brush =
              brushOpts("plot_brush",
                        fill = "#0000ff"),
              click = "plot_click",
              height = '500px'),
         tableOutput("data")
         )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({

    id_info <- get_id(input$player_name)
    new_data <- filter(sc_ip, 
                       batter == id_info$key_mlbam)
    Stand <- ifelse(mean(new_data$stand == "R") == 1,
                    "Right", 
             ifelse(mean(new_data$stand == "L") == 1,
                    "Left", "Both"))
    
    new_data <- filter(new_data,
                       launch_speed >= input$LS[1],
                       launch_speed <= input$LS[2],
                       launch_angle >= input$LA[1],
                       launch_angle <= input$LA[2])
    if(input$measure == "Hit"){
      new_data %>% 
        mutate(Outcome = ifelse(events %in% 
      c("single", "double", "triple", "home_run"),
      "Hit", "Out")) -> new_data
    }
    if(input$measure == "Home Run"){
      new_data %>% 
        mutate(Outcome = ifelse(events == "home_run", 
                      "HR", "Not HR")) -> new_data
    }
    if(input$measure == "Double"){
      new_data %>% 
        mutate(Outcome = ifelse(events == "double", 
                                "Double", "Not Double")) -> new_data
    }
    if(input$measure == "Single"){
      new_data %>% 
        mutate(Outcome = ifelse(events == "single", 
                                " Single", "Not Single")) -> new_data
    }
    if(input$measure == "Triple"){
      new_data %>% 
        mutate(Outcome = ifelse(events == "triple", 
                                " Triple", "Not Triple")) -> new_data
    }
    if(input$measure == "Expected BA"){
      new_data %>% 
        mutate(Outcome = 
          estimated_ba_using_speedangle) -> new_data
    }
    if(input$measure == "Expected wOBA"){
      new_data %>% 
        mutate(Outcome = 
                 estimated_woba_using_speedangle) -> new_data
    }
    title <- paste("2021", id_info$Name, 
                   "- Balls in Play")
    subtitle <- paste("Bats: ", Stand, ", ",
                      input$measure, sep = "")
    draw_field_plot(new_data,
                    title = title,
                    subtitle = subtitle)
    
  }, res = 96)


  output$data <- renderTable({
    correctinput <- function(st){
      str_to_title(str_squish(st))
    }
    req(input$plot_brush)
    id_info <- get_id(input$player_name)
    new_data <- filter(sc_ip, 
                       batter == id_info$key_mlbam,
                       launch_speed >= input$LS[1],
                       launch_speed <= input$LS[2],
                       launch_angle >= input$LA[1],
                       launch_angle <= input$LA[2])
    sc1 <- brushedPoints(new_data,
                      input$plot_brush)
    data.frame(Name = id_info$Name,
               BIP = nrow(sc1),
               H = sum(sc1$events %in% 
                 c("single", "double", "triple", "home_run")),
               Mean_LA =
                 mean(sc1$launch_angle, na.rm = TRUE),
               Mean_LS =
                 mean(sc1$launch_speed, na.rm = TRUE),
               H_Rate = sum(sc1$events %in% 
                c("single", "double", "triple", "home_run")) / 
                    nrow(sc1),
               xBA = mean(sc1$estimated_ba_using_speedangle),
               xwOBA = mean(sc1$estimated_woba_using_speedangle))
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats",
  caption.placement = "top")
}

shinyApp(ui = ui, server = server)
