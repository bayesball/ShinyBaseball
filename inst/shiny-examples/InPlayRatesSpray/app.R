# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# turn off warnings
options(warn=-1)

spray_hit_plot <- function(sc_ip, LA, LS,
                           type = "all",
                           season){

  require(dplyr)
  require(ggplot2)

  sc_ip %>% mutate(
    location_x = 2.5 * (hc_x - 125.42),
    location_y = 2.5 * (198.27 - hc_y),
    spray_angle = atan(location_x / location_y) *
      180 / pi,
    phi1 = ifelse(stand == "L",
                  - spray_angle, spray_angle),
    ) %>%
    filter(phi1 >= -45, phi1 <= 45) -> sc_ip

  sc_ip %>%
    filter(launch_angle >= LA[1],
           launch_angle <= LA[2],
           launch_speed >= LS[1],
           launch_speed <= LS[2]) -> sc_ip2

  bins <- seq(-45, 45, by = 2.5)

  if(type == "outfield"){
    sc_ip2 %>%
      mutate(O_Fielding = of_fielding_alignment) %>%
      filter(O_Fielding %in%
               c("Standard", "Strategic")) %>%
      mutate(cphi1 = cut(phi1, bins)) %>%
      group_by(cphi1,
               O_Fielding) %>%
      summarize(N = n(),
                H = sum(H),
                .groups = "drop") -> S
  }
  if(type == "infield"){
    sc_ip2 %>%
      mutate(I_Fielding = if_fielding_alignment) %>%
      filter(I_Fielding %in%
               c("Infield shift", "Standard")) %>%
      mutate(cphi1 = cut(phi1, bins)) %>%
      group_by(cphi1,
               I_Fielding) %>%
      summarize(N = n(),
                H = sum(H),
                .groups = "drop") -> S
  }
  if(type == "all"){
    sc_ip2 %>%
      mutate(cphi1 = cut(phi1, bins)) %>%
      group_by(cphi1) %>%
      summarize(N = n(),
                H = sum(H),
                .groups = "drop") -> S
  }

  convert_string <- function(y){
    y1 <- gsub("[,(]", " ", y)
    y2 <- gsub("[][]", "", y1)
    y3 <- gsub("^ ", "", y2)
    mean(as.numeric(str_split(y3, " ")[[1]]))
  }

  S$theta <- sapply(S$cphi1, convert_string)

  the_title <- paste(season, "In-Play Hit Probability")
  the_subtitle <- paste("Launch Angle in (",
                        LA[1], ", ", LA[2],
                        "), Launch Speed in (",
                        LS[1], ", ", LS[2], ")",
                        sep = "")

  if(type == "outfield"){
    p1 <- ggplot(S, aes(theta, H / N,
                        color = O_Fielding))
  }
  if(type == "infield"){
    p1 <- ggplot(S, aes(theta, H / N,
                        color = I_Fielding))
  }
  if(type == "all"){
    p1 <- ggplot(S, aes(theta, H / N))
  }

  p1 +
    geom_point(size = 3) +
    geom_smooth(se = FALSE,
                method = "loess",
                formula = "y ~ x",
                span = 0.2) +
    xlab("Adjusted Spray Angle") +
    ylab("In-Play AVG") +
    labs(title = the_title,
         subtitle = the_subtitle) +
    theme(text = element_text(size = 18),
          plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8,
                                    angle = 0),
          plot.subtitle = element_text(colour = "blue",
                                       size = 14,
                                       hjust = 0.5, vjust = 0.8,
                                       angle = 0)) +
    ylim(0, 1)
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  h3("In-Play Hit Rates Over Launch Conditions and Fielding"),
  fluidRow(
    column(4, wellPanel(
      sliderInput("LA", "Range of Launch Angle:",
                  min = -20, max = 50,
                  value = c(20, 40),
                  animate = TRUE),
      sliderInput("LS", "Range of Launch Speed:",
                  min = 70, max = 110,
                  value = c(90, 100),
                  animate = TRUE),
      radioButtons("season", "Season:",
                   choices =
                     c("2019", "2021"),
                   selected = "2021",
                   inline = TRUE),
      radioButtons("type", "Fielding:",
                   choices =
                    c("All", "Infield", "Outfield"),
                   selected = "All",
                   inline = TRUE)
      )),
    column(8,
           plotOutput("plot1",
                      height = "450px")
          )
          )
)

server <- function(input, output, session) {

  output$plot1 <- renderPlot({
    if(input$type == "Infield"){
      if(input$season == "2019"){
      the_plot <- spray_hit_plot(sc2019_ip, input$LA, input$LS,
                     type = "infield", season = "2019")} else {
      the_plot <- spray_hit_plot(sc2021_ip, input$LA, input$LS,
                     type = "infield", season = "2021")
      }
    }
    if(input$type == "Outfield"){
      if(input$season == "2019"){
        the_plot <- spray_hit_plot(sc2019_ip, input$LA, input$LS,
                      type = "outfield", season = "2019")} else {
        the_plot <- spray_hit_plot(sc2021_ip, input$LA, input$LS,
                      type = "outfield", season = "2021")
     }
    }
    if(input$type == "All"){
      if(input$season == "2019"){
        the_plot <- spray_hit_plot(sc2019_ip, input$LA, input$LS,
                     type = "all", season = "2019")} else {
        the_plot <- spray_hit_plot(sc2021_ip, input$LA, input$LS,
                     type = "all", season = "2021")
      }
    }
    the_plot
  }, res = 96)
}

shinyApp(ui = ui, server = server)
