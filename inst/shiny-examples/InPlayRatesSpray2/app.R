# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(gridExtra)

# data is datafile sc_ip_19_20 in data folder
# of package

# turn off warnings
options(warn=-1)

spray_hit_plot2 <- function(sc_ip, LA, LS,
                           hit_type = "All"){

  sc_ip %>% mutate(
    location_x = 2.5 * (hc_x - 125.42),
    location_y = 2.5 * (198.27 - hc_y),
    spray_angle = atan(location_x / location_y) *
      180 / pi,
    phi1 = ifelse(stand == "L",
                  - spray_angle, spray_angle),
    Season = as.character(Season)
    ) %>%
    filter(phi1 >= -45, phi1 <= 45) -> sc_ip

  sc_ip %>%
    filter(launch_angle >= LA[1],
           launch_angle <= LA[2],
           launch_speed >= LS[1],
           launch_speed <= LS[2]) -> sc_ip2

  bins <- seq(-45, 45, by = 2.5)

  if(hit_type == "1B"){
    sc_ip2 %>%
      mutate(Outcome = ifelse(events == "single",
                 1, 0)) -> sc_ip2
  }
  if(hit_type == "2B"){
    sc_ip2 %>%
      mutate(Outcome = ifelse(events == "double",
                              1, 0)) -> sc_ip2
  }
  if(hit_type == "3B"){
    sc_ip2 %>%
      mutate(Outcome = ifelse(events == "triple",
                              1, 0)) -> sc_ip2
  }
  if(hit_type == "HR"){
    sc_ip2 %>%
      mutate(Outcome = ifelse(events == "home_run",
                              1, 0)) -> sc_ip2
  }
  if(hit_type == "All"){
    sc_ip2 %>%
      mutate(Outcome = ifelse(events %in%
                  c("single", "double", "triple",
                    "home_run"), 1, 0)) -> sc_ip2
  }

    sc_ip2 %>%
      mutate(cphi1 = cut(phi1, bins)) %>%
      group_by(Season, cphi1) %>%
      summarize(N = n(),
                Y = sum(Outcome),
                .groups = "drop") -> S

  convert_string <- function(y){
    y1 <- gsub("[,(]", " ", y)
    y2 <- gsub("[][]", "", y1)
    y3 <- gsub("^ ", "", y2)
    mean(as.numeric(str_split(y3, " ")[[1]]))
  }

  S$theta <- sapply(S$cphi1, convert_string)

  the_subtitle <- paste("In-Play", hit_type,
                     "Rate")
  if(hit_type == "All"){
    the_subtitle <- "In-Play Hit Rate"
  }
  the_title <- paste("Launch Angle in (",
                        LA[1], ", ", LA[2],
                        "), Launch Speed in (",
                        LS[1], ", ", LS[2], ")",
                        sep = "")

  plot1 <- ggplot(S, aes(theta, Y / N,
                         color = Season)) +
    geom_point(size = 3) +
    geom_smooth(se = FALSE,
                method = "loess",
                formula = "y ~ x",
                span = 0.2) +
    xlab("") +
    ylab("In-Play Rate") +
    ylim(0, 1) +
    labs(title = the_title,
         subtitle = the_subtitle) +
    theme(text = element_text(size = 18),
          plot.title = element_text(colour = "blue",
                                    size = 16,
                                    hjust = 0.5, vjust = 0.8,
                                    angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8,
                                       angle = 0)) +
    theme(plot.margin=unit(c(0.5,0.3,-0.5,0.5),"cm"))

  plot2 <- ggplot(sc_ip2, aes(phi1,
                         color = Season)) +
    geom_density(size = 2) +
    xlab("Adjusted Spray Angle") +
    ylab("Density") +
    labs(title = "Density Estimate") +
    theme(text = element_text(size = 18),
          plot.title = element_text(colour = "red",
                                    size = 16,
                                    hjust = 0.5, vjust = 0.8,
                                    angle = 0))
  grid.arrange(plot1, plot2,
               heights = c(3, 2))
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  h3("In-Play Hit Rates Over Launch Conditions for 2019, 2021 Seasons"),
  fluidRow(
    column(4, wellPanel(
      h4("Select Launch Conditions:"),
      sliderInput("LA", "Range of Launch Angle:",
                  min = -20, max = 80,
                  value = c(20, 40),
                  animate = TRUE),
      sliderInput("LS", "Range of Launch Speed:",
                  min = 70, max = 110,
                  value = c(90, 100),
                  animate = TRUE),
      h4("Select Hit Type:"),
      radioButtons("hit_type", "",
                   choices =
                    c("All", "1B", "2B", "3B", "HR"),
                   selected = "All",
                   inline = TRUE)
      )),
    column(8,
           plotOutput("plot1",
                      height = "600px")
          )
          )
)

server <- function(input, output, session) {

  output$plot1 <- renderPlot({
    spray_hit_plot2(sc_ip_19_20, input$LA, input$LS,
                   hit_type = input$hit_type)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
