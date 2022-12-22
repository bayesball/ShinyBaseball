library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

#sc_called <- read_csv("sc2022_called.csv")

count_effects_plots <- function(sc_called, epsilon){

  add_zone <- function(Color = "red"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    geom_path(aes(.data$x, .data$y),
              data=kZone, lwd = 1,
              color = Color)
  }

  # look within epsilon of strike zone boundary

  region_low <- function(plate_x, plate_z, epsilon){
    (plate_z < 1.6 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <   0.85 + epsilon)
  }

  region_high <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 3.5 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <   0.85 + epsilon)
  }

  region_left <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <  - 0.85 + epsilon)
  }

  region_right <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x >  0.85 - epsilon) &
      (plate_x <  0.85 + epsilon)
  }

  # check graphically if regions are coded correctly

  sc_called %>%
    mutate(Right = region_right(plate_x, plate_z, epsilon),
           Left = region_left(plate_x, plate_z, epsilon),
           Low = region_low(plate_x, plate_z, epsilon),
           High = region_high(plate_x, plate_z, epsilon)) ->
    sc_called

  plot1 <- ggplot(sample_n(sc_called, size = 10000),
                  aes(plate_x, plate_z,
                      color = (Left | Right | Low | High))) +
    geom_point(size = 0.5) +
    add_zone("black") +
    theme(legend.position="none") +
    xlim(-1.3, 1.3) + ylim(1.0, 4.0) +
    labs(title = "Boundary Region",
         subtitle = paste("Epsilon =", epsilon)) +
    theme(text=element_text(size=14)) +
    coord_fixed() +
    theme(plot.title = element_text(colour = "blue",
                                    size = 14,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 12,
                                       hjust = 0.5,
                                       vjust = 0.8, angle = 0))

  ################

  sc_called %>%
    filter(Right | Left | Low | High) %>%
    group_by(Count) %>%
    summarize(N_Pitches = n(),
              S = mean(Call == "Strike"),
              Balls_Plus_Strikes =
                first(Balls_Plus_Strikes)) -> Summary_3

  plot2 <- ggplot(Summary_3, aes(Balls_Plus_Strikes, S,
                                 label = Count)) +
    geom_label(size = 5,
               fill = "red",
               color = "white") +
    ylab("Strike Probability") +
    labs(title = "Called Strike Probabilities by Count",
         subtitle = paste("Epsilon =", epsilon)) +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 16,
                                       hjust = 0.5,
                                       vjust = 0.8, angle = 0))
  list(plot1 = plot1,
       plot2 = plot2)
}

inning_effects_plot <- function(sc_called, epsilon){

  # look within epsilon of strike zone boundary

  region_low <- function(plate_x, plate_z, epsilon){
    (plate_z < 1.6 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <   0.85 + epsilon)
  }

  region_high <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 3.5 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <   0.85 + epsilon)
  }

  region_left <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <  - 0.85 + epsilon)
  }

  region_right <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x >  0.85 - epsilon) &
      (plate_x <  0.85 + epsilon)
  }

  # check graphically if regions are coded correctly

  sc_called %>%
    mutate(Right = region_right(plate_x, plate_z, epsilon),
           Left = region_left(plate_x, plate_z, epsilon),
           Low = region_low(plate_x, plate_z, epsilon),
           High = region_high(plate_x, plate_z, epsilon)) ->
    sc_called

  ################

  sc_called %>%
    filter(Right | Left | Low | High) %>%
    filter(inning <= 9) %>%
    group_by(inning) %>%
    summarize(N_Pitches = n(),
              S = mean(Call == "Strike")) -> Summary_1

  ##################

  ggplot(Summary_1, aes(inning, S)) +
    geom_point(size = 3, color = "blue") +
    ylab("Strike Probability") +
    xlab("Inning") +
    labs(title = "Called Strike Probabilities by Inning",
         subtitle = paste("Epsilon =", epsilon)) +
    theme(text=element_text(size=18)) +
    scale_x_continuous(breaks=1:9) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0))

}

pitch_effects_plot <- function(sc_called, epsilon){

  # look within epsilon of strike zone boundary

  region_low <- function(plate_x, plate_z, epsilon){
    (plate_z < 1.6 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <   0.85 + epsilon)
  }

  region_high <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 3.5 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <   0.85 + epsilon)
  }

  region_left <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x > - 0.85 - epsilon) &
      (plate_x <  - 0.85 + epsilon)
  }

  region_right <- function(plate_x, plate_z, epsilon){
    (plate_z < 3.5 + epsilon) &
      (plate_z > 1.6 - epsilon) &
      (plate_x >  0.85 - epsilon) &
      (plate_x <  0.85 + epsilon)
  }

  # check graphically if regions are coded correctly

  sc_called %>%
    mutate(Right = region_right(plate_x, plate_z, epsilon),
           Left = region_left(plate_x, plate_z, epsilon),
           Low = region_low(plate_x, plate_z, epsilon),
           High = region_high(plate_x, plate_z, epsilon)) ->
    sc_called

  ################

  sc_called %>%
    filter(Right | Left | Low | High) %>%
    filter(pitch_type %in%
             c("CH", "CU", "FC", "FF", "SI", "SL")) %>%
    group_by(pitch_type) %>%
    summarize(N_Pitches = n(),
              S = mean(Call == "Strike")) -> Summary_1

  ##################

  ggplot(Summary_1, aes(pitch_type, S)) +
    geom_point(size = 3, color = "blue") +
    ylab("Strike Probability") +
    xlab("Pitch Type") +
    labs(title = "Called Strike Probabilities by Pitch Type",
         subtitle = paste("Epsilon =", epsilon)) +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0))

}

ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  column(4, wellPanel(
  h3(id="big-heading", "Called Pitches - 2022 Season"),
  h3(id="big-heading", "Illustrating Biases by Count, Pitch Type and Inning"),
  tags$style(HTML("#big-heading{color: red;}")),
  sliderInput("epsilon", "Choose Width of Boundary Region:",
              min = .01,
              max = .3,
              value = .15),
  plotOutput("plot1",
             width = '100%')
  )),
  column(8,
         tabsetPanel(type = "tabs",
                     tabPanel("Count Effects",
                         br(), br(), br(),
                         plotOutput("plot2",
                           width = '100%')
                    ),
                    tabPanel("Pitch Type Effects",
                         br(), br(), br(),
                         plotOutput("plot3",
                              width = '100%')
                    ),
                    tabPanel("Inning Effects",
                             br(), br(), br(),
                             plotOutput("plot4",
                                        width = '100%')
                    )
         ))
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    count_effects_plots(sc2022_called,
                        input$epsilon)$plot1
  }, res = 96)

  output$plot2 <- renderPlot({
    count_effects_plots(sc2022_called,
                        input$epsilon)$plot2
  }, res = 96)

  output$plot3 <- renderPlot({
    pitch_effects_plot(sc2022_called,
                        input$epsilon)
  }, res = 96)

  output$plot4 <- renderPlot({
    inning_effects_plot(sc2022_called,
                       input$epsilon)
  }, res = 96)

}

shinyApp(ui = ui, server = server)
