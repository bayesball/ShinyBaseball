library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# sc_called <- read_csv("sc_2022_called.csv")

sc_called <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/sc_2022_called.csv")

construct_plot <- function(sc_called_subset,
                           pside, bside){
  add_zone <- function(Color = "red"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    geom_path(aes(.data$x, .data$y), data=kZone,
              lwd=1, col=Color)
  }
  ggplot(data = sample_n(sc_called_subset, size = 5000),
         mapping = aes(plate_x, plate_z,
             color = Call)) +
    geom_point(size = 0.5) +
    add_zone(Color = "black") +
    coord_fixed() +
    ggtitle(paste("Pitcher Side: ", pside,
                  ", Batter Side: ", bside,
                  sep = "")) +
    xlim(-1.5, 1.5) + ylim(1, 4) +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue",
                  size = 16,
              hjust = 0.5, vjust = 0.8, angle = 0)) +
    scale_colour_manual(values =
                          c("tan", "red"))
}

calculate_rates <- function(sc1){
  sc1 %>%
    group_by(inning_topbot) %>%
    summarize(Pitches = n(),
              .groups = "drop") -> SS
  sc1 %>%
    group_by(inning_topbot, Call) %>%
    summarize(N = n(),
              .groups = "drop") -> S2
  inner_join(S2, SS, by = "inning_topbot") %>%
    mutate(Strike_Rate = 100 * N / Pitches)
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  column(4, wellPanel(
  h3(id="big-heading", "Brushing Called Pitches - 2022 Season"),
  h3(id="big-heading", "Illustrating Home/Visitor Bias"),
  tags$style(HTML("#big-heading{color: red;}")),
  radioButtons("pside", "Select Pitcher Arm:",
               c("R", "L"),
               inline = TRUE),
  radioButtons("bside", "Select Batter Side:",
               c("R", "L"),
               inline = TRUE),
  tableOutput("data3"),
  tableOutput("data2")
  )),
  column(8,
         plotOutput("plot", brush =
              brushOpts("plot_brush",
                        fill = "#0000ff"),
              width = '455px'),
         tableOutput("data")
         )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=60*1024^2)
  output$plot <- renderPlot({

    sc_called_subset <- filter(sc_called,
                               p_throws == input$pside,
                               stand == input$bside)
    construct_plot(sc_called_subset,
                    input$pside, input$bside)

  }, res = 96)

  output$data <- renderTable({
    req(input$plot_brush)

    sc1 <- brushedPoints(sc_called,
                      input$plot_brush)

    calculate_rates(sc1)

  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats:",
  caption.placement = "top")

  output$data2 <- renderTable({
    req(input$plot_brush)
    sc1 <- brushedPoints(sc_called,
                         input$plot_brush)

    S3 <- calculate_rates(sc1)

    data.frame(Home_Bias =
                 S3$Strike_Rate[4] -
                 S3$Strike_Rate[2])
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Bias = Visiting Strike Pct MINUS Home Strike Pct:",
  caption.placement = "top")

  output$data3 <- renderTable({
    req(input$plot_brush)
    sc1 <- brushedPoints(sc_called,
                         input$plot_brush)

    r_x <- range(sc1$plate_x)
    r_z <- range(sc1$plate_z)

    data.frame(Variable = c("plate_x", "plate_z"),
               LO = c(r_x[1], r_z[1]),
               HI = c(r_x[2], r_z[2]))
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Selected Rectangle:",
  caption.placement = "top")
}

shinyApp(ui = ui, server = server)
