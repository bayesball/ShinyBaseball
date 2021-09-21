# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)

BA_graph <- function(season, minAB, minBA){
  require(Lahman)
  require(ggplot2)
  require(dplyr)

  increasefont <- function () {
    theme(text = element_text(size = 18))
  }
  centertitle <- function () {
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 16,
                                       hjust = 0.5,
                                       vjust = 0.8,
                                       angle = 0))
  }
  Batting %>%
    filter(yearID == season) -> B_season

  B_season %>%
    group_by(playerID) %>%
    summarize(SO = sum(SO),
              AB = sum(AB),
              H = sum(H)) %>%
    filter(AB >= minAB)  %>%
    mutate(BA = H / AB,
           SO_Rate = SO / AB,
           BACON = H / (AB - SO),
           InPlay_Rate = 1 - SO_Rate) -> S

  S %>%
    filter(AB >= minAB) -> S_min

  select(filter(S_min, BA >= minBA),
         InPlay_Rate, BACON) %>% cor() -> COR

  myf <- function(x, con){
    con / x
  }
  ggplot() +
    geom_point(data = filter(S_min, BA >= minBA),
               mapping = aes(InPlay_Rate, BACON),
               color = "red") +
    geom_smooth(data = filter(S_min, BA >= minBA),
                mapping = aes(InPlay_Rate, BACON),
                method = "lm",
                formula = "y ~ x",
                se = FALSE) +
    geom_point(data = filter(S_min, BA < minBA),
               mapping = aes(InPlay_Rate, BACON),
               color = "grey") +
    stat_function(fun = myf,
                  args = list(con = minBA),
                  color = "red",
                  linetype = 2) +
    increasefont() +
    labs(title =
           paste(season, " Data, ",
                 "AB >= ", minAB,
                 sep = ""),
         subtitle = paste("BA >= ", minBA,
                          ", Correlation = ",
                          round(COR[1, 2], 2),
                          sep = "")) +
    centertitle()
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h3("Demonstration of Selection-Distortion Effect"),
      sliderInput("season", "Choose Season:",
                  min = 1960, max = 2020,
                  value = 2019),
      sliderInput("minAB", "Choose Minimum AB:",
                  min = 1, max = 400,
                  value = 100),
      sliderInput("minBA", "Choose Minimum Batting Average:",
                  min = .100, max = .300,
                  value = .200)
    )),
      column(8,
                          plotOutput("plot1",
                           height = "500px")
      ))
    )

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
     BA_graph(input$season,
              input$minAB,
              input$minBA)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
