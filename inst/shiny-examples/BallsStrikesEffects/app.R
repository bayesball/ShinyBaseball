library(dplyr)
library(ggplot2)
library(readr)
S1 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/count_stats_2021.csv")
#S1 <- read_csv("count_stats_2021.csv")

construct_plot <- function(rate, S, type1){

  # inputs
  # rate - particular rate to plot
  # S - data frame containing all count rates
  # type1 - either "passing through" or "final"

  require(dplyr)
  require(ggplot2)

  S1 <- filter(S, type == type1)

  if(rate == "HR"){
    S1$Rate <- S1$HR_Rate
  }
  if(rate == "SO"){
    S1$Rate <- S1$SO_Rate
  }
  if(rate == "BB_HBP"){
    S1$Rate <- S1$BB_HBP_Rate
  }
  if(rate == "IP_H"){
    S1$Rate <- S1$IP_H_Rate
  }
  if(rate == "IP_Out"){
    S1$Rate <- S1$IP_O_Rate
  }
  if(rate == "wOBA"){
    S1$Rate <- S1$wOBA
  }
  if(rate == "1B"){
    S1$Rate <- S1$Single_Rate
  }
  if(rate == "XB"){
    S1$Rate <- S1$XBase_Rate
  }

  mytitle <- ifelse(type1 == "passing through",
                    paste(rate, "Rates for Passing Through Counts"),
                    paste(rate, "Rate for Final Counts"))

  p1 <- ggplot(S1, aes(N.Pitches, Rate, label=count))

  if(type1 == "passing through"){
    p1 <- p1 +
      geom_path(data=filter(S1, strikes==0),
                aes(N.Pitches, Rate), color="blue") +
      geom_path(data=filter(S1, strikes==1),
                aes(N.Pitches, Rate), color="blue") +
      geom_path(data=filter(S1, strikes==2),
                aes(N.Pitches, Rate), color="blue") +
      geom_path(data=filter(S1, balls==0),
                aes(N.Pitches, Rate), color="blue") +
      geom_path(data=filter(S1, balls==1),
                aes(N.Pitches, Rate), color="blue") +
      geom_path(data=filter(S1, balls==2),
                aes(N.Pitches, Rate), color="blue") +
      geom_path(data=filter(S1, balls==3),
                aes(N.Pitches, Rate), color="blue")
  }

  p1 + geom_point() +
    xlab("Pitch Number") +
    ylab("Rate") +
    ggtitle(mytitle) +
    theme_minimal() +
    geom_label(size = 5,
               fill = "red",
               color = "white") +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
}

construct_plot2 <- function(rate, S, type1){

  # inputs
  # rate - particular rate to plot
  # S - data frame containing all count rates
  # type1 - either "passing through" or "final"

  require(dplyr)
  require(ggplot2)

  S1 <- filter(S, type == type1)

  if(rate == "HR"){
    S1$Rate <- S1$HR_Rate
  }
  if(rate == "SO"){
    S1$Rate <- S1$SO_Rate
  }
  if(rate == "BB_HBP"){
    S1$Rate <- S1$BB_HBP_Rate
  }
  if(rate == "IP_H"){
    S1$Rate <- S1$IP_H_Rate
  }
  if(rate == "IP_Out"){
    S1$Rate <- S1$IP_O_Rate
  }
  if(rate == "wOBA"){
    S1$Rate <- S1$wOBA
  }
  if(rate == "1B"){
    S1$Rate <- S1$Single_Rate
  }
  if(rate == "XB"){
    S1$Rate <- S1$XBase_Rate
  }

  mytitle <- ifelse(type1 == "passing through",
                    paste(rate, "Rates for Passing Through Counts"),
                    paste(rate, "Rate for Final Counts"))

  legendtitle <- paste(rate, "Rate")

  myround <- ifelse(rate == "wOBA", 3, 1)

  ggplot(S1,
         aes(x = strikes, y = balls, fill = Rate)) +
    geom_tile() +
    ggtitle(mytitle) +
    geom_text(aes(label = round(Rate, myround)),
              size = 7,
              fontface = "bold") +
    theme(text=element_text(size=18)) +
    scale_fill_gradient2(legendtitle,
                         low = "grey30",
                         high = "blue",
                         mid = "white",
                         midpoint = median(S1$Rate)) +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
}
ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("Balls & Strikes Effects (2021 Season MLB Data)"),
  column(3,
  hr(),
  radioButtons("type", "Select Count Type:",
                      choices = c("passing through",
                                  "final"),
                      inline = FALSE),
  radioButtons("measure", "Select Measure:",
               choices = c("HR", "SO", "BB_HBP",
                           "IP_H", "IP_Out",
                           "wOBA", "1B", "XB"),
                inline = FALSE),
  hr(),hr(),
  downloadButton("downloadData", "Download Data")
  ),
  column(9,
         tabsetPanel(type = "tabs",
                     tabPanel("Tabular Display",
                        plotOutput("plot2", height = '500px')
                        ),
                     tabPanel("Points Display",
                        plotOutput("plot1", height = '500px')
                        )
         )
         )
  )

server <- function(input, output, session) {
  options(warn=-1)

  output$plot1 <- renderPlot({
   if(input$type == "passing through"){
          construct_plot(input$measure,
                      S1, "passing through")} else {
          construct_plot(input$measure,
                      S1, "final")}
  }, res = 96)

  output$plot2 <- renderPlot({
    if(input$type == "passing through"){
      construct_plot2(input$measure,
                     S1, "passing through")} else {
      construct_plot2(input$measure,
                                      S1, "final")}
  }, res = 96)

  output$downloadData <- downloadHandler(
    filename = "2021_balls_strikes_data.csv",
    content = function(file) {
      write.csv(S1, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
