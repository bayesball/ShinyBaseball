library(shiny)
library(readr)
library(dplyr)

all_predict <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/predict_dataset.csv")
bin_plot <- function(S, LA_breaks, LS_breaks, label,
                     mytitle = "") {
  require(dplyr)
  require(purrr)
  require(stringr)
  require(ggplot2)
  compute_bin_midpoint <- function(x) {
    x |>
      as.character() |>
      str_split_1(",") |>
      map_dbl(parse_number) |>
      mean()
  }
  S |>
    mutate(
      la = map_dbl(LA, compute_bin_midpoint),
      ls = map_dbl(LS, compute_bin_midpoint)
    ) |>
    ggplot(aes(x = la, y = ls)) +
    geom_text(aes(label = {{label}}), size = 8) +
    geom_vline(
      xintercept = LA_breaks,
      color = "blue"
    ) +
    geom_hline(
      yintercept = LS_breaks,
      color = "blue"
    ) +
    theme(text = element_text(size = 18)) +
    labs(x = "Launch Angle", y = "Launch Speed",
         title = mytitle) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
}

ui <- fluidPage(
    theme = shinythemes::shinytheme("slate"),
    h2("Home Run Ball Effects"),
    column(
      3,
      radioButtons("season1", "Choose model season:",
                   c("2015", "2016", "2017", "2018",
                   "2019", "2020", "2021", "2022"),
                   inline = TRUE),
      radioButtons("season2", "Choose predict season:",
                   c("2016", "2017", "2018",
                   "2019", "2020", "2021", "2022", "2023"),
                   inline = TRUE),
      radioButtons("type", "Choose what to display:",
                   c("Observed Bin Counts" = "bin_counts",
                     "Predicted Counts" = "pred_counts",
                     "Difference in Counts" = "dcount",
                     "Pearson Z Scores" = "zscore")),
      hr(),
      p("Fit GAM model for probability of HR in the model season."),
      p("Predict the probability of HR in the observed season."),
      p("Compare observed HR with predicted HR total in each bin of launch angles and exit velocities.")
    ),
    column(
      9,
      plotOutput("plot1", height = "500px"),
      tableOutput("data")
     )
    )

  server <- function(input, output, session) {

    output$plot1 <- renderPlot(
      {
        LA_breaks <- seq(15, 45, by = 5)
        LS_breaks <- seq(90, 115, by = 5)
        df <- filter(all_predict,
                     Model_Season == as.numeric(input$season1),
                     Predict_Season == as.numeric(input$season2))

        if(input$type == "bin_counts"){
            bin_plot(df, LA_breaks, LS_breaks, HR,
                     paste(input$season2, "Observed HR Bin Counts"))
        } else if (input$type == "pred_counts") {
            bin_plot(df, LA_breaks, LS_breaks, round(Predicted),
                     paste(input$season2, "HR Counts Predicted Using",
                           input$season1, "Model Fit"))
        } else if (input$type == "dcount") {
            bin_plot(df, LA_breaks, LS_breaks,
                   HR - round(Predicted),
                   paste(input$season2, "HR Observed Minus",
                         input$season1, "HR Predicted"))
        } else {
            bin_plot(df, LA_breaks, LS_breaks,
                   round(Z, 1),
                   paste("Z-Score Comparing Observed HR with Predicted"))
        }
      },
      res = 96
    )
    output$data <- renderTable({

      df <- filter(all_predict,
                   Model_Season == as.numeric(input$season1),
                   Predict_Season == as.numeric(input$season2))
      data.frame(Type = c("Observed", "Model Predicted"),
                 Season = c(input$season2, input$season1),
                 HR = c(sum(df$HR), sum(df$Predicted)),
                 Observed_Minus_Predicted =
                   c(sum(df$HR) - sum(df$Predicted), NA)
                 )
    }, digits = 0, width = '75%', align = 'c',
    bordered = TRUE,
    caption = "Comparing Observed  with Predicted Totals in Region",
    caption.placement = "top")
  }

  shinyApp(ui = ui, server = server)
