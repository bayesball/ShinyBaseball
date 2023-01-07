library(shiny)
library(flextable)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

Beta <- twentyyears_RA
Beta$Bases <- gsub("[“”]", "", Beta$Bases)

Beta$Outs <- as.character(Beta$Outs)

table_advantage <- function(Beta, seasons){

  Beta %>%
    filter(Season %in% seasons) %>%
    group_by(Bases, Outs) %>%
    summarize(Coef = mean(Coef),
              Bases_Score = first(Bases_Score),
              .groups = "drop") ->
    Beta_summary

  create_table <- function(DF){
    DF <- DF[c(1:3,
               13:15, 7:9, 19:21,
               4:6, 16:18, 10:12,
               22:24), ]

    M <- matrix(round(DF$Coef, 2), 3, 8)
    dimnames(M) <- list(c("0", "1", "2"),
                        c("000", "100", "020", "120",
                          "003", "103", "023", "123"))
    M
  }
  create_table(Beta_summary)
}

plot_advantage <- function(Beta, seasons){

  library(dplyr)
  library(ggplot2)

  Beta$Outs <- as.character(Beta$Outs)

  Beta %>%
    filter(Season %in% seasons) %>%
    group_by(Bases, Outs) %>%
    summarize(Coef = mean(Coef),
              Bases_Score = first(Bases_Score),
              .groups = "drop") ->
    Beta_summary

  states <- c("000", "100", "020", "120",
              "003", "103", "023", "123")

  season_label <- paste(min(seasons),
                        max(seasons), sep = "-")

  ggplot(Beta_summary,
         aes(Bases_Score, Coef, color = Outs)) +
    geom_point(size = 3) +
    geom_smooth(se = FALSE,
                method = "loess",
                formula = "y ~ x") +
    ggtitle(paste(season_label, "Ordinal Coefficients")) +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    xlab("Bases Score") +
    ylab("Coefficient") +
    scale_x_continuous(breaks = 0:7,
                       labels = states)

}

ui <- fluidPage(
  titlePanel("Runs Advantages"),
  sidebarLayout(
    sidebarPanel(
      br(),
      sliderInput("seasons", "Select Seasons:",
                  min = 2000, max = 2019,
                  value = c(2000, 2019), sep = "")
    ),
    mainPanel(uiOutput("runs_advantage"),
              plotOutput("plot"))
  )
)

server <- function(input, output) {
  output$runs_advantage <- renderUI({
    req(input$seasons)

    season_n <- input$seasons[1]: input$seasons[2]
    RA_season <- as.data.frame(table_advantage(Beta,
                              season_n))
    RA_season$Outs <- c("0", "1", "2")
    RA_season <- RA_season[, c(9, 1:8)]

    season_label <- paste(input$seasons[1],
                          input$seasons[2], sep = "-")
    RA_season %>%
      flextable() %>%
      add_header_row(values = c("", "Runners on Base",
                             "", ""),
                     colwidths = c(1, 6, 1, 1)) %>%
      set_caption(
            caption = paste(season_label,
                        "Runs Advantage Matrix")) %>%
      theme_vader() %>%
      autofit() %>%
      htmltools_value()
  })

  output$plot <- renderPlot({

    season_n <- input$seasons[1]: input$seasons[2]
    plot_advantage(Beta, season_n)

})
}

# Run the application
shinyApp(ui = ui, server = server)
