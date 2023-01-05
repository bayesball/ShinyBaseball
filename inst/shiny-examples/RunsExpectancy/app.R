library(shiny)
library(flextable)
library(dplyr)
library(tidyr)
library(ggplot2)

# data is dataset twentyyears_RE in  data
# folder of package

RE <- twentyyears_RE
RE$Bases <- gsub("[“”]", "", RE$Bases)

make_plot <- function(RE, seasons, type){

  title_season <- paste(min(seasons), "-", max(seasons),
                        sep = "")
  filter(RE, Season %in% seasons) %>%
    group_by(Bases, Outs) %>%
    summarize(Mean = mean(Mean),
              Prob = mean(Prob),
              Prob2 = mean(Prob2),
              .groups = "drop") -> R

  R$Bases <- factor(R$Bases,
                    levels = c("000", "100", "020", "120",
                               "003", "103", "023", "123"))
  R$Outs <- as.character(R$Outs)

  if(type == "RE"){
    p1 <- ggplot(R, aes(as.numeric(Bases), Mean,
                        color = Outs)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm",
                  formula = "y ~ x",
                  se = FALSE) +
      ggtitle(paste(title_season, "Seasons, Expected Runs")) +
      theme(text = element_text(size = 18),
            plot.title = element_text(colour = "blue",
                                      size = 18,
                                      hjust = 0.5, vjust = 0.8, angle = 0)) +
      xlab("Bases Score (Column of RE Matrix)") +
      ylab("Runs Expectancy")
  }
  if(type == "P"){
    p1 <- ggplot(R, aes(as.numeric(Bases), Prob,
                        color = Outs)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm",
                  formula = "y ~ x",
                  se = FALSE) +
      ggtitle(paste(title_season,
                    "Seasons, Probability of Scoring")) +
      theme(text = element_text(size = 18),
            plot.title = element_text(colour = "blue",
                                      size = 18,
                                      hjust = 0.5, vjust = 0.8, angle = 0)) +
      xlab("Bases Score (Column of RE Matrix)") +
      ylab("Prob(Scoring)")
  }
  if(type == "P2"){
    p1 <- ggplot(R, aes(as.numeric(Bases), Prob2,
                        color = Outs)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm",
                  formula = "y ~ x",
                  se = FALSE) +
      ggtitle(paste(title_season,
                    "Seasons, Probability of Scoring 2 or More Runs")) +
      theme(text = element_text(size = 18),
            plot.title = element_text(colour = "blue",
                                      size = 18,
                                      hjust = 0.5, vjust = 0.8, angle = 0)) +
      xlab("Bases Score (Column of RE Matrix)") +
      ylab("Prob(Scoring 2+ Runs)")
  }
  p1
}

make_fit <- function(RE, seasons, type){
  filter(RE, Season %in% seasons) %>%
    group_by(Bases, Outs) %>%
    summarize(Mean = mean(Mean),
              Prob = mean(Prob),
              Prob2 = mean(Prob2),
              .groups = "drop") -> R

  compute_score <- function(bases){
    b <- as.numeric(unlist(strsplit(bases, "")))
    sum((b == 1) + 2 * (b == 2) + 4 * (b == 3))
  }
  R$Score <- sapply(R$Bases, compute_score)
  R$Outs <- as.character(R$Outs)

  if(type == "RE"){
    fit <- lm(Mean ~ Outs * Score, data = R)
  }
  if(type == "P"){
    fit <- lm(Prob ~ Outs * Score, data = R)
  }
  if(type == "P2"){
    fit <- lm(Prob2 ~ Outs * Score, data = R)
  }
  B <- coef(fit)
  data.frame(Outs = 0:2,
             Intercepts = round(c(B[1], B[1] + B[2],
                                  B[1] + B[3]), 3),
             Slopes = round(c(B[4], B[4] + B[5],
                              B[4] + B[6]), 3))
}

ui <- fluidPage(
  titlePanel("Runs Expectancies"),
  sidebarLayout(
    sidebarPanel(
      br(),
      sliderInput("seasons", "Select Seasons:",
                  min = 2000, max = 2019,
                  value = c(2000, 2019), sep = ""),
      radioButtons("type", "Select Metric:",
                   choices = c("Expected Runs",
                               "Probability of Scoring",
                               "Probability of ≥ 2 Runs"),
                   selected = "Expected Runs"),
      br(), br(),
      h4("Summary of Least-Squares Fit:"),
      uiOutput("lm_fit"),
      p("Slope is the estimated increase
        in the Metric for one unit change in the Bases
        Score.")
    ),
    mainPanel(uiOutput("runs_expectancy"),
              plotOutput("plot"))
  )
)

server <- function(input, output) {
  output$runs_expectancy <- renderUI({
    req(input$seasons)

    season_title <- paste(input$seasons[1],
                          input$seasons[2],
                          sep = "-")
    season_n <- input$seasons[1]:input$seasons[2]

    if(input$type == "Expected Runs"){
    RE_season <- filter(RE, Season %in% season_n) %>%
      group_by(Outs, Bases) %>%
      summarize(Mean = round(mean(Mean), 3),
                .groups = "drop") %>%
    pivot_wider(names_from = Bases,
                values_from = Mean) -> RE_season
    RE_season <- RE_season[, c(1, 2, 6, 4, 8,
                             3, 7, 5, 9)]

    RE_season %>%
      flextable() %>%
      add_header_row(values = c("Runners on Base", "",
                              "", ""),
                     colwidths = c(6, 1, 1, 1)) %>%
      set_caption(
            caption = paste(season_title,
                        "Runs Expectancy Matrix")) %>%
      theme_vader() %>%
      autofit() %>%
      htmltools_value()
    } else if(input$type ==
              "Probability of Scoring") {
      RE_season <- filter(RE, Season %in% season_n) %>%
        group_by(Outs, Bases) %>%
        summarize(Prob = round(mean(Prob), 3),
                  .groups = "drop") %>%
      pivot_wider(names_from = Bases,
                  values_from = Prob) -> RE_season
      RE_season <- RE_season[, c(1, 2, 6, 4, 8,
                                 3, 7, 5, 9)]

      RE_season %>%
        flextable() %>%
        add_header_row(values = c("Runners on Base", "",
                                  "", ""),
                       colwidths = c(6, 1, 1, 1)) %>%
        set_caption(
          caption = paste(season_title,
              "Probability of Scoring Matrix")) %>%
        theme_vader() %>%
        autofit() %>%
        htmltools_value()
    } else {
      RE_season <- filter(RE, Season %in% season_n) %>%
        group_by(Outs, Bases) %>%
        summarize(Prob2 = round(mean(Prob2), 3),
                  .groups = "drop") %>%
      pivot_wider(names_from = Bases,
                  values_from = Prob2) -> RE_season
      RE_season <- RE_season[, c(1, 2, 6, 4, 8,
                                 3, 7, 5, 9)]

      RE_season %>%
        flextable() %>%
        add_header_row(values = c("Runners on Base", "",
                                  "", ""),
                       colwidths = c(6, 1, 1, 1)) %>%
        set_caption(
          caption = paste(season_title,
                          "Probability of Scoring 2+ Runs Matrix")) %>%
        theme_vader() %>%
        autofit() %>%
        htmltools_value()
    }
  })

  output$lm_fit <- renderUI({
    req(input$seasons)

    season_n <- input$seasons[1]:input$seasons[2]

    if(input$type == "Expected Runs"){
    make_fit(RE, season_n, type = "RE") %>%
      flextable() %>%
      theme_vader() %>%
      autofit() %>%
      htmltools_value()
      } else if(input$type ==
                "Probability of Scoring") {
    make_fit(RE, season_n, type = "P") %>%
      flextable() %>%
      theme_vader() %>%
      autofit() %>%
      htmltools_value()
      } else {
   make_fit(RE, season_n, type = "P2") %>%
      flextable() %>%
      theme_vader() %>%
      autofit() %>%
      htmltools_value()
      }

  })

  output$plot <- renderPlot({

    season_n <- input$seasons[1]:input$seasons[2]

    if(input$type == "Expected Runs"){
      make_plot(RE, season_n,
                               type = "RE")
    } else if(input$type ==
              "Probability of Scoring") {
      make_plot(RE, season_n,
                               type = "P")
    } else {
      make_plot(RE, season_n,
                type = "P2")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
