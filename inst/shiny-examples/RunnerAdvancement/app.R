library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)

#S <- read_csv("runner_advancement_12.csv")

plot_advancement_single <- function(S, runners,
                                    type){

  # runners is either "100" or "010" or "110"

  # type can be
  # ----------------------------------
  # outs - 0, 1, or 2 outs
  # home -- break down by home and away
  # margin -- close or not close score
  # inning -- early or late

  if(runners == "100"){
    S %>%
      mutate(Two_Bases = grepl("1-3", EVENT_TX)) -> S
  }
  if(runners == "010"){
    S %>%
      mutate(Two_Bases = grepl("2-H", EVENT_TX)) -> S
  }
  if(runners == "110"){
    S %>%
      mutate(First_Runner = grepl("1-3", EVENT_TX),
             Second_Runner = grepl("2-H", EVENT_TX),
             Two_Bases = First_Runner * Second_Runner) -> S
  }
  S %>%
    mutate(Home = ifelse(BAT_HOME_ID == 1,
                         "yes", "no")) -> S

  if(type == "home"){
    S %>%
      mutate(Type = Home) -> S
  }
  if(type == "outs"){
    S %>%
      mutate(Type = as.character(Outs)) -> S
  }
  if(type == "margin"){
    S %>%
      mutate(Type = ifelse(Margin <= 1, "Close",
                           "Not Close")) -> S
  }
  if(type == "inning"){
    S %>%
      mutate(Type = ifelse(INN_CT >=8, "Late",
                           "Early")) -> S
  }

  S %>%
    group_by(Season, Type) %>%
    summarize(N = n(),
              Two = sum(Two_Bases),
              .groups = "drop") %>%
    mutate(Two_Pct = 100 * Two / N) -> S_out

  the_title <- paste("Runners: ", runners,
                     ", Type: ", type, sep = "" )

  ggplot(S_out, aes(Season, Two_Pct,
                    color = Type)) +
    geom_point(size = 3) +
    geom_smooth(se = FALSE,
                method = "loess",
                formula = "y ~ x") +
    theme(text=element_text(size=18)) +
    labs(title = "Runner Advancement with a Single",
         subtitle = the_title) +
    theme(plot.title = element_text(colour = "blue",
                          size = 18,
                 hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                          size = 18,
                 hjust = 0.5, vjust = 0.8, angle = 0)) +
    ylab("Pct Taking Extra Base")
}


ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h3(id="big-heading", "Runner Advancement"),
  p("EXTRA BASE: "),
  p("100: 1-3"),
  p("010: 2-H"),
  p("110: 1-3 and 2-H"),
  tags$style(HTML("#big-heading{color: blue;}")),
  radioButtons("runners", "Runners on Base:",
               c("100", "010", "110"),
               inline = TRUE),
  radioButtons("type", "Situational Effect:",
               c("home", "inning",
                 "margin", "outs"),
               inline = TRUE)
  )),
  column(8,
         plotOutput("plot",
                    height = '450px')
        )
)

server <- function(input, output, session) {

  observeEvent(input$minIP, {
    updateSelectInput(inputId = "name",
                      choices =
                SelectPlayers(sc2022ip, input$minIP)$Name)
  })

  output$plot <- renderPlot({

    plot_advancement_single(runner_advancement_12,
                            input$runners,
                            input$type)

    }, res = 96)
}

shinyApp(ui = ui, server = server)
