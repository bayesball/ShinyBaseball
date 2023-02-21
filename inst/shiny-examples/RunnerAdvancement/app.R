library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

S1 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/runner_advancement_1.csv")

plot_advancement_single <- function(S, runners,
                                    type){

  # limit to singles hit to the field
  S %>% filter(Season <= 2019) %>%
         mutate(Field = substr(EVENT_TX, 1, 2)) %>%
         filter(Field %in% c("S7", "S8", "S9")) -> S

  # runners is either "100" or "020" or "120"

  # type can be
  # ----------------------------------
  # outs - 0, 1, or 2 outs
  # home -- break down by home and away
  # margin -- close or not close score
  # inning -- early or late
  # outfield -- either S7, S8, S9

  if(runners == "100"){
    S %>%
      filter(Runner1 == TRUE,
             Runner2 == FALSE) %>%
      mutate(Two_Bases = grepl("1-3", EVENT_TX)) -> S
  }
  if(runners == "020"){
    S %>%
      filter(Runner1 == FALSE,
             Runner2 == TRUE) %>%
      mutate(Two_Bases = grepl("2-H", EVENT_TX)) -> S
  }
  if(runners == "120"){
    S %>%
      filter(Runner1 == TRUE,
             Runner2 == TRUE) %>%
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
  if(type == "close"){
    S %>%
      mutate(Type = ifelse(Margin <= 1, "<= 1 Run",
                           "> 1 Run")) -> S
  }
  if(type == "late_inning"){
    S %>%
      mutate(Type = ifelse(INN_CT >= 8, ">= 8",
                           "< 8")) -> S
  }
  if(type == "close_and_late"){
    S %>%
      mutate(Type = ifelse(Margin <= 1 & INN_CT >= 8, 
                           "yes",
                           "no")) -> S
  }
  if(type == "outfield"){
    S %>%
      mutate(Type = Field) -> S
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
    labs(title = "Runner Advancement with a Single
to Outfield: 2000-2019",
         subtitle = the_title) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 16,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0)) +
    ylab("Pct Taking Extra Base")
}


ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
    h3(id="big-heading", "Runner Advancement"),
    p("EXTRA BASE: "),
    p("100: 1-3"),
    p("020: 2-H"),
    p("120: 1-3 AND 2-H"),
  radioButtons("runners", "Runners on Base:",
               c("100", "020", "120"),
               inline = TRUE),
  radioButtons("type", "Situational Effect:",
               c("home", "late_inning",
                 "close", "close_and_late",
                 "outs", "outfield"),
               inline = TRUE)
  )),
  column(8,
         plotOutput("plot",
                    width = '600px')
        )
)

server <- function(input, output, session) {

  output$plot <- renderPlot({
    plot_advancement_single(S1,
                            input$runners,
                            input$type)
    }, res = 96)
}

shinyApp(ui = ui, server = server)
