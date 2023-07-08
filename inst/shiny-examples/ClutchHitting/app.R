# https://bayesball.shinyapps.io/Clutch_Hitting/

library(dplyr)
library(readr)

source("setup_work.R")
source("clutch_work2.R")
source("get_player_list.R")
d2022a <- read_csv("retro2022.csv")
tr <- read_csv("transitions.csv")
tr_new <- setup_work(d2022a, tr)
names_df <- get_player_list(d2022a)

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("Clutch Hitting Explorer"),
  column(3,
  hr(),
  p("This app simulates the season runs values of a player assuming that events are independent of the bases/outs situation."),
  p("A player displays clutch performance if the observed runs value exceeds what is expected from the no association model."),
  hr(),
  selectInput("player_name",
              "Select Player from 2022 Season (PA >= 400):",
               choices = names_df$Name),
  sliderInput("nsim", "Number of Simulations:",
              50, 500, 200)
  ),
  column(9,
         tabsetPanel(type = "tabs",
            tabPanel("Simulated Runs",
                     plotOutput("plot1",
                     height = '550px'))
         )
  )
)
server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    names_df %>%
      filter(Name == input$player_name) %>%
      pull(retroID) -> retro_id
    clutch_work2(d2022a, tr_new,
                 retro_id, nsims = input$nsim)
  }, res = 96)

}

shinyApp(ui = ui, server = server)
