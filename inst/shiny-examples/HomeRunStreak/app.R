library(shiny)
library(readr)
library(dplyr)

# player_list <- read_csv("data/playerlist.csv")
player_list <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/playerlist.csv")
plist <- c(player_list$retroID)
names(plist) <- player_list$Name
# retrod <- read_csv("data/retro_all_50.csv")
retrod <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/retro_all_50.csv")
# source("R/streaky_player_BF_new.R")
# source("R/collect_streaky.R")

streaky_player_BF_new <- function(year, retro_player, lK = 5){
  require(dplyr)
  require(BayesTestStreak)
  
  names(retro_player) <- str_to_lower(names(retro_player))
  
  retro_player |> 
    mutate(game_id = as.character(game_id))  |> 
    filter(season == year) -> retro_player
  
  streaky_measure <- function(retro_data, logK = lK) {
    retro_data |> 
      mutate(
        Outcome = ifelse(event_cd == 23, 1, 0),
        date = substr(game_id, 4, 12)
      ) |> 
      arrange(date) |> 
      bayes_factor_logK(logK)
  }
  
  retro_player |> 
    summarize(bat_id = first(bat_id),
              Season = first(season),
              PA = n(),
              HR = sum(event_cd == 23)) -> S
  
  if(S$HR > 0){
    lbf_out <-  streaky_measure(retro_player)$log_BF} else {
      lbf_out <- NA
    }
  
  S |> 
    mutate(logBF = lbf_out)
}

collect_streaky <- function(playerid, d, name = "",
                            lK = 5){
  require(purrr)
  require(ggplot2)
  d |> 
    filter(BAT_ID == playerid) -> rdata
  player_seasons <- unique(rdata$Season)
  map(player_seasons, streaky_player_BF_new, rdata, lK) |>
    list_rbind() -> out
  p <- ggplot(out, aes(Season, logBF)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linewidth = 1.5,
               color = "red") +
    ylab("log Bayes Factor") + 
    ggtitle(paste(name, "Home Run Streak Patterns")) +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  list(out = out, p = p)
}

########
ui <- fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    h2("Home Run Streakiness"),
    column(
      3,
      selectInput("player",
                  paste("Select Hitter Among Top 50 in Career Home Runs List:"),
                  choices = plist
      ),
      hr(),
      h4("Graph displays the log Bayes factor in support of a streaky model compared to a consistent model for all seasons of the player's career."),
      h4("log BF = 0 indicates both models supported by data."),
      hr(),
      downloadButton("downloadData", "Download Data"),
    ),
    column(
      9,
      plotOutput("plot1", height = "500px")
     )
    )

  server <- function(input, output, session) {

    output$plot1 <- renderPlot(
      {
        Name <- names(plist)[plist == input$player]
        S <- collect_streaky(input$player, retrod,
                             name = Name)
        print(S$p)
      },
      res = 96
    )
    output$downloadData <- downloadHandler(
      filename = "streak_output.csv",
      content = function(file) {
       
        Name <- names(plist)[plist == input$player]
        D <- collect_streaky(input$player, retrod)$out |> 
          mutate(Name = Name)

        write.csv(D, file, row.names = FALSE)
      }
    )
  }

  shinyApp(ui = ui, server = server)
