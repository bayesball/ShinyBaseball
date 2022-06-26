library(shiny)
library(dplyr)
library(readr)

# read in 2022 in-play data (through 6-24-22)

sc2022_ip <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/sc2022_ip.csv")
chadwick <- read.table("https://raw.githubusercontent.com/bayesball/ShinyBaseball/main/data/chadwick.txt",
                       header = TRUE)

# function to obtain subset of players

SelectPlayers <- function(minIP, ptype){
  if(ptype == "batter") {
      sc2022_ip %>%
      group_by(batter) %>%
      summarize(N = n()) %>%
      filter(N >= minIP) %>%
      inner_join(chadwick,
             by = c("batter" = "key_mlbam")) %>%
      mutate(Name = paste(name_first, name_last))  %>%
      arrange(name_last) -> sc} else {
      sc2022_ip %>%
      group_by(pitcher) %>%
      summarize(N = n()) %>%
      filter(N >= minIP) %>%
      inner_join(chadwick,
              by = c("pitcher" = "key_mlbam")) %>%
      mutate(Name = paste(name_first, name_last))  %>%
      arrange(name_last) -> sc
    }
  sc
}

# main plotting function

ewoba_plot <- function(scip,
                       player_info,
                       ptype,
                       min_wOBA,
                       minmax){
  require(scico)
  require(ggplot2)
  require(dplyr)

  if(ptype == "batter"){
    sc <- filter(scip,
              batter == player_info$batter)} else {
    sc <- filter(scip,
             pitcher == player_info$pitcher)
                 }

  if(minmax == ">="){
      sc <- filter(sc,
                 estimated_woba_using_speedangle >=
                   min_wOBA)
      label <- ">="} else {
      sc <- filter(sc,
                   estimated_woba_using_speedangle <=
                     min_wOBA)
      label <- "<="
    }

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

  ggplot(sc, aes(plate_x, plate_z,
                 color = estimated_woba_using_speedangle)) +
    coord_fixed() +
    geom_point(size = 2) +
    xlim(-1.2, 1.2) +
    ylim(1, 4) +
    add_zone("blue") +
    scale_color_scico(palette = "roma") +
    labs(title = paste("2022", player_info$Name),
         subtitle = paste("E(wOBA)", label, min_wOBA),
         color = "E(wOBA)") +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(color = "red",
                                       size = 14,
                                       hjust = 0.5, vjust = 0.8, angle = 0),
          text=element_text(size=18))
}


# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h4("In-Play E(wOBA) Values About Zone"),
      radioButtons("ptype",
                   "Select Player Type:",
                   c("batter", "pitcher"),
                   inline = TRUE),
      sliderInput("minIP",
                  "Select Minimum Number of In-Play:",
                  min = 50, max = 500,
                  value = 200),
      selectInput("player",
                  "Select Player:",
                  SelectPlayers(200, "batter")$Name),
      sliderInput("min_wOBA",
                  "Select E(wOBA) Bound:",
                  min = 0, max = 2,
                  value = 0,
                  step = 0.1),
      radioButtons("minmax",
                   "Select >= or <= Bound:",
                   c(">=", "<="),
                   inline = TRUE)
    )),
    column(8,
           plotOutput("plot1", height = "560px")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$minIP, {
    updateSelectInput(inputId = "player",
                      choices =
                        SelectPlayers(input$minIP,
                                      input$ptype)$Name)
  })
  observeEvent(input$ptype, {
    updateSelectInput(inputId = "player",
                      choices =
                        SelectPlayers(input$minIP,
                                      input$ptype)$Name)
  })
  output$plot1 <- renderPlot({
    player_info <- filter(SelectPlayers(input$minIP,
                                        input$ptype),
                          Name == input$player)
    ewoba_plot(sc2022_ip,
           player_info,
           input$ptype,
           input$min_wOBA,
           input$minmax)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
