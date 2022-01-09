library(dplyr)
library(ggplot2)
library(Lahman)
library(geomtextpath)

selectPlayers <- function(midYearRange, minGames,
                          position){
  require(Lahman)
  Fielding %>%
    filter(POS == position) %>%
    group_by(playerID) %>%
    summarize(minYear = min(yearID),
              maxYear = max(yearID),
              midYear = (minYear + maxYear) / 2,
              Innings = sum(InnOuts / 3, na.rm = TRUE),
              G = sum(G),
              .groups = "drop")  %>%
    filter(midYear <= midYearRange[2],
           midYear >= midYearRange[1],
           G >= minGames) %>%
    select(playerID) %>%
    inner_join(People, by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    select(playerID, Name)
}
compare_plot <- function(playerid_1, playerid_2,
                         measure, xvar, position){
  require(Lahman)
  if((length(playerid_1) > 0) &
     (length(playerid_2) > 0)){
  # collect names of two players
  Name1 <- filter(Master, playerID == playerid_1) %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    select(Name) %>% pull()
  Name2 <- filter(Master, playerID == playerid_2) %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    select(Name) %>% pull()
  # collect fielding stats for two players for each season
  Fielding %>%
    filter(POS == position) %>%
    filter(playerID %in% c(playerid_1, playerid_2)) %>%
    inner_join(select(Master, playerID,
                      nameFirst, nameLast),
               by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    group_by(Name, yearID) %>%
    summarize(Innings = sum(InnOuts / 3),
              PO = sum(PO),
              A = sum(A),
              E = sum(E),
              G = sum(G),
              RF9 = 9 * (PO + A) / Innings,
              RFG = (PO + A) / G,
              FPct = (PO + A) / (PO + A + E),
              .groups = "drop") -> S

  # function to obtain birthyear for player
  get_birthyear <- function(playerid) {
    Master %>%
      filter(playerID == playerid)  %>%
      mutate(Name = paste(nameFirst, nameLast),
             birthyear = ifelse(birthMonth >= 7,
                                birthYear + 1, birthYear)) %>%
      select(Name, birthyear)
  }
  # collect birthyears and compute ages for each
  # player and season
  S1 <- rbind(get_birthyear(playerid_1),
              get_birthyear(playerid_2))
  inner_join(S, S1, by = "Name") %>%
    mutate(Age = yearID - birthyear) -> S
  # define outcome depending on input
  if(measure == "RF/9"){
    S$Outcome <- S$RF9
    S$Weight <- S$G
    YLAB <- "RF/9"
  }
  if(measure == "RF/G"){
    S$Outcome <- S$RFG
    S$Weight <- S$G
    YLAB <- "RF/G"
  }
  if(measure == "Fld%"){
    S$Outcome <- S$FPct
    S$Weight <- S$G
    YLAB <- "Fld%"
  }
  # plot versus season or age?
  if(xvar == "year"){
    S$XVAR <- S$yearID
    XLAB <- "Season"
  }
  if(xvar == "age"){
    S$XVAR <- S$Age
    XLAB <- "Age"
  }
  plot1 <- ggplot(S,
                  aes(XVAR, Outcome, color = Name,
                      weight = Weight,
                      label = Name)) +
    geom_point(size = 3) +
    geom_textsmooth(se = FALSE,
                method = "loess",
                formula = "y ~ x") +
    ylab(YLAB) +
    xlab(XLAB) +
    labs(title = paste(Name1, "and", Name2),
         subtitle = paste(position,
                          YLAB, "Career Trajectories"),
         color = "Player") +
    theme(text = element_text(size = 15),
          plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "blue",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
  list(plot1 = plot1, S = S)
  }
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  h2("Comparing Career Fielding Trajectories"),
  column(3,
  selectInput("position",
              "Select Position:",
              choices = c("SS", "2B", "OF", "C",
                          "1B", "3B", "P")),
  sliderInput("midyear", "Select Range of Mid Season:",
              1900, 2020,
              value = c(1975, 1985), sep = ""),
  sliderInput("minInnings",
              "Select Minimum Career Games:",
              500, 2000, 1000, sep = ""),
  selectInput("player_name1",
              "Select First Player:",
              choices =
                selectPlayers(c(1975, 1985),
                              2000, "SS")$Name),
  selectInput("player_name2",
              "Select Second Player:",
              choices =
                selectPlayers(c(1975, 1985),
                              2000, "SS")$Name),
  radioButtons("type",
               "Select Measure:",
               c("Fld%", "RF/9", "RF/G"),
               inline = TRUE),
  radioButtons("xvar",
               "Plot Against:",
               c("year", "age"),
               inline = TRUE),
  downloadButton("downloadData", "Download Data")
  ),
  column(9,
         plotOutput("plot1",
                    height = '500px'))
)
server <- function(input, output, session) {
  options(warn=-1)
  observeEvent(input$midyear, {
    updateSelectInput(inputId = "player_name1",
                      choices =
          selectPlayers(input$midyear,
                        input$minInnings,
                        input$position)$Name)
  })
  observeEvent(input$minInnings, {
    updateSelectInput(inputId = "player_name1",
                      choices =
          selectPlayers(input$midyear,
                        input$minInnings,
                        input$position)$Name)
  })
  observeEvent(input$position, {
    updateSelectInput(inputId = "player_name1",
                      choices =
          selectPlayers(input$midyear,
                        input$minInnings,
                        input$position)$Name)
  })
  observeEvent(input$midyear, {
    updateSelectInput(inputId = "player_name2",
                      choices =
          selectPlayers(input$midyear,
                        input$minInnings,
                        input$position)$Name)
  })
  observeEvent(input$minInnings, {
    updateSelectInput(inputId = "player_name2",
                      choices =
          selectPlayers(input$midyear,
                        input$minInnings,
                        input$position)$Name)
  observeEvent(input$position, {
      updateSelectInput(inputId = "player_name2",
                        choices =
          selectPlayers(input$midyear,
                        input$minInnings,
                        input$position)$Name)
    })
  })
  output$plot1 <- renderPlot({
    S <- selectPlayers(input$midyear,
                       input$minInnings,
                       input$position)
    id1 <- filter(S,
              Name == input$player_name1)$playerID
    id2 <- filter(S,
              Name == input$player_name2)$playerID
    compare_plot(id1, id2, input$type, input$xvar,
                 input$position)$plot1
  }, res = 96)
  output$downloadData <- downloadHandler(
    filename = "trajectory_output.csv",
    content = function(file) {
      S <- selectPlayers(input$midyear,
                         input$minInnings,
                         input$position)
      id1 <- filter(S,
                    Name == input$player_name1)$playerID
      id2 <- filter(S,
                    Name == input$player_name2)$playerID
      out <- compare_plot(id1, id2, input$type,
                          input$xvar,
                          input$position)
      write.csv(out$S, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
