library(readr)
library(dplyr)
library(ggplot2)
library(Lahman)
library(geomtextpath)

fg <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/woba_wts.csv")

selectPlayers <- function(midYearRange, minPA){
  require(Lahman)
  Batting %>%
    mutate(PA = ifelse(is.na(SF) == FALSE,
                       AB + BB + SH + SF + HBP,
                       AB + BB + SH + HBP)) %>%
    group_by(playerID) %>%
    summarize(minYear = min(yearID),
              maxYear = max(yearID),
              midYear = (minYear + maxYear) / 2,
              PA = sum(PA),
              .groups = "drop")  %>%
    filter(midYear <= midYearRange[2],
           midYear >= midYearRange[1],
           PA >= minPA) %>%
    select(playerID) %>%
    inner_join(Master, by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    select(playerID, Name)
}
compare_plot <- function(playerid_1, playerid_2,
                         measure, xvar, fg){
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
  # collect hitting stats for two players for each season
  Batting %>%
    filter(playerID %in% c(playerid_1, playerid_2)) %>%
    inner_join(select(Master, playerID,
                      nameFirst, nameLast),
               by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    group_by(Name, yearID) %>%
    summarize(H = sum(H),
              AB = sum(AB),
              HR = sum(HR),
              H = sum(H),
              X2B = sum(X2B),
              X3B = sum(X3B),
              X1B = H - X2B - X3B - HR,
              TB = X1B + 2 * X2B + 3 * X3B + 4 * HR,
              BB = sum(BB),
              HBP = sum(HBP),
              SF = sum(SF),
              SO = sum(SO),
              AVG = H / AB,
              RC = TB * (H + BB) / (AB + BB),
              .groups = "drop") -> S
  # merge fangraphs weights for wOBA
  # compute wOBA for each player each season
  inner_join(S, fg, by = c("yearID" = "Season"))  %>%
    mutate(wOBA_num = (wBB * BB + wHBP * HBP + w1B * X1B +
                         w2B * X2B + w3B * X3B + wHR * HR),
           wOBA_den = ifelse(is.na(SF) == FALSE,
                             AB + BB + SF + HBP,
                             AB + BB + HBP),
           wOBA = wOBA_num / wOBA_den,
           BB_Rate = 100 * BB / wOBA_den,
           SO_Rate = 100 * SO / wOBA_den,
           HR_Rate = 100 * HR / wOBA_den) -> S
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
  if(measure == "AVG"){
    S$Outcome <- S$AVG
    S$Weight <- S$AB
    YLAB <- "AVG"
  }
  if(measure == "HR Rate"){
    S$Outcome <- S$HR_Rate
    S$Weight <- S$wOBA_den
    YLAB = "Home Run Rate"
  }
  if(measure == "wOBA"){
    S$Outcome <- S$wOBA
    S$Weight <- S$wOBA_den
    YLAB = "wOBA"
  }
  if(measure == "RC"){
    S$Outcome <- S$RC
    S$Weight <- S$wOBA_den
    YLAB = "RC"
  }
  if(measure == "SO Rate"){
    S$Outcome <- S$SO_Rate
    S$Weight <- S$wOBA_den
    YLAB = "SO Rate"
  }
  if(measure == "BB Rate"){
    S$Outcome <- S$BB_Rate
    S$Weight <- S$wOBA_den
    YLAB = "BB Rate"
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
         subtitle = paste(YLAB, "Career Trajectories"),
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
  h2("Comparing Career Batting Trajectories"),
  column(3,
  sliderInput("midyear", "Select Range of Mid Season:",
              1900, 2010,
              value = c(1975, 1985), sep = ""),
  sliderInput("minpa", "Select Minimum Career PA:",
              1000, 10000, 5000, sep = ""),
  selectInput("player_name1",
              "Select First Batter:",
              choices =
                selectPlayers(c(1975, 1985), 5000)$Name),
  selectInput("player_name2",
              "Select Second Batter:",
              choices =
                selectPlayers(c(1975, 1985), 5000)$Name),
  radioButtons("type",
               "Select Measure:",
               c("AVG", "HR Rate", "wOBA", "RC",
                 "SO Rate", "BB Rate"),
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
                        input$minpa)$Name)
  })
  observeEvent(input$minpa, {
    updateSelectInput(inputId = "player_name1",
                      choices =
          selectPlayers(input$midyear,
                        input$minpa)$Name)
  })
  observeEvent(input$midyear, {
    updateSelectInput(inputId = "player_name2",
                      choices =
          selectPlayers(input$midyear,
                       input$minpa)$Name)
  })
  observeEvent(input$minpa, {
    updateSelectInput(inputId = "player_name2",
                      choices =
          selectPlayers(input$midyear,
                       input$minpa)$Name)
  })
  output$plot1 <- renderPlot({
    S <- selectPlayers(input$midyear,
                       input$minpa)
    id1 <- filter(S,
              Name == input$player_name1)$playerID
    id2 <- filter(S,
              Name == input$player_name2)$playerID
    compare_plot(id1, id2, input$type, input$xvar,
                 fg)$plot1
  }, res = 96)
  output$downloadData <- downloadHandler(
    filename = "trajectory_output.csv",
    content = function(file) {
      S <- selectPlayers(input$midyear,
                         input$minpa)
      id1 <- filter(S,
                    Name == input$player_name1)$playerID
      id2 <- filter(S,
                    Name == input$player_name2)$playerID
      out <- compare_plot(id1, id2, input$type, input$xvar,
                   fg)
      write.csv(out$S, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
