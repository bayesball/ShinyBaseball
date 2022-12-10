# expected hits version
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(ShinyBaseball)
library(Lahman)

# data is dataset retro2019 located in
# data folder of ShinyBaseball package

# turn off warnings
options(warn=-1)

#scip <- read_csv("sc2021_ip3.csv")
#chadwick <- read_csv("chadwick.csv")

retro2019 %>%
  filter(AB_FL == TRUE) %>%
  group_by(BAT_ID) %>%
  summarize(AB = n()) %>%
  filter(AB >= 200)  %>%
  inner_join(People, c("BAT_ID" = "retroID")) %>%
  mutate(Name = paste(nameFirst, nameLast)) %>%
  arrange(nameLast) %>%
  select(Name, BAT_ID) -> S1

##############################################
setup_data <- function(retrodata, batid){
  retrodata %>%
    filter(BAT_ID == batid)  %>%
    arrange(GAME_ID, INN_CT)
}
#############################################
spacings_sim <- function(retrodata,
                         retroid,
                         type = "H",
                         iter = 100){
  find_spacings_geometric <- function(streak_data) {
    # input is a retrosheet data frame with
    # Outcome variable
    n <- dim(streak_data)[1]
    ab_success <- c((1:n)[streak_data$Outcome == 1],
                    n + 1)
    df <- data.frame(Spacing =
                       diff(c(0, ab_success)) - 1)
    df %>%
      mutate(N = row_number(),
             BAT_ID = streak_data$BAT_ID[1]) %>%
      select(BAT_ID, N, Spacing)
  }
  require(Lahman)

  filter(People, retroID == retroid) %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    pull(Name) -> name

  retrodata %>%
    filter(BAT_ID == retroid) %>%
    mutate(Type = type,
           AB_number = row_number()) -> d

  if(type == "H"){
    d$Outcome <- ifelse(d$EVENT_CD %in% 20:23, 1, 0)
  }
  if(type == "HR"){
    d$Outcome <- ifelse(d$EVENT_CD == 23, 1, 0)
  }
  if(type == "not SO"){
    d$Outcome <- ifelse(d$EVENT_CD == 3, 0, 1)
  }
  if(type == "SO"){
    d$Outcome <- ifelse(d$EVENT_CD == 3, 1, 0)
  }

  find_spacings_geometric(d) %>%
    summarize(Obs = sum(Spacing ^ 2)) %>%
    pull() -> Observed

  one_sim <- function(d){
    d$Outcome <- sample(d$Outcome)
    find_spacings_geometric(d) %>%
      summarize(Obs = sum(Spacing ^ 2)) %>%
      pull()
  }

  S_data <- data.frame(Statistic =
                         replicate(iter, one_sim(d)))

  p_value <- mean(S_data$Statistic >= Observed)

  ggplot(S_data,
         aes(Statistic)) +
    geom_histogram(color = "white",
                   fill = "tan",
                   bins = 10) +
    geom_vline(xintercept = Observed, lwd = 2,
               color = "red") +
    theme(axis.text = element_text(size = rel(1.5))) +
    theme(axis.title = element_text(size = rel(1.5))) +
    xlab("Clumpiness Statistic") +
    labs(title = paste(name, type),
         subtitle = paste("P-Value =", p_value)) +
    theme(plot.title = element_text(colour = "blue",
                  size = 18,
                  hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red",
                  size = 16,
                  hjust = 0.5, vjust = 0.8, angle = 0))

}

geometric_plot_app <- function(retrodata,
                               retroid,
                               type = "H"){
  find_spacings_geometric <- function(streak_data) {
    # input is a retrosheet data frame with
    # Outcome variable
    n <- dim(streak_data)[1]
    ab_success <- c((1:n)[streak_data$Outcome == 1],
                    n + 1)
    df <- data.frame(Spacing =
                       diff(c(0, ab_success)) - 1)
    df %>%
      mutate(N = row_number(),
             BAT_ID = streak_data$BAT_ID[1]) %>%
      select(BAT_ID, N, Spacing)
  }
  require(Lahman)

  filter(People, retroID == retroid) %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    pull(Name) -> name

  retrodata %>%
    filter(BAT_ID == retroid) %>%
    mutate(Type = type,
           AB_number = row_number()) -> d

  if(type == "H"){
    d$Outcome <- ifelse(d$EVENT_CD %in% 20:23, 1, 0)
  }
  if(type == "HR"){
    d$Outcome <- ifelse(d$EVENT_CD == 23, 1, 0)
  }
  if(type == "not SO"){
    d$Outcome <- ifelse(d$EVENT_CD == 3, 0, 1)
  }
  if(type == "SO"){
    d$Outcome <- ifelse(d$EVENT_CD == 3, 1, 0)
  }

  spacings_df <- find_spacings_geometric(d)

  spacings_df %>%
    group_by(Spacing) %>%
    summarize(N = n()) -> spacings_table

  ggplot(spacings_table,
         aes(Spacing, log(N))) +
    geom_point(size=3) +
    stat_smooth(method=lm,
                se=FALSE,
                formula = "y ~ x",
                linewidth=1,
                color="black") +
    stat_smooth(method=loess,
                se=FALSE,
                formula = "y ~ x",
                linewidth=2) +
    theme_minimal() +
    theme(axis.text = element_text(size = rel(1.5))) +
    theme(axis.title = element_text(size = rel(1.5))) +
    xlab("Spacing") +
    ylab("Log Frequency") +
    ggtitle(paste(name, type)) +
    theme(plot.title = element_text(colour = "blue",
                size = 18,
                hjust = 0.5, vjust = 0.8, angle = 0))

}

moving_average_plot <- function(retrodata,
                                width,
                                retroid,
                                type = "H"){
  require(RcppRoll)
  require(Lahman)

  filter(People, retroID == retroid) %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    pull(Name) -> name

  retrodata %>%
    filter(BAT_ID == retroid) %>%
    mutate(Width = width,
           Type = type,
           AB_number = row_number()) -> d

  if(type == "H"){
    d$Outcome <- ifelse(d$EVENT_CD %in% 20:23, 1, 0)
  }
  if(type == "HR"){
    d$Outcome <- ifelse(d$EVENT_CD == 23, 1, 0)
  }
  if(type == "not SO"){
    d$Outcome <- ifelse(d$EVENT_CD == 3, 0, 1)
  }
  if(type == "SO"){
    d$Outcome <- ifelse(d$EVENT_CD == 3, 1, 0)
  }

  d$Roll_BA <-
    roll_mean(d$Outcome,
              n = width,
              align = "center", fill = NA)

  d$Roll_AB_Number <-
    roll_mean(d$AB_number,
              n = width,
              align = "center", fill = NA)

  d$AVG <- mean(d$Outcome,
                na.rm = TRUE)

  subtitle <- paste("Width = ", width)
  the_title <- paste(name, type)

  d <- select(d,
              BAT_ID, Width, Type,
              GAME_ID, INN_CT, EVENT_CD,
              AB_number, Outcome,
              Roll_AB_Number, Roll_BA, AVG)

  plot1 <- ggplot(d, aes(x = Roll_AB_Number,
                         ymax = Roll_BA,
                         ymin = AVG)) +
    geom_ribbon(fill = "blue") +
    ylab("Rolling BA") +
    theme(text = element_text(size = 18)) +
    labs(title = the_title,
         subtitle = subtitle) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(color = "red",
                                       size = 16,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
  if(type == "H"){
    plot1 <- plot1 + ylab("Rolling BA")
  }
  if(type == "HR"){
    plot1 <- plot1 + ylab("Rolling HR Rate")
  }
  if(type == "not SO"){
    plot1 <- plot1 + ylab("Rolling not SO Rate")
  }
  if(type == "SO"){
    plot1 <- plot1 + ylab("Rolling SO Rate")
  }
  list(plot1 = plot1, S = d)
}

#############################################

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  h2("Streakiness in At-Bat Batting Performance"),
  fluidRow(
    column(4, wellPanel(
      selectInput("player_name",
                  "Select 2021 Batter (at Least 200 AB):",
                  S1$Name,
                  selected = "Bryce Harper"),
      radioButtons("type", "Select Measure:",
                   c("H", "HR", "SO", "not SO"),
                   "H", inline = TRUE),
      sliderInput("width", "Width for Moving Average:",
                  min = 1, max = 80,
                  value = 20,
                  animate = FALSE),
      downloadButton("downloadData", "Download Data")
      )),
    column(8,
           tabsetPanel(type = "tabs",
              tabPanel("Moving Average Plot",
                   plotOutput("plot1",
                       height = "500px")
              ),
              tabPanel("Geometric Plot",
                   plotOutput("plot2",
                       height = "500px")
              ),
              tabPanel("Simulation",
                       plotOutput("plot3",
                                  height = "500px")
              ),
              tabPanel("Description",
                       hr(),
                       p("This app displays moving averages
                         and geometric plots of batting data for any
                         2021 batter of interest."),
                       p("One inputs the batter player,
                         the measure (either H, HR, SO,
                         or not SO) and the width
                         for the moving average."),
                       p("The Moving Average tab displays a graph of
                         the moving average against the at-bat
                         number.  The shaded region shows the
                         deviations of the moving average from
                         the overall average."),
                       p("The Geometric Plot tab constructs a
                         geometric plot of the spacings between
                         successes.  If the points follow a line
                         the spacings are approximately
                         Geometric distributed."),
                        p("The Simulation tab shows a histogram
                         of a clumpiness measure when the
                         outcomes are randomly permuted.")
              )
           )
          )
          )
)

server <- function(input, output, session) {

  output$plot1 <- renderPlot({

    player_id <- filter(S1,
                Name == input$player_name) %>%
                pull(BAT_ID)
    moving_average_plot(filter(retro2019,
                               AB_FL == TRUE),
                        width = input$width,
                        retroid = player_id,
                        type = input$type)$plot1
  }, res = 96)

  output$plot2 <- renderPlot({
    player_id <- filter(S1,
                Name == input$player_name) %>%
      pull(BAT_ID)
    geometric_plot_app(filter(retro2019,
                              AB_FL == TRUE),
                        retroid = player_id,
                        type = input$type)
  }, res = 96)

  output$plot3 <- renderPlot({
    player_id <- filter(S1,
                        Name == input$player_name) %>%
      pull(BAT_ID)
    spacings_sim(filter(retro2019,
                        AB_FL == TRUE),
                 retroid = player_id,
                 type = input$type)
  }, res = 96)

  output$downloadData <- downloadHandler(
    filename = "streak_output.csv",
    content = function(file) {
      player_id <- filter(S1,
              Name == input$player_name) %>%
              pull(BAT_ID)
      out <- moving_average_plot(
                          filter(retro2019, AB_FL == TRUE),
                          width = input$width,
                          retroid = player_id,
                          type = input$type)
      write.csv(out$S, file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)
