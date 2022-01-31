# app to compute brushed home run rates
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(gridExtra)
library(broom)

fit_and_residual_compare <- function(d, playerlist){
  require(ggrepel)
  d1 <- filter(d, Player %in% playerlist)

  regressions <- d1 %>%
    group_by(Player) %>%
    do(tidy(lm(HR ~ Age , data=.)))

  d1 %>%
    group_by(Player) %>%
    summarize(Total = n()) -> S1

  inner_join(regressions, S1, by = "Player") ->
    regressions

  individual <- d1 %>%
    group_by(Player) %>%
    do(augment(lm(HR ~ Age, data=.)))

  plist <- NULL
  for(j in playerlist){
    plist <- paste(plist, j, sep = " ")
  }

  plot1 <- ggplot(individual) +
    geom_point(aes(Age, HR, color = Player)) +
    geom_line(aes(Age, .fitted,
                  color = Player)) +
    labs(title = "Home Run Paths") +
    ylab("Total Home Runs") +
    theme(plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0)) +
    theme(text = element_text(size = 18))

  plot2 <- regressions %>%
    filter(term == "Age") %>%
    ggplot(aes(Total, estimate, label = Player)) +
    geom_point(size = 3, color = "red") +
    geom_text_repel(color = "blue") +
    xlab("HR Total") +
    ylab("Slope Estimate") +
    labs(title = "HR Totals and Path Slope Estimates") +
    theme(plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0)) +
    theme(text = element_text(size = 18))

  plot3 <- ggplot(individual) +
    geom_smooth(aes(Age, .resid, color = Player),
                method = "loess",
                formula = "y ~ x") +
    geom_hline(yintercept = 0,
               color = "black") +
    labs(title = "Smoothed Residuals from HR Path Fits") +
    ylab("Residual") +
    theme(plot.title = element_text(colour = "red",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0)) +
    theme(text = element_text(size = 18))

  list(plot1 = plot1,
       plot2 = plot2,
       plot3 = plot3)
}

Players <- c("Aaron", "Rodriguez", "Banks",
             "Bonds", "Cabrera", "Foxx",
             "Gehrig", "Griffey", "Jackson",
             "Killebrew", "Mantle", "Matthews",
             "Mays", "McCovey", "McGriff",
             "McGwire", "Murray", "Ortiz",
             "Ott", "Palmeiro", "Pujols",
             "Ramirez", "Robinson", "Ruth",
             "Schmidt", "Sheffield", "Sosa",
             "Thomas", "Thome", "Williams")
# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(4, wellPanel(
      h3("Home Run Path Exploration"),
      checkboxGroupInput("playerlist",
                   label = "Choose Players from Top 30 in MLB Career Home Runs:",
                   choices = Players,
                           selected = Players[1:2],
                           inline = TRUE)
    )),
    column(8,
           tabsetPanel(type = "tabs",
                       tabPanel("Home Run Paths",
                          plotOutput("plot1",
                           height = "450px")
                              ),
                       tabPanel("Fitted Slopes",
                          plotOutput("plot2",
                            height = "450px")
                              ),
                       tabPanel("Residuals from Fit",
                          plotOutput("plot3",
                            height = "450px")
                              ),
                       tabPanel("Description",
                       p(), p(),
                       p("This Home Run Path Exploration
                         was inspired by Baseball Reference
                         that provides the HR log for each of the career
                         home run leaders in Major League Baseball."),
                       p("https://www.baseball-reference.com/leaders/HR_career.shtml"),
                       p("In this app, one selects several of the top
                         30 HR career leaders to compare."),
                       p("The Home Run Paths tab displays the cumulative
                         count of home runs against age and fitted lines for the
                         selected players."),
                       p("The Fitted Slopes tab displays a scatterplot of
                         the total home run counts and the estimated
                         home run path slopes for the selected players."),
                       p("The Residuals from Fit displays smoothed residuals
                         from the line fits for each selected player.")
                          )
                     )
      )
      )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
     out <-fit_and_residual_compare(top30homerun,
                            input$playerlist)
     out$plot1
  }, res = 96)
  output$plot2 <- renderPlot({
    out <-fit_and_residual_compare(top30homerun,
                                   input$playerlist)
    out$plot2
  }, res = 96)
  output$plot3 <- renderPlot({
    out <-fit_and_residual_compare(top30homerun,
                                   input$playerlist)
    out$plot3
  }, res = 96)
}

shinyApp(ui = ui, server = server)
