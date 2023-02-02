library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

fg <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/fg_pitchers_21_22.csv") 
# fg <- read_csv("fg_pitchers.csv")
varlist <- names(fg)[c(10:18, 20:22)]
seasons <- c(2013:2019, 2021:2022)

############################

plot_2_var <- function(fg_subset, var2){
  
  season1 <- 2021
  season2 <- 2022
  
  fg_subset %>% 
    filter(Season %in% c(season1, season2)) ->
    fg_subset2
  
  d1 <- fg_subset2[fg_subset2$Season == season1, 
                   c("Name", var2)]
  d2 <- fg_subset2[fg_subset2$Season == season2, 
                   c("Name", var2)]
  d12 <- inner_join(d1, d2, by = "Name")
  mycolnames <- paste("Y", c(season1, season2), sep = "")
  names(d12)[2:3] <- mycolnames
  
  r <- round(cor(d12[, 2:3], 
                 use = "pairwise.complete.obs")[1, 2], 3)
  
  the_title <- paste("Var: ", var2, ", Seasons: ",
                     season1, ", ", season2, sep = "")
   ggplot(d12, aes(x = Y2021, y = Y2022)) +
    geom_point(size = 3, color = "blue") +
    labs(title = the_title,
         subtitle = paste("Correlation =", r)) +
    xlab(paste(season1, var2)) +
    ylab(paste(season2, var2)) +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue", 
                                    size = 24,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(plot.subtitle = element_text(colour = "red", 
                                       size = 20,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
}
############################
ui <- fluidPage(
  titlePanel("FanGraphs 2021 to 2022 Pitching Measures"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("var",
                    "Select Pitching Measure:",
                    choices = varlist,
                    selected = varlist[1],
                   inline = FALSE),
      h4("Data source: "),
      p("FanGraphs Pitching Leaders, Dashboard, 
         All pitchers with at least 100 innings pitched in 
        each of 2021 and 2022 seasons.")
    ),
    mainPanel(
      plotOutput("plot", brush =
                   brushOpts("plot_brush",
                             fill = "#0000ff"),
                 width = '455px'),
      h4("Brush to see individual pitchers"),
      tableOutput("data")
      )
  )
)

server <- function(input, output) {

  output$plot <- renderPlot({
    plot_2_var(fg,
                input$var)
})
  
  output$data <- renderTable({
    req(input$plot_brush)
    p <- plot_2_var(fg,
               input$var)
    brushedPoints(p$data, input$plot_brush)
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region",
  caption.placement = "top")
  
}

# Run the application
shinyApp(ui = ui, server = server)
