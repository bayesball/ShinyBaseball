library(shiny)
library(ggplot2)

# data is dataset fb2020batting in data folder
# of package

fg <- fg2020batting

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "superhero"),
  fluidRow(
    column(6, wellPanel(
      selectInput("xvar1", "Select X1 Variable:",
                  names(fg),
                  selected = "OBP"),
      selectInput("yvar1", "Select Y1 Variable:",
                  names(fg),
                  selected = "SLG"),
      selectInput("xvar2", "Select X2 Variable:",
                  names(fg),
                  selected = "BB_Pct"),
      selectInput("yvar2", "Select Y2 Variable:",
                  names(fg),
                  selected = "K_Pct"),
      tableOutput("table1")
    )),
    column(6,
      plotOutput("plot1",
                 brush = "plot_brush",
                 height = "300px"),
      plotOutput("plot2",
                 brush = "plot_brush",
                 height = '300px')
    ))
)
server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    ggplot() +
    geom_point(data = fg,
               mapping = aes_string(input$xvar1,
                                    input$yvar1)) +
    geom_point(data =
                 brushedPoints(fg, input$plot_brush),
                 mapping = aes_string(input$xvar1,
                                      input$yvar1),
                 color = "red")
  }, res = 96)
  output$plot2 <- renderPlot({
    ggplot() +
      geom_point(data = fg,
                 mapping = aes_string(input$xvar2,
                                      input$yvar2)) +
      geom_point(data =
                 brushedPoints(fg, input$plot_brush),
                 mapping = aes_string(input$xvar2,
                                      input$yvar2),
                 color = "red")
  }, res = 96)
  output$table1 <- renderTable({
      brushedPoints(fg, input$plot_brush)[,
           c("Name", input$xvar1, input$yvar1,
                     input$xvar2, input$yvar2)]
  }, digits = 3)
}

shinyApp(ui = ui, server = server)
