library(shiny)
library(ggplot2)
fg <- fg2020batting

ui <- basicPage(
  fluidRow(
    column(4, wellPanel(
      h2("Point Hover Demo"),
      selectInput("xvar", "Select X Variable:",
                  names(fg),
                  selected = "OBP"),
      selectInput("yvar", "Select Y Variable:",
                  names(fg),
                  selected = "SLG")
    )),
    column(8,
           plotOutput("plot",
                      hover = "plot_hover",
                      height = "400px"),
           verbatimTextOutput("info")
    ))
)

server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot() +
      geom_point(data = fg,
                 mapping = aes_string(input$xvar,
                                      input$yvar))
  }, res = 96)

  output$info <- renderPrint({
    req(input$plot_hover)
    nearPoints(fg[, c("Name", input$xvar, input$yvar)],
               input$plot_hover)
  })
}

shinyApp(ui = ui, server = server)
