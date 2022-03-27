library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(mgcv)
library(metR)

pitch_value_contour <- function(df,
                                L = seq(-0.2, 0.2, by = 0.01),
                                title = "Pitch Value",
                                NCOL = 2){
  if(is.data.frame(df) == TRUE) {
    df <- list(df)
    names(df) <- "Group"
  }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    gam(Pitch_Value ~ s(plate_x, plate_z),
        data = df[[j]]) %>%
      grid_predict()  %>%
      mutate(lp = -lp) %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  if(N_df == 1){
    contour_graph(df_p, L, title)
  } else {
    contour_graph(df_p, L, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
}

contour_graph <- function(df, L, title){

  ggplot(df)  +
    geom_contour_fill(aes(x=plate_x,
                          y=plate_z,
                          z=lp),
                      breaks=c(L),
                      size=1.5) +
    scale_fill_distiller(palette="Spectral") +
    add_zone("black") +
    xlim(-1.5, 1.5) +
    ylim(1.0, 4.0)  +
    coord_fixed() +
    ggtitle(title) +
    centertitle() +
    increasefont()
}

grid_predict <- function(fit){
  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  grid$lp <- predict(fit, grid, type = "response")
  grid
}

increasefont <- function(){
  theme(text=element_text(size=18))
}

centertitle <- function(){
  theme(plot.title = element_text(colour = "blue", size = 18,
                                  hjust = 0.5, vjust = 0.8, angle = 0))
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                    bootswatch = "lumen"),
  fluidRow(
    column(4, wellPanel(
      h3(id="big-heading", " Pitch Value Across Counts:"),
      h4("2019 Statcast Data"),
      tags$style(HTML("#big-heading{color: blue;}")),
      radioButtons("p_type", "Pitch Type:",
                   c("CH", "CU", "FC", "FF", "FT",
                     "SI", "SL"),
                   inline = TRUE),
      radioButtons("p_side", "Pitcher Side:",
                   c("L", "R"),
                   inline = TRUE),
      radioButtons("b_side", "Batter Side:",
             c("L", "R"),
             inline = TRUE),
      checkboxGroupInput(
            "counts",
            "Choose Counts to Compare:",
            choices = c("0-0" = "0-0",
                        "0-1" = "0-1",
                        "1-0" = "1-0",
                        "0-2" = "0-2",
                        "1-1" = "1-1",
                        "2-0" = "2-0",
                        "1-2" = "1-2",
                        "2-1" = "2-1",
                        "3-0" = "3-0",
                        "2-2" = "2-2",
                        "3-1" = "3-1",
                        "3-2" = "3-2"),
            selected = "0-0",
            inline = TRUE
),
      tags$head(
        tags$style(HTML('#goButton{background-color:orange}'))
      ),
      actionButton("goButton", "Make Plot")
    )),
    column(8,
           plotOutput("mplot", height = "600px")
           )
  )
)

server <- function(input, output, session) {

  options(shiny.maxRequestSize=60*1024^2)

  data <- eventReactive(input$goButton, {
    df <- filter(sc2019_pv,
                 stand == input$b_side,
                 p_throws == input$p_side,
                 pitch_type == input$p_type,
                 Count %in% input$counts) %>%
        mutate(The_Count = paste("Count =", Count))
    df2 <- split(df, df$The_Count)
    N <- length(input$counts)
    NCOL <- ifelse(N <= 4, 2, 3)
    the_title <- paste("Pitch Type: ", input$p_type,
                      ", Pitcher: ", input$p_side,
                      ", Batter: ", input$b_side,
                      sep = "")
    pitch_value_contour(df2,
                        title = the_title,
                        NCOL = NCOL)
  })
  output$mplot <- renderPlot({
    data()
  })
}


shinyApp(ui, server)
