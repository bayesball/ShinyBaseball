library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(mgcv)
library(metR)

# uses dataset sc2021_2500
# from Github site

sc2021_2500 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/sc2021_2500.csv")

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
      h3(id="big-heading", "LocationValue"),
      tags$style(HTML("#big-heading{color: red;}")),
      selectInput("pitcher",
                  "Select 2021 Pitcher:",
                  unique(sc2021_2500$Name)),
      radioButtons("p_type",
                   "Select Pitch Type:",
                   c("CH", "CU", "FC", "FF", "FT",
                     "KC","SI", "SL"),
                   inline = TRUE),
      tags$head(
        tags$style(HTML('#goButton{background-color:orange}'))
      ),
      tableOutput("pitchtypes"),
      actionButton("goButton", "Make Plot")
    )),
    column(8,
           plotOutput("mplot", height = "300px"),
           plotOutput("mplot2", height = "300px")
           )
  )
)

server <- function(input, output, session) {

  options(shiny.maxRequestSize=60*1024^2)

  pv_plot <- eventReactive(input$goButton, {
    df <- filter(sc2021_2500,
                 Name == input$pitcher,
                 pitch_type == input$p_type)
    df2 <- split(df, df$stand)
    NCOL <- 2
    the_title <- paste(input$pitcher,
                      " Pitch Values, Pitch Type: ",
                      input$p_type,
                      sep = "")
    pitch_value_contour(df2,
                        title = the_title,
                        NCOL = NCOL) +
      theme(legend.position = "none")
  })
  loc_plot <- eventReactive(input$goButton, {
    df <- filter(sc2021_2500,
                 Name == input$pitcher,
                 pitch_type == input$p_type)
    df2 <- split(df, df$stand)
    NCOL <- 2
    the_title <- paste(input$pitcher,
                       " Pitch Locations, Pitch Type: ",
                       input$p_type,
                       sep = "")
    ggplot(df, aes(plate_x, plate_z)) +
      geom_point(size = 2, color = "red") +
      facet_wrap(~ stand, ncol = 2) +
      add_zone("black") +
      xlim(-1.5, 1.5) +
      ylim(1.0, 4.0)  +
      coord_fixed() +
      ggtitle(the_title) +
      centertitle() +
      increasefont()
  })
  output$mplot <- renderPlot({
    loc_plot()
  })
  output$mplot2 <- renderPlot({
    pv_plot()
  })
  output$pitchtypes <- renderTable({
    df <- filter(sc2021_2500,
                 Name == input$pitcher)
    df %>%
      group_by(pitch_type) %>%
      summarize(Count = n(),
                .groups = "drop")
  })
}


shinyApp(ui, server)
