library(shiny)
library(ggplot2)
library(dplyr)
library(CalledStrike)
library(readr)
library(mgcv)
library(metR)
library(gridExtra)

#sc2021_pv_new %>%
#  group_by(Name
#) %>%
#  summarize(N = n())  %>%
#  filter(N >= 2500) %>%
#  select(Name) %>%
#  pull() -> pitcher2500

pitcher2500 <- unique(sc2021_pv_new$Name)

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
      h3(id="big-heading",
         "LocationValue"),
      tags$style(HTML("#big-heading{color: red;}")),
      selectInput("name",
                  "Select 2021 Pitcher:",
                  pitcher2500),
      tableOutput("table"),
      radioButtons("p_type", "Select Pitch Type:",
                   c("CH", "CU", "FC", "FF", "FT",
                     "KC","SI", "SL"),
                   inline = TRUE),
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

  plot_values <- eventReactive(input$goButton, {
    df <- filter(sc2021_pv_new,
                 Name == input$name,
                 pitch_type == input$p_type)
    df2 <- split(df, df$stand)
    names(df2) <- c("Stand: Left",
                    "Stand: Right")
    the_title <- paste(input$name, " Pitch Values",
                       ", Pitch Type: ", input$p_type,
                       sep = "")
    pitch_value_contour(df2,
                        title = the_title,
                        NCOL = 2)
  })
  plot_locations <- eventReactive(input$goButton, {
    df <- filter(sc2021_pv_new,
                 Name == input$name,
                 pitch_type == input$p_type)
    df_r <- filter(df, stand == "R") %>%
      mutate(Type = "Stand: Right")
    df_l <- filter(df, stand == "L") %>%
      mutate(Type = "Stand: Left")
    df2 <- rbind(df_r, df_l)
    the_title <- paste(input$name, " Pitch Locations",
                       ", Pitch Type: ", input$p_type,
                       sep = "")
    ggplot(df2, aes(plate_x, plate_z)) +
      geom_point(color = "red") +
      facet_wrap(~ Type, ncol = 2) +
      add_zone("black") +
      xlim(-1.5, 1.5) +
      ylim(1.0, 4.0)  +
      coord_fixed() +
      ggtitle(the_title) +
      centertitle() +
      increasefont()
  })
  output$mplot <- renderPlot({
    pvalues <- plot_values() +
      theme(legend.position = "none")
    lvalues <- plot_locations()
    grid.arrange(lvalues, pvalues)
  })
  output$table <- renderTable({
    df <- filter(sc2021_pv_new,
                 Name == input$name)
    df %>%
      group_by(pitch_type) %>%
      summarize(N = n(), .groups = "drop")
  })
}


shinyApp(ui, server)
