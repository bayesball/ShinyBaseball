library(shiny)
library(ggplot2)
library(dplyr)
library(ShinyBaseball)

pv_region_new <- function(scdata, ptype, radius,
                          thecounts,
                          thestand,
                          pitch_arm){

  require(CalledStrike)
  require(LearnBayes)
  require(dplyr)
  require(purrr)
  pv_region2 <- function(x, z, r,
                         scv,
                         pitchtype = "FF",
                         counts = "0-0",
                         stand0 = "R",
                         p_arm = "R"){

    scv %>%
      filter(pitch_type == pitchtype,
             Count %in% counts,
             stand == stand0,
             p_throws == p_arm) -> sc_new

    sc_new %>%
      mutate(z = (plate_x - x) ^ 2 / r ^ 2 +
               (plate_z - z) ^ 2 / r ^ 2,
             weight = exp( - 1 / 2 * z)) %>%
      summarize(V = sum(weight * Pitch_Value,
                        na.rm = TRUE) /
                  sum(weight,
                      na.rm = TRUE)) %>%
      pull(V)
  }

  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=10),
                      plate_z = seq(1, 4, length=10))

  grid$Value <- map2_dbl(grid$plate_x,
                         grid$plate_z,
                         pv_region2,
                         radius,
                         scv = scdata,
                         pitchtype = ptype,
                         counts = thecounts,
                         stand = thestand,
                         p_arm = pitch_arm)

  all_counts <- NULL
  for(j in 1:length(thecounts)){
    all_counts <- paste(all_counts, thecounts[j])
  }

  minvalue = round(min(grid$Value), 3)

  ggplot(grid,
         aes(plate_x, plate_z,
             color = Value < 0)) +
    add_zone("black") +
    geom_point(size = 4) +
    geom_point(data = filter(grid, Value == min(Value)),
               color = "red",
               size = 10,
               shape = 23,
               fill = "red") +
    coord_fixed() +
    scale_color_manual(values = c("blue", "orange")) +
    labs(title = paste("Pitch Type: ", ptype,
                       "\n Pitcher Side: ", pitch_arm,
                       ", Batter Side: ", thestand,
                       "\n Counts: ", all_counts,
                       "\n Std. Dev. = ", radius,
                       sep = ""),
         subtitle = paste("Minimum Value =",
                          minvalue)) +
    theme(plot.title = element_text(colour = "red",
                                    size = 20,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0),
          plot.subtitle = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = 0.8,
                                    angle = 0)) +
    theme(text=element_text(size=18))
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                    bootswatch = "lumen"),
  fluidRow(
    column(4, wellPanel(
      h3(id="big-heading", "Optimal Pitch Location:"),
      tags$style(HTML("#big-heading{color: blue;}")),
      radioButtons("p_type", "Select Pitch Type:",
                   c("CH", "CU", "FC", "FF", "FT",
                     "SI", "SL"),
                   selected ="FF",
                   inline = TRUE),
      radioButtons("pb_sides", "Select Pitcher/Batter Sides:",
                   c("L/L", "L/R", "R/L", "R/R"),
                   selected = "R/R",
                   inline = TRUE),
      checkboxGroupInput(
            "counts",
            "Select Counts:",
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
sliderInput("radius",
            "Select Standard Deviation:",
            0, 1.0, 0.2)
    )),
    column(8,
           plotOutput("mplot", height = "600px")
           )
  )
)

server <- function(input, output, session) {
  output$mplot <- renderPlot({
    sides <- unlist(strsplit(input$pb_sides, "/"))
    p_side <- sides[1]
    b_side <- sides[2]
    pv_region_new(sc2019_pv,
                   input$p_type,
                   input$radius,
                   input$counts,
                   b_side,
                   p_side)
  })
}


shinyApp(ui, server)
