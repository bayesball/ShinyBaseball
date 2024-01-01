library(shiny)
library(ggplot2)
library(dplyr)
library(ShinyBaseball)

# uses dataset sc2019_pv in data folder

pv_region <- function(scv,
                      pitchtype,
                      counts,
                      p_arm,
                      stand,
                      x, z, r){
  require(CalledStrike)
  require(ggforce)
  require(LearnBayes)
  require(ggforce)

  xy <- rmnorm(500, c(x, z), diag(r ^ 2 * c(1, 1)))
  dxy <- data.frame(X = xy[, 1], Y = xy[, 2])

  all_counts <- NULL
  for(j in 1:length(counts)){
    all_counts <- paste(all_counts, counts[j])
  }
  scv %>%
    filter(pitch_type == pitchtype,
           Count %in% counts,
           stand == stand,
           p_throws == p_arm) -> sc_new

  sc_new %>%
    mutate(z = (plate_x - x) ^ 2 / r ^ 2 +
             (plate_z - z) ^ 2 / r ^ 2,
           weight = exp( - 1 / 2 * z)) %>%
    summarize(V = sum(weight * Pitch_Value,
                      na.rm = TRUE) /
                sum(weight,
                    na.rm = TRUE)) %>% pull(V) -> value

  mytext <- ifelse(value > 0, "BATTER", "PITCHER")
  mytext <- paste("(Favors ", mytext, ")", sep = "")

  ggplot() +
    geom_point(data = dxy,
               mapping = aes(X, Y),
               color = "orange") +
    geom_circle(aes(x0 = x, y0 = z, r = 2 * r),
                color = "blue",
                size = 2) +
    add_zone() + coord_fixed() +
    xlim(-1.4, 1.4) +
    ylim(1, 4) +
    centertitle() +
    increasefont() +
    xlab("plate_x") +
    ylab("plate_z") +
    labs(title = paste("Pitch Type: ", pitchtype,
                       "\n Pitcher Side: ", p_arm,
                       ", Batter Side: ", stand,
                       "\n Counts: ", all_counts,
                       "\n Std. Dev. = ", r,
                       sep = ""),
         subtitle = paste("Pitch Value = ",
                          round(value, 3),
                          "\n", mytext)) +
    theme(plot.title = element_text(colour = "red",
                                    size = 20,
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(color = "blue",
                                       size = 24,
                                       hjust = 0.5, vjust = 0.8))
}

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                    bootswatch = "lumen"),
  fluidRow(
    column(4, wellPanel(
      h3(id="big-heading", "Pitch Accuracy:"),
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
sliderInput("xloc", "Target X Location:", -1.25, 1.25, 0),
sliderInput("zloc", "Target Z Location:", 1.25, 3.75, 2.5),
sliderInput("radius","Standard Deviation (Error):", 0.05, 1, 0.2)
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
    pv_region(sc2019_pv,
              input$p_type,
              input$counts,
              p_side,
              b_side,
              input$xloc,
              input$zloc,
              input$radius)
  })
}

shinyApp(ui, server)
