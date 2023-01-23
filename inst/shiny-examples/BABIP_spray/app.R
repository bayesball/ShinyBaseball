library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

plot_bip_rate <- function(sc_ip,
                          launch_angles,
                          launch_speeds,
                          type,
                          seasons,
                          keep_hr = "yes"){

  hits <- c("single", "double", "triple",
            "home_run")

  sc_ip %>%
    mutate(Season = as.character(Season)) %>%
    filter(Season %in% seasons) -> sc_ip

  if(keep_hr == "no"){
    sc_ip <- filter(sc_ip, events != "home_run")
  }

  sc_ip %>%
    filter(launch_angle > launch_angles[1],
           launch_angle < launch_angles[2],
           launch_speed > launch_speeds[1],
           launch_speed < launch_speeds[2]) -> sc_ip_new

  mean_babip <- mean(sc_ip_new$events %in% hits,
                     na.rm = TRUE)

  sc_ip_new %>%
    group_by(Season, mid_angle) %>%
    summarize(BIP = n(),
              H = sum(events %in% hits),
              E = sum(estimated_ba_using_speedangle,
                      na.rm = TRUE),
              HA = H - E,
              Z = HA / sqrt(E),
              .groups = "drop") -> Sg

  title_info = paste(launch_angles[1], " <  Launch Angle < ",
                     launch_angles[2], ", ",
                     launch_speeds[1], " <  Exit Velocity < ",
                     launch_speeds[2])

  if(type == "HA"){
    p1 <- ggplot(Sg, aes(mid_angle, HA,
                         color = Season)) +
      geom_hline(yintercept = 0,
                 color = "black") +
      ylab("Hits Added")
  }
  if(type == "BABIP"){
    p1 <- ggplot(Sg, aes(mid_angle, H / BIP,
                         color = Season)) +
      geom_hline(yintercept = mean_babip,
                 color = "black") +
      ylab("BABIP")
  }
  if(type == "Z"){
    p1 <- ggplot(Sg, aes(mid_angle, Z,
                         color = Season)) +
      geom_hline(yintercept = 0,
                 color = "black") +
      ylab("Standardized Hits Added")
  }
  p1 +
    geom_point(size = 4) +
    geom_line(linewidth = 1.5) +
    theme(text=element_text(size=18)) +
    labs(title = paste(type, "Against Spray Angle"),
         subtitle = title_info) +
    xlab("Adjusted Spray Angle") +
    theme(plot.title = element_text(colour = "blue",
                                    size = 20,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(plot.subtitle = element_text(colour = "red",
                                       size = 18,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
}

seasons <- c("2018", "2019", "2021", "2022")

ui <- fluidPage(
  titlePanel("BABIP and Spray Angle"),
  sidebarLayout(
    sidebarPanel(
      br(),
      sliderInput("la",
                  "Select Range of Launch Angle:",
                  min = -80, max = 80,
                  c(-80, 80)),
      sliderInput("ls",
                  "Select Range of Launch Speed:",
                  min = 40, max = 120,
                  c(40, 120)),
      radioButtons("keep_hr",
                   "Include Home Runs?",
                   choices = c("yes", "no"),
                   inline = TRUE),
      radioButtons("type",
                    "Choose Y Variable:",
                    choices = c("BABIP", "HA", "Z"),
                    inline = TRUE),
      checkboxGroupInput("seasons",
                         "Select Seasons:",
                         choices = seasons,
                         selected = seasons[1],
                         inline = TRUE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Graph",
      plotOutput("plot")),
      tabPanel("Explanation",
               hr(),
               p("Have Statcast data
                  for all balls in play during the
                 2018, 2019, 2021 and 2022 seasons."),
               p("One selects a range of values of
              launch angle and exit velocity values."),
              p("Ground balls correspond to  launch
                angles less than 10 degrees, line drives
                10-25 degrees, fly balls 25-50 degrees, and
                pop ups greater than 50 degrees"),
              p("One decides if one wishes to include home
                runs."),
              p("One selects one of three possible measures:"),
              p("- BABIP = batting average for subset of
                batted balls"),
              p("- HA = H - E(H) where E(H) is the expected
                number of hits given values of the launch
                angle and exit velocity"),
              p("- Z = HA / sqrt(E(H)), the standardized
                value of hits added"),
              p("One selects one or more seasons of interest."),
              hr(),
              p("Graph displays the measure plotted against the
                adjusted spray angle for each season.")
      ))
      )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    plot_bip_rate(scip_four_seasons,
                  input$la,
                  input$ls,
                  input$type,
                  input$seasons,
                  input$keep_hr)
})
}

# Run the application
shinyApp(ui = ui, server = server)
