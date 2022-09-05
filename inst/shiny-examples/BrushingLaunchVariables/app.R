library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)

#sc2022ip <- read_csv("sc2022ip.csv")
#chadwick <- read_csv("chadwick.csv")

SelectPlayers <- function(sc_ip, minIP){
  require(dplyr)
  sc_ip %>%
    group_by(batter) %>%
    summarize(N = n()) %>%
    filter(N >= minIP) %>%
    inner_join(chadwick,
               by = c("batter" = "key_mlbam")) %>%
    mutate(Name = paste(name_first, name_last))  %>%
    arrange(name_last) %>%
    select(N, batter, Name)
}

launch_var_individual <- function(scip,
                                  player_id,
                                  player_name,
                                  type){
  require(dplyr)
  require(ggplot2)

  #  names <- unlist(str_split(player, " "))
  #  chadwick %>%
  #    filter(name_first == names[1],
  #           name_last == names[2]) %>%
  #   select(key_mlbam) %>% pull() -> player_id

  hits <- c("single", "double", "triple",
            "home_run")

  scip %>%
    mutate(HIT = ifelse(events %in% hits,
                        "yes", "no"),
           HR = ifelse(events == "home_run",
                       "yes", "no")) ->
    scip

  the_plot <- ggplot(filter(scip, batter == player_id),
                     aes(launch_angle, launch_speed)) +
    labs(title = paste("2022", player_name,
                       "Balls in Play"),
         x = "Launch Angle",
         y = "Launch Speed") +
    theme(text=element_text(size=18)) +
    theme(plot.title =
            element_text(colour = "blue",
                         size = 18,
                         hjust = 0.5,
                         vjust = 0.8, angle = 0))

  if(type == "Hit"){
    the_plot <- the_plot +
      geom_point(aes(color = HIT)) +
      scale_colour_manual(values =
                            c("tan", "red"))
  }
  if(type == "Home Run"){
    the_plot <- the_plot +
      geom_point(aes(color = HR)) +
      scale_colour_manual(values =
                            c("tan", "red"))
  }
  if(type == "Expected BA"){
    the_plot <- the_plot +
      geom_point(aes(color =
                       estimated_ba_using_speedangle)) +
      labs(colour = "XBA") +
      scale_color_distiller(palette="RdYlBu")
  }
  if(type == "Expected wOBA"){
    the_plot <- the_plot +
      geom_point(aes(color =
                       estimated_woba_using_speedangle)) +
      labs(color = "XwOBA") +
      scale_color_distiller(palette="RdYlBu")
  }
  the_plot
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
  h3(id="big-heading", "Brushing Launch Variables"),
  tags$style(HTML("#big-heading{color: blue;}")),
  sliderInput("minIP",
              "Select Minimum Number In-Play:",
              min = 50, max = 500,
              value = 200),
  selectInput("name",
              "Select Player:",
              SelectPlayers(sc2022ip, 200)$Name),
  radioButtons("measure", "Measure:",
               c("Hit", "Home Run",
                 "Expected BA",
                 "Expected wOBA"),
               inline = FALSE),
  tableOutput("data2")
  )),
  column(8,
         plotOutput("plot", brush =
              brushOpts("plot_brush",
                        fill = "#0000ff"),
              click = "plot_click",
              width = '555px'),
         tableOutput("data")
         )
)

server <- function(input, output, session) {

  observeEvent(input$minIP, {
    updateSelectInput(inputId = "name",
                      choices =
                SelectPlayers(sc2022ip, input$minIP)$Name)
  })

  output$plot <- renderPlot({

    SelectPlayers(sc2022ip, input$minIP) %>%
      filter(Name == input$name) -> S

    launch_var_individual(sc2022ip,
                          S$batter,
                          S$Name,
                          input$measure)

    }, res = 96)

  output$data2 <- renderTable({

    SelectPlayers(sc2022ip, input$minIP) %>%
      filter(Name == input$name) %>%
      select(batter) %>%
      pull() -> player_id

    sc1 <- filter(sc2022ip, batter == player_id)
    hits <- c("single", "double", "triple",
              "home_run")
    data.frame(BIP = nrow(sc1),
               Hard_Hit_Rate = mean(sc1$launch_speed >= 95,
                              na.rm = TRUE),
               LA_20_35 = mean(sc1$launch_angle >= 20 &
                                sc1$launch_angle <= 35,
                              na.rm = TRUE))
  }, digits = 3)

  output$data <- renderTable({
    req(input$plot_brush)

    SelectPlayers(sc2022ip, input$minIP) %>%
      filter(Name == input$name) %>%
      select(batter) %>%
      pull() -> player_id

    sc1 <- brushedPoints(filter(sc2022ip,
                batter == player_id),
                      input$plot_brush)

    hits <- c("single", "double", "triple",
              "home_run")
    data.frame(Name = input$name,
               BIP = nrow(sc1),
               H = sum(sc1$events %in% hits),
               HR = sum(sc1$events == "home_run"),
               H_Rate = sum(sc1$events %in% hits) /
                 nrow(sc1),
               HR_Rate = sum(sc1$events == "home_run") /
                 nrow(sc1),
               xBA = mean(sc1$estimated_ba_using_speedangle),
               xwOBA = mean(sc1$estimated_woba_using_speedangle))
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats",
  caption.placement = "top")
}

shinyApp(ui = ui, server = server)
