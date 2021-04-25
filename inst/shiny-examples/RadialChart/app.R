library(shiny)
library(dplyr)
library(stringr)

radial_plot <- function(sc_ip, type,
                        title = ""){
  # note:  requirement is that the file
  # player.png is in the same folder as this
  # function
  # but function will still work without an
  # error if the file is missing

  # load in packages and turn off warnings
  require(latex2exp)
  require(ggplot2)
  require(patchwork)
  require(png)
  options(warn=-1)
  # setup work
  sc_ip <- sc_ip %>%
    mutate(BB_Type = ifelse(launch_angle > 50,
                            "pop up",
                            ifelse(launch_angle > 25,
                                   "fly ball",
                                   ifelse(launch_angle > 10,
                                          "line drive",
                                          "ground ball"))))
  # change to radial coordinates
  sc_ip %>%
    mutate(Xcoord = launch_speed / 120 *
             cos(launch_angle * pi / 180),
           Ycoord = launch_speed / 120 *
             sin(launch_angle * pi / 180)) -> sc_ip
  # define boundaries of polygons
  th <- seq(- pi / 2, pi / 2, length.out = 200)
  df_new <- data.frame(x = cos(th),
                       y = sin(th))
  df_add <- data.frame(x = 0,
                       y = sin(- pi / 2))
  df_new2 <- rbind(df_new, df_add)
  # read in png image of player
  player <- readPNG("player.png", native = TRUE)

  # construct graphic
  p1 <- ggplot() +
    coord_equal() +
    geom_polygon(data = df_new2, aes(x, y),
                 fill = "cadetblue1") +
    geom_polygon(data = df_new2, aes(x / 2, y / 2),
                 fill = "cadetblue3") +
    geom_segment(aes(x = 0, y = 0,
                     xend = cos(pi / 4),
                     yend = sin(pi / 4)),
                 color = "grey") +
    geom_segment(aes(x = 0, y = 0,
                     xend = 1,
                     yend = 0),
                 color = "grey")

   if (type == "xba"){
      sc_ip$estimated_ba <-
         as.numeric(sc_ip$estimated_ba)
      p1 <- p1 + geom_point(data = sc_ip,
                aes(Xcoord, Ycoord,
                color = estimated_ba),
                        size = 4) +
            scale_color_distiller(palette="Spectral")
   }
  if (type == "bb"){
    p1 <- p1 + geom_point(data = sc_ip,
                          aes(Xcoord, Ycoord,
                              color = BB_Type),
                          size = 4)
  }
  if (type == "h"){
    sc_ip$H <- as.character(sc_ip$H)
    p1 <- p1 + geom_point(data = sc_ip,
                          aes(Xcoord, Ycoord,
                              color = H),
                          size = 4) +
      scale_color_manual(values = c("blue", "red"))
  }
  if (type == "hr"){
    sc_ip$HR <- as.character(sc_ip$HR)
    p1 <- p1 + geom_point(data = sc_ip,
                          aes(Xcoord, Ycoord,
                              color = HR),
                          size = 4) +
      scale_color_manual(values = c("blue", "red"))
  }
    p1 <- p1 +
    annotate(geom = "text", x = 0.75, y = 0.75,
             label = TeX("45^o"), color = "red") +
    annotate(geom = "text", x = 1.05, y = 0,
             label = TeX("0^o"), color = "red") +
    annotate(geom = "text", x = 0, y = 1.05,
             label = TeX("90^o"), color = "red") +
    annotate(geom = "text", x = 0, y = -1.05,
             label = TeX("-90^o"), color = "red") +
    annotate(geom = "text", x = 0.57, y = 0.91,
             label = "120 mph", color = "blue") +
    annotate(geom = "text", x = 0.2, y = 0.45,
             label = "60 mph", color = "white") +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    xlim(0, 1.1) +
    ylim(-1.1, 1.1) +
    ggtitle(paste("Radial Chart of Balls in Play",
                  "\n", title)) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 20,
             hjust = 0.5, vjust = 0.8, angle = 0))
    p1 <- p1 +
      inset_element(p = player,
                    left = 0.3 - 0.52,
                    bottom = 0.425,
                    right = 0.46 - 0.42,
                    top = 0.565)
  p1
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  column(4, wellPanel(
    h3(id="big-heading", "Radial Chart"),
    tags$style(HTML("#big-heading{color: blue;}")),
    textInput("name",
              label = h4("Pitcher Name:"),
              value = "Aaron Nola"),
    dateInput("date", label = h4("2019 Date Input:"),
              value = "2019-03-28"),
 #   textInput("game_pk", "2019 Game Id:",
 #            value = "567059"),
    radioButtons("type",
                 label = h4("Point Color:"),
                 c("Batted Ball Type",
                   "xBA",
                   "Hit",
                   "Home Run"),
                 inline = FALSE)
  )),
  column(8,
         plotOutput("plot",
                    width = '100%',
                    height = "500px")
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    get_id <- function(st){
      names <- str_to_lower(unlist(str_split(str_squish(st), " ")))
      if(length(names) == 3){
        names <- c(paste(names[1], names[2]), names[3])
      }
      chadwick %>%
        mutate(fname = str_to_lower(name_first),
               lname = str_to_lower(name_last),
               Name = paste(name_first,
                            name_last)) %>%
        filter(fname == names[1],
               lname == names[2]) %>%
        select(key_mlbam, Name)
    }

    pid <- get_id(input$name)$key_mlbam
    req(length(pid) > 0)

 #   sc_subset <- sc2019_ip %>%
 #     filter(pitcher == pid,
 #            game_date == input$date)
    sc_subset <- sc2019_ip[sc2019_ip$pitcher == pid &
                   sc2019_ip$game_date ==
                     as.character(input$date), ]

 #   radial_plot(sc_subset)
 #   if(input$type == "Batted Ball Type"){
 #        radial_plot(sc_subset)
 #   }
    outcomes <- c("Batted Ball Type", "xBA",
                  "Hit", "Home Run")
    n_outcome <- which(outcomes == input$type)
    if(n_outcome == 1){
      p1 <- radial_plot(sc_subset,
                  type = "bb",
                  title = paste(input$name,
                                ", ",
                                input$date,
                                sep = ""))
    }
    if(n_outcome == 2){
      p1 <- radial_plot(sc_subset,
                  type = "xba",
                  title = paste(input$name,
                                ", ",
                                input$date,
                                sep = ""))
    }
   if(n_outcome == 3){
       p1 <- radial_plot(sc_subset,
                  type = "h",
                  title = paste(input$name,
                                ", ",
                                input$date,
                               sep = ""))
   }
    if(n_outcome == 4){
      p1 <- radial_plot(sc_subset,
                  type = "hr",
                  title = paste(input$name,
                                ", ",
                                input$date,
                                sep = ""))
    }
    p1
  })
}

shinyApp(ui = ui, server = server)
