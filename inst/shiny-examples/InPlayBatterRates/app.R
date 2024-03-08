library(shiny)
library(readr)
library(dplyr)

player_list <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/player1500_list.csv")
scip <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/SC_ip_6_seasons_1500.csv")

######## functions

bin_rates <- function(sc_ip, platex_breaks, platez_breaks) {
  require(dplyr)
  sc_ip |>
    mutate(
      PX = cut(plate_x, breaks = platex_breaks),
      PZ = cut(plate_z, breaks = platez_breaks)
    ) |>
    filter(!is.na(PX), !is.na(PZ)) |>
    group_by(PX, PZ) |>
    summarize(
      BIP = n(),
      H = sum(H),
      HR = sum(HR),
      .groups = "drop"
    ) |>
    mutate(
      H_Rate = round(100 * H / BIP, 1),
      HR_Rate = round(100 * HR / BIP, 1)
    )
}

bin_plot <- function(S, platex_breaks, platez_breaks, label,
                     name = "") {
  require(dplyr)
  require(purrr)
  require(stringr)
  require(ggplot2)
  compute_bin_midpoint <- function(x) {
    x |>
      as.character() |>
      str_split_1(",") |>
      map_dbl(parse_number) |>
      mean()
  }
  S |>
    mutate(
      px = map_dbl(PX, compute_bin_midpoint),
      pz = map_dbl(PZ, compute_bin_midpoint)
    ) |>
    ggplot(aes(x = px, y = pz)) +
    geom_text(aes(label = {{label}}), size = 8) +
    geom_vline(
      xintercept = platex_breaks,
      color = "blue"
    ) +
    geom_hline(
      yintercept = platez_breaks,
      color = "blue"
    ) +
    coord_fixed() +
    theme(text = element_text(size = 14)) +
    labs(x = "plate_x", y = "plate_z",
         title = enquo(label))  +
    theme(plot.title = element_text(colour = "blue", size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    labs(subtitle = name) +
    theme(plot.subtitle = element_text(colour = "red", size = 18,
                                       hjust = 0.5, vjust = 0.8, angle = 0))
}

bin_plot_hm <- function(S, H = TRUE, Z = FALSE,
                        name = "") {
  require(dplyr)
  require(stringr)
  require(ggplot2)
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
  compute_bin_midpoint <- function(x) {
    x |>
      as.character() |>
      str_split_1(",") |>
      map_dbl(parse_number) |>
      mean()
  }
  if(H == TRUE & Z == FALSE){
    S$Rate <- S$H_Rate
    the_title <- "Hit Rate"
    legend_title <- "Rate"
  }
  if(H == TRUE & Z == TRUE){
    S$Rate <- S$Z_H
    the_title <- "Z Hit Rate"
    legend_title <- "Z"
  }
  if(H == FALSE & Z == FALSE){
    S$Rate <- S$HR_Rate
    the_title <- "HR Rate"
    legend_title <- "Rate"
  }
  if(H == FALSE & Z == TRUE){
    S$Rate <- S$Z_HR
    the_title <- "Z HR Rate"
    legend_title <- "Z"
  }

  S |>
    mutate(
      px = map_dbl(PX, compute_bin_midpoint),
      pz = map_dbl(PZ, compute_bin_midpoint)
    ) |>
    ggplot() +
    geom_tile(aes(x = px, y = pz, fill = Rate)) +
    scale_fill_distiller(palette = "Spectral")  +
    theme(text = element_text(size = 18)) +
    labs(x = "plate_x", y = "plate_z",
         title = paste("In-Play", the_title)) +
    add_zone("black") +
    coord_fixed() +
    theme(plot.title = element_text(colour = "red", size = 20,
                                    hjust = 0.5, vjust = 0.8, angle = 0)) +
    labs(subtitle = name) +
    theme(plot.subtitle = element_text(colour = "blue", size = 20,
                 hjust = 0.5, vjust = 0.8, angle = 0)) +
    guides(fill = guide_legend(title = legend_title))
}

add_h_hr <- function(S){
  hits <- c("single", "double", "triple", "home_run")
  S |>
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% hits, 1, 0))
}

add_Z <- function(S){
  p_H <- sum(S$H) / sum(S$BIP)
  p_HR <- sum(S$HR) / sum(S$BIP)
  S |>
    mutate(Z_H = round((H / BIP - p_H) /
                sqrt(p_H * (1 - p_H) / BIP), 1),
           Z_HR = round((HR / BIP - p_HR) /
                sqrt(p_HR * (1 - p_HR) / BIP), 1)
    )
}

########
ui <- fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    h2("Binned In-Play Hit and Home Run Rates 2018-2023"),
    column(
      3,
      selectInput("player",
                  paste("Select Hitter (min 1500 Balls in Play):"),
                  choices =
                    player_list$Name,
                  selected = player_list$Name[1]
      ),
      radioButtons("n_bins", "Choose number of bins:",
                   c("4" = "4",
                     "5" = "5",
                     "6" = "6",
                     "8" = "8",
                     "10" = "10"),
                   inline = TRUE),
      radioButtons("type", "Choose what to display:",
                   c("Balls in Play" = "bip",
                     "Hit Counts" = "h_counts",
                     "Hit Rates" = "h_rates",
                     "Hit Z-Scores" = "z_hit",
                     "Hit Heat Map" = "heat_h",
                     "Home Run Counts" = "hr_counts",
                     "Home Run Rates" = "hr_rates",
                     "Home Run Z-Scores" = "z_hr",
                     "Home Run Heat Map" = "heat_hr")),
      radioButtons("round", "Round values?",
                   c("No", "Yes"), "No",
                   inline = TRUE)
    ),
    column(
      9,
      plotOutput("plot1", height = "500px")
     )
    )

  server <- function(input, output, session) {

    output$plot1 <- renderPlot(
      {
        player_list |>
          filter(Name == input$player) |>
          pull(key_mlbam) -> playerid

        filter(scip, batter == playerid) |> add_h_hr() ->
          pdata

        px_breaks <- seq(-1.1, 1.1,
                length.out = as.numeric(input$n_bins) + 1)
        pz_breaks <- seq(1.5, 3.6,
                length.out = as.numeric(input$n_bins) + 1)

        out <- bin_rates(pdata, px_breaks, pz_breaks) |>
          add_Z()

        if(input$type == "heat_h"){
          p <- bin_plot_hm(out, H = TRUE, Z = TRUE,
                           name = input$player)
        }
        if(input$type == "heat_hr"){
          p <- bin_plot_hm(out, H = FALSE, Z = TRUE,
                           name = input$player)
        }
        if(input$round == "Yes"){
          out$H_Rate <- round(out$H_Rate)
          out$HR_Rate <- round(out$HR_Rate)
          out$Z_H = round(out$Z_H)
          out$Z_HR = round(out$Z_HR)
        }
        if(input$type == "bip"){
          p <- bin_plot(out, px_breaks, pz_breaks, BIP,
                        name = input$player)
        }
        if(input$type == "h_counts"){
          p <- bin_plot(out, px_breaks, pz_breaks, H,
                        name = input$player)
        }
        if(input$type == "hr_counts"){
          p <- bin_plot(out, px_breaks, pz_breaks, HR,
                        name = input$player)
        }
        if(input$type == "h_rates"){
          p <- bin_plot(out, px_breaks, pz_breaks, H_Rate,
                        name = input$player)
        }
        if(input$type == "hr_rates"){
          p <- bin_plot(out, px_breaks, pz_breaks, HR_Rate,
                        name = input$player)
        }
        if(input$type == "z_hit"){
          p <- bin_plot(out, px_breaks, pz_breaks, Z_H,
                        name = input$player)
        }
        if(input$type == "z_hr"){
          p <- bin_plot(out, px_breaks, pz_breaks, Z_HR,
                        name = input$player)
        }
        print(p)
      },
      res = 96
    )

  }

  shinyApp(ui = ui, server = server)
