library(shiny)
library(readr)
library(dplyr)

sc_ip <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/scip2023_bf.csv")
roster <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/scip2023_player_names.csv")
roster200 <- filter(roster, BIP >= 200) |> 
  arrange(player_name)

batting_profile <- function(sc_ip, batter_no,
                            n_bins = 6){
  require(dplyr)
  require(ggplot2)
  require(purrr)
  require(stringr)
  
  logit <- function(y){
    log(y) - log(1 - y)
  }
  bin_rates <- function(sc_ip, phi1_breaks, LA_breaks,
                        name = "") {
    Total_BIP <- nrow(sc_ip)
    sc_ip |> 
      mutate(
        PH = cut(phi1, breaks = phi1_breaks),
        LA = cut(launch_angle, breaks = LA_breaks)
      ) |> 
      filter(!is.na(LA), !is.na(PH)) |> 
      group_by(PH, LA) |> 
      summarize(
        BIP = n(),
        .groups = "drop"
      ) |> 
      mutate(
        BIP_Rate = BIP / Total_BIP
      ) 
  }
  bin_plot_label <- function(S, P_breaks, LA_breaks,
                             name = "") {
    logit <- function(y){
      log(y) - log(1 - y)
    }
    compute_bin_midpoint <- function(x) {
      x |>
        as.character() |>
        str_split_1(",") |>
        map_dbl(parse_number) |>
        mean()
    }
    S |> 
      mutate(
        ph = map_dbl(PH, compute_bin_midpoint),
        la = map_dbl(LA, compute_bin_midpoint),
        Sign = ifelse(Dlogit > 0, "pos", "neg"),
        DL = round(Dlogit, 1)
      ) |>
      ggplot(aes(x = ph, y = la)) + 
      geom_text(aes(label = DL, color = Sign), 
                size = 6) +
      geom_vline(
        xintercept = P_breaks,
        color = "blue"
      ) +
      geom_hline(
        yintercept = LA_breaks,
        color = "blue"
      ) +
      theme(text = element_text(size = 18)) +
      labs(x = "Adjusted Spray Angle", 
           y = "Launch Angle",
           title = paste("Comparative Batting Profile of", Name)) +
      theme(plot.title = element_text(colour = "blue", size = 16,
                                      hjust = 0.5, vjust = 0.8, angle = 0))
  }
  bin_plot_color <- function(S, P_breaks, LA_breaks) {
    compute_bin_midpoint <- function(x) {
      x |>
        as.character() |>
        str_split_1(",") |>
        map_dbl(parse_number) |>
        mean()
    }
    S |> 
      mutate(
        ph = map_dbl(PH, compute_bin_midpoint),
        la = map_dbl(LA, compute_bin_midpoint)
      ) |>
      ggplot(aes(x = ph, y = la)) + 
      geom_tile(aes(fill = D_LOGIT)) +
      geom_vline(
        xintercept = P_breaks,
        color = "blue"
      ) +
      geom_hline(
        yintercept = LA_breaks,
        color = "blue"
      ) +
      theme(text = element_text(size = 18)) +
      labs(x = "Adjusted Spray Angle", 
           y = "Launch Angle",
           title = paste("Comparative Batting Profile of", Name)) +
      #    theme(legend.position = "none") +
      scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) +
      theme(plot.title = element_text(colour = "blue", size = 16,
                                      hjust = 0.5, vjust = 0.8, angle = 0))
  }
  bin_plot_scatter <- function(ff, P_breaks, LA_breaks) {
    ggplot(ff, aes(x = phi1, y = launch_angle)) + 
      geom_point() +
      geom_vline(
        xintercept = P_breaks,
        color = "blue"
      ) +
      geom_hline(
        yintercept = LA_breaks,
        color = "blue"
      ) +
      theme(text = element_text(size = 18)) +
      labs(x = "Adjusted Spray Angle", 
           y = "Launch Angle",
           title = paste("Batting Profile of", Name)) +
      #    theme(legend.position = "none") +
      scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) +
      theme(plot.title = element_text(colour = "blue", size = 18,
                                      hjust = 0.5, 
                                      vjust = 0.8, angle = 0))
  }
  bin_plot <- function(S, PH_breaks, LA_breaks, label) {
    compute_bin_midpoint <- function(x) {
      x |>
        as.character() |>
        str_split_1(",") |>
        map_dbl(parse_number) |>
        mean()
    }
    S |> 
      mutate(
        ph = map_dbl(PH, compute_bin_midpoint),
        la = map_dbl(LA, compute_bin_midpoint)
      ) |>
      ggplot(aes(x = ph, y = la)) + 
      geom_text(aes(label = {{label}}), size = 8) +
      geom_vline(
        xintercept = PH_breaks,
        color = "blue"
      ) +
      geom_hline(
        yintercept = LA_breaks,
        color = "blue"
      ) +
      theme(text = element_text(size = 18)) +
      labs(x = "Adjusted Spray Angle", y = "Launch Angle",
           title = paste("Batting Profile of", Name)) +
      theme(plot.title = element_text(colour = "blue", size = 18,
                                      hjust = 0.5, 
                                      vjust = 0.8, angle = 0))
  }
  
  # 6, 9, 12
  p_breaks <- seq(-45, 45, length.out = n_bins + 1)
  la_breaks <- seq(-50, 70, length.out = n_bins + 1)
  
  # binning
  out <- bin_rates(sc_ip, p_breaks, la_breaks)
  ff <- filter(sc_ip, batter == batter_no)
  Name <- ff$player_name[1]
  out_ff <- bin_rates(ff, p_breaks, la_breaks)
  
  # merge two datasets
  inner_join(out, out_ff,
             by = c("PH" = "PH",
                    "LA" = "LA")) -> out_new
  # difference in logits measure
  out_new <- mutate(out_new,
                    Dlogit = logit(BIP_Rate.y) -
                      logit(BIP_Rate.x))
  
  p0 <- bin_plot_scatter(ff, p_breaks, la_breaks)
  p0a <- bin_plot(out_ff, p_breaks, la_breaks, BIP)
  p1 <- bin_plot_label(out_new, p_breaks, la_breaks) 
  out_new <- mutate(out_new,
                    D_LOGIT = round(Dlogit, 1))
  p2 <- bin_plot_color(out_new, p_breaks, la_breaks) 
  
  list(p0 = p0, p0a = p0a, p1 = p1, p2 = p2)
}

ui <- fluidPage(
    theme = shinythemes::shinytheme("slate"),
    h2("2023 Batted Ball Profile"),
    h4("Compared with MLB Averages"),
    column(
      3,
      selectInput("player",
                  paste("Select Hitter:"),
                  choices =
                    roster200$player_name,
                  selected = "Freeman, Freddie"
      ),
      hr(),
      radioButtons("type", "Choose what to display:",
                   c("Scatterplot" = "scatter",
                     "Bin Counts" = "bin_counts",
                     "Difference in Logits" = "dlogit",
                     "Tile Graph" = "tile")),
      radioButtons("n_bins", "Choose number of bins:",
                   c("four" = "4",
                     "five" = "5",
                     "eight" = "8",
                     "eleven" = "11"))
    ),
    column(
      9,
      plotOutput("plot1", height = "500px")
     )
    )

  server <- function(input, output, session) {

    output$plot1 <- renderPlot(
      {
        roster200 |> filter(player_name == input$player) |> 
          pull(batter) -> batter_no
        
        OUT <- batting_profile(sc_ip, batter_no,
                           n_bins = as.numeric(input$n_bins))
        
        if(input$type == "scatter"){
          OUT$p0
        } else if (input$type == "bin_counts") {
          OUT$p0a
        } else if (input$type == "dlogit") {
          OUT$p1
        } else {
          OUT$p2
        }
      },
      res = 96
    )

  }

  shinyApp(ui = ui, server = server)
