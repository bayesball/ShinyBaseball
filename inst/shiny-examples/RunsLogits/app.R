library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

logit_work <- function(Observed){
  observed <- matrix(Observed$N, 5, 24)
  states <- c("000 0", "000 1", "000 2",
              "001 0", "001 1", "001 2",
              "010 0", "010 1", "010 2",
              "011 0", "011 1", "011 2",
              "100 0", "100 1", "100 2",
              "101 0", "101 1", "101 2",
              "110 0", "110 1", "110 2",
              "111 0", "111 1", "111 2")
  run_values <- c("0", "1", "2", "3", "4+")
  dimnames(observed) <- list(run_values, states)
  compute_logits <- function(counts){
    prob <- counts / sum(counts)
    cum_prob <- cumsum(prob)
    logits <- log(cum_prob / (1 - cum_prob))
    logits[1:4]
  }
  L_20_seasons <- apply(observed, 2, compute_logits)
  data.frame(L_20_seasons) %>%
    mutate(Breakpoint = 1:4)
}
compare_logit_plot_many <- function(L_20,
                                    states,
                                    bases_outs = "bases",
                                    logit_type = "<="){

  require(purrr)

  cols <- c( paste("X", states, ".0", sep = ""),
             paste("X", states, ".1", sep = ""),
             paste("X", states, ".2", sep = ""))

  many_states <- L_20[, c("Breakpoint", cols)]
  N <- length(cols)

  restate <- function(state){
    s <- unlist(strsplit(state, ""))
    s[2] <- ifelse(s[2] == 1, "2", "0")
    s[3] <- ifelse(s[3] == "1", "3", "0")
    paste(s[1], s[2], s[3], sep = "")
  }

  pivot_longer(many_states,
               cols = starts_with("X"),
               names_to = "State",
               values_to = "Logit") %>%
    mutate(Bases = substr(State, 2, 4),
           Outs = paste("Outs =",
                        substr(State, 6, 6))) -> Many_States

  Many_States$Bases <- sapply(Many_States$Bases, restate)

  breakpoints <- c("0/1", "1/2", "2/3", "3/4+")

  if(bases_outs == "bases"){
    p1 <- ggplot(Many_States,
                 aes(Breakpoint, Logit, color = Bases)) +
      geom_point(size = 4) + geom_line() +
      facet_wrap(~ Outs)
  } else {
    p1 <- ggplot(Many_States,
                 aes(Breakpoint, Logit, color = Outs)) +
      geom_point(size = 4) + geom_line() +
      facet_wrap(~ Bases)
  }
  p1 <- p1 +
    ylab("Cumulative Logit") +
    ggtitle(paste("Observed Cumulative Logits P(Runs ",
                  logit_type, " j)", sep = "")) +
    theme(text=element_text(size=18)) +
    scale_x_continuous(breaks = 1:4,
                       labels = breakpoints) +
    theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  print(p1)
}
table_advantage <- function(Beta, seasons){

  Beta %>%
    filter(Season %in% seasons) %>%
    group_by(Bases, Outs) %>%
    summarize(Coef = mean(Coef),
              Bases_Score = first(Bases_Score),
              .groups = "drop") ->
    Beta_summary

  create_table <- function(DF){
    DF <- DF[c(1:3,
               13:15, 7:9, 19:21,
               4:6, 16:18, 10:12,
               22:24), ]

    M <- matrix(round(DF$Coef, 2), 3, 8)
    dimnames(M) <- list(c("0", "1", "2"),
                        c("000", "100", "020", "120",
                          "003", "103", "023", "123"))
    M
  }
  create_table(Beta_summary)
}

Beta <- twentyyears_RA
Beta$Bases <- gsub("[“”]", "", Beta$Bases)
Beta$Outs <- as.character(Beta$Outs)
S <- twenty_seasons_counts
S %>%
  group_by(STATE, O_RUNS.ROI) %>%
  summarize(N = sum(N),
            .groups = "drop") -> S_20_seasons
L_20 <- logit_work(S_20_seasons)
bases <- c("000", "100", "020", "120",
           "003", "103", "023", "123")

ui <- fluidPage(
  titlePanel("Logit Comparison of Run Scoring Across States: 2000-2019"),
  sidebarLayout(
    sidebarPanel(
      br(),
      checkboxGroupInput("sel_bases",
                         "Selected Runners on Base:",
                         choices = bases,
                         selected = c("000", "100"),
                         inline = FALSE),
      radioButtons("logit_type",
                   "Select Logit Type:",
                   choices = c("<=", ">"),
                   inline = TRUE),
      radioButtons("outs_bases",
                    "Compare Bases or Outs:",
                    choices = c("bases", "outs"),
                    inline = TRUE)
    ),
    mainPanel(
      h4("Runs Advantages from Ordinal Model:"),
      tableOutput("runs_advantage"),
      plotOutput("plot"))
  )
)

server <- function(input, output) {
  output$runs_advantage <- renderTable({

    season_n <- 2000:2019
    d <- as.data.frame(table_advantage(Beta,
                    season_n))
    d$Outs <- c(0, 1, 2)
    d[, c(9, 1:8)]

  })
  output$plot <- renderPlot({
    req(input$sel_bases)
    bases <- input$sel_bases
    bases <- ifelse(bases == "020", "010", bases)
    bases <- ifelse(bases == "120", "110", bases)
    bases <- ifelse(bases == "103", "101", bases)
    bases <- ifelse(bases == "023", "011", bases)
    bases <- ifelse(bases == "123", "111", bases)
    bases <- ifelse(bases == "003", "001", bases)

    if(input$logit_type == ">"){
      L_20[, 1:24] <- - L_20[, 1:24]
    }
    compare_logit_plot_many(L_20, bases,
                            input$outs_bases,
                            input$logit_type)
})
}

# Run the application
shinyApp(ui = ui, server = server)
