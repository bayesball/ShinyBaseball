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
compare_logit_plot4 <- function(C_all,
                                bases,
                                outs,
                                logit_type = "<="){

  require(tidyr)
  require(ggplot2)

  cols <- c( paste("X", bases[1], ".", outs[1], sep = ""),
             paste("X", bases[2], ".", outs[2], sep = ""))

  many_states <- C_all[, c("Type", "Breakpoint", cols)]
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
  Many_States$Bases_Outs <- paste(Many_States$Bases,
                                  Many_States$Outs)

  breakpoints <- c("0/1", "1/2", "2/3", "3/4+")

    p1 <- ggplot(Many_States,
                 aes(Breakpoint, Logit, color = Bases_Outs)) +
      geom_point(size = 4) + geom_line() +
      facet_wrap(~ Type, ncol = 1) +
      ylab("Cumulative Logit") +
      ggtitle(paste("Cumulative Logits P(Runs ",
                  logit_type, " j)", sep = "")) +
      xlab("Breakpoint (j | j + 1)") +
      theme(text=element_text(size=18)) +
      scale_x_continuous(breaks = 1:4,
                       labels = breakpoints) +
      theme(plot.title = element_text(colour = "blue",
                                    size = 18,
                                    hjust = 0.5, vjust = 0.8, angle = 0))
  print(p1)
}

#########################
S <- twenty_seasons_counts
S %>%
  group_by(STATE, O_RUNS.ROI) %>%
  summarize(N = sum(N),
            .groups = "drop") -> S_20_seasons
L_20 <- logit_work(S_20_seasons)
L_20 %>%
  mutate(Type = "Observed") -> L_20

S1 <- ordinal_model_logits %>%
  mutate(Type = "Model") -> S1

L_all <- rbind(L_20, S1)

bases <- c("000", "100", "020", "120",
           "003", "103", "023", "123")
############################
ui <- fluidPage(
  titlePanel("Logit Comparison of Run Scoring Across States: 2000-2019"),
  sidebarLayout(
    sidebarPanel(
      h4("Select First State:"),
      selectInput("sel_bases_1",
                    "Select Runners on Base:",
                    choices = bases,
                    selected = "000"),
      radioButtons("sel_outs_1",
                   "Select Outs:",
                   choices = c("0", "1", "2"),
                   selected = "0",
                   inline = TRUE),
      h4("Select Second State:"),
      selectInput("sel_bases_2",
                  "Select Runners on Base:",
                  choices = bases,
                  selected = "100"),
      radioButtons("sel_outs_2",
                   "Select Outs:",
                   choices = c("0", "1", "2"),
                   selected = "0",
                   inline = TRUE),
      radioButtons("logit_type",
                   "Select Logit Type:",
                   choices = c("<=", ">"),
                   inline = TRUE)
    ),
    mainPanel(
      plotOutput("plot",
                 height = '550px'))
  )
)

server <- function(input, output) {

  output$plot <- renderPlot({

    recode_bases <- function(bases){
      bases <- ifelse(bases == "020", "010", bases)
      bases <- ifelse(bases == "120", "110", bases)
      bases <- ifelse(bases == "103", "101", bases)
      bases <- ifelse(bases == "023", "011", bases)
      bases <- ifelse(bases == "123", "111", bases)
      bases <- ifelse(bases == "003", "001", bases)
    }
    bases1 <- recode_bases(input$sel_bases_1)
    bases2 <- recode_bases(input$sel_bases_2)

    if(input$logit_type == ">"){
      L_all[, 1:24] <- - L_all[, 1:24]
    }

    compare_logit_plot4(L_all,
                        c(bases1, bases2),
                        c(input$sel_outs_1, input$sel_outs_2),
                        input$logit_type)
})
}

# Run the application
shinyApp(ui = ui, server = server)
