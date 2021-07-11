# adjust so that we have PA instead of AB

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(LearnBayes)
library(tidyr)

retro_data <- read_csv("https://raw.githubusercontent.com/bayesball/Predicting_Outcomes/main/retro2019data.csv")

prediction_exercise <- function(d,
                                date_break,
                                minPA = 100,
                                type = "H",
                                logn = 3,
                                ab = c(1, 1),
                                exclude_pitchers = "Yes"){
  # define Outcome variable
  if(type == "H"){
    d %>% mutate(Outcome = ifelse(H == 1, 1, 0)) ->
      d
  }
  if(type == "SO"){
    d %>% mutate(Outcome =
                   ifelse(EVENT_CD == 3, 1, 0)) ->
      d
  }
  if(type == "HR"){
    d %>% mutate(Outcome =
                   ifelse(EVENT_CD == 23, 1, 0)) ->
      d
  }
  # include pitcher batting?
  if(exclude_pitchers == "Yes"){
    d <- filter(d, BAT_FLD_CD != 1)
  }
  # divide data into two parts using date_break
  d1 <- filter(d, Date <= date_break)
  d2 <- filter(d, Date > date_break)
  # fit bb model to data from 1st part
  d1 %>%
    group_by(BAT_ID) %>%
    summarize(y = sum(Outcome),
              n = n(),
              AVG = y / n) %>%
    filter(n >= minPA) -> S1
  fit <- fit_bb_model2(S1,
                       prior = list(ab = ab, logn = logn))
  S1$MLM_Predicted <- fit$d$est
  # compute AVG in 2nd part and merge with 1st part
  d2 %>%
    group_by(BAT_ID) %>%
    summarize(y2 = sum(Outcome),
              n2 = n()) %>%
    filter(n2 > 0) %>%
    mutate(AVG2 = y2 / n2) %>%
    select(BAT_ID, y2, n2, AVG2) -> S2
  # merge two datasets
  S12 <- inner_join(S1, S2, by = "BAT_ID")

  # compute sum of squared prediction errors for
  # raw and multilevel estimates
  S12 %>%
    summarize(Observed = sum((AVG - AVG2) ^ 2),
              Multilevel = sum((MLM_Predicted - AVG2) ^ 2)) -> crit
  list(S12 = S12,
       K = fit$K, eta = fit$eta,
       crit = crit)
}

fit_bb_model2 <- function(data, prior){
  bblogpost <- function(theta, datapar){
    y <- datapar$df$y
    n <- datapar$df$n
    ab <- datapar$ab
    logn <- datapar$logn
    eta <- exp(theta[1])/(1 + exp(theta[1]))
    K <- exp(theta[2])
    logf <- function(y, n, K, eta){
      lbeta(K * eta + y, K * (1 - eta) + n - y) -
        lbeta(K * eta, K * (1 - eta))
    }
    loglike <- sum(logf(y, n, K, eta))
    loglike + dbeta(eta, ab[1], ab[2], log = TRUE) +
      + dlogis(theta[2], logn, 1, log = TRUE) +
      log(eta * (1 - eta))
  }
  datapar <- list(df = data,
                  ab = prior$ab,
                  logn = prior$logn)
  mode <- laplace(bblogpost, c(1, 1), datapar)$mode
  eta <- exp(mode[1])/(1 + exp(mode[1]))
  K <- exp(mode[2])
  list(eta = eta, K = K,
       d = data.frame(data,
             est = (data$y + K * eta) / (data$n + K)))
}

# shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4,
                          bootswatch = "darkly"),
  fluidRow(
    column(4, wellPanel(
      h4(id="big-heading",
         "Choose Inputs"),
      radioButtons(
        inputId = "type",
        label = h6("Outcome:"),
        choices = c("H", "SO", "HR"),
        selected = c("H"),
        inline = TRUE),
      dateInput("date_break",
                label = h6("Date Breakpoint:"),
                value = "2019-07-01"),
      sliderInput("minPA",
                  h6("Minimum Plate Appearances:"),
                  min = 1,
                  max = 300,
                  value = 1),
      radioButtons(
        inputId = "exclude_pitcher",
        label = h6("Exclude pitcher batting:"),
        choices = c("Yes", "No"),
        selected = c("Yes"),
        inline = TRUE),
      h5("Parameter Estimates:"),
      tableOutput("table1"),
      downloadButton("downloadData", "Download Rates")
    )),
    column(8, wellPanel(
      h4(id="big-heading",
         "Predicting 2019 Batting Rates (PA)", align = "center"),
      tabsetPanel(type = "tabs",
                  tabPanel("Intro",
                           hr(),
                           h5("Prediction Problem:"),
                           p("Have Retrosheet data
                             for all plate appearances during 2019
                             season.  Divide data into
                             'Train' and 'Test' groups.
                             The problem is to accurately
                             predict the batting rates in the
                             Test group from the observed rates in
                             the Train group."),
                           img(src="Rplot.png",
                               height = 200, width = 300),
                           hr(),
                           tags$ul(
                           tags$li("Choose Outcome -- either
                           H (hit), SO (strikeout) or
                           HR (home run)."),
                           tags$li("Choose the Date Breakpoint that divides
                             the Test and Train datasets."),
                           tags$li("Choose the Minimum number of
                             Plate Appearances for the Train dataset."),
                           tags$li("Do you wish to exclude pitcher batting?
                                   (Yes or No)")
                           ),
                           p("This app illustrates the use of a
                             multilevel model to predict
                             the rates in the Test dataset.")
                           ),
                  tabPanel("Rates",
                           plotOutput("plot1",
                                      height = "405px"),
                           h5("Sum of Squared Errors in Predicting Future Rates:"),
                           tableOutput("table2")
                  ),
                  tabPanel("Talents",
                           plotOutput("plot2",
                                      height = "405px"),
                           hr(),
                           p("This graph displays the estimated Beta density for the true rates
          using the Beta/Binomial multilevel model where the Beta shape parameters
          are given by a = K eta and b = K (1 - eta).")
                  ),
          tabPanel("Description",
                   p('This app illustrates prediction of batting rates using the following
                    Beta/Binomial multilevel model'),
                   p('We observe y_1, ..., y_N,
                      where y_i, the count of either H, SO, or HR
                      for player i in PA_i plate appearances,
                      is Binomial(PA_i, p_i) where p_i is the
                      true rate.  Assume p_1, ..., p_N
                      are drawn from a Beta distribution with
                      shape parameters K eta and K (1 - eta).  At the
                      last stage, eta is assumed Beta(1, 1) and log K
                      is Logistic with location 3 and scale 1.'),
                   p('Given values of K and eta, the posterior estimate of
                    the true rate p_i is (AB_i / (AB_i + K) (y_i / AB_i) +
                    (K / (AB_i + K) eta.'),
                   h5('Using the App'),
                   p("One selects a date during the 2019 season.  One trains
                    the model using hitting data up to that date, and predicts
                    rates for hitting data after that date.  One
                    decides on the type of rate (H, SO or HR), the
                    minimum number of PA for batters in the training
                    dataset, and whether or not you wish to exclude pitchers
                    batting from the dataset."),
                   p("Point estimates for the parameters K and eta
                    are shown in the left panel.
                    The `Rates` tab displays dotplots for
                    the observed first-period rates and the predicted
                    second-period rates.  The sum of squared
                    prediction errors are displayed both for the observed
                    first-period rates and the
                    multilevel model predictions.  The
                    `Talents`
                    tab shows the estimated Beta density curve for the
                    true rates.")))
    ))
  ))

server <- function(input, output, session) {
  output$plot1 <- renderPlot({

    out <- prediction_exercise(retro_data,
                               input$date_break,
                               input$minPA,
                               input$type,
                               exclude_pitchers = input$exclude_pitcher)

    out$S12 %>%
      mutate(Observed = y / n) %>%
      select(BAT_ID, Observed, MLM_Predicted) %>%
      pivot_longer(cols = Observed:MLM_Predicted,
                   names_to = "Type",
                   values_to = "Estimate") -> s1

    the_title <- paste("Observed ", input$type,
                       " Rates & MLM Predictions",
                       sep = "")
    the_subtitle <- paste("Date Breakpoint: ",
                          input$date_break,
                          ",  minAB = ",
                          input$minPA, sep = "")
    if(input$exclude_pitcher == "Yes"){
      the_subtitle <- paste(the_subtitle, ", Pitchers Excluded",
                            sep = "")
    } else {
      the_subtitle <- paste(the_subtitle, ", All Batters",
                            sep = "")
    }
    s1$Type <- factor(s1$Type,
                      levels = c("Observed", "MLM_Predicted"))
    ggplot(s1, aes(Type, Estimate)) +
      geom_jitter(width = 0.2, color = "red") +
      ylab("Rate") + xlab("Method") +
      labs(title = the_title,
           subtitle = the_subtitle) +
      theme(text = element_text(size=14),
            plot.title =
              element_text(colour = "white", size = 14,
                           hjust = 0.5),
            plot.subtitle =
              element_text(colour = "white", size = 10,
                           hjust = 0.5)) +
      theme(plot.background = element_rect(fill = "grey32"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white")) +
      theme(
        panel.background = element_rect(fill = "bisque",
                                        colour = "grey"))
  }, res = 96)

  output$plot2 <- renderPlot({
    out <- prediction_exercise(retro_data,
                               input$date_break,
                               input$minPA,
                               input$type,
                               exclude_pitchers = input$exclude_pitcher)

    a <- round(out$K * out$eta, 1)
    b <- round(out$K * (1 - out$eta), 1)

    s <- sqrt(out$eta * (1 - out$eta) / (out$K + 1))
    x_lo <- max(out$eta - 4 * s, 0)
    x_hi <- min(out$eta + 4 * s, 1)

    TH <- theme(plot.title = element_text(colour = "white",
                                          size = 18, hjust = 0.5),
                plot.subtitle = element_text(colour = "white",
                                             size = 18, hjust = 0.5),
                text = element_text(size = 18),
                panel.background = element_rect(fill = "bisque",
                                                colour = "grey")) +
      theme(plot.background = element_rect(fill = "grey32"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"))
    Title <- paste("Beta(", a, ",", b, ")")
    Subtitle <- paste("Talent Curve for", input$type, "Rates")
    if(input$exclude_pitcher == "Yes"){
      Subtitle <- paste(Subtitle, ", Pitchers Excluded",
                            sep = "")
    } else {
      Subtitle <- paste(Subtitle, ", All Batters",
                        sep = "")
    }
    x <- NULL
    ggplot(data.frame(x = c(x_lo, x_hi)), aes(x)) +
      stat_function(fun = dbeta,
                    geom = "line",
                    color = "red",
                    size = 1.5,
                    args = list(shape1 = a, shape2 = b)) +
      labs(title = Title, subtitle = Subtitle) +
      TH +
      xlab(paste(input$type, "Rate")) +
      ylab("Density")
  })

  output$table1 <- renderTable({
    out <- prediction_exercise(retro_data,
                               input$date_break,
                               input$minPA,
                               input$type,
                               exclude_pitchers = input$exclude_pitcher)
    data.frame(eta = out$eta,
               K = out$K)
  }, digits = 3)

  output$table2 <- renderTable({
    out <- prediction_exercise(retro_data,
                               input$date_break,
                               input$minPA,
                               input$type,
                               exclude_pitchers = input$exclude_pitcher)
    d <- out$crit
    names(d) <- paste(names(d), input$type, "Rate")
    d
  }, digits = 3)

  output$downloadData <- downloadHandler(
    filename = "hitting_rates_output.csv",
    content = function(file) {
      out <- prediction_exercise(retro_data,
                                 input$date_break,
                                 input$minPA,
                                 input$type,
                  exclude_pitchers = input$exclude_pitcher)$S12
      out$Date_Break <- input$date_break
      out$Min_AB <- input$minPA
      out$Type <- input$type
      out$Pitchers_Excluded <- input$exclude_pitcher
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
