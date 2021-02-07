BetaBinomialABC <- function() {
  appDir <- system.file("shiny-examples", "BetaBinomialABC",
                        package = "ShinyBaseball")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
