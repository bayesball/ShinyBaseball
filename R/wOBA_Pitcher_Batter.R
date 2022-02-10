wOBA_Pitcher_Batter <- function() {
  appDir <- system.file("shiny-examples",
                        "wOBA_Pitcher_Batter",
                        package = "ShinyBaseball")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
