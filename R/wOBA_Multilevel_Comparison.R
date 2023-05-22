wOBA_Multilevel_Comparison <- function() {
  appDir <- system.file("shiny-examples",
                        "wOBA_Multilevel_Comparison",
                        package = "ShinyBaseball")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
