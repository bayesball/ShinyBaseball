ExpectedBA <- function() {
  appDir <- system.file("shiny-examples",
                        "ExpectedBA",
                        package = "ShinyBaseball")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
