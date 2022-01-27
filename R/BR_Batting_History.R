BR_Batting_History <- function() {
  appDir <- system.file("shiny-examples",
                        "BR_Batting_History",
                        package = "ShinyBaseball")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
