BR_Pitching_History <- function() {
  appDir <- system.file("shiny-examples",
                        "BR_Pitching_History",
                        package = "ShinyBaseball")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TeachBayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
