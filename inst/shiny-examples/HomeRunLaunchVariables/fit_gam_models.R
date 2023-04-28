fit_gam_models <- function(){

  data_work <- function(){
    sc_2021 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast2021.csv")
    sc_2022 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2022.csv")
    sc_2023 <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/statcast_2023.csv")
    sc_2022 %>%
      mutate(HR = ifelse(events == "home_run", 1, 0),
             game_date = Game_Date)  %>%
      select(game_year, game_date, launch_angle,
             launch_speed, HR) ->
      scip_2022
    sc_old <- read_csv("https://raw.githubusercontent.com/bayesball/HomeRuns2021/main/SC_BB_mini.csv")

    names(sc_old)[2] <- "Game_Date"

    hits <- c("single", "double", "triple",
              "home_run")
    sc_2021 %>%
      mutate(HR = ifelse(events == "home_run", 1, 0),
             H = ifelse(events %in% hits, 1, 0)) %>%
      select(game_year, Game_Date, launch_angle,
             launch_speed, events, HR, H) -> sc_2021
    sc_2022 %>%
      mutate(HR = ifelse(events == "home_run", 1, 0),
             H = ifelse(events %in% hits, 1, 0))  %>%
      select(game_year, Game_Date, launch_angle,
             launch_speed, events, HR, H) ->
      sc_2022
    sc_2023 %>%
      mutate(HR = ifelse(events == "home_run", 1, 0),
             H = ifelse(events %in% hits, 1, 0))  %>%
      select(game_year, Game_Date, launch_angle,
             launch_speed, events, HR, H) ->
      sc_2023
    sc <- rbind(sc_old, sc_2021, sc_2022, sc_2023)

    sc %>%
      mutate(Season = year(Game_Date))
  }

   require(dplyr)
   require(mgcv)
   require(lubridate)

   scip <- data_work()

   fit <- NULL
   k <- 0
   for (year in c(2019, 2021, 2022)){
       for (month in 4:9){
        k <- k + 1
        fit[[k]] <- gam(HR ~ s(launch_angle, launch_speed),
              family = "binomial",
              data = filter(scip, Season == year,
                      month(Game_Date) == month))
        print(k)
       }
     }
fit
}



