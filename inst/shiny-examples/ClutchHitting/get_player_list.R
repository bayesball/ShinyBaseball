get_player_list <- function(rdata, minPA = 400){
  library(dplyr)
  library(Lahman)

  rdata %>%
    filter(BAT_EVENT_FL == TRUE) %>%
    group_by(BAT_ID) %>%
    summarize(PA = n()) %>%
    filter(PA >= minPA) %>%
    pull(BAT_ID) -> player_ids

  People %>%
    filter(retroID %in% player_ids) %>%
    mutate(Name = paste(nameFirst, nameLast))  %>%
    arrange(nameLast, nameFirst) %>%
    select(Name, retroID)
}
