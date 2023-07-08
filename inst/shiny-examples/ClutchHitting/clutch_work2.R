clutch_work2 <- function(rdata, tr_new, retro_id, nsims = 200){
  require(Lahman)
  require(dplyr)
  require(ggplot2)

#  names <- unlist(strsplit(player, " "))
#  id <- filter(People, nameLast==names[2],
#               nameFirst == names[1])$retroID

  People %>%
    filter(retroID == retro_id) %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    pull(Name) -> player

  results <- select(filter(rdata,
                      BAT_ID == retro_id, BAT_EVENT_FL),
                    STATE, EVENT_CD)
  results <- filter(results, EVENT_CD %in%
                      c(2, 3, 14, 15, 16, 20, 21, 22, 23))
  results <- mutate(results,
                    Type=ifelse(EVENT_CD < 4, "Out",
                         ifelse(EVENT_CD %in% c(14, 15, 16), "Walk",
                         ifelse(EVENT_CD == 20, "Single",
                         ifelse(EVENT_CD == 21, "Double",
                         ifelse(EVENT_CD == 22, "Triple", "Home Run"))))))
  results <- inner_join(select(results, STATE, Type),
                        select(tr_new, STATE, Type, RUNS),
                        by=c("STATE", "Type"))

  one_sim <- function(results){
    results$Type <- sample(results$Type)
    results <- inner_join(select(results, STATE, Type),
                           select(tr_new, STATE, Type, RUNS),
                           by=c("STATE", "Type"))
    sum(results$RUNS)
  }
  out <- data.frame(Observed = sum(results$RUNS),
    Simulated = replicate(nsims, one_sim(results)))

  Expected <- mean(out$Simulated)
  Clutch <- round(out$Observed[1] - Expected, 1)

  ggplot(out, aes(Simulated)) +
    geom_histogram(color = "white",
                   fill = "tan",
                   bins = 10) +
    geom_vline(aes(xintercept = Observed), color = "red",
               linewidth = 2) +
    geom_vline(aes(xintercept = Expected), color = "blue",
               linewidth = 2,
               linetype = 2) +
    labs(title = paste("2022", player, "Simulated Runs"),
         subtitle = paste("CLUTCH = Runs Minus E(Runs) =", Clutch)) +
    xlab(paste("Simulated Runs, # of Simulations =", nsims)) + ylab("") +
    annotate(geom = "text", x = out$Observed[1] + 4,
             y = 40 * nsims / 200, label = "Observed",
             size = 5, color = "red") +
    annotate(geom = "text", x = Expected + 4,
             y = 30 * nsims / 200, label = "Expected",
             size = 5, color = "blue") +
    theme(text=element_text(size=18)) +
    theme(plot.title = element_text(colour = "blue", size = 18,
                  hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "red", size = 16,
                          hjust = 0.5, vjust = 0.8, angle = 0)) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}
