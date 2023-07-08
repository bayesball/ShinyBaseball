setup_work <- function(rdata, tr){
  require(dplyr)
  rdata_b <- filter(rdata, BAT_EVENT_FL==TRUE)
  runs <- summarize(group_by(rdata_b,
                             STATE), R=mean(RUNS.ROI))
  runs <- rbind(runs, data.frame(STATE="3", R=0))
  runs_before <- inner_join(select(tr, STATE),
                            runs, by="STATE")
  runs_after <- inner_join(select(tr, NEW.STATE),
                           runs, by=c("NEW.STATE"="STATE"))
  tr_new <- cbind(runs_before, runs_after, tr[, 3:4])
  names(tr_new) <- c("STATE", "R_before", "NEW_STATE",
                     "R_after", "RUNS.SCORED", "Type")
  tr_new <- mutate(tr_new,
                   RUNS=R_after - R_before + RUNS.SCORED)
  tr_new
}
