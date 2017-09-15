input <- 0
while(T) {
  outputString(paste("[1] <<<",
                     sett_sim_temp$pos4carryout_precise,
                     ">>> [2]",
                     "| STOP [3]"))
  input <- readline(">>> ")
  if (input == 1) {
    sett_sim_temp$dist2 <- sett_sim_temp$dist2 - 1 * sett_proc$carryout_step
    break
  }
  if (input == 2 | input == "")  {
    sett_sim_temp$dist2 <- sett_sim_temp$dist2 + 1 * sett_proc$carryout_step
    break
  }
  if (input == 3) {
    opt <- options(show.error.messages = FALSE) 
    on.exit(options(opt)) 
    stop() 
  }
}
next