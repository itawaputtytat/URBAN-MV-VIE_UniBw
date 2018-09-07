input <- 0
while(T) {
  outputString(paste("[1] <<<",
                     sett_sim_temp$am2,
                     ">>> [2]",
                     "| STOP [3]"))
  input <- readline(">>> ")
  if (input == 1) {
    sett_sim_temp$am2 <- sett_sim_temp$am2 - 1 * sett_pred$carryout_am_step
    break
  }
  if (input == 2 | input == "")  {
    sett_sim_temp$am2 <- sett_sim_temp$am2 + 1 * sett_pred$carryout_am_step
    break
  }
  if (input == 3) {
    opt <- options(show.error.messages = FALSE) 
    on.exit(options(opt)) 
    outputString("Stopped Prediction")
    stop() 
  }
}
next