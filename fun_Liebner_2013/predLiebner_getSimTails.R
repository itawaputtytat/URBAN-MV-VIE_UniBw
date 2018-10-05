predLiebner_getSimTails <- function(dat_sim) {
  
  ## Extract simulation values
  ## ... for all hypothesis
  ## ... and both variables speed and distance
  
  dat_sim_tails <- lapply(dat_sim, function(hyp)
    lapply(hyp, function(var) {
      if (is.null(var)) {
        NA
      } else {
        tail(var, 1)
      }
    }))
  
  dat_sim_tails <-
    as.data.frame(
      data.table::rbindlist(dat_sim_tails,
                            idcol = T,
                            fill  = T))
  
  return(dat_sim_tails)
}
