idmGap <- function(intent, d0, v_sim.prev, model_acclon.max, b, s_sim.prev, posobj) {
  if (intent %in% c(1, 3)) {
    gap.des <- 0
    gap.act <- 1
  } else {
    gap.des <- computeIDM_gap.desired(d0, v_sim.prev, model_acclon.max, b)
    gap.act <- computeIDM_gap.actual(s_sim.prev, posobj)
  }
  
  return(list(gap_desired = gap.des,
              gap_actual = gap.act))
}
