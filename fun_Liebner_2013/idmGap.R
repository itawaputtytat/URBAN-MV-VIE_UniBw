idmGap <- function(intent, d0, v_sim.prev, model_acclon.max, b, s_sim.prev, posobj) {
  if (intent %in% c(1, 3)) {
    gap.des <- 0
    gap.act <- 1
  } else {
    gap.des <- idmGap_desired(d0, v_sim.prev, model_acclon.max, b)
    gap.act <- idmGap_actual(s_sim.prev, posobj)
  }
  
  return(list(gap_desired = gap.des,
              gap_actual = gap.act))
}
