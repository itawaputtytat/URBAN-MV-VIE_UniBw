deriveOrderOfHypotheses <- function(states_n_I,
                                    states_n_S,
                                    states_n_A) {
  coll_s <- c()
  for (s in 0:(states_n_S-1)) {
    coll_a <- c()
    for (a in 0:(states_n_A-1)) { 
      temp <- 
        seq(0, 
            a+(states_n_I-1) * states_n_A*states_n_S, 
            states_n_A*states_n_S)
      temp <- temp + a*states_n_S + 1
      coll_a <- c(coll_a, temp)
    }
    coll_s <- c(coll_s, coll_a + s*1)
  }
  
  return(coll_s)
}

# deriveOrderOfHypotheses(sett_bn$states_n$I, sett_bn$states_n$S, sett_bn$states_n$A)
# 
# idm_createSimDat(
#   list(Mk = sett_bn$states_n$S,
#        al = sett_bn$states_n$A,
#        Ij = sett_bn$states_n$I))
# 
# 1-13-25-37
# 5-14
# 
# nA <- sett_bn$states_n$A
# nS <- sett_bn$states_n$S
# nS <- 2
# nI <- sett_bn$states_n$I
# 
# t_s <- c()
# for (s in 0:(nS-1)) {
#   t_a <- c()
#   for (a in 0:(nA-1)) {
#     temp <- seq(0, a+(nI-1)*nA*nS, nA*nS)
#     temp <- temp + a*nS + 1
#     t_a <- c(t_a, temp)
#   }
#   t_s <- c(t_s, t_a + s*1)
# }
# print(t_s)
# 
