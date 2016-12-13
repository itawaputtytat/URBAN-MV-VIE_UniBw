
# Select data -------------------------------------------------------------

dat4prob <- 
  datwclust %>% 
  select(passing, round_txt, clustgroup)



# Compute P(Mk) -----------------------------------------------------------

prob_Mk <- 
  predLiebner_compProb_Mk(dat4prob, 
                          showplot = T)
# prob_Mk <- 
#   predLiebner_compProb_Mk(datwclust, 
#                           varname4group = "round_txt", 
#                           showplot = T)

plotdat_prob_Mk <- prob_Mk$plotdat
prob_Mk <- prob_Mk$prob$rate



# Compute P(al|Mk) --------------------------------------------------------

acclon_ms2.thresh <- c(1.5, 2, 2.5)

prob_al_Mk <- predLiebner_compProb_al_Mk(dat4prob,
                                         dat4acclon_ms2.est.max,
                                         acclon_ms2.thresh,
                                         showplot = T)

# prob_al_Mk <- predLiebner_compProb_al_Mk(dat4prob, 
#                                          dat4acclon_ms2.est.max,
#                                          acclon_ms2.thresh,
#                                          varname4group = "round_txt",
#                                          showplot = T)

plotdat_prob_al_Mk <- prob_al_Mk$plotdat
prob_al_Mk <- prob_al_Mk$prob$rate



# Visualise P(Mk) and P(al|Mk) --------------------------------------------

grid.arrange(plotdat_prob_Mk, plotdat_prob_al_Mk, nrow = 1)