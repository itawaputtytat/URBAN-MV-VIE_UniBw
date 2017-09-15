
# Preparatory settings ----------------------------------------------------

sett_prob <- c()
sett_prob$acc_thresholds <- c(1.5, 2, 2.5)



# Select data -------------------------------------------------------------

dat_prob <- 
  dat_clust %>% 
  select_("passing", "round_txt", "cluster_group")



# Compute P(Mk) -----------------------------------------------------------

prob_Mk <- 
  predLiebner_compProb_Mk(dat_prob, 
                          #varname4group = "round_txt",
                          showplot = F)

plot_prob_Mk <- prob_Mk$plotdat
prob_Mk <- prob_Mk$prob$rate



# Compute P(al|Mk) --------------------------------------------------------

prob_al_Mk <- 
  predLiebner_compProb_al_Mk(dat_prob,
                             dat_acc_lon_max,
                             sett_prob$acc_thresholds,
                             #varname4group = "round_txt",
                             showplot = F)

plot_prob_al_Mk <- prob_al_Mk$plotdat
prob_al_Mk <- prob_al_Mk$prob$rate



# Visualise P(Mk) and P(al|Mk) --------------------------------------------

plot_prob <- arrangeGrob(plot_prob_Mk, plot_prob_al_Mk, nrow = 1)
plot(plot_prob)
