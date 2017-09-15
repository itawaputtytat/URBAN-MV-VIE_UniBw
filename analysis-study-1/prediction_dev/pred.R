
sett_sim_temp$time_s_diff <- rep(0.01, 1 / 0.01)
sett_sim_temp$pos4carryout_precise <- sett_sim_temp$dist2

# Start timer -------------------------------------------------------------

# outputSectionTitle("Start of prediction")
# outputString(paste("* Approach:", sett_algo$hypscore))
# outputString(paste("* Carried out at:", sett_sim$pos4carryout, "m"))
# outputString(paste("* Time lag:", sett_sim$timelag_s, "s"))

if (sett_proc$stepwise_proc_time)
  ptm_step <- proc.time()

# Simulate trajectories ---------------------------------------------------

coll4simtail <- coll4simtail_template
#rm(dat_sim); invisible(gc())
dat_sim <-
  predLiebner_modelDrivBehav_batch("simulation-based",
                                   sett_sim_temp$pos4carryout_precise,
                                   sett_sim,
                                   sett_sim_temp,
                                   sett_dat,
                                   dat_dsm,
                                   coll4simtail)



# Compute P(O|Hi) ---------------------------------------------------------

dat_prob <- predLiebner_compProb_O_Hi(sett_sim, sett_sim_temp$pos4carryout_precise, dat_sim, P_O_Hi)


# Re-order P(O|Hi) --------------------------------------------------------

# dat_prob_temp <- data.frame(dat_prob[sett_bn$idorder],
#                             1 - dat_prob[sett_bn$idorder])
# dat_prob_temp <- as.vector(t(dat_prob_temp))
# dat_prob_temp <- data.frame(as.vector(dat_prob), 1 - as.vector(dat_prob))
# dat_prob_temp <- as.vector(t(dat_prob_temp))
dat_prob_temp <- as.vector(t(dat_prob))
dat_prob_temp <- rbind(dat_prob_temp, 1 - dat_prob_temp)
  # dat_prob_temp <- 
#   data.table::rbindlist(list(dat_prob, 1-dat_prob)) %>% 
#   data.frame()


# Set evidence and query results ------------------------------------------

## Prepare CPT for node O
sett_bn$O <-
  array(dat_prob_temp,
        dim = c(2, 4, 3, 3),
        dimnames = list(O = sett_bn$states$O,
                        I = sett_bn$states$I,
                        V = sett_bn$states$V,
                        A = sett_bn$states$A))

bn <- predLiebner_updateBN(bn, sett_bn$O, sett_bn)
bn.evidence <- setEvidence(bn, nodes = c("O"), states = c("dat_prob$obs"))
dat_pred_results <- querygrain(bn.evidence, nodes = "I")$I



# Stop timer --------------------------------------------------------------

if (sett_proc$stepwise_proc_time)
  outputProcTime(ptm_step)