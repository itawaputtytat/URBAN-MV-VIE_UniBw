
# Start timer -------------------------------------------------------------

if (sett_pred$print_ptm_stepwise) { 
  ptm_step <- proc.time()
}



# Simulate trajectories ---------------------------------------------------

sett_sim_temp$time_s_diff <- rep(0.01, sett_sim$time_sim_s / 0.01)

dat_sim <-
  predLiebner_modelDrivBehav_batch(
    states_n_I = sett_bn$states_n$I,
    states_n_S = sett_bn$states_n$S,
    states_n_A = sett_bn$states_n$A,
    thresholds_u_max = sett_dsm$thresholds$u_max,
    thresholds_acc_lon_max = sett_dsm$thresholds$acc_lon_max,
    dat_dsm_spread = dat_dsm_spread,
    am1 = sett_sim_temp$am1,
    am2 = sett_sim_temp$am2,
    speed1 = sett_sim_temp$speed1,
    time_s_diff = sett_sim_temp$time_s_diff,
    idm_delta = sett_idm$delta,
    idm_d0 = sett_idm$d0,
    idm_b = sett_idm$b,
    obj_positions = sett_sim$obj_pos)

## Extract simulation tails
dat_sim_tails <- predLiebner_getSimTails(dat_sim)



# Compute P(O|Hi) ---------------------------------------------------------

dat_prob <- 
  predLiebner_compProb_O_Hi(
    dat_sim_tails,
    am2 = sett_sim_temp$am2,
    speed2 = sett_sim_temp$speed2,
    coll_prob = coll_prob_template,
    sigma_s_m = sett_pred$sigma_s_m,
    sigma_v_ms = sett_pred$sigma_v_ms)



# Set evidence and query results ------------------------------------------

## Prepare CPT for node O
## Compute complementary probabilities
sett_bn$likelihoods$O <- dat_prob
sett_bn$likelihoods$O <- as.vector(t(sett_bn$likelihoods$O))
sett_bn$likelihoods$O <- 
  rbind(sett_bn$likelihoods$O, 
        1 - sett_bn$likelihoods$O)
attributes(sett_bn$likelihoods$O) <- attributes(sett_bn$priors$O)

bn <- 
  predLiebner_updateBN(
    "bn", 
    likelihood_O = sett_bn$likelihoods$O,
    state_names_O = sett_bn$state_names$O)

## Set evidence and query results
bn.evidence <- 
  setEvidence(bn, 
              nodes = c("O", sett_pred$bn_evidence$nodes), 
              states = c("dat_prob$obs", unlist(sett_pred$bn_evidence$states)))

dat_pred_results <- querygrain(bn.evidence, nodes = "I")$I



# Stop timer --------------------------------------------------------------

if (sett_pred$print_ptm_stepwise) {
  outputProcTime(ptm_step)
}