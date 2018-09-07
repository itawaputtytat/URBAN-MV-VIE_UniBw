
# Load DSM ----------------------------------------------------------------

dat_dsm <- 
  dbGetSrc(sett_dsm$db$conn_name, 
           sett_dsm$db$src_names$dsm)

dat_dsm_spread <- 
  predLiebner_initDSM(
    dat_dsm,
    col_name_cluster_group = sett_dsm$col_names$cluster_group,
    col_name_am = sett_dsm$col_names$am,
    col_name_speed = sett_dsm$col_names$speed,
    threshold_am = sett_dsm$thresholds$u_max_am,
    thresholds_u_max = sett_dsm$thresholds$u_max)



# Load priors -------------------------------------------------------------

## Load priors for speed models and name values
dat_priors_speed <- 
  dbGetSrc(sett_dsm$db$conn_name, 
           sett_dsm$db$src_names$priors_speed)

dat_priors_speed_rates <- dat_priors_speed %>% pull(rate)
names(dat_priors_speed_rates) <- dat_priors_speed$M

## Load priors for max. lon. acc. and name values
dat_priors_acc_lon_max <- 
  dbGetSrc(sett_dsm$db$conn_name, 
           sett_dsm$db$src_names$priors_acc_lon_max) %>% 
  mutate(name = paste_(M, a))

dat_priors_acc_lon_max_rates <- dat_priors_acc_lon_max %>% pull(rate)
names(dat_priors_acc_lon_max_rates) <- dat_priors_acc_lon_max$name



# fsdfds ------------------------------------------------------------------

v_v50 <- 
  dbGetSrc(sett_dsm$db$conn_name,
           "t_pred_study2_p02_v50_priors_dti_m_rnd1_speed_models")

v_v70 <- 
  dbGetSrc(sett_dsm$db$conn_name,
           "t_pred_study2_p02_v70_priors_dti_m_rnd1_speed_models")

v_probs <- 
  rbind(v_v50 %>% mutate(stress = "no stress"),
        v_v70 %>% mutate(stress = "stress"))

v_probs <- 
  v_probs %>% 
  arrange(desc(stress), M)

dat_priors_speed <- v_probs

if (sett_pred$bn_version == "stress2") {
  
  dat_priors_speed_rates <- dat_priors_speed %>% pull(rate)
  names(dat_priors_speed_rates) <- dat_priors_speed$M
  
}


if (sett_pred$bn_version == "stress_style") {
  
  dat_priors_speed_rates <- dat_priors_speed %>% pull(rate)
  names(dat_priors_speed_rates) <- dat_priors_speed$M
  
}


test <- rbind(v_probs, v_probs, v_probs)
test$driving_style <- 
  c(paste0("group", rep(1, 6)),
    paste0("group", rep(2, 6)),
    paste0("group", rep(3, 6)))
  

# fsdfs -------------------------------------------------------------------

acc_v50 <- 
  dbGetSrc(sett_dsm$db$conn_name, 
           "t_pred_study2_p02_v50_priors_acc_lon_max") %>% 
  mutate(stress = "no_stress")

acc_v70 <- 
  dbGetSrc(sett_dsm$db$conn_name, 
           "t_pred_study2_p02_v70_priors_acc_lon_max") %>% 
  mutate(stress = "stress")

acc_max <- 
  rbind(acc_v50, acc_v70) %>% 
  mutate(name = paste_(M, a, stress)) %>% 
  arrange(M, desc(stress), a)

dat_priors_acc_lon_max <- acc_max


if (sett_pred$bn_version == "stress" | sett_pred$bn_version == "stress2") {

  ## Load priors for max. lon. acc. and name values
  # dat_priors_acc_lon_max <- 
  #   dbGetSrc(sett_dsm$db$conn_name, 
  #            sett_dsm$db$src_names$priors_acc_lon_max) %>% 
  #   mutate(name = paste_(M, a))
  
  dat_priors_acc_lon_max_rates <- dat_priors_acc_lon_max %>% pull(rate)
  names(dat_priors_acc_lon_max_rates) <- dat_priors_acc_lon_max$name
  
# dat_priors_acc_lon_max <- 
#   expand.grid(M = paste0("k", 1:3), 
#               a = paste0("l", 1:3), 
#               stress = c("stress", "no_stress"))
# 
# dat_priors_acc_lon_max <- 
#   dat_priors_acc_lon_max %>% 
#   arrange(M, stress, a)

# dat_priors_acc_lon_max$rate <-
#   c(0.1, 0.8, 0.1,
#     0, 0.4, 0.6,
#     0.2, 0.7, 0.1,
#     0.1, 0.2, 0.7,
#     0.7, 0.2, 0.1,
#     0.5, 0.3, 0.2
#     ) 
  # c(0,0,1,
  #   0,0,1,
  #   0,0,1,
  #   0,0,1,
  #   0,0,1,
  #   0.5,0.4,0.1)

# dat_priors_acc_lon_max <- 
#   dat_priors_acc_lon_max %>% 
#   mutate(name = paste_(M, a, stress))
# 
# dat_priors_acc_lon_max_rates <- dat_priors_acc_lon_max %>% pull(rate)
# names(dat_priors_acc_lon_max_rates) <- dat_priors_acc_lon_max$name

}


# Initialise IDM settings -------------------------------------------------

source("fun_Liebner_2013/settings/sett_idm.R")



# Initialise BN components ------------------------------------------------

## BN
if (sett_pred$bn_version == "Liebner") {
  
  sett_bn <- 
    predLiebner_initSettingsForBN(
      priors_speed = dat_priors_speed_rates,
      priors_acc_lon_max = dat_priors_acc_lon_max_rates)
  
  bn <- predLiebner_initBN(sett_bn)
  
  ## Collector for probabilities (P_O_Hi)
  coll_prob_template <- 
    idm_createSimDat(
      list(Ij = sett_bn$states_n$I,
           Mk = sett_bn$states_n$S, 
           al = sett_bn$states_n$A),
      id_order = sett_bn$id_order)
}



# fdsfdsf# ----------------------------------------------------------------

if (sett_pred$bn_version == "stress") {
  
  sett_bn <- 
    predLiebner_initSettingsForBN_stress(
      priors_speed = dat_priors_speed_rates,
      priors_acc_lon_max = dat_priors_acc_lon_max_rates)
  
  bn <- predLiebner_initBN_stress(sett_bn)
  
  ## Collector for probabilities (P_O_Hi)
  coll_prob_template <- 
    idm_createSimDat(
      list(Ij = sett_bn$states_n$I,
           Mk = sett_bn$states_n$S, 
           al = sett_bn$states_n$A),
      id_order = sett_bn$id_order)
}



if (sett_pred$bn_version == "stress2") {
  
  sett_bn <- 
    predLiebner_initSettingsForBN_stress2(
      priors_speed = dat_priors_speed_rates,
      priors_acc_lon_max = dat_priors_acc_lon_max_rates)
  
  bn <- predLiebner_initBN_stress2(sett_bn)
  
  ## Collector for probabilities (P_O_Hi)
  coll_prob_template <- 
    idm_createSimDat(
      list(Ij = sett_bn$states_n$I,
           Mk = sett_bn$states_n$S, 
           al = sett_bn$states_n$A),
      id_order = sett_bn$id_order)
}
