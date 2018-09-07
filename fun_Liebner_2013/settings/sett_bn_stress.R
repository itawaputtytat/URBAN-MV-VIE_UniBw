
# Initialise settings for BN ----------------------------------------------

## Abbreviations
## I: Intentions
## S: Speeds
## A: Accelerations
## O: Observations

predLiebner_initSettingsForBN_stress <- function(
  state_names_intent_prefix = "Intent",
  state_names_speed_prefix = "k",
  state_names_acc_lon_prefix = "l",
  state_names_stress = c("stress", "no_stress"),
  state_names_obs_prefix = c("dat_prob", "no_dat_prob"),
  length_intent = 4,
  length_speed = 3,
  length_acc_lon_max = 3,
  length_stress = 2,
  priors_intent = rep(1 / length_intent, length_intent),
  priors_speed,
  priors_acc_lon_max,
  priors_stress = rep(1 / length_stress, length_stress)) {
  
  ## Initialise settings
  sett_bn <- c()
  
  ## ----------------------------------------------------------------------
  
  ## State names
  sett_bn$state_names$stress <- state_names_stress
  ##
  sett_bn$state_names$I <- paste0("Intent", seq(length_intent))
  sett_bn$state_names$S <- paste0("k", seq(length_speed))
  sett_bn$state_names$A <- list()
  sett_bn$state_names$A[["A"]] <- paste0("l", seq(length_acc_lon_max))
  sett_bn$state_names$A[["stress"]] <- sett_bn$state_names$stress
  sett_bn$state_names$A[["S"]] <- sett_bn$state_names$S
  sett_bn$state_names$O <- paste0(c("dat_prob", "no_dat_prob"), "$obs")
  
  # Number of states per node
  sett_bn$states_n$stress <- length(sett_bn$state_names$stress)
  ##
  sett_bn$states_n$I <- length(sett_bn$state_names$I)
  sett_bn$states_n$S <- length(sett_bn$state_names$S)
  sett_bn$states_n$A <- length(sett_bn$state_names$A)
  sett_bn$states_n$O <- length(sett_bn$state_names$O)

  ## ----------------------------------------------------------------------
  
  ## Priors: stress
  sett_bn$priors$stress <- priors_stress
  sett_bn$priors$stress <- array(sett_bn$priors$stress)
  dimnames(sett_bn$priors$stress) <- list(sett_bn$state_names$stress)
  
  ## Priors: Intents
  sett_bn$priors$I <- priors_intent
  sett_bn$priors$I <- array(sett_bn$priors$I)
  dimnames(sett_bn$priors$I) <- list(sett_bn$state_names$I)
  
  ## Priors: Speed
  sett_bn$priors$S <- priors_speed
  sett_bn$priors$S <- array(sett_bn$priors$S)
  dimnames(sett_bn$priors$S) <- list(sett_bn$state_names$S)
  
  ## Priors: Maximum longitudinal acceleration
  sett_bn$priors$A_S <- priors_acc_lon_max
  sett_bn$priors$A_S <- array(sett_bn$priors$A_S)
  dim(sett_bn$priors$A_S) <- 
    c(sett_bn$states_n$A, 
      sett_bn$states_n$stress,
      sett_bn$states_n$S)
  dimnames(sett_bn$priors$A_S) <- sett_bn$state_names$A
  
  ## Priors: Observations
  temp_n <- 
    prod(
      c(length_intent, 
        length_speed, 
        length_acc_lon_max,
        2 # Observations
      ))
  sett_bn$priors$O <- array(rep(0.5, temp_n))
  dim(sett_bn$priors$O) <- 
    c(2, ## Ovservations 
      length_intent, 
      length_speed, 
      length_acc_lon_max)
  dimnames(sett_bn$priors$O) <- 
    list(O = sett_bn$state_names$O,
         I = sett_bn$state_names$I,
         S = sett_bn$state_names$S,
         A = sett_bn$state_names$A)
  
  ## ----------------------------------------------------------------------
  
  ## Reorder P_O_Hi
  ## After P_O_Hi values has been computed ...
  ## ... values have to be re-collected in following order:
  ## For each k and l: Collect values for Intention 1-4
  sett_bn$id_order <-
    c(c(1, 10), c(19, 28),
      c(4, 13), c(22, 31),
      c(7, 16), c(25, 34),
      c(2, 11), c(20, 29),
      c(5, 14), c(23, 32),
      c(8, 17), c(26, 35),
      c(3, 12), c(21, 30),
      c(6, 15), c(24, 33),
      c(9, 18), c(27, 36))
  
  return(sett_bn)
}

