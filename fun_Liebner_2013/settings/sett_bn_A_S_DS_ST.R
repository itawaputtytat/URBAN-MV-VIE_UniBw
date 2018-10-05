
# Initialise settings for BN ----------------------------------------------

## Abbreviations
## I: Intentions
## S: Speeds
## A: Accelerations
## O: Observations

predLiebner_initSettingsForBN_A_S_DS_ST <- function(
  state_names_intent_prefix = "Intent",
  state_names_speed_prefix = "k",
  state_names_acc_lon_prefix = "l",
  state_names_ST = c("stress", "no_stress"),
  state_names_DS = c("sporty", "moderate", "comfortable"),
  state_names_obs_prefix = c("dat_prob", "no_dat_prob"),
  length_intent = 4,
  length_speed = 3,
  length_acc_lon_max = 3,
  length_ST = 2,
  length_DS = 3,
  priors_intent = rep(1 / length_intent, length_intent),
  priors_speed,
  priors_acc_lon_max,
  priors_ST = rep(1 / length_ST, length_ST),
  priors_DS = rep(1 / length_DS, length_DS)) {
  
  ## Initialise settings
  sett_bn <- c()
  
  
  
  ## ----------------------------------------------------------------------
  
  ## State names
  sett_bn$state_names$ST <- state_names_ST
  sett_bn$state_names$DS <- state_names_DS
  ##
  sett_bn$state_names$I <- paste0("Intent", seq(length_intent))
  sett_bn$state_names$S <- list()
  sett_bn$state_names$S[["S"]] <- paste0("k", seq(length_speed))
  sett_bn$state_names$S[["ST"]] <- sett_bn$state_names$ST
  sett_bn$state_names$S[["DS"]] <- sett_bn$state_names$DS
  sett_bn$state_names$A <- list()
  sett_bn$state_names$A[["A"]] <- paste0("l", seq(length_acc_lon_max))
  sett_bn$state_names$A[["ST"]] <- sett_bn$state_names$ST
  sett_bn$state_names$A[["DS"]] <- sett_bn$state_names$DS
  sett_bn$state_names$A[["S"]] <- paste0("k", seq(length_speed)) #sett_bn$state_names$S
  sett_bn$state_names$O <- paste0(c("dat_prob", "no_dat_prob"), "$obs")
  
  
  
  ## ----------------------------------------------------------------------
  
  # Number of states per node
  sett_bn$states_n$ST <- length_ST
  sett_bn$states_n$DS <- length_DS
  ##
  sett_bn$states_n$I <- length_intent
  sett_bn$states_n$S <- length_speed
  sett_bn$states_n$A <- length_acc_lon_max
  sett_bn$states_n$O <- length(sett_bn$state_names$O)
  
  
  
  ## ----------------------------------------------------------------------
  
  ## Priors: ST
  sett_bn$priors$ST <- priors_ST
  sett_bn$priors$ST <- array(sett_bn$priors$ST)
  dimnames(sett_bn$priors$ST) <- list(sett_bn$state_names$ST)
  
  ## Priors: Driving style
  sett_bn$priors$DS <- priors_DS
  sett_bn$priors$DS <- array(sett_bn$priors$DS)
  dimnames(sett_bn$priors$DS) <- list(sett_bn$state_names$DS)
  
  ## Priors: Intents
  sett_bn$priors$I <- priors_intent
  sett_bn$priors$I <- array(sett_bn$priors$I)
  dimnames(sett_bn$priors$I) <- list(sett_bn$state_names$I)
  
  ## Priors: Speed
  sett_bn$priors$S <- priors_speed
  sett_bn$priors$S <- array(sett_bn$priors$S)
  dim(sett_bn$priors$S) <- 
    c(sett_bn$states_n$S, 
      sett_bn$states_n$ST,
      sett_bn$states_n$DS)
  dimnames(sett_bn$priors$S) <- sett_bn$state_names$S
  
  ## Priors: Maximum longitudinal acceleration
  sett_bn$priors$A_S <- priors_acc_lon_max
  sett_bn$priors$A_S <- array(sett_bn$priors$A_S)
  dim(sett_bn$priors$A_S) <- 
    c(sett_bn$states_n$A, 
      sett_bn$states_n$ST,
      sett_bn$states_n$DS,
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
    c(2, ## Observations 
      length_intent, 
      length_speed, 
      length_acc_lon_max)
  dimnames(sett_bn$priors$O) <-
    list(O = sett_bn$state_names$O,
         I = sett_bn$state_names$I,
         S = sett_bn$state_names$S$S,
         A = sett_bn$state_names$A$A)

  
  ## ----------------------------------------------------------------------
  
  ## Reorder P_O_Hi
  ## After P_O_Hi values has been computed ...
  ## ... values have to be re-collected in following order:
  ## For each k and l: Collect values for Intention 1-4
  sett_bn$id_order <-
    deriveOrderOfHypotheses(sett_bn$states_n$I,
                            sett_bn$states_n$S,
                            sett_bn$states_n$A)
    # c(c(1, 10), c(19, 28),
    #   c(4, 13), c(22, 31),
    #   c(7, 16), c(25, 34),
    #   c(2, 11), c(20, 29),
    #   c(5, 14), c(23, 32),
    #   c(8, 17), c(26, 35),
    #   c(3, 12), c(21, 30),
    #   c(6, 15), c(24, 33),
    #   c(9, 18), c(27, 36))
  
  return(sett_bn)
}

