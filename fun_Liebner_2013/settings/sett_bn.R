
sett_bn <- c()

# Level indizes for nodes -------------------------------------------------

sett_bn$states$I <- paste("Intent", 1:4, sep = "")
sett_bn$states$V <- paste("k", 1:length(sett_sim$v_ms.max), sep = "")
sett_bn$states$A <- paste("l", 1:length(sett_sim$acc_lon_ms2.max), sep = "")
sett_bn$states$O <- c("dat_prob$obs", "nodat_prob$obs")



# Prior probabilities -----------------------------------------------------

## Intents
#sett_bn$prior$I <- c(0.25, 0.25, 0.25, 0.25)
sett_bn$prior$I <- rep(1 / sum(sett_sim$computeI), sum(sett_sim$computeI))

## Special cases for Prior: In case of already passed obstacles
if(sett_proc$carryout_am1 >= sett_sim$objpos[2]) {
  sett_bn$prior$I <- c(1/3, 0, 1/3, 1/3)
  sett_bn$prior$I <- array(sett_bn$prior$I, 
                          dim = 4, 
                          dimnames = list(sett_bn$states$I))
}

if(sett_proc$carryout_am1 >= sett_sim$objpos[4]) {
  sett_bn$prior$I <- c(1/2, 0, 1/2, 0)
  sett_bn$prior$I <- array(sett_bn$prior$I, 
                          dim = 4, 
                          dimnames = list(sett_bn$states$I))
}


## P(M)
## if set to "auto", P(M) will be computed from empirical data
#sett_bn$prior$V <- c(38/94, 23/94, 33/94)
#sett_bn$prior$V <- c(0.350, 0.475, 0.175)
#sett_bn$prior$V <- c(75/94, 11/94, 8/94)
#sett_bn$prior$V <- c(85/94, 4/94, 5/94)
#sett_bn$prior$V <- "auto"
## Assuming same probability for maximum u models as for DVM
sett_bn$prior$V <- prob_Mk
## Assuming uniform probability for maximum u models compared to DVM
#sett_bn$prior$V_I <- c(rep(1/3, 3), rep(1/3, 3), prob_Mk, prob_Mk)


## P(a|M)
## if set to "auto", P(a|M) will be computed from empirical data
# sett_bn$prior$V_A <-
#   c(c(0.3, 0.65, 0.05),
#     c(0.5, 0.4, 0.1),
#     c(0.15, 0.6, 0.25))
#sett_bn$prior$V_A <- "auto"
sett_bn$prior$A_V <- prob_al_Mk


## Prepare CPT for node O
sett_bn$prior$O <-
  array(#dat4prob$P_O_Hi_temp,
    rep(0.5, 36),
    dim = c(2, 4, 3, 3),
    dimnames = list(O = sett_bn$states$O,
                    I = sett_bn$states$I,
                    V = sett_bn$states$V,
                    A = sett_bn$states$A))



# Misc --------------------------------------------------------------------

## After P_O_Hi values has been computed ...
## ... values have to be re-collected in following order:
## For each k and l: Collect values for Intention 1-4
sett_bn$idorder <-
  c(c(1, 10), c(19, 28),
    c(4, 13), c(22, 31),
    c(7, 16), c(25, 34),
    c(2, 11), c(20, 29),
    c(5, 14), c(23, 32),
    c(8, 17), c(26, 35),
    c(3, 12), c(21, 30),
    c(6, 15), c(24, 33),
    c(9, 18), c(27, 36))



# Initialise probability collector ----------------------------------------

P_O_Hi <- idm_createSimDat(
  list(j = length(sett_bn$states$I),
       k = length(sett_bn$states$V), 
       l = length(sett_bn$states$A)), "", prefix = "")

P_O_Hi <- P_O_Hi[sett_bn$idorder]

bn <- predLiebner_initBN("V1", sett_bn)