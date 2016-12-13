profvis({
# Prepare data for simulation ---------------------------------------------

set4sim_temp <- predLiebner_getStartVal4Sim(dat4test, set4dat, set4sim)



# Start timer -------------------------------------------------------------

outputSectionTitle("Start of prediction")
outputString(paste("* Approach:", set4algo$hypscore))
outputString(paste("* Carried out at:", set4sim$pos4carryout, "m"))
outputString(paste("* Time lag:", set4sim$timelag_s, "s"))

ptm <- proc.time()



# Simulate trajectories ---------------------------------------------------

dat4sim <- 
  predLiebner_modelDrivBehav_batch("simulation-based",
                                   set4sim,
                                   set4dat,
                                   dat4sim,
                                   dat4dvm)



# Compute P(O|Hi) ---------------------------------------------------------

dat4prob <- predLiebner_compProb_O_Hi(set4sim, dat4sim, P_O_Hi)


# Re-order P(O|Hi) --------------------------------------------------------

# dat4prob_temp <- data.frame(dat4prob[set4bn$idorder],
#                             1 - dat4prob[set4bn$idorder])
# dat4prob_temp <- as.vector(t(dat4prob_temp))
# dat4prob_temp <- data.frame(as.vector(dat4prob), 1 - as.vector(dat4prob))
# dat4prob_temp <- as.vector(t(dat4prob_temp))
dat4prob_temp <- as.vector(t(dat4prob))
dat4prob_temp <- rbind(dat4prob_temp, 1 - dat4prob_temp)


# Set evidence and query results ------------------------------------------

## Prepare CPT for node O
set4bn$O <-
  array(dat4prob_temp,
        dim = c(2, 4, 3, 3),
        dimnames = list(O = set4bn$states$O,
                        I = set4bn$states$I,
                        V = set4bn$states$V,
                        A = set4bn$states$A))

bn <- predLiebner_updateBN(bn, set4bn$O, set4bn)
bn.evidence <- setEvidence(bn, nodes = c("O"), states = c("dat4prob$obs"))
results <- querygrain(bn.evidence, nodes = "I")



# Stop timer --------------------------------------------------------------

outputProcTime(ptm)



# Output intent probabilities ---------------------------------------------
cat("\n")
results <- as.data.frame(results) * 100
print(round(results, 2))
#barplot( t(results), ylim = c(0, 100) )
})