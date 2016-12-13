#set4sim$pos4carryout <- -12



# Start timer -------------------------------------------------------------

# outputSectionTitle("Start of prediction")
# outputString(paste("* Approach:", set4algo$hypscore))
# outputString(paste("* Simulation carried out at:", set4sim$pos4carryout, "m"))
# outputString(paste("* Time lag:", set4sim$timelag_s, "s"))
ptm <- proc.time()


# Simulate trajectories ---------------------------------------------------

dat4sim <- 
  predLiebner_modelDrivBehav_batch("simulation-based",
                                   set4sim,
                                   set4dat,
                                   dat4sim,
                                   dat4dvm)


# plot(dat4sim[[1]], type = "l", xlim = c(-50, 25), ylim = c(0, 20))
# invisible(lapply(dat4sim[2:length(dat4sim)], function(x) lines(x, col = "red")))

# Compute P(O|Hi) ---------------------------------------------------------

## Complete list of column names on data (will be used several times)
#cols <- colnames(dat4sim)

## Initialise collector for posterior probabilities
#dat4prob <- c() 

## Initialise collector for columns relating to 36 hypothesis values
#coll4names <- c() 

## Counter for necessary runs
#i <- 0 

## Likelihood of current observation
## ... given the longitudinal behavior of the driver could be modeled
## ... by the parameter set associated with hypothesis Hi
## Initialise collector
# dat4prob$P_O_Hi_temp <- c() ## Receives 36 values

#predLiebner_compProb_O_Hi <- ()
#source("fun_Liebner_2013/predLiebner_compProb_O_Hi.R")
dat4prob <- predLiebner_compProb_O_Hi(set4sim, dat4sim, P_O_Hi)


# Re-order P(O|Hi) --------------------------------------------------------

# for(i in set4bn$idorder) {
#   dat4prob$P_O_Hi_temp <-
#     c(dat4prob$P_O_Hi_temp,
#       dat4prob$P_O_Hi[i], 1 - dat4prob$P_O_Hi[i])
# }


dat4prob_temp <-
  as.vector(t(data.frame(dat4prob[set4bn$idorder],
                         1 - dat4prob[set4bn$idorder])))

## Prepare CPT for node O
set4bn$O <-
  array(dat4prob_temp,
        dim = c(2, 4, 3, 3),
        dimnames = list(O = set4bn$states$O,
                        I = set4bn$states$I,
                        V = set4bn$states$V,
                        A = set4bn$states$A))


# Set evidence and query results ------------------------------------------

bn <- predLiebner_updateBN(bn, set4bn$O, set4bn)
bn.evidence <- setEvidence(bn, nodes = c("O"), states = c("dat4prob$obs"))
results <- querygrain(bn.evidence, nodes = "I")

# Stop timer --------------------------------------------------------------

outputProcTime(ptm)



# Output intent probabilities ---------------------------------------------
results <- as.data.frame(results) * 100
print(round(results, 2))
#barplot( t(results), ylim = c(0, 100) )
