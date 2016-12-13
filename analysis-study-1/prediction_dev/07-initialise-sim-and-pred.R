

# Initialise settings -----------------------------------------------------

source("fun_Liebner_2013/settings/set4sim.R")
set4sim$v_ms.max <- c(48/3.6, 54/3.6, 60/3.6)
#set4sim$objpos <- c(0, -0.1, 0, 7)
#source("fun_Liebner_2013/settings/set4idm.R")
source("fun_Liebner_2013/settings/set4algo.R")

source("fun_Liebner_2013/settings/set4bn.R")



# Initialise bayesian network ---------------------------------------------

## Initialise probability collector
P_O_Hi <- idm_createSimDat(
            list(j = length(set4bn$states$I),
                 k = length(set4bn$states$V), 
                 l = length(set4bn$states$A)), "", prefix = "")

P_O_Hi <- P_O_Hi[set4bn$idorder]

bn <- predLiebner_initBN("V1", set4bn)



# Prepare data for simulation ---------------------------------------------

set4sim_temp <- predLiebner_getStartVal4Sim(dat4test, set4dat, set4sim)



# # LOOP OPERATOR -----------------------------------------------------------
# 
 set4sim$pos4carryout <- 0
# coll <- c()
# for(s in seq(-25,10.1,0.1)) {
#   print(s)
#   set4sim$pos4carryout <- round(s, 1)
#   source("analysis-study-1/prediction_dev/xx-pred.R")
#   coll <- rbind(coll, cbind(s, t(results)))
# }
# 
# coll <- as.data.frame(coll)
# rownames(coll) <- NULL
# plot(x = coll$s, y = coll$Intent1, type = "l", col = "blue", ylim = c(0,100))
# lines(x = coll$s, coll$Intent2, col = "orange")
# lines(x = coll$s, coll$Intent3, col = "red")
# lines(x = coll$s, coll$Intent4, col = "magenta")
# 
# 
