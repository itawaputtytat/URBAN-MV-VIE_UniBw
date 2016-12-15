
# Preparatory settings ----------------------------------------------------

## Load test data
source("fun_Liebner_2013/settings/set4dat.R")
set4dat$passing <- "s07_intro_subject03"
source("analysis-study-1/prediction_dev/06-load-test-data.R")

## Initialise Algorithm, DVM and BN
source("fun_Liebner_2013/settings/set4sim.R")
set4sim$objpos <- c(0, -0.1, 0, 7)

source("fun_Liebner_2013/settings/set4bn.R")
source("fun_Liebner_2013/settings/set4dvm.R")
set4dvm$objname4dvm <- "dat4dvm.spread_v2"
source("analysis-study-1/prediction_dev/07-initialise-sim-dvm-pred.R")

source("fun_Liebner_2013/settings/set4algo.R")
source("fun_Liebner_2013/settings/set4idm.R")

set4sim$objpos[2] <- -10
set4sim$objpos[4] <- -10
set4idm$d0 <- 2
set4idm$d0 <- 0

## Create visualisation template
# graphics::layout(matrix(c(1,1,1,2), nrow = 1, byrow = T))
# par(xaxs = "i", yaxs = "i")
# plot(dat4test$sxx_dist_m_rnd1, dat4test$speed_ms, 
#      type = "l", col = "#3953A4", lwd = 2,
#      xlim = c(-50, 25), ylim = c(0, 30))
# title("Simulated driving behaviour corresponding to Hypothesis Hi",
#       adj = 0)
# #grid(lwd = 2) # grid only in y-direction
# 
# lines(dat4dvm$dist, dat4dvm$k1, col = "grey70")
# lines(dat4dvm$dist, dat4dvm$k2, col = "grey70")
# lines(dat4dvm$dist, dat4dvm$k3, col = "grey70")
# abline(v = set4sim$objpos[2], col = "orange")
# abline(v = set4sim$objpos[4], col = "#B9539F")
#x <- recordPlot()



# Run prediction for single position --------------------------------------

set4sim$pos4carryout <- -3
pos4carryout <- set4sim$pos4carryout
set4sim_temp <- predLiebner_getStartVal4Sim(dat4test, set4dat, pos4carryout)
set4sim_temp$time_s_diff <- rep(0.01, 1 / 0.01)
# set4sim_temp$time_s_diff <- rep(0.01, 2 / 0.01)
# set4sim_temp$time_s_diff <- rep(0.01, 5 / 0.01)

source("analysis-study-1/prediction_dev/xx-pred.R")
results4output <- data.frame(P_O_Hi = results)
print(round(results4output * 100, 2))
#source("analysis-study-1/prediction_dev/xz-vis.R")
source("analysis-study-1/prediction_dev/xz-vis-classic.R")



# Run prediction for complete passing -------------------------------------

## Initialise collector for results
coll4results <- c()

dummy <- 
  data.frame(dist_m = -40, 
             speed_ms = dat4test$speed_ms[which(dat4test$sxx_dist_m_rnd1 == -40)])
dummy_names <- names(idm_createSimDat(c(j = 4, k = 3, l = 3), varnames = "", prefix = ""))
coll4dat4sim <- vector("list", length(dummy_names))
names(coll4dat4sim) <- dummy_names
coll4dat4sim <- lapply(coll4dat4sim, function(x)
  x <- dummy)

## Loop through passing
ptm2 <- proc.time()
# for(s in seq(-40,20,0.1)) {
for(s in seq(-40,20,1)) {
#for(s in seq(-10,20,1)) {
  pos4carryout <- round(s, 1)
  print(pos4carryout)
  ## Prepare data for simulation
  set4sim_temp <- predLiebner_getStartVal4Sim(dat4test, set4dat, pos4carryout)
  #print(set4sim_temp$dist1)
  set4sim_temp$time_s_diff <- rep(0.01, 1 / 0.01)
  source("analysis-study-1/prediction_dev/xx-pred.R")
  source("analysis-study-1/prediction_dev/xz-vis-classic.R")
  
  temp <- lapply(dat4sim, function(x) tail(x, 1))
  
  for(i in seq_along(temp)) {
    finder <- which(names(coll4dat4sim) == names(temp[i]))
    coll4dat4sim[[finder]] <- rbind(coll4dat4sim[[finder]], temp[[i]])
  }
  
  rm(dat4sim)
  gc()
  
  ## Collect results
  coll4results <- 
    data.table::rbindlist(list(coll4results, 
                               data.frame(s, t(results))))
}
outputProcTime(ptm2)

## Print result collection
par(mfrow=c(1,1))
coll4results <- as.data.frame(coll4results)
rownames(coll4results) <- NULL
plot(x = coll4results$s, y = coll4results$Intent1, type = "l", col = "blue", ylim = c(0,1))
lines(x = coll4results$s, coll4results$Intent2, col = "orange")
lines(x = coll4results$s, coll4results$Intent3, col = "red")
lines(x = coll4results$s, coll4results$Intent4, col = "magenta")




# Playground --------------------------------------------------------------

coll4dat4sim <- lapply(coll4dat4sim, function(x){
  x <- x[-1, ]
})

#replayPlot(x)

par(mfrow = c(1,1))
par(xaxs = "i", yaxs = "i")
plot(dat4test$sxx_dist_m_rnd1, dat4test$speed_ms, 
     type = "l", col = "#3953A4", lwd = 2,
     xlim = c(-50, 25), ylim = c(0, 30))
title("Simulated driving behaviour corresponding to Hypothesis Hi",
      adj = 0)
#grid(lwd = 2) # grid only in y-direction

lines(dat4dvm$dist, dat4dvm$k1, col = "grey70")
lines(dat4dvm$dist, dat4dvm$k2, col = "grey70")
lines(dat4dvm$dist, dat4dvm$k3, col = "grey70")
abline(v = set4sim$objpos[2], col = "#FFA500")
abline(v = set4sim$objpos[4], col = "#B9539F")

invisible(lapply(seq_along(coll4dat4sim), function(x) {
  name <- names(coll4dat4sim)[[x]]
  #print(name)
  if (grepl("j1", name)) 
  { col <- "#6FCDDD50"; shape = 17 } # cyan
  if (grepl("j2", name)) 
  { col <- "#FFA50050"; shape = 18 } # orange
  if (grepl("j3", name)) 
  { col <- "#ED212550"; shape = 16 } # red
  if (grepl("j4", name)) 
  { col <- "#B9539F50"; shape = 15 } # magenta
  
  lines(coll4dat4sim[[x]]$dist_m, y = coll4dat4sim[[x]]$speed_ms,
        col = col, lwd = 1)
  # points(coll4dat4sim[[x]]$dist_m, y = coll4dat4sim[[x]]$speed_ms,
  #       col = col, cex = 0.5, pch = shape)
}))
