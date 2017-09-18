
# Heyho -------------------------------------------------------------------



ptm99 <- proc.time()

source("analysis-study-1/prediction_dev/01_retrieve-data.R")
source("analysis-study-1/prediction_dev/02_preprocess-data.R")
source("analysis-study-1/prediction_dev/03_feature-enrichment.R")
source("analysis-study-1/prediction_dev/04_obtain-dsm-v1-1-synthesise.R")
source("analysis-study-1/prediction_dev/04_obtain-dsm-v1-2-cluster.R")
source("analysis-study-1/prediction_dev/05_adjust-dsm-to-u-after-turning.R")
source("analysis-study-1/prediction_dev/06_extract-v-and-a-models.R")



# cdfsdf ------------------------------------------------------------------



## Load test data
source("fun_Liebner_2013/settings/set4dat.R")
set4dat$passing <- "p15_intro_s03"
source("analysis-study-1/prediction_dev/06-load-test-data.R")

## Initialise Algorithm, dsm and BN
source("fun_Liebner_2013/settings/set4sim.R")
#set4sim$objpos <- c(0, -0.1, 0, 7)

source("fun_Liebner_2013/settings/set4bn.R")
#set4bn$prior$V <- c(0.01, 0.98, 0.01)
source("fun_Liebner_2013/settings/set4dsm.R")
set4dsm$objname4dsm <- "dat4dsm.spread"
#set4dsm$objname4dsm <- "dat4dsm.spread_v2"
source("analysis-study-1/prediction_dev/07-initialise-sim-dsm-pred.R")

source("fun_Liebner_2013/settings/set4algo.R")
source("fun_Liebner_2013/settings/set4idm.R")

set4sim$objpos[2] <- -10
set4sim$objpos[4] <- -15
#set4idm$d0 <- 2
set4idm$d0 <- 0

set4vis <- c()
set4vis$sim$xmin <- -50
set4vis$sim$xmax <- 25
set4vis$sim$ymin <- 0
set4vis$sim$ymax <- 20

## Template for visualisation
source("analysis-study-1/prediction_dev/zz-vis-classic-template_single.R")



source("analysis-study-1/prediction_dev/zz-vis-classic-template_complete.R")

dist2start <- -35
dist2end <- 20
distint <- 1 # 0.1 # 0.5

## Initialise collector for results
#coll4results <- c()
results <- set4bn$prior$I
names(results) <- set4bn$states$I
coll4results <- data.frame(pos4carryout = dist2start, t(results))

dummy <-
  data.frame(dist_m = dist2start,
             speed_ms = dat4test$speed_ms[which(dat4test[, set4dat$varname4pxx_dist_m] == -40)])
dummy_names <- names(idm_createSimDat(c(j = 4, k = 3, l = 3), varnames = "", prefix = ""))
coll4dat4sim <- vector("list", length(dummy_names))
names(coll4dat4sim) <- dummy_names
coll4dat4sim <- lapply(coll4dat4sim, function(x)
  x <- dummy)
#coll4prior <- c()

## Loop through passing
ptm2 <- proc.time()


# test <- lapply(dat4sim, function(x) lapply(x, function(y){
#   tail(y, 1)
#   print(y)
# } ))

## Real-time
#s = 151; while (s < 721) {
#for(s in seq(-40,20,0.1)) {
#for(s in seq(-40,20,0.5)) {
for(s in seq(dist2start,dist2end,distint)) {
  
  pos4carryout <- round(s, 1)
  ## Real-time
  #pos4carryout <- round(dat4test$pxx_dist_m_rnd1[s], 1)
  
  #print(pos4carryout)
  ## Prepare data for simulation
  #rm(dat4sim)
  #gc()
  set4sim_temp <- predLiebner_getStartVal4Sim(dat4test, set4dat, pos4carryout)
  pos4carryout <- set4sim_temp$dist2
  #print(set4sim_temp$dist1)
  #print(time_s_diff)
  #time_s_diff <- set4sim_temp$time_s_diff
  set4sim_temp$time_s_diff <- rep(0.01, 1 / 0.01)
  source("analysis-study-1/prediction_dev/xx-pred.R")
  
  ## Collect results
  # coll4results <- 
  #   data.table::rbindlist(list(coll4results, 
  #                              data.frame(s, t(results))))
  coll4results <-
    data.table::rbindlist(list(coll4results,
                               data.frame(pos4carryout, t(results))))
  
  temp2 <- lapply(dat4sim, function(x) 
    lapply(x, function(y) 
      if (is.null(y)) NA else
        tail(y, 1)) )
  temp2 <- data.table::rbindlist(temp2, idcol = T, fill  = T)
  
  
  # Collect tails of simulates speed profiles (history / trace)
  temp <- lapply(dat4sim, function(x)
    lapply(x, function(y)
      tail(y, 1)) )
  temp <- as.data.frame(data.table::rbindlist(temp, idcol = T, fill  = T))
  for(i in seq_along(temp$.id)) {
    finder <- temp$.id[i]
    coll4dat4sim[[finder]] <- rbind(coll4dat4sim[[finder]], temp[i,2:3])
  }
  
  source("analysis-study-1/prediction_dev/xz-vis-classic_complete.R")
  
  # Adjust plotting speed
  #time_s_diff <- dat4test$time_s[s] - dat4test$time_s[s-7]
  #Sys.sleep(time_s_diff)
  # input <- 0
  # while(input == 0) {
  #   input <- readline("1 = forward | 2 = backward: >> ")
  #   if (input == 1) {
  #     input <- s - 7
  #     } else {
  #       if (input == 2) {
  #         input <- s + 7
  #       } else {
  #         input <- 0
  #       }
  #     }
  # }
  # s <- s + input
  # rm(input)
  #s <- s + 7
  
  
  #ani.record()
  #x <- recordPlot()
  # P_O_Hi <- idm_createSimDat(
  #   list(j = length(set4bn$states$I),
  #        k = length(set4bn$states$V),
  #        l = length(set4bn$states$A)), "", prefix = "")
  # P_O_Hi <- P_O_Hi[set4bn$idorder]
  # #weight <- (as.numeric(results) / set4bn$prior$I) * (as.numeric(results) - set4bn$prior$I)
  # #weight <- round(results, 2) - set4bn$prior$I#rep(0.25, 4) - as.numeric(results)
  # #print(weight)
  # #set4bn$prior$I <- set4bn$prior$I + weight * set4bn$prior$I
  # #print(sum(set4bn$prior$I))
  # weight <- results / set4bn$prior$I
  # weigth <- weight / sum(weight)
  # print(sum(weight))
  # set4bn$prior$I <- set4bn$prior$I + weight * (as.numeric(results) - set4bn$prior$I)
  # print(sum(set4bn$prior$I))
  # bn <- predLiebner_initBN("V1", set4bn)
  # coll4prior <- data.table::rbindlist(list(coll4prior, data.frame(s, t(set4bn$prior$I))))
  
  
}
outputProcTime(ptm2)

cat("-------------")
outputProcTime(ptm99)