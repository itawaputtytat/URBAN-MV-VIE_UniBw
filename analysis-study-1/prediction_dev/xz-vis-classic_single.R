
# Visualise simulated speed profiles --------------------------------------
dev.set(2)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-simulation-call.R")



# Visualise current intent probabilites -----------------------------------
dev.set(3)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-current-prob-call.R")



# Visualise history of intent probabilities -------------------------------
# source("analysis-study-1/prediction_dev/zz-vis-classic-template-history-prob-call.R")





# Simulation values -------------------------------------------------------

# dev.set(5)
# # replayPlot(plot_template4simvalues)
# # 
# # invisible(
# #   lapply(seq_along(dat4sim), function(x)  {
# #     current_dat4sim <- dat4sim[x]
# #     name <- names(dat4sim[x])
# #     tails <- lapply(current_dat4sim, function(y) {
# #       speed <- tail(y$speed_ms, 1)
# #       dist <- tail(y$dist_m, 1)
# #       if ( is.null(speed) ) {
# #         speed = "-"
# #         dist = "-"
# #       } else {
# #         speed = round(speed, 1)
# #         dist = round(dist, 1)
# #       } 
# #       
# #       text(x = 0.02, 
# #            y = 1 - x/(length(dat4sim) + 1), 
# #            paste(name),
# #            col = "black", adj = 0)
# #       
# #       text(x = 0.25, 
# #            y = 1 - x/(length(dat4sim) + 1), 
# #            paste("speed: ", speed),
# #            col = "black", adj = 0)
# #       
# #       text(x = 0.55, 
# #            y = 1 - x/(length(dat4sim) + 1), 
# #            paste("dist: ", dist),
# #            col = "black", adj = 0)
# #     })
# #     
# #   })
# # )
