
# Visualise simulated speed profiles --------------------------------------
dev.set(3)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-simulation-call.R")



# Visualise current intent probabilites -----------------------------------
dev.set(4)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-current-prob-call.R")



# Visualise history of intent probabilities -------------------------------
dev.set(5)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-history-prob-call.R")





# Simulation values -------------------------------------------------------

dev.set(6)
# replayPlot(plot_template4simvalues)
#
# invisible(
#   lapply(seq_along(dat4sim), function(x)  {
#     current_dat4sim <- dat4sim[x]
#     name <- names(dat4sim[x])
#     tails <- lapply(current_dat4sim, function(y) {
#       speed <- tail(y$speed_ms, 1)
#       dist <- tail(y$dist_m, 1)
#       if ( is.null(speed) ) {
#         speed = ""
#         dist = ""
#       } else {
#         speed = round(speed, 1)
#         dist = round(dist, 1)
#       }
#
#       text(x = 0.37,
#            y = 1 - x/(length(dat4sim) + 1),
#            paste(speed),
#            col = "black", adj = 0)
#
#       text(x = 0.67,
#            y = 1 - x/(length(dat4sim) + 1),
#            paste(dist),
#            col = "black", adj = 0)
#     })
#
#   })
# )
## Draw plot for speed values
barplot(height = unlist(temp2$speed_ms),
        names.arg = temp2$.id,
        xaxt = "n",
        #yaxt = "n",
        ylim = c(0, 20),
        col = c(rep("#6FCDDD", 9), rep("orange", 9), rep("#ED2125", 9), rep("#B9539F", 9)),
        ylab = "Speed (m/s)")
#font.lab = 2)
#axis(side = 2)

## Left-align title
title("Simulated speed",
      adj = 0)

dev.set(7)
## Draw plot for distance values
barplot(height = abs(pos4carryout - unlist(temp2$dist_m)),
        names.arg = temp2$.id,
        xaxt = "n",
        #yaxt = "n",
        #ylim = c(-4, 4),
        ylim = c(0, 5),
        col = c(rep("#6FCDDD", 9), rep("orange", 9), rep("#ED2125", 9), rep("#B9539F", 9)),
        ylab = "Distance (m)")
#font.lab = 2)
#axis(side = 2)

## Left-align title
title("Simulated driven distance",
      adj = 0)


## Dummy window to avoid flickering
dev.set(2)