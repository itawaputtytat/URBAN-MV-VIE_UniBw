dev.set(5)
# par(mar=c(0, 0, 0, 0),
#     xaxs = "i", yaxs = "i")
# plot(c(0, 1), 
#      c(0, 1), 
#      ann = F, 
#      bty = 'n', 
#      type = 'n', 
#      xaxt = 'n', 
#      yaxt = 'n')
# 
# rect(xleft = 0, xright = 1,
#      ybottom = 3 / 36 * 9 - 0.006, 1,
#      col = "#6FCDDD65", border = 0)
# rect(xleft = 0, xright = 1,
#      ybottom = 2 / 36 * 9 -0.002, 3 / 36 * 9 - 0.006,
#      col = "#FFA50065", border = 0)
# rect(xleft = 0, xright = 1,
#      ybottom = 1 / 36 * 9 + 0.005,  2 / 36 * 9 - 0.002,
#      col = "#ED212565", border = 0)
# rect(xleft = 0, xright = 1,
#      ybottom = 0,  1 / 36 * 9 + 0.005,
#      col = "#B9539F65", border = 0)
# 
# abline(h = 3 / 36 * 9 - 0.006)
# abline(h = 2 / 36 * 9 - 0.002)
# abline(h = 1 / 36 * 9 + 0.005)
# 
# invisible(
#   lapply(seq_along(dat4sim), function(x)  {
#   current_dat4sim <- dat4sim[x]
#   name <- names(dat4sim[x])
#   tails <- lapply(current_dat4sim, function(y) {
#     speed <- tail(y$speed_ms, 1)
#     dist <- tail(y$dist_m, 1)
#     if ( is.null(speed) ) {
#       speed = "-"
#       dist = "-"
#     } else {
#       speed = round(speed, 1)
#       dist = round(dist, 1)
#     } 
# 
#     text(x = 0.02, 
#          y = 1 - x/(length(dat4sim) + 1), 
#          paste(name),
#          col = "black", adj = 0)
#     
#     text(x = 0.25, 
#          y = 1 - x/(length(dat4sim) + 1), 
#          paste("speed: "),
#          col = "black", adj = 0)
#     
#     text(x = 0.55, 
#          y = 1 - x/(length(dat4sim) + 1), 
#          paste("dist: "),
#          col = "black", adj = 0)
#   })
#   
# })
# )


#par(xaxs = "r", yaxs = "r", mfrow = c(2,1))
par(xaxs = "i", yaxs = "i", mfrow = c(2,1))
## Draw plot for speed values
barplot(height = temp$speed_ms, 
        names.arg = temp$.id,
        font = 2,
        #yaxt = "n",
        ylim = c(0, 20),
        col = c(rep("#6FCDDD", 4), rep("orange", 4), rep("#ED2125", 4), rep("#B9539F", 4)),
        ylab = "Simulated speed (m/s)",
        font.lab = 2)
axis(side = 2)

## Left-align title
title("Simulated speed", 
      adj = 0)

## Draw plot for speed values
barplot(height = temp$dist_m - pos4carryout, 
        names.arg = temp$.id,
        font = 2,
        #yaxt = "n",
        ylim = c(0, 20),
        col = c(rep("#6FCDDD", 4), rep("orange", 4), rep("#ED2125", 4), rep("#B9539F", 4)),
        ylab = "Simulated distance (m)",
        font.lab = 2)
axis(side = 2)

## Left-align title
title("Simulated distance", 
      adj = 0)

plot_template4simvalues <- recordPlot()