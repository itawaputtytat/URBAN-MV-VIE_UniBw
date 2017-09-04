## Avoid additional space to axis
par(xaxs = "i", yaxs = "i")

## Empty plot
plot(x = 0, 
     y = 0, 
     xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), 
     ylim = c(set4vis$sim$ymin, set4vis$sim$ymax), 
     col = "white",
     xlab = "Distance-to-intersection (m)",
     ylab = "Speed (m/s)",
     font.lab = 2)

## Left-align title
title("Simulated driving behaviour corresponding to Hypothesis Hi",
      adj = 0)
#grid(lwd = 2) # grid only in y-direction

## Draw horizintal lines for set of maximum u (deprecated)
#cols <- c("#ED212450", "#6ABD4550", "#3953A450")

## Draw maximum u
## Line width depends in model probability
cols <- rep("grey85", 3)
for(i in 1:length(set4sim$v_ms.max)) {
  
  ## Draw maximum u
  abline(h = rev(set4sim$v_ms.max)[i], 
         col = cols[i], 
         lwd = 10 * set4bn$prior$V[clustcentres_order$order[i]], 
         lty = "solid")
}

## Draw DSM
## Line width depends in model probability
lines(dat4dsm$dist, 
      dat4dsm$k1, 
      col = "#ED212450", 
      lwd = 10 * set4bn$prior$V[1])
lines(dat4dsm$dist, 
      dat4dsm$k2, 
      col = "#6ABD4550", 
      lwd = 10 * set4bn$prior$V[2])
lines(dat4dsm$dist, 
      dat4dsm$k3, 
      col = "#3953A450",
      lwd = 10 * set4bn$prior$V[3])

## Draw lines for object positions
abline(v = set4sim$objpos[2], col = "orange")
abline(v = set4sim$objpos[4], col = "#B9539F")

## Passing
lines(x = dat4test[, set4dat$varname4pxx_dist_m], 
      y = dat4test[, set4dat$varname4speed_ms],
      col = "#3953A4", lwd = 2)

## Legend
legend(x = -49, y = 6.35,
#legend(x = -49, y = 5.35, # When showing no title
       c("Go straight", 
         "Stop at stop line", 
         "Turn right", 
         "Turn right but stop"),
       title = expression(bold("Intent simulation")),
       title.adj = 0.1,
       lty = c("solid", 
               "solid", 
               "solid", 
               "solid"),
       col = c("#6FCDDD", 
               "orange", 
               "#ED2125", 
               "#B9539F"),
       bg = "grey92")

legend(x = -31, y = 6.35,
#legend(x = -31, y = 5.35, # When showing no title
       title = expression(bold("Data")),
       title.adj = 0.1,
       c("Passing", 
         "M1", 
         "M2", 
         "M3"),
       lty = c("solid", 
               "solid", 
               "solid", 
               "solid"),
       col = c("#3953A4", 
               "#ED212475", 
               "#6ABD4575", 
               "#3953A475"),
       bg = "grey92")

## Save plot to template
plot_template4sim <- recordPlot()
replayPlot(plot_template4sim)