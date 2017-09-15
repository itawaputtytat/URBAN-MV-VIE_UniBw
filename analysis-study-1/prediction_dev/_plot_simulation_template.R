
## Avoid additional space to axis
par(xaxs = "i", yaxs = "i")

## Empty plot
plot(x = 0, 
     y = 0, 
     xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), 
     ylim = c(sett_vis$sim$ymin, sett_vis$sim$ymax), 
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
#cols <- rep("grey85", 3)
cols <- c("#ED212450", "#6ABD4550", "#3953A450")
for(i in 1:length(sett_sim$v_ms.max)) {
  
  ## Draw maximum u
  abline(h = rev(sett_sim$v_ms.max)[i], 
         col = cols[i], 
         lwd = 10 * sett_bn$prior$V[clustcentres_order$order[i]], 
         #lty = "solid",
         lty = "dashed")
}

## Draw DSM
## Line width depends in model probability
lines(dat_dsm$dist, 
      dat_dsm$k1, 
      col = "#ED212450", 
      lwd = 10 * sett_bn$prior$V[1])
lines(dat_dsm$dist, 
      dat_dsm$k2, 
      col = "#6ABD4550", 
      lwd = 10 * sett_bn$prior$V[2])
lines(dat_dsm$dist, 
      dat_dsm$k3, 
      col = "#3953A450",
      lwd = 10 * sett_bn$prior$V[3])

## Draw lines for object positions
abline(v = sett_sim$objpos[2], col = "orange", lty = "dotted")
abline(v = sett_sim$objpos[4], col = "#B9539F", lty = "dotted")

## Passing
lines(x = dat_test[, sett_dat$col_name_am], 
      y = dat_test[, sett_dat$col_name_speed],
      col = "#3953A4", lwd = 2)

## Legend
legend(x = sett_vis$sim$xmin + 1.6, 
       y = 0.5,
       yjust = 0,
#legend(x = -49, y = 5.35, # When showing no title
       c("Go straight", 
         "Stop at stop line", 
         "Turn", 
         "Turn but stop"),
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

legend(x = sett_vis$sim$xmin + 1.6 + 22.7, y = 0.5,
       yjust = 0,
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

#replayPlot(plot_template4sim)

