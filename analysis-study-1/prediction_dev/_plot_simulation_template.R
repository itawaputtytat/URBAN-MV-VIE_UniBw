
## Avoid additional space to axis
par(xaxs = "i", yaxs = "i")

## Empty plot
plot(x = 0, 
     y = 0, 
     xlim = c(sett_plot$xmin, sett_plot$xmax), 
     ylim = c(sett_plot$ymin, sett_plot$ymax), 
     col = "white",
     xlab = "Distance-to-intersection (m)",
     ylab = "Speed (m/s)",
     font.lab = 2)

## Left-align title
title("Simulated driving behaviour corresponding to Hypothesis Hi",
      adj = 0)

## Maximum u and DSM
## Line width depends in model probability
for(i in 1:length(sett_dsm$thresholds$u_max)) {
  
  ## Maximum u
  abline(h = rev(sett_dsm$thresholds$u_max)[i], 
         col = sett_plot$colors$dsm[i], 
         lwd = 10 * sett_bn$prior$S[dat_cluster_center_order$cluster_group_ordered[i]], 
         #lty = "solid",
         lty = "dashed")
  
  ## DSM
  lines(dat_dsm_spread$am, 
        dat_dsm_spread[, paste0("k", i)], 
        col = sett_plot$colors$dsm[i], 
        lwd = 10 * sett_bn$prior$S[i])
}

## Draw lines for object positions
abline(v = sett_sim$objpos[2], col = "orange", lty = "dotted")
abline(v = sett_sim$objpos[4], col = "#B9539F", lty = "dotted")

## Individual Passing
lines(x = dat_test[, sett_dat$col_names$am], 
      y = dat_test[, sett_dat$col_names$speed],
      col = "#3953A4", lwd = 2)

## Legend
legend(x = sett_plot$xmin + 1.6, 
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

legend(x = sett_plot$xmin + 1.6 + 22.7, y = 0.5,
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

