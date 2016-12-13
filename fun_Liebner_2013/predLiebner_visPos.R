predLiebner_visPos <- function(plotdat_passing.dvm, set4sim, set4sim_temp, dat4sim, showplot) {
  
  plotdat <- plotdat_passing.dvm
  
  ## Stop line
  plotdat <-
    plotdat +
    geom_vline(xintercept = set4sim$objpos[2] ,
               color = "orange",
               size = 1)
  
  ## Stop line / object
  if(set4sim$computeI[4] == T)
    plotdat <-
    plotdat +
    geom_vline(xintercept = set4sim$objpos[4],
               color = "magenta",
               size = 1)
  
  ## Position where simulation is carried out
  ## Position corresponding to time lag
  plotdat <-
    plotdat +
    geom_vline(xintercept = set4sim_temp$dist1,
               color = "olivedrab3",
               size = 1) +
    geom_vline(xintercept = set4sim_temp$dist2,
               color = "olivedrab3",
               size = 1) +
    geom_rect(aes(xmin = set4sim_temp$dist1, xmax = set4sim_temp$dist2,
                  ymin = -Inf, ymax = 2), fill = "olivedrab2", alpha = 0.5) +
    geom_text(aes(x = (set4sim_temp$dist1 +
                         (set4sim_temp$dist2 - set4sim_temp$dist1)/2),
                  y = 1,
                  label = paste(set4sim$timelag_s, "s"),
                  fontface = "bold")) +
    geom_segment(aes(x = set4sim_temp$dist2, xend = set4sim_temp$dist1,
                     y = 0.5, yend = 0.5),
                 arrow = arrow(length = unit(6, "pt")),
                 size = 1)
  
  if (showplot) plot(plotdat)
  return(plotdat)
}