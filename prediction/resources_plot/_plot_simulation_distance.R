## Draw plot for distance values
barplot(#height = abs(pos4carryout - unlist(temp2$dist_m)),
  height = abs(sett_sim_temp$am1 - unlist(dat_sim_tails$dist_m)),
  names.arg = dat_sim_tails$.id,
  xaxt = "n",
  #yaxt = "n",
  #ylim = c(-4, 4),
  ylim = c(0, 20),
  col = c(rep("#6FCDDD", 9), rep("orange", 9), rep("#ED2125", 9), rep("#B9539F", 9)),
  ylab = "Distance (m)")
abline(h = sett_sim_temp$dist_diff, col = "green4")
#font.lab = 2)
#axis(side = 2)

## Left-align title
title("Simulated driven distance",
      adj = 0)
