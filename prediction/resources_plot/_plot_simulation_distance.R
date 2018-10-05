## Draw plot for distance values
barplot(#height = abs(pos4carryout - unlist(temp2$dist_m)),
  height = abs(sett_sim_temp$am1 - unlist(dat_sim_tails$dist_m)),
  names.arg = dat_sim_tails$.id,
  xaxt = "n",
  #yaxt = "n",
  #ylim = c(-4, 4),
  ylim = c(0, 20),
  col = c(rep("#6FCDDD", sett_plot$n_simulations), 
          rep("orange", sett_plot$n_simulations), 
          rep("#ED2125", sett_plot$n_simulations), 
          rep("#B9539F", sett_plot$n_simulations)),
  ylab = "Distance (m)")
abline(h = sett_sim_temp$dist_diff, col = "green4")
#font.lab = 2)
#axis(side = 2)

mtext('Go\nstraight', side=1, line=1.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2)
mtext('Stop', side=1, line=0.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2 + 1*(sett_bn$states_n$A * sett_bn$states_n$S+2))
mtext('Turn', side=1, line=0.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2 + 2*(sett_bn$states_n$A * sett_bn$states_n$S+2))
mtext('Turn/\nStop', side=1, line=1.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2 + 3*(sett_bn$states_n$A * sett_bn$states_n$S+2))

## Left-align title
title("Simulated driven distance",
      adj = 0)
