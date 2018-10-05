## Draw plot for speed values
barplot(#
        height = unlist(dat_sim_tails$speed_ms),
        names.arg = dat_sim_tails$.id,
        xaxt = "n",
        #yaxt = "n",
        ylim = c(0, 20),
        col = c(rep("#6FCDDD", sett_plot$n_simulations), 
        rep("orange", sett_plot$n_simulations), 
        rep("#ED2125", sett_plot$n_simulations), 
        rep("#B9539F", sett_plot$n_simulations)),
        ylab = "Speed (m/s)")

abline(h = sett_sim_temp$speed2, col = "green4")
#font.lab = 2)
#axis(side = 2)

mtext('Go\nstraight', side=1, line=1.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2)
mtext('Stop', side=1, line=0.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2 + 1*(sett_bn$states_n$A * sett_bn$states_n$S+2))
mtext('Turn', side=1, line=0.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2 + 2*(sett_bn$states_n$A * sett_bn$states_n$S+2))
mtext('Turn/\nStop', side=1, line=1.5, at=(sett_bn$states_n$A * sett_bn$states_n$S+2)/2 + 3*(sett_bn$states_n$A * sett_bn$states_n$S+2))

## Left-align title
title("Simulated speed",
      adj = 0)