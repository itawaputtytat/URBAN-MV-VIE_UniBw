## Draw plot for speed values
barplot(height = unlist(dat_sim_tails$speed_ms),
        names.arg = dat_sim_tails$.id,
        xaxt = "n",
        #yaxt = "n",
        ylim = c(0, 20),
        col = c(rep("#6FCDDD", 9), rep("orange", 9), rep("#ED2125", 9), rep("#B9539F", 9)),
        ylab = "Speed (m/s)")
abline(h = sett_sim_temp$speed2, col = "green4")
#font.lab = 2)
#axis(side = 2)


## Left-align title
title("Simulated speed",
      adj = 0)