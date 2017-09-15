
replayPlot(plot_template4sim)
#source("analysis-study-1/prediction_dev/zz-vis-classic-template-simulation.R")

# History of simulated speed profiles
if (sett_vis$plot_simulation_history) {
  invisible(lapply(seq_along(dat_sim_tails_coll), function(x) {
    dat <- dat_sim_tails_coll[[x]][-1, ]
    #dat <- tail(coll4dat4sim[[x]], 2)
    name <- names(dat_sim_tails_coll)[[x]]
    if (grepl("j1", name))
    { col <- "#6FCDDD65"; shape = 17 } # cyan
    if (grepl("j2", name))
    { col <- "#FFA50065"; shape = 18 } # orange
    if (grepl("j3", name))
    { col <- "#ED212565"; shape = 16 } # red
    if (grepl("j4", name))
    { col <- "#B9539F65"; shape = 15 } # magenta
    lines(lwd = 0.75,
          #points(pch = ".",
          x = dat$dist_m, 
          y = dat$speed_ms,
          col = col)
  }))
}


# plot_template4sim <- recordPlot()
# replayPlot(plot_template4sim)

## Rectangle for timelag
rect(xleft = sett_sim_temp$dist1, xright = sett_sim_temp$pos4carryout_precise,
     ybottom = sett_vis$sim$ymin, ytop = sett_vis$sim$ymax,
     col = "#00EE0022")
abline(v = sett_sim_temp$pos4carryout_precise, col = "green4")
abline(v = sett_sim_temp$dist1, col = "green4")
arrows(x0 = sett_sim_temp$pos4carryout_precise, 
       x1 = sett_sim_temp$dist1,
       y0 = sett_vis$sim$ymax - 2.5,
       lwd = 2,
       angle = 40,
       col = "green4",
       length = 0.15,
       xpd = T)

## Text for timelag
rect(xleft = (sett_sim_temp$pos4carryout_precise + sett_sim_temp$dist1)/2 - 3, 
     xright = (sett_sim_temp$pos4carryout_precise + sett_sim_temp$dist1)/2 + 3,
     ybottom = sett_vis$sim$ymax - 4.5, ytop = sett_vis$sim$ymax - 3.5,
     col = "#00EE0099")
text(x = (sett_sim_temp$pos4carryout_precise + sett_sim_temp$dist1)/2,
     y =  sett_vis$sim$ymax - 4,
     labels = paste(sett_sim$timelag_s, "s"))

## Text for current position
rect(xleft = (sett_sim_temp$pos4carryout_precise + sett_sim_temp$dist1)/2 - 3, 
     xright = (sett_sim_temp$pos4carryout_precise + sett_sim_temp$dist1)/2 + 3,
     ybottom = sett_vis$sim$ymax - 1.5, ytop = sett_vis$sim$ymax - 0.5,

     col = "white")
text(x = (sett_sim_temp$pos4carryout_precise + sett_sim_temp$dist1)/2,
     y =  sett_vis$sim$ymax - 1,
     labels = paste(round(sett_sim_temp$pos4carryout_precise, 1), "m"))


## Current simulated speed profiles
invisible(lapply(seq_along(dat_sim), function(x) {
  name <- names(dat_sim)[[x]]
  if (grepl("j1", name))
  { col <- "#6FCDDD"; shape = 17 } # cyan
  if (grepl("j2", name))
  { col <- "orange"; shape = 18 } # orange
  if (grepl("j3", name))
  { col <- "#ED2125"; shape = 16 } # red
  if (grepl("j4", name))
  { col <- "#B9539F"; shape = 15 } # magenta
  lines(x = dat_sim[[x]]$dist_m,
        y = dat_sim[[x]]$speed_ms,
        col = col, lwd = 1)
  points(x = tail(dat_sim[[x]]$dist_m),
         y = tail(dat_sim[[x]]$speed_ms),
         col = col, pch = shape, cex = 1.5)
}))
