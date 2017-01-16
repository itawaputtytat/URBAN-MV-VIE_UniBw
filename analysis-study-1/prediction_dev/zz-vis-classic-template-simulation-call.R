
replayPlot(plot_template4sim)


# History of simulated speed profiles
invisible(lapply(seq_along(coll4dat4sim), function(x) {
  name <- names(coll4dat4sim)[[x]]
  if (grepl("j1", name))
  { col <- "#6FCDDD65"; shape = 17 } # cyan
  if (grepl("j2", name))
  { col <- "#FFA50065"; shape = 18 } # orange
  if (grepl("j3", name))
  { col <- "#ED212565"; shape = 16 } # red
  if (grepl("j4", name))
  { col <- "#B9539F65"; shape = 15 } # magenta
  lines(x = coll4dat4sim[[x]]$dist_m[-1], 
        y = coll4dat4sim[[x]]$speed_ms[-1],
        col = col, lwd = 0.75)
}))

#plot_template4sim <- recordPlot()

## Rectangle for timelag
rect(xleft = set4sim_temp$dist1, xright = pos4carryout,
     ybottom = set4vis$sim$ymin, ytop = set4vis$sim$ymax,
     col = "#00EE0022")
abline(v = pos4carryout, col = "green4")
abline(v = set4sim_temp$dist1, col = "green4")
arrows(x0 = pos4carryout, x1 = set4sim_temp$dist1,
       y0 = set4vis$sim$ymax - 2.5,
       lwd = 2,
       col = "green4",
       length = 0.2)

## Text for timelag
rect(xleft = (pos4carryout + set4sim_temp$dist1)/2 - 2, xright = (pos4carryout + set4sim_temp$dist1)/2 + 2,
     ybottom = set4vis$sim$ymax - 1.5, ytop = set4vis$sim$ymax - 0.5,
     col = "#00EE0099")
text(x = (pos4carryout + set4sim_temp$dist1)/2,
     y =  set4vis$sim$ymax - 1,
     labels = paste(set4sim$timelag_s, "s"))

## Text for current position
rect(xleft = (pos4carryout + set4sim_temp$dist1)/2 - 2, xright = (pos4carryout + set4sim_temp$dist1)/2 + 2,
     ybottom = set4vis$sim$ymax - 4.5, ytop = set4vis$sim$ymax - 3.5,
     col = "white")
text(x = (pos4carryout + set4sim_temp$dist1)/2,
     y =  set4vis$sim$ymax - 4,
     labels = paste(round(pos4carryout, 1), "m"))


## Current simulated speed profiles
invisible(lapply(seq_along(dat4sim), function(x) {
  name <- names(dat4sim)[[x]]
  if (grepl("j1", name))
  { col <- "#6FCDDD"; shape = 17 } # cyan
  if (grepl("j2", name))
  { col <- "orange"; shape = 18 } # orange
  if (grepl("j3", name))
  { col <- "#ED2125"; shape = 16 } # red
  if (grepl("j4", name))
  { col <- "#B9539F"; shape = 15 } # magenta
  lines(x = dat4sim[[x]]$dist_m,
        y = dat4sim[[x]]$speed_ms,
        col = col, lwd = 1)
  points(x = tail(dat4sim[[x]]$dist_m),
         y = tail(dat4sim[[x]]$speed_ms),
         col = col, pch = shape, cex = 1.5)
}))
