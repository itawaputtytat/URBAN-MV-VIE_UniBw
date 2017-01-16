## Settings
set4windowpos <- c()
set4windowpos$top <- 0
set4windowpos$right <- -7
set4windowpos$width <- 10
set4windowpos$height <- 4
## Probability history
windows(xpos = set4windowpos$right, 
        ypos = set4windowpos$height*96 + 65, 
        width = set4windowpos$width, 
        height = set4windowpos$height)
## Simulation
windows(xpos = set4windowpos$right, 
        ypos = set4windowpos$top, 
        width = set4windowpos$width, 
        height = set4windowpos$height)
## Current intent probability
windows(xpos = set4windowpos$right - set4windowpos$width*96 - set4windowpos$width - 6, 
        ypos = set4windowpos$top, 
        width = 3, 
        height = set4windowpos$height)





# Preparatory settings ----------------------------------------------------

results2 <- unlist(results)

## Load template with speed profiles
replayPlot(plot_template4sim)
plot.new()


# Visualise simulated speed profiles --------------------------------------

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


## Current simulated seed profiles
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



# Visualise current intent probabilites -----------------------------------

# par(xaxs = "r", yaxs = "r")
bp <- barplot(results2, ylim = c(0, 1),
        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"))
# # barplot(results2, ylim = c(0, 1),
# #        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"), add = T)
title("Probability P(Hi|v(t), s(t))",
      adj = 0)
text(x = bp, y = results2, labels = paste(round(results2 * 100, 1), "%", sep  =""), pos = 3, xpd = NA)
# #text(x = c(0.7, 1.9, 3.1, 4.3), y = results2, labels = paste(round(results2 * 100, 1), "%", sep  =""), pos = 3, xpd = NA)

# Visualise history of intent probabilities -------------------------------



coll4results2 <- as.data.frame(coll4results)
rownames(coll4results2) <- NULL

#par(xaxs = "i", yaxs = "i")
# plot(x = coll4results2$s, y = coll4results2$Intent1, type = "l", col = "blue", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
# lines(x = coll4results2$s, coll4results2$Intent2, col = "orange")
# lines(x = coll4results2$s, coll4results2$Intent3, col = "red")
# lines(x = coll4results2$s, coll4results2$Intent4, col = "magenta")
# plot(0, xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
# abline(v = set4sim$objpos[2], col = "orange")
# abline(v = set4sim$objpos[4], col = "#B9539F")
# 
# y <- recordPlot()
plot(x = coll4results2$s, y = coll4results2$Intent1, type = "l", col = "blue", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
par(new = TRUE)
plot(x = coll4results2$s, y = coll4results2$Intent2, type = "l", col = "orange", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
par(new = TRUE)
plot(x = coll4results2$s, y = coll4results2$Intent3, type = "l", col = "red", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
par(new = TRUE)
plot(x = coll4results2$s, y = coll4results2$Intent4, type = "l", col = "magenta", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
#lines(x = coll4results2$s, coll4results2$Intent1, col = "blue")
#lines(x = coll4results2$s, coll4results2$Intent2, col = "orange")
#lines(x = coll4results2$s, coll4results2$Intent3, col = "red")
#lines(x = coll4results2$s, coll4results2$Intent4, col = "magenta")
par(new = TRUE)
abline(v = pos4carryout, col = "green4")
# z <- recordPlot()
# replayPlot(z)


# points(x = coll4results2$s, coll4results2$Intent1, pch = 17, col = "blue")
# points(x = coll4results2$s, coll4results2$Intent2, pch = 18, col = "orange")
# points(x = coll4results2$s, coll4results2$Intent3, pch = 16, col = "red")
# points(x = coll4results2$s, coll4results2$Intent4, pch = 15, col = "magenta")


#plot_template4sim <- recordPlot()
