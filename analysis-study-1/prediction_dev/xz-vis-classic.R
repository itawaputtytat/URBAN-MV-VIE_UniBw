

#plot.new()
#replayPlot(x)

results2 <- unlist(results)


graphics::layout(matrix(c(1,1,1,2), nrow = 1, byrow = T))
par(xaxs = "i", yaxs = "i")
plot(dat4test$sxx_dist_m_rnd1, dat4test$speed_ms, 
     type = "l", col = "#3953A4", lwd = 2,
     xlim = c(-50, 25), ylim = c(0, 30))
title("Simulated driving behaviour corresponding to Hypothesis Hi",
      adj = 0)
#grid(lwd = 2) # grid only in y-direction

lines(dat4dvm$dist, dat4dvm$k1, col = "grey70")
lines(dat4dvm$dist, dat4dvm$k2, col = "grey70")
lines(dat4dvm$dist, dat4dvm$k3, col = "grey70")
abline(v = set4sim$objpos[2], col = "orange")
abline(v = set4sim$objpos[4], col = "#B9539F")

rect(xleft = set4sim_temp$dist1, xright = pos4carryout,
     ybottom = 0, ytop = 30,
     col = "#00EE0022")
abline(v = pos4carryout, col = "green4")
abline(v = set4sim_temp$dist1, col = "green4")
arrows(x0 = pos4carryout, x1 = set4sim_temp$dist1,
       y0 = 27.5,
       lwd = 2,
       col = "green4",
       length = 0.2)

rect(xleft = (pos4carryout + set4sim_temp$dist1)/2 - 2, xright = (pos4carryout + set4sim_temp$dist1)/2 + 2,
     ybottom = 28.5, ytop = 29.5,
     col = "#00EE0099")
text(x = (pos4carryout + set4sim_temp$dist1)/2,
     y = 29,
     labels = paste(set4sim$timelag_s, "s"))

rect(xleft = (pos4carryout + set4sim_temp$dist1)/2 - 2, xright = (pos4carryout + set4sim_temp$dist1)/2 + 2,
     ybottom = 25.5, ytop = 26.5,
     col = "white")

text(x = (pos4carryout + set4sim_temp$dist1)/2,
     y = 26,
     labels = paste(pos4carryout, "m"))

invisible(lapply(seq_along(dat4sim), function(x) {
  name <- names(dat4sim)[[x]]
  #print(name)
  if (grepl("j1", name)) 
  { col <- "#6FCDDD"; shape = 17 } # cyan
  if (grepl("j2", name)) 
  { col <- "orange"; shape = 18 } # orange
  if (grepl("j3", name)) 
  { col <- "#ED2125"; shape = 16 } # red
  if (grepl("j4", name)) 
  { col <- "#B9539F"; shape = 15 } # magenta
  
  lines(dat4sim[[x]]$dist_m, y = dat4sim[[x]]$speed_ms, 
        col = col, lwd = 1)
  
  points(tail(dat4sim[[x]]$dist_m), y = tail(dat4sim[[x]]$speed_ms),
         col = col, pch = shape, cex = 0.7)
}))

par(xaxs = "r", yaxs = "r")
#bp <- barplot(unlist(results), ylim = c(0, 1),
bp <- barplot(results2, ylim = c(0, 1),
        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"))
text(x = bp, y = results2, labels = paste(round(results2 * 100, 1), "%", sep  =""), pos = 3, xpd = NA)
title("Probability P(Hi|v(t), s(t))",
      adj = 0)