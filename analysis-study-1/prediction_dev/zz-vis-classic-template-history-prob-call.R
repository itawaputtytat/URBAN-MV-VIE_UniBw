
coll4results2 <- as.data.frame(coll4results)
rownames(coll4results2) <- NULL

#par(xaxs = "i", yaxs = "i")
# plot(x = coll4results2$s, y = coll4results2$Intent1, type = "l", col = "blue", xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), ylim = c(0,1))
# lines(x = coll4results2$s, coll4results2$Intent2, col = "orange")
# lines(x = coll4results2$s, coll4results2$Intent3, col = "red")
# lines(x = coll4results2$s, coll4results2$Intent4, col = "magenta")
# plot(0, xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), ylim = c(0,1))
# abline(v = sett_sim$objpos[2], col = "orange")
# abline(v = sett_sim$objpos[4], col = "#B9539F")
# 
# y <- recordPlot()
# replayPlot(plot_template4probhist)
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout,
#      y = coll4results2$Intent1,
#      type = "l", 
#      col = "blue", 
#      xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)
# 
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout, 
#      y = coll4results2$Intent2, type = "l", 
#      col = "orange", xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)
# 
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout, 
#      y = coll4results2$Intent3, type = "l", 
#      col = "red", xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)
# 
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout, 
#      y = coll4results2$Intent4, type = "l", 
#      col = "magenta", xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)

if(!is.na(am_1st))
  abline(v = am_1st)
## Text for current position
rect(xleft = sett_vis$sim$xmax -7, xright = sett_vis$sim$xmax -1,
     ybottom = 0.925, ytop =  0.975,
     col = "white")
text(x = sett_vis$sim$xmax - 4,
     y =  0.95,
     labels = paste(round(sett_proc$carryout_am1, 1), "m"))

row_finder <- which(coll4results2$pos4carryout >= dist2_prev & 
                      coll4results2$pos4carryout <= sett_sim_temp$dist2)
testy <- coll4results2[row_finder, ]

lines(x = testy$pos4carryout, testy$Intent1, col = "#6FCDDD")
lines(x = testy$pos4carryout, testy$Intent2, col = "orange")
lines(x = testy$pos4carryout, testy$Intent3, col = "#ED2125")
lines(x = testy$pos4carryout, testy$Intent4, col = "#B9539F")

lines(x = testy$pos4carryout, testy$Intent3 + testy$Intent4, col = "#ED212550", lty = "dashed")
#par(new = TRUE)
#abline(v = pos4carryout, col = "green4")
# z <- recordPlot()
# replayPlot(z)


# points(x = coll4results2$s, coll4results2$Intent1, pch = 17, col = "blue")
# points(x = coll4results2$s, coll4results2$Intent2, pch = 18, col = "orange")
# points(x = coll4results2$s, coll4results2$Intent3, pch = 16, col = "red")
# points(x = coll4results2$s, coll4results2$Intent4, pch = 15, col = "magenta")


#plot_template4sim <- recordPlot()


