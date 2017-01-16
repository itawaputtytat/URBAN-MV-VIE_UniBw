
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
# replayPlot(plot_template4probhist)
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout,
#      y = coll4results2$Intent1,
#      type = "l", 
#      col = "blue", 
#      xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)
# 
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout, 
#      y = coll4results2$Intent2, type = "l", 
#      col = "orange", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)
# 
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout, 
#      y = coll4results2$Intent3, type = "l", 
#      col = "red", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)
# 
# par(new = TRUE)
# plot(x = coll4results2$pos4carryout, 
#      y = coll4results2$Intent4, type = "l", 
#      col = "magenta", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), 
#      ylim = c(0,1),
#      ann = F,
#      axes = F)

## Text for current position
rect(xleft = set4vis$sim$xmax -5, xright = set4vis$sim$xmax - 1,
     ybottom = 0.925, ytop =  0.975,
     col = "white")
text(x = set4vis$sim$xmax - 3,
     y =  0.95,
     labels = paste(round(pos4carryout, 1), "m"))

lines(x = coll4results2$pos4carryout, coll4results2$Intent1, col = "#6FCDDD")
lines(x = coll4results2$pos4carryout, coll4results2$Intent2, col = "orange")
lines(x = coll4results2$pos4carryout, coll4results2$Intent3, col = "#ED2125")
lines(x = coll4results2$pos4carryout, coll4results2$Intent4, col = "#B9539F")
#par(new = TRUE)
#abline(v = pos4carryout, col = "green4")
# z <- recordPlot()
# replayPlot(z)


# points(x = coll4results2$s, coll4results2$Intent1, pch = 17, col = "blue")
# points(x = coll4results2$s, coll4results2$Intent2, pch = 18, col = "orange")
# points(x = coll4results2$s, coll4results2$Intent3, pch = 16, col = "red")
# points(x = coll4results2$s, coll4results2$Intent4, pch = 15, col = "magenta")


#plot_template4sim <- recordPlot()


