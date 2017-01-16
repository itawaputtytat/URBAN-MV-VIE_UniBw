#graphics::layout(matrix(c(1,1,1,2), nrow = 1, byrow = T))
graphics.off()

graphics::layout(matrix(c(c(1,1,1,2),
                          c(3,3,3,2)),
                        nrow = 2,
                        byrow = T))



par(xaxs = "i", yaxs = "i")
plot(x = 0, 
     y = 0, 
     xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), 
     ylim = c(set4vis$sim$ymin, set4vis$sim$ymax), 
     col = "white")
title("Simulated driving behaviour corresponding to Hypothesis Hi",
      adj = 0)
#grid(lwd = 2) # grid only in y-direction

## Draw horizintal lines for set of u
#cols <- c("#ED212450", "#6ABD4550", "#3953A450")
cols <- rep("grey85", 3)
for(i in 1:length(set4sim$v_ms.max)) {
  abline(h = rev(set4sim$v_ms.max)[i], col = cols[i], lwd = 10 * set4bn$prior$V[clustcentres_order$order[i]], lty = "solid")
}

## DSM
lines(dat4dsm$dist, dat4dsm$k1, col = "#ED212450", lwd = 10 * set4bn$prior$V[1])
lines(dat4dsm$dist, dat4dsm$k2, col = "#6ABD4550", lwd = 10 * set4bn$prior$V[2])
lines(dat4dsm$dist, dat4dsm$k3, col = "#3953A450", lwd = 10 * set4bn$prior$V[3])

abline(v = set4sim$objpos[2], col = "orange")
abline(v = set4sim$objpos[4], col = "#B9539F")

## Passing
lines(x = dat4test[, set4dat$varname4sxx_dist_m], 
      y = dat4test[, set4dat$varname4speed_ms],
      col = "#3953A4", lwd = 2)


test <- set4bn$prior$I
names(test) <- set4bn$states$I
bp <- barplot(test, ylim = c(0, 1),
              col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"))
title("Probability P(Hi|v(t), s(t))",
      adj = 0)
text(x = bp, y = set4bn$prior$I, labels = paste(round(results2 * 100, 1), "%", sep  =""), pos = 3, xpd = NA)


plot(x = rep(set4sim$pos4carryout, 4), y = set4bn$prior$I, type = "l", col = "blue", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))


plot_template4sim <- recordPlot()



# 
# test <- set4bn$prior$I
# names(test) <-  set4bn$states$I
# bp <- barplot(test, ylim = c(0, 1),
#               col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"))
#               # # barplot(results2, ylim = c(0, 1),
#               # #        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"), add = T)
#               title("Probability P(Hi|v(t), s(t))",
#                     adj = 0)
#               text(x = bp, y = set4bn$prior$I, labels = paste(round(results2 * 100, 1), "%", sep  =""), pos = 3, xpd = NA)
# 
#               
              
              
              
# 
# 
# par(xaxs = "i", yaxs = "i")
# # plot(x = coll4results2$s, y = coll4results2$Intent1, type = "l", col = "blue", xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
# # lines(x = coll4results2$s, coll4results2$Intent2, col = "orange")
# # lines(x = coll4results2$s, coll4results2$Intent3, col = "red")
# # lines(x = coll4results2$s, coll4results2$Intent4, col = "magenta")
# plot(0, xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), ylim = c(0,1))
# abline(v = set4sim$objpos[2], col = "orange")
# abline(v = set4sim$objpos[4], col = "#B9539F")
# #

