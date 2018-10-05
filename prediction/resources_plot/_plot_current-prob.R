
#replayPlot(plot_template4intentprob)
# par(xaxs = "r", yaxs = "r")
#results2 <- unlist(results)
barplot(height = dat_pred_results,
        ylim = c(-0.05,1.05),
        # names.arg = 
        #   c("Go \nstraight",
        #     "Stop\n",
        #     "Turn\n",
        #     "nTurn/\nStop"),
        xaxt = "n",
        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"),
        ylab = "Probability")
# axis(side = 2, 
#      at = seq(0,1.1,0.5),
#      labels = seq(0,1.1,0.5))
# abline(h = results2[1], col = "#6FCDDD")
# abline(h = results2[2], col = "orange")
# abline(h = results2[3], col = "#ED2125")
# abline(h = results2[4], col = "#B9539F")
# 
# abline(h = results2[3] + results2[4], col = "#ED212550")

# # barplot(results2, ylim = c(0, 1),
# #        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"), add = T)
text(x = c(0.7, 1.9, 3.1, 4.3), 
     y = dat_pred_results, 
     labels = paste(round(dat_pred_results * 100, 1), "%", sep  =""), 
     pos = 3)
# #text(x = c(0.7, 1.9, 3.1, 4.3), y = results2, labels = paste(round(results2 * 100, 1), "%", sep  =""), pos = 3, xpd = NA)

mtext('Go\nstraight', side=1, line=1.5, at=0.7)
mtext('Stop', side=1, line=0.5, at=1.9)
mtext('Turn', side=1, line=0.5, at=3.1)
mtext('Turn/\nStop', side=1, line=1.5, at=4.3)

## Left-align title
title(expression(bold("P(H"["i"]*"|v(t),s(t))")), adj = 0)
