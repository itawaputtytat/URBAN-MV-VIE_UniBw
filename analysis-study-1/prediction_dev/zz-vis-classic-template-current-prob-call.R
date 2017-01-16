
#replayPlot(plot_template4intentprob)
# par(xaxs = "r", yaxs = "r")
results2 <- unlist(results)

barplot(height = results2,
        ylim = c(0, 1),
        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"),
        ylab = "Probability")

# # barplot(results2, ylim = c(0, 1),
# #        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"), add = T)
text(x = c(0.7, 1.9, 3.1, 4.3), 
     y = results2, 
     labels = paste(round(results2 * 100, 1), "%", sep  =""), 
     pos = 3)
# #text(x = c(0.7, 1.9, 3.1, 4.3), y = results2, labels = paste(round(results2 * 100, 1), "%", sep  =""), pos = 3, xpd = NA)

## Left-align title
title("Current P( Hi | v(t), s(t) )", 
      adj = 0)