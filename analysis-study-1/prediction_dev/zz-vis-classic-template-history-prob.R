
## Avoid additional space to axis
par(xaxs = "i", yaxs = "i")

## Draw empty plot
plot(x = 0, 
     y = 0, 
     xlim = c(sett_vis$sim$xmin, sett_vis$sim$xmax), 
     ylim = c(0,1),
     col = "white",
     xlab = "Distance-to-intersection (m)",
     ylab = "Probability",
     font.lab = 2)

## Left-align title
title("History of P(Hi | v(t), s(t)) ",
      adj = 0)

## Draw lines for object positions
abline(v = sett_sim$objpos[2], col = "orange", lty = "dotted")
abline(v = sett_sim$objpos[4], col = "#B9539F", lty = "dotted")

## Draw legend
legend(x = sett_vis$sim$xmin + 1.6, 
       y = 1 - 0.025,
       title = expression(bold("Intent probability")),
       title.adj = 0.1,
       c("Go straight", 
         "Stop at stop line", 
         "Turn", 
         "Turn but stop"),
       lty = c("solid", 
               "solid", 
               "solid", 
               "solid"),
       col = c("#6FCDDD", 
               "orange", 
               "#ED2125", 
               "#B9539F"),
       bg = "grey92")

plot_template4probhist <- recordPlot()

replayPlot(plot_template4probhist)
