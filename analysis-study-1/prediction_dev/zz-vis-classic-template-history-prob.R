## Avoid additional space to axis
par(xaxs = "i", yaxs = "i")

## Draw empty plot
plot(x = 0, 
     y = 0, 
     xlim = c(set4vis$sim$xmin, set4vis$sim$xmax), 
     ylim = c(0,1),
     col = "white",
     xlab = "Distance-to-intersection (m)",
     ylab = "Probability",
     font.lab = 2)

## Left-align title
title("History of P( Hi | v(t), s(t) ) ",
      adj = 0)

## Draw lines for object positions
abline(v = set4sim$objpos[2], col = "orange")
abline(v = set4sim$objpos[4], col = "#B9539F")

## Draw legend
legend(x = -49, y = 0.975,
       title = expression(bold("Intent probability")),
       title.adj = 0.1,
       c("Go straight", 
         "Stop at stop line", 
         "Turn right", 
         "Turn right but stop"),
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
