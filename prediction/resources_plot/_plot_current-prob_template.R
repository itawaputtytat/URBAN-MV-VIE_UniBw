
## Set intervals to axis
#par(xaxs = "r", yaxs = "r")

## Draw plot with (initial) prior
barplot(height = sett_bn$prior$I, 
        ylim = c(-0.05,1.05),
        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"),
        ylab = "Probability",
        xaxt = "n",
        #font.lab = 2
        )

# axis(side = 2, 
#      at = seq(0,1.1,0.5),
#      labels = seq(0,1.1,0.5))

text(x = c(0.7, 1.9, 3.1, 4.3), 
     y = sett_bn$prior$I, 
     labels = paste(sett_bn$prior$I * 100, "%", sep  =""), 
     pos = 3)

mtext('Go\nstraight', side=1, line=1.5, at=0.7)
mtext('Stop', side=1, line=0.5, at=1.9)
mtext('Turn', side=1, line=0.5, at=3.1)
mtext('Turn/\nStop', side=1, line=1.5, at=4.3)

## Left-align title
title(expression(bold("P(H"["i"]*"|v(t),s(t))")), adj = 0)
