## Set intervals to axis
par(xaxs = "r", yaxs = "r")

## Draw plot with (initial) prior
barplot(height = set4bn$prior$I, 
        names.arg = set4bn$states$I,
        font = 2,
        yaxt = "n",
        ylim = c(0, 1),
        col = c("#6FCDDD", "orange", "#ED2125", "#B9539F"),
        ylab = "Probability",
        font.lab = 2)
axis(side = 2)

text(x = c(0.7, 1.9, 3.1, 4.3), 
     y = set4bn$prior$I, 
     labels = paste(set4bn$prior$I * 100, "%", sep  =""), 
     pos = 3)

## Left-align title
title("Current P( Hi | v(t), s(t) )", 
      adj = 0)

## Save plot to template
plot_template4intentprob <- recordPlot()