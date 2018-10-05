
# Windows configuration ---------------------------------------------------

graphics.off()

## Global positions
sett_windows <- c()

sett_windows$pointsize <- 8

## Simulation
sett_windows$sim$width <- 7.5
sett_windows$sim$height <- 4.9
sett_windows$sim$xpos <- 0
sett_windows$sim$ypos <- 0

sett_windows$sim_speed$width <- sett_windows$sim$width/2
sett_windows$sim_speed$height <- sett_windows$sim$height/2 - 44/2/96
sett_windows$sim_speed$xpos <- sett_windows$sim$xpos + sett_windows$sim$width*96 - 2
sett_windows$sim_speed$ypos <- sett_windows$sim$ypos

sett_windows$sim_distance$width <- sett_windows$sim_speed$width
sett_windows$sim_distance$height <- sett_windows$sim_speed$height
sett_windows$sim_distance$xpos <- sett_windows$sim_speed$xpos
sett_windows$sim_distance$ypos <- sett_windows$sim_speed$ypos + sett_windows$sim_speed$height*96 + 44 + 4

## Intent probability
sett_windows$bn$width <- sett_windows$sim_speed$width
sett_windows$bn$height <- sett_windows$sim$height/2 - 44/2/96
sett_windows$bn$xpos <- sett_windows$sim_speed$xpos + sett_windows$sim_speed$width*96 - 6
sett_windows$bn$ypos <- 0

sett_windows$prob$width <- sett_windows$bn$width
sett_windows$prob$height <- sett_windows$bn$height
sett_windows$prob$xpos <- sett_windows$bn$xpos
sett_windows$prob$ypos <- sett_windows$bn$ypos + sett_windows$bn$height*96 + 44 + 4

sett_windows$prob_hist$width <- sett_windows$sim$width
sett_windows$prob_hist$height <- sett_windows$sim$height
sett_windows$prob_hist$xpos <- sett_windows$sim$xpos
sett_windows$prob_hist$ypos <- sett_windows$sim$height*96 + 44 + 4

## Dummy to avoid flickering
windows(xpos = sett_windows$sim$xpos,
        ypos = sett_windows$sim$ypos,
        width = 0.1,
        height = 0.1)

## Simulation
windows(xpos = sett_windows$sim$xpos, 
        ypos = sett_windows$sim$ypos, 
        width = sett_windows$sim$width, 
        height = sett_windows$sim$height,
        pointsize = sett_windows$pointsize)

## Simulation parameter: Speed
windows(xpos = sett_windows$sim_speed$xpos,
        ypos = sett_windows$sim_speed$ypos,
        width = sett_windows$sim_speed$width,
        height = sett_windows$sim_speed$height,
        pointsize = sett_windows$pointsize)

## Simulation parameter: Driven distance
windows(xpos = sett_windows$sim_distance$xpos,
        ypos = sett_windows$sim_distance$ypos,
        width = sett_windows$sim_distance$width,
        height = sett_windows$sim_distance$height,
        pointsize = sett_windows$pointsize)

## BN
windows(xpos = sett_windows$bn$xpos,
        ypos = sett_windows$bn$ypos, 
        width = sett_windows$bn$width, 
        height = sett_windows$bn$height,
        pointsize = sett_windows$pointsize)
plot(bn)

## Current intent probability
windows(xpos = sett_windows$prob$xpos,
        ypos = sett_windows$prob$ypos, 
        width = sett_windows$prob$width, 
        height = sett_windows$prob$height,
        pointsize = sett_windows$pointsize)

## Probability history
windows(xpos = sett_windows$prob_hist$xpos,
        ypos = sett_windows$prob_hist$ypos, 
        width = sett_windows$prob_hist$width, 
        height = sett_windows$prob_hist$height,
        pointsize = sett_windows$pointsize)



# Create templates --------------------------------------------------------

## Simulation
dev.set(3)
source("prediction/resources_plot/_plot_simulation_template.R")

## Current probability
dev.set(7)
source("prediction/resources_plot/_plot_current-prob_template.R")

## Probability history
dev.set(8)
source("prediction/resources_plot/_plot_history-prob_template.R")
