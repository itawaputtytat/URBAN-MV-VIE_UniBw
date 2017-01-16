
# Windows configuration ---------------------------------------------------

graphics.off()
## Settings
set4windows <- c()
set4windows$top <- 0
set4windows$right <- -7
set4windows$width <- 7
set4windows$height <- 4
set4windows$pointsize <- 8
## Simulation
windows(xpos = set4windows$right, 
        ypos = set4windows$top, 
        width = set4windows$width, 
        height = set4windows$height,
        pointsize = set4windows$pointsize)
## Current intent probability
windows(xpos = set4windows$right - set4windows$width*96 - set4windows$width - 8, 
        ypos = set4windows$top, 
        width = 3, 
        height = set4windows$height,
        pointsize = set4windows$pointsize)
## Probability history
windows(xpos = set4windows$right, 
        ypos = set4windows$height*96 + 65, 
        width = set4windows$width, 
        height = set4windows$height,
        pointsize = set4windows$pointsize)
# ## Simulation parameter: Speed
windows(xpos = set4windows$right - set4windows$width*96 - set4windows$width - 8,
        ypos = set4windows$height*96 + 65,
        width = 3,
        height = set4windows$height/2 - (65/2)/96 + 4/96,
        pointsize = set4windows$pointsize)
windows(xpos = set4windows$right - set4windows$width*96 - set4windows$width - 8,
        ypos = set4windows$height*96 + 65 + set4windows$height/2*96 + 65/2 - 4,
        width = 3,
        height = set4windows$height/2 - (65/2)/96 + 4/96,
        pointsize = set4windows$pointsize)



# Create templates --------------------------------------------------------

## Simulation
dev.set(2)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-simulation.R")

## Current probability
dev.set(3)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-current-prob.R")

## Probability history
dev.set(4)
source("analysis-study-1/prediction_dev/zz-vis-classic-template-history-prob.R")