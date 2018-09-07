
# Windows configuration ---------------------------------------------------

graphics.off()

## Settings
sett_windows <- c()
# sett_windows$sys_os_sysname <- unlist(Sys.info())["sysname"]
# sett_windows$sys_os_release <- unlist(Sys.info())["release"]
# sett_windows$screen_width <- getScreenResolution()$width
# sett_windows$screen_width <- getScreenResolution()$height



#if (sett_windows$sys_os_sysname == "Windows") {
  ## Windows >= 8
#  if (grepl("8", sett_windows$sys_os_release)) {
    sett_windows$top <- 0
    sett_windows$right <- -1
    sett_windows$width <- 6.7
    sett_windows$height <- 4.825
    sett_windows$pointsize <- 8
    ## Else: Windows 7
#  } else {
    sett_windows$top <- 0
    sett_windows$right <- -7
    sett_windows$width <- 6.7
    sett_windows$height <- 4.825
    sett_windows$pointsize <- 8
#  }
#}


## Simulation
windows(xpos = sett_windows$right, 
        ypos = sett_windows$top, 
        width = sett_windows$width, 
        height = sett_windows$height,
        pointsize = sett_windows$pointsize)
## Current intent probability
windows(xpos = (sett_windows$right - sett_windows$width*96 - sett_windows$width - 8),
        ypos = sett_windows$top, 
        width = 3, 
        height = sett_windows$height,
        pointsize = sett_windows$pointsize)
# ## Probability history
# windows(xpos = sett_windows$right, 
#         ypos = sett_windows$height*96 + 65, 
#         width = sett_windows$width, 
#         height = sett_windows$height,
#         pointsize = sett_windows$pointsize)
# ## Simulation parameter
# windows(xpos = sett_windows$right - sett_windows$width*96 - sett_windows$width - 8, 
#         ypos = sett_windows$height*96 + 65, 
#         width = 3, 
#         height = sett_windows$height,
#         pointsize = sett_windows$pointsize)



# Create templates --------------------------------------------------------

## Simulation
dev.set(2)
source("prediction/resources_plot/_plot_simulation_template.R")

## Current probability
dev.set(3)
source("prediction/resources_plot/_plot_current-prob_template.R")

# ## Probability history
# dev.set(4)
# source("analysis-study-1/prediction_dev/zz-vis-classic-template-history-prob.R")

#lapply(dev.list(), dev.off)