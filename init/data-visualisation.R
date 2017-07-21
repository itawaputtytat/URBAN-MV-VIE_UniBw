outputSectionTitle("Visualisation")

outputString("* Attaching libraries ...")
library(ggplot2)
library(gridExtra) # e.g. grid.arrange()
require(googleVis) # e.g. mouse over plots in browser
library(ggmap) # GPS-plots
library(GGally) #for parallel coordinates
library(plotly)

outputString("* Attaching functions ...")
source("fun/getMapImage.R")
#source("fun/adjustGrid.R")
#source("plotting/adjustMarginGtable.R")
#source("fun/adjustPanel.R")

outputDone(step = T)