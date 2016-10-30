
# Version info ------------------------------------------------------------

library(puttytat4R)
outputSectionTitle("Project: URBAN-MV-VIE_UniBw", char4aes = "#", seplinechar = "=")
outputString("* Framework V3.1", type = "message")



# Database ----------------------------------------------------------------

outputSectionTitle("Database")
outputString("Attaching libraries ...")
library(RPostgreSQL)
outputDone(step = T)

outputString("Initialise set4db")
dbInit()
outputDone(step = T)

outputString("Attaching functions ...")
source("fun/dbConnect.R")
source("fun/dbQueryString.R")
source("fun/dbQueryLoop.R") ## Can be renamed to study1 similar to next
outputDone(step = T)

outputString("Connecting to database ...")
connect2db()
outputDone(step = T)



# Data manipulation -------------------------------------------------------

outputSectionTitle("Data manipulation")
outputString("Attaching libraries ...")
library(dplyr)
library(tidyr)
library(reshape2)
outputDone(step = T)

outputString("Attaching functions ...")
source("fun/renameVar_sxx.R")
source("fun/intrpldf_batch.R")
source("fun/correctPosAnomalies.R")
source("fun/cut2dist.R")
source("fun/computeVar_dist2steermax.R")
source("fun/rbindPassings.R")
source("fun/convertGPS2XYDistances.R")
outputDone(step = T)


# Visualisation -----------------------------------------------------------

outputSectionTitle("Visualisation")

outputString("Attaching libraries ...")
library(ggplot2)
library(gridExtra) # e.g. grid.arrange()
require(googleVis) # e.g. mouse over plots in browser
library(ggmap) # GPS-plots
library(GGally) #for parallel coordinates
library(plotly)

outputString("Attaching functions ...")
#source("fun/adjustGrid.R")
#source("plotting/adjustMarginGtable.R")
#source("fun/adjustPanel.R")

outputDone(step = T)



# Processing --------------------------------------------------------------

outputString("Attaching libraries ...")
library(geosphere) ## Computing gps-distances
library(TraMineR) # Sequence analysis
library(TraMineRextras) # Sequence analysis
#library(lazyeval) # Dynamic filter variables in dplyr
## Cluster analysis
library(LICORS) ## kmeanspp
library(dtw) ## Hierarchical clustering distance measures
library(TSclust) # Calculating different distances measures
library(kml) ## Clustering of longitudinal data

outputDone(step = T)



# Attaching functions -----------------------------------------------------

# outputString("Attaching functions")
## Database

# source("db/dbGetDataFromStudy2.R")
# ## Data processing
# source("preprocessing/computeRoundDiff.R")
# source("preprocessing/computeRoundDiff_ttest.R")
# source("preprocessing/sxxVarRename.R")
# source("preprocessing/accpedalposCorrection.R")
# source("preprocessing/codePedalActivity.R")
# source("preprocessing/excludeCasesFromAnomalies.R")
# source("preprocessing/correctPosAnomalies.R")
# source("preprocessing/cut2dist.R")
# source("preprocessing/createSeqIds4PedalActivity.R")
# source("preprocessing/computeGlanceRates.R")
# source("preprocessing/glanceDataRatesCompletion.R")
# source("preprocessing/rbindIntersectionPassings.R")
# source("preprocessing/addVar4dist2steermax.R")
# source("preprocessing/fitVisibility.R")
# source("preprocessing/filterRangeValue.R")
# source("preprocessing/intrpl4dist_study2.R")
# source("preprocessing/computeArrivalMeasures.R")
# source("preprocessing/correctLatPosDev.R")
# source("preprocessing/createSeqIds4PedalActivity_study2.R")
# outputDone(step = T)



# Miscellaneous settings --------------------------------------------------

outputSectionTitle("Miscellaneous settings")

outputString("* Declare \"select\" as function of dplyr: dplyr::select")
select <- dplyr::select

# outputString("* Force R to use non-exponential notations (scipen)")
# options("scipen" = 100, "digits" = 4)

outputString("* Deactivate outputFunProc")
.outputFunProc_status(F, print = T)


# Loading data ------------------------------------------------------------

outputSectionTitle("Pre-loading data ...")

## Initialise t_sxx_critdist
if (set4db$studyselect == 1) {
  outputString("* Loading t_sxx_critdist from database ...")
  t_sxx_critdist <- dbGetSrc("dbconn_study1", "t_sxx_critdist")
}
outputDone(step = T)