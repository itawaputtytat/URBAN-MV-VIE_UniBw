library(puttytat4R)

# Version info ------------------------------------------------------------

outputSectionTitle("Project: URBAN-MV-VIE_UniBw")
outputString("* Framework V3.1")



# Database ----------------------------------------------------------------

outputSectionTitle("Database")

outputString("* Attaching libraries ...")
source("init-framework/db-lib.R")
outputDone(step = T)

outputString("* Initialise settings (db, idnames")
source("init-framework/db-init-settings.R") ## Requires dbInitSettings()
outputDone(step = T)

outputString("* Attaching functions ...")
source("init-framework/db-fun.R")
outputDone(step = T)

outputString("* Connecting to database ...")
dbConnectOperator()
outputDone(step = T)



# Data manipulation -------------------------------------------------------

outputSectionTitle("* Data manipulation")

outputString("* Attaching libraries ...")
source("init-framework/data-manipulation-lib.R")
outputDone(step = T)

outputString("Attaching functions ...")
source("init-framework/data-manipulation-fun.R")
outputDone(step = T)



# Visualisation -----------------------------------------------------------

outputSectionTitle("Visualisation")

outputString("* Attaching libraries ...")
source("init-framework/vis-lib.R")
outputDone(step = T)

outputString("* Attaching functions ...")
source("init-framework/vis-fun.R")
outputDone(step = T)



# Processing --------------------------------------------------------------

outputString("* Attaching libraries ...")
source("init-framework/data-processing-lib.R")
outputDone(step = T)



# Liebner et al. (2013) ---------------------------------------------------

outputSectionTitle("Liebner et al. (2013)")
source("init-framework/Liebner-lib.R")
source("init-framework/Liebner-fun.R")



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
source("init-framework/misc-settings.R")



# Loading data ------------------------------------------------------------

## DOES NOT WORK ANYMORE
#source("init-framework/preloading-data.R") # t_sxx_critdist
