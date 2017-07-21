## @knitr start

# Version info ------------------------------------------------------------

library(puttytat4R)
outputSectionTitle("Project: URBAN-MV-VIE_UniBw")
outputString("* Framework V3.1", type = "message")



# Database ----------------------------------------------------------------

# outputSectionTitle("Database")
# outputString("* Attaching libraries ...")
# library(RPostgreSQL)
# outputDone(step = T)
# 
# outputString("* Initialise set4db")
dbInitSettings("settings", "set4db.R")
# outputDone(step = T)
# 
# outputString("* Initialise set4idnames")
# source("settings/set4idnames.R")
# outputDone(step = T)
# 
# outputString("* Attaching functions ...")
source("fun/dbConnect_operator.R")
# source("fun/dbCreateQueryString.R")
# source("fun/dbGetQuery_batch.R") ## Can be renamed to study1 similar to next
# outputDone(step = T)
# 
# outputString("* Connecting to database ...")
# #dbConnect_operator()
# outputDone(step = T)
