outputSectionTitle("Database")
outputString("* Attaching libraries ...")
library(RPostgreSQL)
outputDone(step = T)

outputString("* Initialise set4db")
dbInitSettings("settings", "set4db.R")
outputDone(step = T)

outputString("* Initialise set4idnames")
source("settings/set4idnames.R")
outputDone(step = T)

outputString("* Attaching functions ...")
source("fun/dbConnect_operator.R")
source("fun/dbCreateQueryString.R")
source("fun/dbGetQuery_batch.R") ## Can be renamed to study1 similar to next
outputDone(step = T)

