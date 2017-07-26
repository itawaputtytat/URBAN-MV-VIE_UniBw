# Objective ---------------------------------------------------------------

## Load data from study 2

# Function ----------------------------------------------------------------

dbGetDataFromStudy2 <- function(sid, mdir, apprspeed, tx, dist1pre, dist2pre) {

  cat("\n")
  cat("Running dbGetDataFromStudy2: \n")
  cat("---------------------------- \n")

  ## Output parameters
  cat("Using parameters ... \n")
  cat("* Subjects:", sid, "\n")
  cat("* Target direction:", mdir, "\n")
  cat("* Target speed:", apprspeed, "\n")
  cat("* First / Second round:", tx, "\n")
  cat("* Using pre-distances from", dist1pre, "to", dist2pre)
  cat("\n")

  ## Load query template
  path4query <- "resources/study2/query-template_V2.txt"
  string4query_template <- readChar(path4query, file.info(path4query)$size)

  ## Adjust query to settings above
  string4query <- string4query_template
  string4query <- gsub("PLACEHOLDER-SUBID", sid, string4query)
  string4query <- gsub("PLACEHOLDER-SPEED", apprspeed, string4query)
  string4query <- gsub("PLACEHOLDER-T", tx, string4query)
  string4query <- gsub("PLACEHOLDER-DIRECTION", mdir, string4query)
  string4query <- gsub("PLACEHOLDER-DIST1PRE", dist1pre, string4query)
  string4query <- gsub("PLACEHOLDER-DIST2PRE", dist2pre, string4query)

  ## Load data
  dat <- dbGetQuery(dbconn_study2, string4query)

  return(dat)
}
