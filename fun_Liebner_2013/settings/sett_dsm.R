sett_dsm <- c()
## Source data from study 1 oder study 2
sett_dsm$src <- "study1"
sett_dsm$src <- "study2"

## Path and file name to desired velocity models
sett_dsm$path <- file.path("modeling_Liebner_2013_V3", "_archive")
sett_dsm$filename <- "clustresults_means_h.txt"

## Load desired velocity models
sett_dsm$filepath <- file.path(sett_dsm$path, sett_dsm$filename)