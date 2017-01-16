set4dsm <- c()
## Source data from study 1 oder study 2
set4dsm$src <- "study1"
set4dsm$src <- "study2"

## Path and file name to desired velocity models
set4dsm$path <- file.path("modeling_Liebner_2013_V3", "_archive")
set4dsm$filename <- "clustresults_means_h.txt"

## Load desired velocity models
set4dsm$filepath <- file.path(set4dsm$path, set4dsm$filename)