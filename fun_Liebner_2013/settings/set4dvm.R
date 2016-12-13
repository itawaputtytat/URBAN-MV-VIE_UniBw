set4dvm <- c()
## Source data from study 1 oder study 2
set4dvm$src <- "study1"
set4dvm$src <- "study2"

## Path and file name to desired velocity models
set4dvm$path <- file.path("modeling_Liebner_2013_V3", "_archive")
set4dvm$filename <- "clustresults_means_h.txt"

## Load desired velocity models
set4dvm$filepath <- file.path(set4dvm$path, set4dvm$filename)