pckgs <- c(
  "RPostgreSQL",
  "dplyr",
  "tidyr",
  "data.table",
  "zoo",
  "LICORS",
  "dtw",
  "TSclust",
  "kml",
  "psych",
  "lazyeval",
  "gRain",
  "devtools",
  "ggplot2",
  "grid",
  "gridExtra",
  "ggmap"
)

install.packages(pckgs)
devtools::install_github("magrai/puttytat4R")

## gRain requires RBGL
source("https://bioconductor.org/biocLite.R")
biocLite("RBGL")

 ##Additional build tools must be installed (RBuildTools)