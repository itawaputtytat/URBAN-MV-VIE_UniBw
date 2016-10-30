
# Save workspace ----------------------------------------------------------

## Path and name settings
filepath <-
  file.path("analysis-study-1",
            "160203_gps-paths-cruvature-and-velocity-models")
filename <- "can_s01-s18_dist_m_rnd1_intrpl__dist_-50-to-50_intro-normal-stress__gps.Rdata"

## Processing of path and name
datetoday <- format(Sys.time(), "%y%m%d")
filename <- paste(datetoday, filename, sep = "_")
filepath <- file.path(filepath, filename)

## Find object names
objlist <- objectNamesFinder(txt2include = "can", txt2exclude = "rbound")

## Save objects
save(list = objlist, file = filepath)



# Load data ---------------------------------------------------------------

## Path and name settings
filepath <-
  file.path("analysis-study-1",
            "path-curvature")
filename <- "160203_can_s01-s18_dist_m_rnd1_intrpl__dist_-50-to-50_intro-normal-stress__gps.Rdata"

## Load objects
load(file.path(filepath, filename))



