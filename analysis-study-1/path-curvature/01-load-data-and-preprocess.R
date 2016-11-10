writeSelfAsLog("seq4query")

# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_can_full_aggr_dist_m_rnd1_max_dist2sxx_v2"
set4query$sxx   <- c(1:2, 4:18)
set4query$round <- c("intro", "normal", "stress")
set4query$subid <- c(1:31)
set4query$distvar <- "dist_m_rnd1"
set4query$dist1 <- -50
set4query$dist2 <- 50
set4query$distpuffer <- 50
set4query$save2df_prefix <- "can"
set4query$var_session <-
  c("subid",
    "round_txt",
    "time_s",
    "dist_m_rnd1",
    "gps_lat",
    "gps_long")
set4query$var_sxx <-
  c("_dist_s_rnd1",
    "_dist_m_rnd1")
set4query$var_data <-
  c("steerangle_deg",
    "speed_kmh")



# Data processing ---------------------------------------------------------

dbGetQuery_batch("dbconn_study1", set4query, rb = T)
intrpldf_batch4rb(can_sxx_dist_m_rnd1_rb, suffix = ".intrpl", outputFlag = T)
correctPositionAnomalies_batch4rb(can_sxx_dist_m_rnd1_rb)
cut2dist_batch4rb(can_sxx_dist_m_rnd1_rb.intrpl, "sxx_dist_m_rnd1", set4query$dist1, set4query$dist2)


computeVar_dist2steermax("can", "dist_m_rnd1")



# Save data ---------------------------------------------------------------

## Path and name settings
filepath <-
  file.path("analysis-study-1",
            "path-curvature")
filename <- "path-curvature.Rdata"

## Processing of path and name
datetoday <- format(Sys.time(), "%y%m%d_%H%M%S")
filename <- paste(datetoday, filename, sep = "_")
filepath <- file.path(filepath, filename)

## Find object names
objlist <- findObjNames(c("can", "cut"))

## Save objects
save(list = objlist, file = filepath)



# Load data ---------------------------------------------------------------

# ## Path and name settings
# filepath <-
#   file.path("analysis-study-1",
#             "path-curvature")
# filename <- "160203_can_s01-s18_dist_m_rnd1_intrpl__dist_-50-to-50_intro-normal-stress__gps.Rdata"

## Load objects
#load(file.path(filepath, filename))
load(file.choose())
