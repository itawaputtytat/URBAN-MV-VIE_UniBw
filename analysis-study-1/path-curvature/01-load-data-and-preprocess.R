writeSelfAsLog("seq4query")

# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_adtf_dist_m_rnd1_full"
#set4query$sxx   <- c(1:2, 4:18)
#set4query$sxx   <- c(1:2, 5)
set4query$sxx   <- c(5)
set4query$round <- c("intro", "normal", "stress")
set4query$subject <- c(1:31)
set4query$distvar <- "dist_m_rnd1"
set4query$dist1 <- -50
set4query$dist2 <- 25
set4query$distbuffer <- 50
set4query$save2df_prefix <- "adtf"
set4query$var_session <-
  c("subject_id",
    "round_txt",
    "gps_lat",
    "gps_lon")
set4query$var_sxx <-
  c("_dist_s",
    "_dist_m_rnd1")
set4query$var_data <-
  c("steerangle_deg",
    "speed_kmh")



# Data processing ---------------------------------------------------------

dbGetQuery_batch("dbconn_study1", set4query, rb = T)
intrpldf_batch4rb(adtf_sxx_dist_m_rnd1_rb, suffix = ".intrpl", outputFlag = T)
corrPosAnom_batch4rb(adtf_sxx_dist_m_rnd1_rb.intrpl)
cut2dist_batch4rb(adtf_sxx_dist_m_rnd1_rb.intrpl, "sxx_dist_m_rnd1", set4query$dist1, set4query$dist2)

#computeVar_dist2steermax("can", "dist_m_rnd1")



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
objlist <- findObjNames(c("adtf", "cut"))

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
