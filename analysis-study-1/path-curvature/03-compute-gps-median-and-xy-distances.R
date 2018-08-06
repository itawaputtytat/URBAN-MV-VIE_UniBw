
# Preparatory settings ----------------------------------------------------

## Processing
sett_proc <- c()
sett_proc$objname <- "dat_study1_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
sett_proc$sxx <- 4
sett_proc$group_by <- c("pxx", "dti_m_rnd1")
sett_proc$plot <- T
sett_proc$plotdev <- T
sett_proc$plotmap <- T
sett_proc$plotmap_zoom <- 19
sett_proc$smooth_gps$loess_span <- 1/10
sett_proc$smooth_gps$degree <- 1 ## 1 is enough, see s14
sett_proc$row4origin <- 501
sett_proc$xlim <- c(-50, 50)
sett_proc$ylim <- c(-50, 50)
sett_proc$kwidth <- 75

## Data
sett_query <- c()
sett_query$db_name <- "URBAN-MV-VIE_UniBw_Study-1"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name <- "t_steering_angle_outliers"



# Remove cases with GPS anomalies -----------------------------------------

## Cases with GPS anomalies might have incorrect GPS interpolations

## Get information on steering angle outliers
cases_to_rm <-
  dbGetSrc(sett_query$db_conn_name, sett_query$src_name) %>%
  filter_("is_outlier")

## Create filter for passings
cases_to_rm <-
  paste(
    sprintf("p%02d", cases_to_rm$pxx),
    cases_to_rm$round_txt,
    sprintf("s%02d", cases_to_rm$subject_id),
    sep = "_")

## Remove cases from data
dat <-
  get(sett_proc$objname) %>%
  filter(!passing %in% cases_to_rm)



# Compute GPS path median -------------------------------------------------

dat <- 
  dat %>% 
  group_by_(.dots = lapply(sett_proc$group_by, as.symbol)) %>%
  summarise(gps_lon_med = median(gps_lon),
            gps_lat_med = median(gps_lat)) %>%
  data.frame()




# if (sett_proc$sxx == 9) 
#   dat4med <- 
#   get(sett_proc$objname) %>% 
#   filter(sxx == sett_proc$sxx) %>% 
#   ## Following filters were results from exact data exploration
#   ## But there is no better solution
#   # filter(subid != 2 & round_txt != "normal") %>% 
#   # filter(subid != 26 & !round_txt %in% c("normal", "stress")) %>% 
#   # mutate(gps_<lon = ifelse(sxx_dist_m_rnd1 <= 0 & gps_lon < 11.64993, 11.64993, gps_lon)) %>% 
#   # mutate(gps_lat = ifelse(sxx_dist_m_rnd1 <= 0 & gps_lat < 48.10323, 48.10323, gps_lat)) %>% 
#   group_by(sxx, sxx_dist_m_rnd1) %>%
#   summarise(gps_lon_med = median(gps_lon),
#             gps_lat_med = median(gps_lat)) %>%
#   data.frame()

# ggplot(can_sxx_dist_m_rnd1_rb.intrpl.cut %>%
#          # filter(sxx == 9, subid %in% c(26))) +
#          filter(sxx == sett_proc$sxx)) +
#          #filter(subid != 2 & round_txt != "normal") %>%
#          #filter(subid != 26 & !round_txt %in% c("normal", "stress"))) +
#   geom_path(aes(x = gps_lon,
#                 y = gps_lat,
#                 group = subid)) +
#   facet_grid(.~round_txt)
## Insight
## normal 2
## normal stress 26



# Visualise GPS path median -----------------------------------------------

if(sett_proc$plot == T)
  ggplot() + 
  geom_path(data = dat4med,
            aes(x = gps_lon_med, 
                y = gps_lat_med))



# Smooth GPS path ---------------------------------------------------------

## Smooth GPS-variables seperatly!
## Reason: values can't be projected as real smoothing function

dat4model_lon_med <- dat4med$gps_lon_med
dat4model_lat_med <- dat4med$gps_lat_med

if(sett_proc$sxx == 9) {
  dat4model_lon_med <- dat4model_lon_med[224:length(dat4model_lon_med)]
  dat4model_lat_med <- dat4model_lat_med[227:length(dat4model_lat_med)]

  ## See figures for better understanding
  #plot(dat4model_lat_med[226:250], type = "l", ylim = c(48.1029, 48.1031))
  #plot(dat4model_lat_med[227:250], type = "l", ylim = c(48.1029, 48.1031))
  #plot(dat4model_lon_med[223:250], type = "l", ylim = c(11.6498, 11.65))
  #plot(dat4model_lon_med[224:250], type = "l", ylim = c(11.6498, 11.65))
}

# Compute model
# model.loess_gps_lon_med_smooth <-
#   loess(dat4model_lon_med ~ c(1:length(dat4model_lon_med)),
#         span = sett_proc$smooth_gps$loess_span,
#         degree = sett_proc$smooth_gps$degree)
# 
# model.loess_gps_lat_med_smooth <-
#   loess(dat4model_lat_med ~ c(1:length(dat4model_lat_med)),
#         span = sett_proc$smooth_gps$loess_span,
#         degree = sett_proc$smooth_gps$degree)

# Predict values
model.loess_gps_lon_med_smooth <- smoothWithLoess(dat4model_lon_med, sett_proc$smooth_gps$loess_span, sett_proc$smooth_gps$degree)
model.loess_gps_lat_med_smooth <- smoothWithLoess(dat4model_lat_med, sett_proc$smooth_gps$loess_span, sett_proc$smooth_gps$degree)
gps_lon_med_smooth <- model.loess_gps_lon_med_smooth$fitted
gps_lat_med_smooth <- model.loess_gps_lat_med_smooth$fitted
# gps_lon_med_smooth <-
#   predict(model.loess_gps_lon_med_smooth,
#           c(1:length(dat4med$gps_lon_med)))
# 
# gps_lat_med_smooth <-
#   predict(model.loess_gps_lat_med_smooth,
#           c(1:length(dat4med$gps_lat_med)))



# Visualise smooth gps path -----------------------------------------------

if(sett_proc$plot == T) {

  plot(dat4med$gps_lon_med,
       dat4med$gps_lat_med,
       type = "l")
  lines(gps_lon_med_smooth,
        gps_lat_med_smooth,
        col = "red",
        lwd = 2)
}



# Visualise smooth deviations from median ---------------------------------

if (sett_proc$plotdev)
  plot(abs(model.loess_gps_lon_med_smooth$residuals),
       abs(model.loess_gps_lat_med_smooth$residuals), type = "l",
       xlim = c(0, 0.00001),
       ylim = c(0, 0.00001))
plot(abs(model.loess_gps_lon_med_smooth$residuals), type = "l")
plot(abs(model.loess_gps_lat_med_smooth$residuals), type = "l")




# Visualise results on map image ------------------------------------------

if (sett_proc$plotmap) {
  
  filepath <- file.path("ressources/study1/map-images")
  filelist <- list.files(filepath)
  filelist <- filelist[grepl("RData", filelist)]
  filelist <- filelist[grepl("zoom19", filelist)]
  filename <- filelist[grepl(sprintf("s%02d", sett_proc$sxx), filelist)]
  load(file.path(filepath, filename))

    testplot <- 
      ggmap(map) + 
      geom_path(data = data.frame(lon = gps_lon_med_smooth, lat = gps_lat_med_smooth),
                aes(x = lon, y = lat),
                colour = "red",
                size = 1.5) +
      geom_path(data = dat4med,
                aes(x = gps_lon_med, y = gps_lat_med),
                colour = "yellow")
    
    plot(testplot)
}



# Convert GPS data to xy distances ----------------------------------------

gps_med_smooth_xyconv <-
  convertGPS2XYDistances(gps_lon_med_smooth,
                         gps_lat_med_smooth,
                         sett_proc$row4origin,
                         sett_proc$plot)



# # Merge data and new values -----------------------------------------------

dat4med <-
  cbind(dat4med,
        gps_lon_med_smooth,
        gps_lat_med_smooth,
        gps_lon_med_smooth_xyconv =
          gps_med_smooth_xyconv$gps_lon_conv,
        gps_lat_med_smooth_xyconv =
          gps_med_smooth_xyconv$gps_lat_conv)