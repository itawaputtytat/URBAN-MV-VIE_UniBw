
# Preparatory settings ----------------------------------------------------

## Processing
sett_proc <- c()
sett_proc$objname <- "dat_study1_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
sett_proc$pxx <- 9
sett_proc$group_by <- c("pxx", "dti_m_rnd1")
sett_proc$plot <- T
sett_proc$plot_residuals <- T
sett_proc$plot_map <- T
sett_proc$plot_map_zoom <- 19
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



# Create data set ---------------------------------------------------------

dat <- 
  get(sett_proc$objname) %>% 
  filter(pxx %in% sett_proc$pxx)


# ggmap(getMapImage(sett_proc$pxx, zoom = 19)) +
  # geom_path(data = dat %>%
  #             filter(subject_id != 2 & round_id != "normal") %>%
  #             filter(subject_id != 26 & !round_id %in% c("normal", "stress")) %>% 
  #             # mutate(gps_lon = ifelse(dti_m_rnd1 <= 0 & gps_lon < 11.64993, 11.64993, gps_lon)) %>%
  #             # mutate(gps_lat = ifelse(dti_m_rnd1 <= 0 & gps_lat < 48.10323, 48.10323, gps_lat)),
  #           mutate(gps_lon = ifelse(dti_m_rnd1 <= 0 & gps_lon < 11.64993, 11.64985, gps_lon)),
  #           #mutate(gps_lat = ifelse(dti_m_rnd1 <= 0 & gps_lat < 48.10323, 48.10323, gps_lat)),
  #           aes(x = gps_lon, y = gps_lat, group = "subject_id"),
  #           colour = "red") +
  # geom_vline(xintercept = 11.64985, color = "white") 

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
  dat %>%
  filter(!passing %in% cases_to_rm)



# Compute GPS path median -------------------------------------------------

## Special case: Intersection nr. 9
if (sett_proc$pxx != 9) {
  
  dat_med <- 
    dat %>% 
    group_by_(.dots = lapply(sett_proc$group_by, as.symbol)) %>%
    summarise(gps_lon_med = median(gps_lon),
              gps_lat_med = median(gps_lat)) %>%
    data.frame()
  
} else {
  
  dat_med <-
    dat %>%
    # Following filters were results from exact data exploration
    # But there is no better solution
    filter(subject_id != 2 & round_id != "normal") %>%
    filter(subject_id != 26 & !round_id %in% c("normal", "stress")) %>%
    # mutate(gps_lon = ifelse(dti_m_rnd1 <= 0 & gps_lon < 11.64993, 11.64993, gps_lon)) %>%
    # mutate(gps_lat = ifelse(dti_m_rnd1 <= 0 & gps_lat < 48.10323, 48.10323, gps_lat)) %>%
    mutate(gps_lon = ifelse(dti_m_rnd1 <= 0 & gps_lon < 11.64991, 11.64991, gps_lon)) %>%
    mutate(gps_lat = ifelse(dti_m_rnd1 <= 0 & gps_lat < 48.1031, 48.1031, gps_lat)) %>%
    group_by_(.dots = lapply(sett_proc$group_by, as.symbol)) %>%
    summarise(gps_lon_med = median(gps_lon),
              gps_lat_med = median(gps_lat)) %>%
    data.frame()
  
}



# Visualise GPS path median -----------------------------------------------

if (sett_proc$plot == T) {
  
  plot_gps_med <- 
    ggplot() + 
    geom_path(data = dat_med,
              aes(x = gps_lon_med, 
                  y = gps_lat_med)) +
    facet_grid(.~pxx, scales = "free")

  plot(plot_gps_med)
}
  


# Smooth GPS path ---------------------------------------------------------

## Smooth GPS-variables seperatly!
## Reason: values can't be projected as real smoothing function

dat_model_lon_med <- dat_med$gps_lon_med
dat_model_lat_med <- dat_med$gps_lat_med

# if(sett_proc$pxx == 9) {
#   #dat_model_lon_med <- dat_model_lon_med[224:length(dat_model_lon_med)]
#   dat_model_lon_med <- dat_model_lon_med[227:length(dat_model_lon_med)]
#   dat_model_lat_med <- dat_model_lat_med[227:length(dat_model_lat_med)]
# 
#   ## See figures for better understanding
#   #plot(dat_model_lat_med[226:250], type = "l", ylim = c(48.1029, 48.1031))
#   #plot(dat_model_lat_med[227:250], type = "l", ylim = c(48.1029, 48.1031))
#   #plot(dat_model_lon_med[223:250], type = "l", ylim = c(11.6498, 11.65))
#   #plot(dat_model_lon_med[224:250], type = "l", ylim = c(11.6498, 11.65))
# }

# Compute model
# model.loess_gps_lon_med_smooth <-
#   loess(dat_model_lon_med ~ c(1:length(dat_model_lon_med)),
#         span = sett_proc$smooth_gps$loess_span,
#         degree = sett_proc$smooth_gps$degree)
# 
# model.loess_gps_lat_med_smooth <-
#   loess(dat_model_lat_med ~ c(1:length(dat_model_lat_med)),
#         span = sett_proc$smooth_gps$loess_span,
#         degree = sett_proc$smooth_gps$degree)

# Predict values
model.loess_gps_lon_med_smooth <- smoothWithLoess(dat_model_lon_med, sett_proc$smooth_gps$loess_span, sett_proc$smooth_gps$degree)
model.loess_gps_lat_med_smooth <- smoothWithLoess(dat_model_lat_med, sett_proc$smooth_gps$loess_span, sett_proc$smooth_gps$degree)
gps_lon_med_smooth <- model.loess_gps_lon_med_smooth$fitted
gps_lat_med_smooth <- model.loess_gps_lat_med_smooth$fitted
# gps_lon_med_smooth <-
#   predict(model.loess_gps_lon_med_smooth,
#           c(1:length(dat$gps_lon_med)))
# 
# gps_lat_med_smooth <-
#   predict(model.loess_gps_lat_med_smooth,
#           c(1:length(dat$gps_lat_med)))



# Visualise smooth gps path -----------------------------------------------

if(sett_proc$plot == T) {

  plot(dat_med$gps_lon_med,
       dat_med$gps_lat_med,
       type = "l")
  lines(gps_lon_med_smooth,
        gps_lat_med_smooth,
        col = "red",
        lwd = 2)
}



# Visualise smooth deviations from median ---------------------------------

if (sett_proc$plot_residuals) {
  
  dat_temp <- 
    data.frame(
      gps_lon_res = model.loess_gps_lon_med_smooth$residuals,
      gps_lat_res = model.loess_gps_lat_med_smooth$residuals
    )
  
  plot_gps_res <- 
    ggplot() + 
    geom_path(data = dat_temp,
              aes(x = gps_lon_res,
                  y = gps_lat_res)) + 
    coord_cartesian(xlim = c(-0.000005, 0.000005),
                    ylim = c(-0.000005, 0.000005))
  
  plot_gps_lon_res <- 
    ggplot() + 
    geom_line(data = dat_temp,
              aes(x = 1:nrow(dat_temp),
                  y = gps_lon_res))
  
  plot_gps_lat_res <- 
    ggplot() + 
    geom_line(data = dat_temp,
              aes(x = 1:nrow(dat_temp),
                  y = gps_lat_res))
  
  plot_gps_res_coll <- 
    arrangeGrob(plot_gps_res,
                 plot_gps_lon_res,
                 plot_gps_lat_res,
                 nrow = 3)
  
  plot(plot_gps_res_coll)
}



# Visualise results on map image ------------------------------------------

if (sett_proc$plot_map) {
  
    plot_map <- 
      ggmap(getMapImage(sett_proc$pxx)) + 
      geom_path(data = data.frame(lon = gps_lon_med_smooth, lat = gps_lat_med_smooth),
                aes(x = lon, y = lat),
                colour = "red",
                size = 1.5) +
      geom_path(data = dat_med,
                aes(x = gps_lon_med, y = gps_lat_med),
                colour = "yellow")
    
    plot(plot_map)
}



# Convert GPS data to xy distances ----------------------------------------

gps_med_smooth_xyconv <-
  convertGPS2XYDistances(gps_lon_med_smooth,
                         gps_lat_med_smooth,
                         sett_proc$row4origin,
                         sett_proc$plot)



# # Merge data and new values -----------------------------------------------

dat_med <-
  cbind(dat_med,
        gps_lon_med_smooth,
        gps_lat_med_smooth,
        gps_lon_med_smooth_xy =
          gps_med_smooth_xyconv$gps_lon_conv,
        gps_lat_med_smooth_xy =
          gps_med_smooth_xyconv$gps_lat_conv)