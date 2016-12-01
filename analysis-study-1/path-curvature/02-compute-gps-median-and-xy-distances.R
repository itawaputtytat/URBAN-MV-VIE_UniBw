
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$objname <- "adtf_sxx_dist_m_rnd1_rb.intrpl.cut"
set4proc$sxx <- 4
set4proc$groupby <- c("sxx", "sxx_dist_m_rnd1")
set4proc$plot <- T
set4proc$plotdev <- T
set4proc$plotmap <- T
set4proc$plotmap_zoom <- 19
set4proc$smooth_gps$loess_span <- 1/10
set4proc$smooth_gps$degree <- 1 ## 1 is enough, see s14
set4proc$row4origin <- 501
set4proc$xlim <- c(-50, 50)
set4proc$ylim <- c(-50, 50)
set4proc$kwidth <- 75



# Compute GPS path median -------------------------------------------------
 
## Remove cases with GPS anomalies
## (might have incorrect GPS interpolation)
cases2corr <-
  dbGetSrc("dbconn_study1", "v_steerangle_pos_correction") %>%
  filter(grepl("correction", consequence))

cases2corr <-
  paste(paste(sprintf("s%02d", cases2corr$sxx),
              cases2corr$round_txt,
              sprintf("subject%02d", cases2corr$subid), sep = "_"))

dat4med <-
  get(set4proc$objname) %>%
  filter(!passing %in% cases2corr)

dat4med <- 
  dat4med %>% 
  group_by_(.dots = lapply(set4proc$groupby, as.symbol)) %>%
  summarise(gps_lon_med = median(gps_lon),
            gps_lat_med = median(gps_lat)) %>%
  data.frame()

# if (set4proc$sxx == 9) 
#   dat4med <- 
#   get(set4proc$objname) %>% 
#   filter(sxx == set4proc$sxx) %>% 
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
#          filter(sxx == set4proc$sxx)) +
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

if(set4proc$plot == T)
  ggplot() + 
  geom_path(data = dat4med,
            aes(x = gps_lon_med, 
                y = gps_lat_med))



# Smooth GPS path ---------------------------------------------------------

## Smooth GPS-variables seperatly!
## Reason: values can't be projected as real smoothing function

dat4model_lon_med <- dat4med$gps_lon_med
dat4model_lat_med <- dat4med$gps_lat_med

if(set4proc$sxx == 9) {
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
#         span = set4proc$smooth_gps$loess_span,
#         degree = set4proc$smooth_gps$degree)
# 
# model.loess_gps_lat_med_smooth <-
#   loess(dat4model_lat_med ~ c(1:length(dat4model_lat_med)),
#         span = set4proc$smooth_gps$loess_span,
#         degree = set4proc$smooth_gps$degree)

# Predict values
model.loess_gps_lon_med_smooth <- smoothWithLoess(dat4model_lon_med, set4proc$smooth_gps$loess_span, set4proc$smooth_gps$degree)
model.loess_gps_lat_med_smooth <- smoothWithLoess(dat4model_lat_med, set4proc$smooth_gps$loess_span, set4proc$smooth_gps$degree)
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

if(set4proc$plot == T) {

  plot(dat4med$gps_lon_med,
       dat4med$gps_lat_med,
       type = "l")
  lines(gps_lon_med_smooth,
        gps_lat_med_smooth,
        col = "red",
        lwd = 2)
}



# Visualise smooth deviations from median ---------------------------------

if (set4proc$plotdev)
  plot(abs(model.loess_gps_lon_med_smooth$residuals),
       abs(model.loess_gps_lat_med_smooth$residuals), type = "l",
       xlim = c(0, 0.00001),
       ylim = c(0, 0.00001))
plot(abs(model.loess_gps_lon_med_smooth$residuals), type = "l")
plot(abs(model.loess_gps_lat_med_smooth$residuals), type = "l")




# Visualise results on map image ------------------------------------------

if (set4proc$plotmap) {
  
  filepath <- file.path("ressources/study1/map-images")
  filelist <- list.files(filepath)
  filelist <- filelist[grepl("RData", filelist)]
  filelist <- filelist[grepl("zoom19", filelist)]
  filename <- filelist[grepl(sprintf("s%02d", set4proc$sxx), filelist)]
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
                         set4proc$row4origin,
                         set4proc$plot)



# # Merge data and new values -----------------------------------------------

dat4med <-
  cbind(dat4med,
        gps_lon_med_smooth,
        gps_lat_med_smooth,
        gps_lon_med_smooth_xyconv =
          gps_med_smooth_xyconv$gps_lon_conv,
        gps_lat_med_smooth_xyconv =
          gps_med_smooth_xyconv$gps_lat_conv)