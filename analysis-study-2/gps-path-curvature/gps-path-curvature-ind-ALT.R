
# ALT ---------------------------------------------------------------------






dat2

convertGPS2XYDistances(dat_gps_path_median$gps_lon_med_smooth,
                       dat_gps_path_median$gps_lat_med_smoothed,
                       sett_proc$row4origin,
                       sett_proc$plot)
















sett_proc$row4origin <- round(nrow(dat_gps_path_median)/2)

# Visualise GPS path median -----------------------------------------------

# s = wheel base 
# a = steering wheel ange
# n = steering ratio (e.g. for 16:1, n = 16) 
# r = radius of curvature, in the same units as the wheel base
# https://www.physicsforums.com/threads/steering-wheel-angle-radius-of-curvature.59881/
# r = s / (sqrt(2 - 2 * cos(2*a/n))
s <- 2843
n = 16

## Compute curvature
#https://stackoverflow.com/questions/27614840/curve-recontruction-using-velocity-and-curvature

dat_gps_path <- 
  get(sett_proc$objname) %>% 
  filter(pxx == sett_proc$pxx) %>% 
  filter(passing == "p04_stress_s04") 

ggplot() + 
  geom_line(data = dat_gps_path,
            aes(x = gps_lon,
                y = gps_lat,
                group = passing)) +
  geom_point(data = dat_gps_path %>% filter(pxx_dist_m_rnd1 == -60),
             aes(x = gps_lon,
                 y = gps_lat,
                 group = passing),
             colour = "red")

ggplot() + 
  geom_line(data = dat_gps_path,
            aes(x = pxx_dist_m_rnd1,
                y = yaw_rate_degs,
                group = passing))

dat_gps_path <- 
  dat_gps_path %>%
  mutate(yaw_rate_rads = yaw_rate_degs * pi/180) %>% 
  mutate(curvature = abs(yaw_rate_rads)/(speed_kmh*3.6) ) %>% 
  mutate(curvature_rollavg = rollAvg(curvature, k = 2))

ggplot() + 
  geom_line(data = dat_gps_path,
            aes(x = pxx_dist_m_rnd1,
                y = curvature,
                group = passing)) + 
  geom_line(data = dat_gps_path,
            aes(x = pxx_dist_m_rnd1,
                y = curvature_rollavg,
                group = passing),
            colour = "red")



ggplot() + 
  geom_line(data = dat_gps_path,
            aes(x = gps_lon,
                y = gps_lat,
                group = passing,
                colour = ""),
            size = 2)




plot(curv)





test <- 
  dat_gps_path %>% 
  filter(pxx_dist_m_rnd1 == min(pxx_dist_m_rnd1)) %>% 
  mutate(gps_lon_quant25 = quantile(gps_lon, probs = 0.25)) %>% 
  select(passing, gps_lon_quant25) 

test$gps_lon_quant25 <- codeOutliersZ(test$gps_lon_quant25)

dat_gps_path <- 
  dat_gps_path %>% 
  filter(!round_txt %in% "intro") %>% 
  group_by(passing) %>% 
  mutate(gps_lon = ifelse(gps_lon >= min(test$gps_lon_quant25, na.rm = T), gps_lon, NA) ) %>% 
  mutate(gps_lon_rollavg = rollAvg(gps_lon, k = 4, avg_trunc = T),
         gps_lat_rollavg = rollAvg(gps_lat, k = 4, avg_trunc = T))

ggplot() + 
  geom_path(data = dat_gps_path,
            aes(x = gps_lon,
                y = gps_lat,
                group = passing),
            size = 1.5) + 
  geom_path(data = dat_gps_path,
            aes(x = gps_lon_rollavg, 
                y = gps_lat_rollavg,
                group = passing),
            col = "red",
            size = 2.5,
            alpha = 0.5)



# Smooth GPS path ---------------------------------------------------------

## Smooth GPS-variables seperatly!
## Reason: values can't be projected as real smoothing function

# Predict values
model_gps_lon_smooth_loess <- 
  smoothWithLoess(dat_gps_path_median$gps_lon_med, 
                  sett_proc$smooth_gps$loess_span, 
                  sett_proc$smooth_gps$degree)

dat_gps_path_median$gps_lon_med_smoothed <- model_gps_lon_smooth_loess$fitted

model_gps_lat_smooth_loess <- 
  smoothWithLoess(dat_gps_path_median$gps_lat_med, 
                  sett_proc$smooth_gps$loess_span, 
                  sett_proc$smooth_gps$degree)

dat_gps_path_median$gps_lat_med_smoothed <- model_gps_lat_smooth_loess$fitted

#dat_gps_path_median$gps_lat_med_rollmean <- rollmean


# Visualise smooth gps path -----------------------------------------------

if(sett_proc$plot == T) {
  
  ggplot() + 
    geom_path(data = dat_gps_path_median,
              aes(x = gps_lon_med, 
                  y = gps_lat_med)) + 
    geom_path(data = dat_gps_path_median,
              aes(x = gps_lon_med_smoothed, 
                  y = gps_lat_med_smoothed),
              colour = "red")
}



# Visualise smooth deviations from median ---------------------------------

if (sett_proc$plotdev) {
  
  ## Settings for three plots
  par(mfrow = c(3, 1))
  
  plot(abs(model_gps_lon_smooth_loess$residuals),
       abs(model_gps_lat_smooth_loess$residuals), type = "l",
       xlim = c(0, 0.00001),
       ylim = c(0, 0.00001))
  plot(abs(model_gps_lon_smooth_loess$residuals), type = "l")
  plot(abs(model_gps_lat_smooth_loess$residuals), type = "l")
  
  ## Reset of plot settings
  par(mfrow = c(1, 1)) 
  
}

## BREMS-VERHALTEN zu RADIUS


# Visualise results on map image ------------------------------------------

if (sett_proc$plotmap) {
  
  filepath <- file.path("resources/study1/map-images")
  filelist <- list.files(filepath)
  filelist <- filelist[grepl("RData", filelist)]
  filelist <- filelist[grepl("zoom19", filelist)]
  filename <- filelist[grepl(sprintf("s%02d", sett_proc$pxx), filelist)]
  load(file.path(filepath, filename))
  
  testplot <- 
    ggmap(map) + 
    # geom_path(data = dat_gps_path_median,
    #           aes(x = gps_lon_med_smoothed, 
    #               y = gps_lat_med_smoothed),
    #           colour = "red",
    #           size = 1.5) +
    geom_path(data = dat_gps_median,
              aes(x = gps_lon_med, 
                  y = gps_lat_med),
              colour = "yellow")
  
  plot(testplot)
}





## TEST TRI-ANG



# testplot + 
#   geom_hline(yintercept = 48.077003) + 
#   geom_vline(xintercept = 11.641311) + 
#   geom_path(data = dat_pedal %>% filter(pxx == 2),
#             aes(x = gps_lon,
#                 y = gps_lat,
#                 group = passing),
#             colour = "white")


# Convert GPS data to xy distances ----------------------------------------

dat_gps_med_smoothed_xyconv <-
  convertGPS2XYDistances(dat_gps_path_median$gps_lon_med_smooth,
                         dat_gps_path_median$gps_lat_med_smoothed,
                         sett_proc$row4origin,
                         sett_proc$plot)



# # Merge data and new values -----------------------------------------------

dat4med <-
  cbind(dat_gps_path_median,
        gps_lon_smooth_xyconv =
          dat_gps_med_smoothed_xyconv$gps_lon_conv,
        gps_lat_smooth_xyconv =
          dat_gps_med_smoothed_xyconv$gps_lat_conv)