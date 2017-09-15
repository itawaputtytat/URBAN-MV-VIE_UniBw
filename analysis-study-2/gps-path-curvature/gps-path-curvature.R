
# Preparatory settings ----------------------------------------------------

sett_proc <- c()
sett_proc$objname <- "study1_t_adtf_pxx_full_dti_rnd1_intrpld_cut"
sett_proc$pxx <- 4
sett_proc$groupby <- c("pxx", "pxx_dti_m_rnd1")
sett_proc$plot <- T
sett_proc$plotdev <- T
sett_proc$plotmap <- T
sett_proc$plotmap_zoom <- 19
sett_proc$smooth_gps$loess_span <- 1/10
sett_proc$smooth_gps$degree <- 1 ## 1 is enough, see s14
sett_proc$row4origin <- 1
sett_proc$xlim <- c(-50, 50)
sett_proc$ylim <- c(-50, 50)
sett_proc$kwidth <- 75



# Compute GPS path median -------------------------------------------------

dat_gps_path <- 
  get(sett_proc$objname) %>% 
  filter(pxx == sett_proc$pxx) %>% 
  filter(pxx_dti_m_rnd1 >= -20) %>% 
  group_by_(.dots = lapply(sett_proc$groupby, as.symbol)) %>%
  summarise_(.dots = 
               c(setNames(list(interp(~ median(v), v = as.name("gps_lon"))),
                          "gps_lon_med"),
                 setNames(list(interp(~ median(v), v = as.name("gps_lat"))),
                          "gps_lat_med"))) %>% 
  data.frame()



# Visualise GPS path median -----------------------------------------------

if(sett_proc$plot == T)
  ggplot() + 
  geom_path(data = dat_gps_path,
            aes(x = gps_lon_med, 
                y = gps_lat_med))


dat_gps_path <- 
  get(sett_proc$objname) %>% 
  filter(pxx == sett_proc$pxx) %>% 
  #filter(passing == "p02_t1_50_s01") %>% 
  filter(passing == "p04_intro_s22") %>% 
  filter(pxx_dti_m_rnd1 >= -20) %>% 
  group_by(passing) %>% 
  mutate(gps_lon_rollavg = rollAvg(gps_lon, k = 3),
         gps_lat_rollavg = rollAvg(gps_lat, k = 3))

ggplot() + 
  geom_path(data = dat_gps_path %>% filter(passing == "p04_intro_s22"),
            aes(x = gps_lon, 
                y = gps_lat),
            colour = "grey85") +
  geom_path(data = dat_gps_path %>% filter(passing == "p04_intro_s22"),
            aes(x = gps_lon_rollavg, 
                y = gps_lat_rollavg,
                group = passing),
            colour = "red")


sett_proc$rollavg_k = 4
# 
# rollAvg <- function(dat, k = 3, avg_trunc = T) {
#   
#   dat_rollavg <- dat
#   
#   ## Implementation of moving average with filling truncated areas
#   ## First value remains the same
#   ## Second to kth value is truncated, and averaged over values < k
#   if (avg_trunc) {
#     for (i in 2:k) {
#       dat_rollavg[i] <- mean(dat_rollavg[1:i])
#     }
#   }
#   
#   for(i in (k+1):length(dat) ) {
#     dat_rollavg[i] <- mean(dat_rollavg[(i-k):i])
#   }
#   return(dat_rollavg)
# }
# 
# 
# ## Remove truncated areas
# engine01_smooth <- engine01_smooth[(k+2):nrow(engine01),]
# head(engine01_smooth, 20
# 
# dat_gps_path$gps_lon_med_rollavg
# 
# 
# 
# #dat_gps_path$gps_lat_med_rollavg
# 
# )
# 
# 


# Smooth GPS path ---------------------------------------------------------

## Smooth GPS-variables seperatly!
## Reason: values can't be projected as real smoothing function

# Predict values
model_gps_lon_smooth_loess <- 
  smoothWithLoess(dat_gps_path$gps_lon_med, 
                  sett_proc$smooth_gps$loess_span, 
                  sett_proc$smooth_gps$degree)

dat_gps_path$gps_lon_med_smoothed <- model_gps_lon_smooth_loess$fitted

model_gps_lat_smooth_loess <- 
  smoothWithLoess(dat_gps_path$gps_lat_med, 
                  sett_proc$smooth_gps$loess_span, 
                  sett_proc$smooth_gps$degree)

dat_gps_path$gps_lat_med_smoothed <- model_gps_lat_smooth_loess$fitted

dat_gps_path$gps_lat_med_rollmean <- rollmean


# Visualise smooth gps path -----------------------------------------------

if(sett_proc$plot == T) {
  
  ggplot() + 
    geom_path(data = dat_gps_path,
              aes(x = gps_lon_med, 
                  y = gps_lat_med)) + 
    geom_path(data = dat_gps_path,
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
  
  filepath <- file.path("resources/study-2/map-images")
  filelist <- list.files(filepath)
  filelist <- filelist[grepl("RData", filelist)]
  filelist <- filelist[grepl("zoom19", filelist)]
  filename <- filelist[grepl(sprintf("s%02d", sett_proc$pxx), filelist)]
  load(file.path(filepath, filename))
  
  testplot <- 
    ggmap(map) + 
    geom_path(data = dat_gps_path,
              aes(x = gps_lon_med_smoothed, 
                  y = gps_lat_med_smoothed),
              colour = "red",
              size = 1.5) +
    geom_path(data = dat_gps_path,
              aes(x = gps_lon_med, 
                  y = gps_lat_med),
              colour = "yellow")
  
  plot(testplot)
}


testplot + 
  geom_hline(yintercept = 48.077003) + 
  geom_vline(xintercept = 11.641311) + 
  geom_path(data = dat_pedal %>% filter(pxx == 2),
            aes(x = gps_lon,
                y = gps_lat,
                group = passing),
            colour = "white")


# Convert GPS data to xy distances ----------------------------------------

dat_gps_med_smoothed_xyconv <-
  convertGPS2XYDistances(dat_gps_path$gps_lon_med_smooth,
                         dat_gps_path$gps_lat_med_smoothed,
                         sett_proc$row4origin,
                         sett_proc$plot)



# # Merge data and new values -----------------------------------------------

dat4med <-
  cbind(dat_gps_path,
        gps_lon_smooth_xyconv =
          dat_gps_med_smoothed_xyconv$gps_lon_conv,
        gps_lat_smooth_xyconv =
          dat_gps_med_smoothed_xyconv$gps_lat_conv)