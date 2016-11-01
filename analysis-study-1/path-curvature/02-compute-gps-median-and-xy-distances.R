
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$objname <- "can_sxx_dist_m_rnd1.intrpl.cut.rb"
set4proc$sxx <- 1
set4proc$plot <- T
set4proc$smooth_gps$loess_span <- 1/10
set4proc$smooth_gps$degree <- 1
set4proc$row4origin <- 501
set4proc$xlim <- c(-50, 50)
set4proc$ylim <- c(-50, 50)
set4proc$kwidth <- 75



# Compute GPS path median -------------------------------------------------

dat4med <- 
  get(set4proc$objname) %>% 
  filter(sxx == set4proc$sxx) %>% 
  group_by(sxx, sxx_dist_m_rnd1) %>%
  summarise(gps_lon_med = median(gps_long),
            gps_lat_med = median(gps_lat)) %>%
  data.frame()



# Visualise GPS path median -----------------------------------------------

if(set4proc$plot == T)
  plot(dat4med$gps_lon_med, dat4med$gps_lat_med, type = "l")



# Smooth GPS path ---------------------------------------------------------

## Smooth GPS-variables seperatly! 
## Reason: values can't be projected as real smoothing function

## Compute model and predict values for longitudinal GPS values
model <-
  loess(dat4med$gps_lon_med ~ c(1:length(dat4med$gps_lon_med)),
        span = set4proc$smooth_gps$loess_span, 
        degree = set4proc$smooth_gps$degree)

gps_lon_med_smooth <- predict(model, c(1:length(dat4med$gps_lon_med)))

## Compute model and predict values for lateral GPS values
model <-
  loess(dat4med$gps_lat_med ~ c(1:length(dat4med$gps_lat_med)),
        span = set4proc$smooth_gps$loess_span, 
        degree = set4proc$smooth_gps$degree)

gps_lat_med_smooth <- predict(model, c(1:length(dat4med$gps_lat_med)))



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



# Convert GPS data to xy distances ----------------------------------------

gps_lonlat_med_smooth_xyconv <-
  convertGPS2XYDistances(gps_lon_med_smooth, 
                         gps_lat_med_smooth, 
                         set4proc$row4origin)



# Merge data and new values -----------------------------------------------

dat4med <- 
  cbind(dat4med,
        gps_lon_med_smooth,
        gps_lat_med_smooth,
        gps_lon_med_smooth_xyconv = 
          gps_lonlat_med_smooth_xyconv$gps_lon_conv,
        gps_lat_med_smooth_xyconv = 
          gps_lonlat_med_smooth_xyconv$gps_lat_conv)



# Filter data (for special cases) -----------------------------------------

if(set4proc$sxx == 9)
  dat4med <- dat4med %>% filter(gps_lat_med_smooth_xyconv > -35)

# if(set4proc$sxx == 14) {
#   dat4med <-
#     data.frame(
#       gps_lon_med_smooth_xyconv =
#         rollmean(dat4med$gps_lon_med_smooth_xyconv, set4proc$kwidth),
#       gps_lat_med_smooth_xyconv =
#         rollmean(dat4med$gps_lat_med_smooth_xyconv, set4proc$kwidth))
# } else {
#   dat4med <-
#     data.frame(gps_lon.dist.rollavg = dat4med$gps_lon_med_smooth_xyconv,
#                gps_lat.dist.rollavg = dat4med$gps_lat_med_smooth_xyconv)
# }



# Visualise xy-distance ---------------------------------------------------

if(set4proc$plot == T) 
  plot(gps_lonlat_med_smooth_xyconv$gps_lon_conv,
       gps_lonlat_med_smooth_xyconv$gps_lat_conv, 
       type = "l", xlim = set4proc$xlim, ylim = set4proc$ylim)

