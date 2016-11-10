set4proc <- c()
set4proc$objname <- "can_sxx_dist_m_rnd1_rb.intrpl.cut"
set4proc$sxx <- 9
set4proc$groupby <- c("sxx", "sxx_dist_m_rnd1")
set4proc$plot <- F
set4proc$plotdev <- F
set4proc$plotmap <- F
set4proc$plotmap_zoom <- 19
set4proc$smooth_gps$loess_span <- 1/4
set4proc$smooth_gps$degree <- 2 ## 1 is enough, see sxx 8, 14
set4proc$row4origin <- 501
set4proc$xlim <- c(-50, 50)
set4proc$ylim <- c(-50, 50)
set4proc$colname4gps_lon_conv <- "gps_lon_conv"
set4proc$colname4gps_lat_conv <- "gps_lat_conv"
set4proc$smooth_r$loess_span <- 1/10
set4proc$smooth_r$degree <- 1
set4proc$rfilter <- 100
set4proc$seqlength <- 75 ## Must be uneven
set4proc$rollmean_k <- NA

set4proc$plot <- T
set4proc$plotmap <- T
set4proc$rollmean_k <- NA
test <- computeSmoothGPSMedian(set4proc$sxx, set4proc, pause = T)
test_xyconv <- convertGPS2XYDistances(test, set4proc)
set4proc$rollmean_k <- NA
set4proc$plot <- T
test_curv <- computeCurvature(test_xyconv, set4proc)


mapdat <- getMapImage(set4proc$sxx, zoom = set4proc$plotmap_zoom) 

plotcurv <-
  ggplot() +
  geom_path(data = test_curv,
            aes_string(x = set4proc$colname4gps_lon_conv,
                       y = set4proc$colname4gps_lat_conv),
            size = 2) +
  geom_path(data = test_curv,
            aes_string(x = set4proc$colname4gps_lon_conv,
                       y = set4proc$colname4gps_lat_conv,
                       #alpha = "curv_norm"),
            #alpha = curv.rollmean),
            alpha = "( ( curv - min(curv) ) / ( max(curv) - min(curv) ) )" ),
            #alpha = test.z3),
            colour = "yellow",
            #alpha = dat2plottest$curv.adj.rollmean.z,
            size = 2) +
  guides(alpha = F) +
  coord_cartesian(xlim = set4proc$xlim,
                  ylim = set4proc$ylim)




test2 <- data.frame(cbind(test, curv_norm = test_curv$curv_norm))

plotcurv <-
  ggmap(mapdat,
        darken = c(0.25, "white")) +
  geom_path(data = test,
            aes(x = gps_lon,
                y = gps_lat),
            size = 2) +
  geom_path(data = test2,
            aes_string(x = "gps_lon",
                       y = "gps_lat",
                       alpha = "curv_norm"),
            #alpha = curv.rollmean),
            # alpha = ( ( curv - min(curv) ) / ( max(curv) - min(curv) ) )^(1/3.5),
            #alpha = test.z3),
            colour = "yellow",
            #alpha = dat2plottest$curv.adj.rollmean.z,
            size = 2) +
  guides(alpha = F)

pauseAndContinue()

plot(plotcurv)


# 5: 1/10; 2
# 7: 1/10: 2
# 10: 1/3; 2
# 11: 1/10
# 13: 1/10
# 14: 1/3; 2
# 15: 1/10
# 18 1/4; 2