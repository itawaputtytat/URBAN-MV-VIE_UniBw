set4proc <- c()
set4proc$objname <- "can_sxx_dist_m_rnd1_rb.intrpl.cut"
set4proc$sxx <- 14
set4proc$groupby <- c("sxx", "sxx_dist_m_rnd1")
set4proc$plot <- T
set4proc$plotdev <- T
set4proc$plotmap <- T
set4proc$plotmap_zoom <- 19
set4proc$smooth_gps$loess_span <- 1/10
set4proc$smooth_gps$degree <- 1 ## 1 is enough, see s14
set4proc$row4origin <- 501
set4proc$colname4gps_lon_conv <- "gps_lon_conv"
set4proc$colname4gps_lat_conv <- "gps_lat_conv"
set4proc$seqlength <- 3 ## Must be uneven
set4proc$xlim <- c(-50, 50)
set4proc$ylim <- c(-50, 50)

test <- computeSmoothGPSMedian(set4proc$sxx, set4proc, pause = T)
test_xyconv <- convertGPS2XYDistances(test, set4proc)
test_curv <- computeCurvature(test_xyconv, set4proc)

plotcurv <-
  ggplot() +
  geom_path(data = test_curv,
            aes_string(x = set4proc$colname4gps_lon_conv,
                       y = set4proc$colname4gps_lat_conv),
            size = 2) +
  geom_path(data = test_curv,
            aes_string(x = set4proc$colname4gps_lon_conv,
                       y = set4proc$colname4gps_lat_conv),
            #alpha = curv.rollmean),
            alpha = ( ( curv - min(curv) ) / ( max(curv) - min(curv) ) )^(1/3.5),
            #alpha = test.z3),
            colour = "yellow",
            #alpha = dat2plottest$curv.adj.rollmean.z,
            size = 2) +
  guides(alpha = F) +
  coord_cartesian(xlim = set4proc$xlim,
                  ylim = set4proc$ylim)
plot(plotcurv)