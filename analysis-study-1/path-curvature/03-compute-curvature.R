
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$objname <- "dat4med"
set4proc$colname4gps_lon_conv <- "gps_lon_smooth_xyconv"
set4proc$colname4gps_lat_conv <- "gps_lat_smooth_xyconv"
set4proc$seqlength <- 3 ## Must be uneven
## Info: Resulting number of radius values will be nrow(data) - seglength
set4proc$rfilter <- 100

## Create data copy
dat4curv <- get(set4proc$objname)

## In case of duplicate values create mini-mini-mini-deviations
## Otherwise circum for computing radius won't work
dat4curv[, set4proc$colname4gps_lon_conv] <-
  jitter( dat4curv[, set4proc$colname4gps_lon_conv], factor = 1/10^10)
dat4curv[, set4proc$colname4gps_lat_coZnv] <-
  jitter( dat4curv[, set4proc$colname4gps_lat_conv], factor = 1/10^10)



# Compute radius ----------------------------------------------------------

radius <- computeRadius_batch(dat4curv[, set4proc$colname4gps_lon_conv], 
                              dat4curv[, set4proc$colname4gps_lat_conv], 
                              set4proc$seqlength,
                              output = F)

radius_alt <- radius

radius <- computeRadius_batch(dat4curv[, "gps_lon_med"], 
                               dat4curv[, "gps_lat_med"], 
                               set4proc$seqlength,
                               output = F)

dat4curv_curv_alt <- dat4curv_curv


# Smooth and plot radius values -------------------------------------------

## Settings for plot combination (including next step)
par(mfrow = c(4, 1), oma=c(0,0,2,0))

## Plot original values
plot(radius, type = "l", main = paste("Radius with seqlength =", set4proc$seqlength))
title(paste("Intersection #", set4proc$sxx, sep = ""), outer=TRUE)

## Filter values
radius.filtered <- radius
#coll.filtered[coll.filtered > set4curv$thresh] <- set4curv$thresh
radius.filtered[radius.filtered > set4proc$rfilter] <- set4proc$rfilter

## Plot filtered values
plot(radius.filtered, type = "l", main = "Filtered radius")

## Remember maximum value
radius.filtered_max <- max(radius.filtered)

## Create and predict smooth model
model <- loess(radius.filtered ~ c(1:length(radius.filtered)), span = 1/10, degree = 1)
radius.filtered.smooth <- predict(model, c(1:length(radius.filtered)))

## In case of overfitting: Adjust peak values to original maximum
radius.filtered.smooth[which(radius.filtered.smooth > radius.filtered_max)] <- radius.filtered_max

## Plot smoothed values
plot(radius.filtered, type = "l", main = "Smoothed radius values (span = 1/10, degree = 1)")
lines(radius.filtered.smooth, col = "red")

## Compute curvature
curv <- 1 / radius.filtered.smooth
#curv <- curv^(1/3)
#curv <- curv^2

## Plot curvature values
plot(curv, type = "l", main = "Adjusted curvature")

## Reset of plot settings
par(mfrow = c(1, 1)) 



# Merge with data ---------------------------------------------------------

# row_first <- ceiling(set4proc$seqlength/2)
# row_last <- nrow(dat4curv) - floor(set4proc$seqlength/2)
#dat4curv_curv <- cbind(dat4curv[c(row_first:row_last), ], curv)
dat4curv_curv <- cbind(dat4curv, curv)

yellowness <- ( curv - min(curv) ) / ( max(curv) - min(curv) )



# Visualise ---------------------------------------------------------------

plotcurv <-
  ggplot() +
  geom_path(data = dat4curv_curv,
            aes_string(x = set4proc$colname4gps_lon_conv,
                       y = set4proc$colname4gps_lat_conv),
            size = 2) +
  geom_path(data = dat4curv_curv,
            aes_string(x = set4proc$colname4gps_lon_conv,
                       y = set4proc$colname4gps_lat_conv),
                #alpha = curv.rollmean),
                alpha = yellowness^(1/3.5),
            #alpha = test.z3),
            colour = "yellow",
            #alpha = dat2plottest$curv.adj.rollmean.z,
            size = 2) +
  guides(alpha = F)+
  coord_cartesian(xlim = set4proc$xlim,
                  ylim = set4proc$ylim) #+
 # ggtitle(paste("kwidth:", set4proc$kwidth, "+ treshold:", set4proc$thresh))

plot(plotcurv)

# ggsave(paste(sprintf("s%02d", set4proc$sxx),
#              "_seg", set4proc$seglength,
#              "_xydist.smoothed.curv.pdf", sep = ""),
#        plotcurv,
#        path = "plots",
#        dpi = 300,
#        width = 20,
#        height = 10,
#        units = "cm")
# 
# ggsave(paste(sprintf("s%02d", set4proc$sxx),
#              "_seg", set4proc$seglength,
#              "_xydist.smoothed.curv.svg", sep = ""),
#        plotcurv,
#        path = "plots",
#        dpi = 300,
#        width = 20,
#        height = 10,
#        units = "cm")



