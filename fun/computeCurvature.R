computeCurvature <- function(dat2proc, set4proc) {
  
  ## In case of duplicate values create mini-mini-mini-deviations
  ## Otherwise circum for computing radius won't work
  gps_lon <- jitter(dat2proc[, 1], factor = 1/10^10)
  gps_lat <- jitter(dat2proc[, 2], factor = 1/10^10)
  
  ## Compute radius
  r <- computeRadius_batch(gps_lon, gps_lat, set4proc$seqlength, output = F)

  ## Smooth and plot radius values
  r.filtered <- r
  ## Limit range
  r.filtered[r.filtered > set4proc$rfilter] <- set4proc$rfilter
  
  ## Special case: GPS-anomaly in s14
  if (set4proc$sxx == 14) r.filtered[1:(set4proc$row4origin - 100)] <- 100
  
  ## Remember maximum value
  r.filtered_max <- max(r.filtered)
  ## Create and predict smooth model
  model <- loess(r.filtered ~ c(1:length(r.filtered)), span = set4proc$smooth_r$loess_span, degree = set4proc$smooth_r$degree)
  r.filtered.smooth <- predict(model, c(1:length(r.filtered)))
  ## In case of overfitting: Adjust peak values to original maximum
  r.filtered.smooth[which(r.filtered.smooth > r.filtered_max)] <- r.filtered_max
  
  if (!is.na(set4proc$rollmean_k)) {
    r.filtered.rm <- rollmedian(r.filtered, set4proc$rollmean_k)
    model <- loess(r.filtered.rm ~ c(1:length(r.filtered.rm)), span = 1/10, degree = 1)
    r.filtered.rm.smooth <- predict(model, c(1:length(r.filtered.rm)))
    r.filtered.rm.smooth <- c(rep(0, set4proc$rollmean_k/2), r.filtered.rm.smooth, rep(0, set4proc$rollmean_k/2))
  }
  
  ## Compute curvature
  curv <- 1 / r.filtered.smooth
  
  if (!is.na(set4proc$rollmean_k))
    #curv.rm <- rollmean(curv, set4proc$rollmean_k) + 0.001
    curv.rm <- 1/r.filtered.rm.smooth
  
  ## Merge with data
  #row_first <- ceiling(set4proc$seqlength/2)
  #row_last <- nrow(dat2proc) - floor(set4proc$seqlength/2)
  #dat2proc <- cbind(dat2proc[c(row_first:row_last), ], curv)
  dat2proc <- cbind(dat2proc, curv)
  
  if (set4proc$plot) {
    
    ## Settings for plot combination (including next step)
    #par(mfrow = c(3, 1), oma = c(0, 0, 2, 0))
    par(mfrow = c(4, 1), oma = c(0, 0, 2, 0))
    
    # Plot original values
    plot(r,
         type = "l",
         main = paste("Radius with seqlength =", set4proc$seqlength))
    title(paste("Intersection #", set4proc$sxx, sep = ""), outer = T)

    ## Plot filtered values
    # plot(r.filtered, type = "l", main = "Filtered radius")

    ## Plot smoothed values
    # plot(r.filtered,
    #      type = "l",
    #      main = "Smoothed radius values (span = 1/10, degree = 1)")
    # lines(r.filtered.smooth,
    #       col = "red")

    ## Plot filtered and smoothed values
    plot(r.filtered,
         type = "l",
         main = paste("Filtered and smoothed radius values:",
                      "span =", set4proc$smooth_r$loess_span, ";",
                      "degree =", set4proc$smooth_r$degree, sep = ""),
         ylim = c(0, set4proc$rfilter ))
    lines(r.filtered.smooth,
          col = "red")
    if (!is.na(set4proc$rollmean_k))
    lines(r.filtered.rm,
          col = "green3")

    ## Plot curvature values
    plot(curv,
         type = "l",
         main = "Adjusted curvature",
         col = "red")
         #ylim = c(0, 0.2))
    abline(max(curv), 0, col = "blue")

    if (!is.na(set4proc$rollmean_k))
    lines(curv.rm, col = "green3")
    
    ## Normalised
    curv_norm <- ( ( curv - min(curv) ) / ( max(curv) - min(curv) ) )^(1/3.5)
    curv_norm <- curv / max(curv)
    plot(curv_norm,
         type = "l",
         ylim = c(0, 1))
    
    ## Reset of plot settings
    par(mfrow = c(1, 1)) 
  }
  
  return(data.frame(dat2proc, curv_norm))
}