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
  r.filtered[r.filtered > 100] <- 100
  ## Remember maximum value
  r.filtered_max <- max(r.filtered)
  ## Create and predict smooth model
  model <- loess(r.filtered ~ c(1:length(r.filtered)), span = 1/10, degree = 1)
  r.filtered.smooth <- predict(model, c(1:length(r.filtered)))
  ## In case of overfitting: Adjust peak values to original maximum
  r.filtered.smooth[which(r.filtered.smooth > r.filtered_max)] <- r.filtered_max
  
  ## Compute curvature
  curv <- 1 / r.filtered.smooth
  
  ## Merge with data
  row_first <- ceiling(set4proc$seqlength/2)
  row_last <- nrow(dat2proc) - floor(set4proc$seqlength/2)
  dat2proc <- cbind(dat2proc[c(row_first:row_last), ], curv)
  
  if (set4proc$plot) {
    
    ## Settings for plot combination (including next step)
    par(mfrow = c(4, 1), oma = c(0, 0, 2, 0))
    
    ## Plot original values
    plot(r, 
         type = "l", 
         main = paste("Radius with seqlength =", set4proc$seqlength))
    title(paste("Intersection #", set4proc$sxx, sep = ""), outer = TRUE)
    
    ## Plot filtered values
    plot(r.filtered, type = "l", main = "Filtered radius")
    
    ## Plot smoothed values
    plot(r.filtered, 
         type = "l", 
         main = "Smoothed radius values (span = 1/10, degree = 1)")
    lines(r.filtered.smooth, 
          col = "red")
    
    ## Plot curvature values
    plot(curv, 
         type = "l", 
         main = "Adjusted curvature")
    
    ## Reset of plot settings
    par(mfrow = c(1, 1)) 
  }
  
  return(dat2proc)
}