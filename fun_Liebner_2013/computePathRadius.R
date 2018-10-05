computePathRadius <- function(dat, 
                              col_name_x, 
                              col_name_y,
                              step = 50) {
  
  ## Initialise radius
  dat$r <- 0
  
  ## For all rows
  ## Extract three points
  ## Compute radius
  for (i in (step+1):(nrow(dat)-step)) {
    dat_temp <- dat[c(i-step, i, i+step),]
    dat$r[i] <- circum(dat_temp[, col_name_x], dat_temp[, col_name_y])$radius
  }
  
  return (dat$r)
}