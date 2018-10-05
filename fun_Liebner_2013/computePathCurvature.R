computePathCurvature <- function(dat,
                                 col_name_r,
                                 threshold_r = 10^3,
                                 roll_avg_k = 150) {
  
  ## Restrict value range
  dat$r_restr <- dat[, col_name_r]
  dat$r_restr[which(dat$r_restr > threshold_r)] <- threshold_r
  
  ## Smooth values
  dat$r_restr_smooth <-
    rollAvg(dat$r_restr, k = roll_avg_k)
  
  ## Derive curvature
  dat$curv <- 1/dat$r_restr_smooth
  finder <- is.infinite(dat$curv)
  dat$curv[finder] <- NA
  dat$curv <- na.locf0(dat$curv, fromLast = TRUE)
  dat$curv <- na.locf(dat$curv)
  
  ## Normalize curvature
  dat <-
    dat %>% 
    mutate(curv_norm = (curv - min(curv)) / (max(curv) - min(curv)))
  
  return (dat)
}