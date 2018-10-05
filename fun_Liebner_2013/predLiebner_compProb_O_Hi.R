
## Likelihood of current datervation
## ... given the longitudinal behavior of the driver could be modeled
## ... by the parameter set associated with hypothesis Hi

predLiebner_compProb_O_Hi <- function(dat,
                                      am2,
                                      speed2,
                                      coll_prob,
                                      sigma_s_m = NULL,
                                      sigma_v_ms = NULL) {
  
  ## Compute standard deviations for simulated distances and speed
  if (is.null(sigma_s_m)) {
    sigma_s_m <- sd(dat$dist_m)
  }
  if (is.null(sigma_v_ms)) {
    sigma_v_ms <- sd(dat$speed_ms) 
  }
  # row_finder <- test_summary$dti_m_rnd1 == am2
  # sigma_v_ms <- test_summary$sigma_v_ms[row_finder]
  #coll4sd <<- data.table::rbindlist(list(coll4sd, data.frame(sigma_s_m, sigma_v_ms)))
  
  #coll_prob <- c()
  for(i in 1:length(dat$.id)) {
    #print(dat$.id[i])
    if (is.na(dat$dist_m[i])) {
      y = 0 
    } else {
      y <- 
        predLiebner_pdf4sim(
          am2,
          dat$dist_m[i],
          sigma_s_m,
          speed2,
          dat$speed_ms[i],
          sigma_v_ms
        )
    }
    
    ## Collect probability of current observation
    col_finder <- dat$.id[i]
    coll_prob[, col_finder] <- y
    #coll_prob <- c(coll_prob, y)
  }
  names(coll_prob) <- dat$.id
  return(coll_prob)
}
