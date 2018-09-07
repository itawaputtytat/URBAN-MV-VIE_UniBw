
## Likelihood of current observation
## ... given the longitudinal behavior of the driver could be modeled
## ... by the parameter set associated with hypothesis Hi

predLiebner_compProb_O_Hi <- function(dat_sim,
                                      am2,
                                      speed2,
                                      coll_prob,
                                      sigma_s_m = NULL,
                                      sigma_v_ms = NULL) {
  
  ## Get final values from IDM simulation
  obs <- lapply(dat_sim, function(x) 
    lapply(x, function(y) 
      tail(y, 1)) )
  obs <- as.data.frame(data.table::rbindlist(obs, idcol = T, fill  = T))
  
  ## Compute standard deviations for simulated distances and speed
  if (is.null(sigma_s_m)) {
    sigma_s_m <- sd(obs$dist_m)
  }
  if (is.null(sigma_v_ms)) {
    sigma_v_ms <- sd(obs$speed_ms) 
  }
  # row_finder <- test_summary$dti_m_rnd1 == am2
  # sigma_v_ms <- test_summary$sigma_v_ms[row_finder]
  #coll4sd <<- data.table::rbindlist(list(coll4sd, data.frame(sigma_s_m, sigma_v_ms)))
  
  for(i in 1:length(obs$.id)) {
    
    if (is.na(obs$dist_m[i])) {
      y = 0 
    } else {
      y <- 
        predLiebner_pdf4sim(
          am2,
          obs$dist_m[i],
          sigma_s_m,
          speed2,
          obs$speed_ms[i],
          sigma_v_ms
        )
    }
    
    ## Collect probability of current observation
    col_finder <- obs$.id[i]
    coll_prob[, col_finder] <- y
    
  }
  
  return(coll_prob)
}
