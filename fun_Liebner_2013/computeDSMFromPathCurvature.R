computeDSMFromPathCurvature <- function(dat, 
                                        col_name_am,
                                        col_name_curv,
                                        u_max, 
                                        acc_lat_max, 
                                        gradients,
                                        threshold_am_for_max = NULL) {
  
  dat_dsm <- 
    dat %>% 
    select_(.dots = c(col_name_am, col_name_curv)) %>% 
    mutate_(.dots = setNames(list(
      interp(~ am - lag(am),
             am = as.name(col_name_am))),
      "s_diff"))
  
  ## Convert curvature to speed profile
  for (i in 1:length(u_max)) {
    dat_dsm[, col_name_curv] <- dat_dsm[, col_name_curv]
    dat_dsm[, paste0("u", i)] <- sqrt(acc_lat_max[i] / dat_dsm[, col_name_curv])
    row_finder <- dat_dsm[, paste0("u", i)] > u_max[i]
    dat_dsm[row_finder, paste0("u", i)] <- u_max[i]
    
    ## Add gradients to speed profile
    row_finder <- which(dat_dsm[, paste0("u", i)] == min(dat_dsm[, paste0("u", i)]))
    #row_finder <- which(dat_dsm[, col_name_am] == 0)
    dat_dsm[, paste0("u", i, "_gr")] <- dat_dsm[, paste0("u", i)]
    for (j in (row_finder-1):1) {
      dat_dsm[j, paste0("u", i, "_gr")] <-
        min(dat_dsm[j+1, paste0("u", i, "_gr")] +
              gradients[i] * dat_dsm$s_diff[j+1],
            u_max[i])

      if (!is.null(threshold_am_for_max)) {
        row_finder <- which(dat_dsm[, col_name_am] >= 10)
        dat_dsm[j, paste0("u", i, "_gr")]
      }

    }
  }
  
  return(dat_dsm)
}