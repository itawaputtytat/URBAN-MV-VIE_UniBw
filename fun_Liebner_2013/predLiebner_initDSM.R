predLiebner_initDSM <- function(dat,
                                #db_conn_name,
                                #src_name_dsm,
                                col_name_cluster_group,
                                col_name_am,
                                col_name_speed,
                                threshold_am,
                                thresholds_u_max) {
  
  ## Select relevant variables
  dat <- 
    dat %>% 
    select_(col_name_cluster_group,
            col_name_am,
            col_name_speed)
  
  ## Rename columns
  dat <- 
    dat %>% 
    rename_at(col_name_cluster_group, funs(paste0("k"))) %>% 
    rename_at(col_name_am, funs(paste0("am"))) %>% 
    rename_at(col_name_speed, funs(paste0("speed")))
  
  ## Create dataframe for thresholds
  ## Convert k to character (cluster groups are saved as factors)
  thresholds_u_max < rev(thresholds_u_max)
  thresholds_u_max <- 
    data.frame(k = seq(length(thresholds_u_max)),
               u_max = rev(thresholds_u_max))
  thresholds_u_max$k <- as.character(thresholds_u_max$k)
  
  ## Set speed profile to max. u after reaching AM threshold
  ## Join with max. u thresholds
  ## Remove max. u threshold after adjusting speed
  dat <- 
    dat %>% 
    left_join(thresholds_u_max) %>% 
    mutate(speed =
             ifelse(am >= threshold_am,
                    u_max,
                    speed)) %>% 
    mutate(u_max = NULL)
  
  ## Add prefix to cluster group numbers
  dat$k <- paste0("k", dat$k)
  
  ## Spread speed values into separate columns for each cluster group
  dat_spread <- dat %>% spread(k, speed) 
  
  return(dat_spread)
}

