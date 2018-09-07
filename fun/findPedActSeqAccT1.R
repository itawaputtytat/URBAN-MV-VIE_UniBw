findPedActSeqAccT1 <- function(dat,
                               col_names_id,
                               col_names_am_start,
                               col_names_am_end,
                               threshold) {
  
  # ## Identify accelerating activity after critical distance
  rowfinder <-
    dat[, "pedal_act"] == -1 &
    dat[, col_names_am_start] <= threshold
  dat$pedal_act_nr_break_max <- 0
  dat$pedal_act_nr_break_max <- NA
  dat$pedal_act_nr_break_max[rowfinder] <- dat$pedal_act_nr[rowfinder]
  
  # ## Find pedal activity number of first braking
  dat <-
    dat %>%
    group_by_("passing") %>%
    mutate(pedal_act_nr_break_max = max(pedal_act_nr_break_max, na.rm = T)) %>%
    ## Filter
    filter(pedal_act == 1 & pedal_act_nr < pedal_act_nr_break_max) %>%
    filter_(paste(col_names_am_start, "<=", 
                  threshold)) %>% 
    #filter(pxx_dist_m_rnd1_pedal_act_end < 0) %>%
    ## In case of multiple accleration activites
    summarise_all(max) %>%
    ## Complete data for all passings
    right_join(dat %>% distinct_(col_names_id) ) %>%
    rename(pedal_act_nr_acc_last_before_brake = pedal_act_nr) %>%
    data.frame()
  
  ## Replace missing values with minimum AM
  row_finder <- is.na(dat[, col_names_am_start])
  dat[row_finder, col_names_am_start] <- 
    min(dat[, col_names_am_start], na.rm = T)
  dat[row_finder, col_names_am_end] <- 
    min(dat[, col_names_am_start], na.rm = T)
  
  return(dat)
}