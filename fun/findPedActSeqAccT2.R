findPedActSeqAccT2 <- function (dat,
                                  col_name_id,
                                  col_name_am_start,
                                  col_name_am_end,
                                  threshold) {
  
  dat_new <- 
    dat %>% 
    group_by_(col_name_id) %>% 
    ## Filter for accelerating activity
    filter(pedal_act == 1) %>% 
    ## Filter for distance criteria:
    ## Activity begins after critical distance OR
    ## ... begins before but ends after
    filter_( paste(
      col_name_am_start, ">=", threshold, "|",
      "(", col_name_am_start, "<=", threshold, "&",
      col_name_am_end, ">=", threshold, ")") ) %>% 
    ## In case of multiple accleration activites
    summarise_all(min) %>% 
    ## Complete data for all passings
    right_join( dat %>% 
                  distinct_(col_name_id) ) %>% 
    rename(pedal_act_nr_acc_t2 = pedal_act_nr) %>% 
    data.frame()
  
  return(dat_new)
  
}


reorderPedalSeqIds <- function(dat, 
                            col_name_id, 
                            col_name_to_order) {
  
  ## Order sequences
  new_order <- 
    dat %>% 
    arrange_(col_name_to_order) %>% 
    ungroup() %>% 
    select_(col_name_id) %>% 
    pull() %>% 
    rev()
  
  return(new_order)
}
