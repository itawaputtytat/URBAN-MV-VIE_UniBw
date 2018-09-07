findPedActSeqFirstBraking <- function(dat,
                                      col_name_id,
                                      col_name_am_start) {
  dat_new <- 
    dat %>% 
    filter(pedal_act == -1) %>% 
    filter(pedal_act_nr > pedal_act_nr_acc_last_before_brake |
             is.na(pedal_act_nr_acc_last_before_brake)) %>% 
    ## In case of multiple braking activites
    summarise_all(min) %>% 
    ## Complete data for all passings
    right_join( dat %>% 
                  distinct_(col_name_id) ) %>% 
    rename(pedal_act_nr_brake_first = pedal_act_nr) %>% 
    data.frame()
  
  return(dat_new)
}