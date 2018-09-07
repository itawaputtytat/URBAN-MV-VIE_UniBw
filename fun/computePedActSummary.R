computePedActSummary <- function(dat,
                                 col_name_id,
                                 col_name_am,
                                 col_name_am_start,
                                 col_name_am_end,
                                 col_name_pedal_act = "pedal_act",
                                 col_name_pedal_act_id = "pedal_act_id") {
  
  dat_new <- 
    dat %>% 
    ## Reduce pedal activity to starting arrival measure
    group_by_(col_name_id,
              col_name_pedal_act_id) %>%
    summarise_(.dots = c(
      setNames(list(
        interp(~ min(var), 
               var = as.name(col_name_pedal_act)) ), 
               col_name_pedal_act),
      setNames(list(
        interp(~ min(var), 
               var = as.name(col_name_am)) ), 
               col_name_am_start),
      setNames(list(
        interp(~ max(var), 
               var = as.name(col_name_am)) ), 
               col_name_am_end)
    )) %>% 
    ## Arrange by id and arrival measure
    arrange_(col_name_id, 
             col_name_am_start) %>% 
    ## Enumerate each type of pedal activity
    group_by_(col_name_id) %>% 
    mutate_(.dots = 
              setNames(list(
                interp(~ row_number() )),
            paste_(col_name_pedal_act, "nr")))
  
  return(dat_new)
}