computeAMToSteerMax <- function(dat,
                                col_name_am,
                                col_name_steer,
                                col_names_group,
                                col_name_target) {
  
  dat <- 
    dat %>% 
    group_by_(.dots = col_names_group) %>% 
    
    ## Find corresponding AM value
    mutate_(.dots = setNames(list(
      interp(~ ifelse(abs(measure) == max(abs(measure)), am, NA),
             measure = as.name(col_name_steer),
             am = as.name(col_name_am))),
      col_name_target)) %>% 
    
    ## Fill missing values
    mutate_(.dots = setNames(list(
      interp(~ max(am_max, na.rm = T),
             am_max = as.name(col_name_target))),
      col_name_target)) %>% 
    
    ## Adjust AM
    mutate_(.dots = setNames(list(
      interp(~ ifelse(am_max > 0, 
                      round(am - am_max, 1),
                      round(am + am_max, 1)),
             am = as.name(col_name_am),
             am_max = as.name(col_name_target))),
      col_name_target)) %>% 
    data.frame()
  
  return(dat)
}
