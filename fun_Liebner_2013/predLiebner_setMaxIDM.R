## Set desired speed profiles to max. desired speed after turning
## Set max. desired speed for all succeeding values
predLiebner_setMaxIDM <- function(dat,
                                  col_name_id,
                                  col_name_am,
                                  col_name_speed,
                                  u_max,
                                  s_max,
                                  s_max0) {
  
  dat <- 
    dat %>% 
    ## Sett u to u max after s_max threshold
    mutate_(.dots = setNames(list(
      interp(~ ifelse(s >= s_max, u_max, u),
             s = as.name(col_name_am),
             s_max = s_max,
             u_max = u_max,
             u = as.name(col_name_speed))),
      col_name_speed)) %>%
    mutate_(.dots = setNames(list(
      interp(~ ifelse(s > s_max, 1, 0),
             s = as.name(col_name_am),
             s_max = s_max0)),
      "s_threshold")) %>% 
    group_by_(.dots = 
                c(col_name_id,
                  "s_threshold")) %>% 
    mutate_(.dots = setNames(list(
      interp(~ cummax(u),
             u = as.name(col_name_speed))),
      paste(col_name_speed, "max_ind"))) %>% 
    mutate_(.dots = setNames(list(
      interp(~ ifelse(s >= 0 & u_max_ind >= u_max, u_max, u),
             s = as.name(col_name_am),
             u_max_ind = as.name(paste(col_name_speed, "max_ind")),
             u_max = u_max,
             u = as.name(col_name_speed))),
      col_name_speed)) %>% 
    data.frame()
  
  return (dat)
}