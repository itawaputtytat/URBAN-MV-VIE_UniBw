identifyStoppings <- function(dat,
                            col_name_am = "pxx_dti_m_rnd1",
                            col_name_speed = "speed_kmh",
                            col_names_identifier = "passing",
                            dist1 = -50,
                            dist2 = 25,
                            speed_threshold_kmh = 5,
                            col_name_group = "passing",
                            return_cases_only = F) {
  
  
  outputFunProc(R)
  
  obj_name <- deparseDataFunArg(dat, return_dat = F)
  dat <- deparseDataFunArg(dat)
  
  ## If column "stopping" already in data, remove column
  if ("stopping" %in% colnames(dat)) {
    dat[, "stopping"] <- NULL
  }
  
  ## If speed column bases on m/s
  if (grepl("ms", col_name_speed)) {
    speed_threshold <- speed_threshold_kmh / 3.6
  } else {
    speed_threshold <- speed_threshold_kmh
  }
  
  ## Find stopping drivers
  dat_stoppings <-
    dat %>% 
    select_(.dots = c(col_name_group, 
                      col_name_speed, 
                      col_name_am)) %>% 
    filter_(paste(col_name_am, ">", dist1, "&",
                  col_name_am, "<", dist2)) %>% 
    group_by_(col_name_group) %>% 
    mutate_(.dots =
              setNames(list(
                interp(~(ifelse(v < w, T, F)),
                       v = as.name(col_name_speed),
                       w = speed_threshold)),
                "is_stopping")) %>% 
    #filter_("is_stopping") %>%
    select_(col_name_group, "is_stopping") %>% 
    group_by_(col_name_group) %>%
    summarise_all(.funs = max)
  


  if (!return_cases_only) {
    
    ## Join original data and new stopping filter
    dat <-
      left_join(dat,
                dat_stoppings,
                by = col_name_group)
    #dat$stopping[is.na(dat$stopping)] <- "no_stopping"
    
    assign(obj_name, dat, env = .GlobalEnv)
    
  } else {
    
    dat_cases <- 
      dat %>% 
      select_(.dots = unlist(col_names_identifier, use.names = F)) %>% 
      distinct()
    
    dat_stoppings <- 
      left_join(dat_stoppings, 
                dat_cases, 
                by = sett_dat$col_names$case) %>% 
      select_(.dots = unlist(col_names_identifier, use.names = F),
              "is_stopping")
    
    return(dat_stoppings)
  }

}