addVar4Stopping <- function(dat2proc,
                            varname4dist = "sxx_dist_m_rnd1",
                            dist1 = -50,
                            dist2 = 25,
                            varname4subject = "subject_id",
                            varname4round = "round_txt",
                            varname4group = "passing") {
  
  ## Find stopping drivers
  stopfinder <-
    dat2proc %>% 
    select_(.dots = c("passing", 
                      speed = "speed_kmh", 
                      dist = "sxx_dist_m_rnd1")) %>% 
    filter(dist > dist1 & dist < dist2) %>% 
    group_by_(.dots = lapply("passing", as.symbol)) %>% 
    mutate(stopping = ifelse(speed <= 5, "stopping", "no_stopping")) %>% 
    filter(stopping == "stopping") %>%
    group_by_(.dots = lapply(list("passing", "stopping"), as.symbol)) %>%
    summarise()
  
  ## Join original data and new stopping filter
  dat2proc <-
    left_join(dat2proc,
              stopfinder,
              by = "passing")
  
  dat2proc$stopping[is.na(dat2proc$stopping)] <- "no_stopping"
  
  return(dat2proc)
}