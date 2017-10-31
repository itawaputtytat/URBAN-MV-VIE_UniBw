addVar4Stopping <- function(dat,
                            col_name_am = "pxx_dist_m_rnd1",
                            dist1 = -50,
                            dist2 = 25,
                            col_name_subject = "subject_id",
                            col_name_round = "round_txt",
                            col_name_group = "passing") {
  
  name4obj <- deparseDataFunArg(dat, return_dat = F)
  dat <- deparseDataFunArg(dat)
  
  if ("stopping" %in% colnames(dat))
    dat[, "stopping"] <- NULL
  
  ## Find stopping drivers
  stopfinder <-
    dat %>% 
    select_(.dots = c(col_name_group, 
                      speed = "speed_kmh", 
                      dist = col_name_am)) %>% 
    filter(dist > dist1 & dist < dist2) %>% 
    group_by_(.dots = lapply(col_name_group, as.symbol)) %>% 
    mutate(stopping = ifelse(speed <= 5, "stopping", "no_stopping")) %>% 
    filter(stopping == "stopping") %>%
    group_by_(.dots = lapply(list(col_name_group, "stopping"), as.symbol)) %>%
    summarise()
  
  ## Join original data and new stopping filter
  dat <-
    left_join(dat,
              stopfinder,
              by = col_name_group)
  
  dat$stopping[is.na(dat$stopping)] <- "no_stopping"
  
  assign(name4obj, dat, env = .GlobalEnv)
}