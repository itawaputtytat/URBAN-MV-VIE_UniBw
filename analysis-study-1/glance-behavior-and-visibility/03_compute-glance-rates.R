
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study1_t_glances_pxx_full_dti_rnd1_intrpld_cut"
sett_dat$col_name_act_level <- "glance_dir_level_v2"
sett_dat$col_name_act_label <- "glance_dir_label_v2"
sett_dat$col_name_case <- "passing"
sett_dat$col_name_am <- "pxx_dti_m_rnd1"

sett_proc <- c()
#sett_proc$position <- c(1:4)
sett_proc$position <- c(1:18)
sett_proc$round_txt <- c("normal", "stress")

sett_act <- c()
sett_act$glances <- 
  dbGetSrc(dbFindConnObj("Study-1"), 
           "t_glances_dir_id_description_v2") %>% 
  rename(label = glance_dir_label,
         level = glance_dir_level) %>% 
  distinct(level, label)

sett_save <- c()
sett_save$db_conn_name <- dbFindConnObj("Study-1")
sett_save$db_src_name_prefix <- "t_glances_rates"
sett_save$db_src_name_suffix <- "dti_rnd1"



# Compute glance rates ----------------------------------------------------

for(p in sett_proc$position) {
  
  coll <- c()
  coll1st <- c()
  
  for(r in sett_proc$round_txt) {
    
    ## Prepare data
    dat <- 
      get(sett_dat$df_name) %>% 
      filter_(paste(sett_dat$col_name_am, "<= 0")) %>% 
      filter(pxx == p) %>% 
      filter(round_txt == r)
    
    dat2 <- dat
    dat2[, sett_dat$col_name_act_level] <- 
      factor(dat2[, sett_dat$col_name_act_level],
             #levels = sort(unique(dat2[, sett_dat$col_name_act_level])),
             levels = sett_act$glances$level,
             labels = sett_act$glances$label)
    
    dat2[, sett_dat$col_name_case] <- 
      factor(dat2[, sett_dat$col_name_case])
    
    dat2[, "is_stopping_dist1_m50_dist2_0_speed_5"] <- 
      factor(dat2[, "is_stopping_dist1_m50_dist2_0_speed_5"])
    
    ## Compute glance rates
    glancerates_temp <-
      computeActivityRates(dat,
                           sett_dat$col_name_act_level,
                           sett_dat$col_name_am,
                           sett_dat$col_name_case,
                           unique_level = sett_act$glances$level)
    
    glances_rates1st <- glancerates_temp$dat_full_1st
    glances_rates <- glancerates_temp$dat_full
    
    ## Add info on position and round
    glances_rates1st$pxx <- p
    glances_rates1st$round_txt <- r
    glances_rates$pxx <- p
    glances_rates$round_txt <- r
    
    coll <- rbind(coll, glances_rates)
    coll1st <- rbind(coll1st, glances_rates1st)
    
  }
  
  ## Save glances_rates1st
  sett_save$db_src_name <- 
    paste_(paste0(sett_save$db_src_name_prefix, "1st"),
           sprintf("p%02d", p),
           sett_save$db_src_name_suffix)
  
  dbWriteTable(get(sett_save$db_conn_name),
               sett_save$db_src_name,
               coll1st,
               overwrite = T,
               row.names = F)
  
  ## Save glances_rates
  sett_save$db_src_name <- 
    paste_(sett_save$db_src_name_prefix,
           sprintf("p%02d", p),
           sett_save$db_src_name_suffix)
  
  dbWriteTable(get(sett_save$db_conn_name),
               sett_save$db_src_name,
               coll,
               overwrite = T,
               row.names = F)
  
}



# test <- glancerates_temp$dat_n_1st
# test_wide <- 
#   test %>% 
#   gather("col_name", "value", -one_of("passing", "glance_dir_level_v2", "act_id")) %>% 
#   unite(temp, col_name, glance_dir_level_v2, sep = "__") %>% 
#   spread(temp, value)
