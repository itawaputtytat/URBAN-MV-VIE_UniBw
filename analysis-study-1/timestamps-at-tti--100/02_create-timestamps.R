
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$dat_name <- "dat_study1_t_adtf_pxx_full_dti_rnd1"
sett_dat$col_name$case <- "passing"
sett_dat$col_name$position <- "pxx"
sett_dat$col_name$subject <- "subject_id"
sett_dat$col_name$group <- "round_txt"
sett_dat$col_name$time <- "time_s"
# sett_dat$col_name$am_dti <- "pxx_dti_m_rnd1"
# sett_dat$col_name$am_tti <- "pxx_tti_s"



# Create timestamps -------------------------------------------------------

dat <- 
  get(sett_dat$dat_name) %>% 
  select_(.dots = unlist(sett_dat$col_name, use.names = F)) %>% 
  group_by_(sett_dat$col_name$case, 
            sett_dat$col_name$position,
            sett_dat$col_name$subject,
            sett_dat$col_name$group) %>%
  summarise_all(funs(min, max)) %>% 
  rename_(.dots = setNames("min", paste_(sett_dat$col_name$time, "min"))) %>% 
  rename_(.dots = setNames("max", paste_(sett_dat$col_name$time, "max"))) %>% 
  data.frame()
  