
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study1_t_glances_pxx_full_dti_rnd1_intrpld_cut"
sett_dat$col_name_act_level <- "glance_dir_level_v2"
sett_dat$col_name_act_label <- "glance_dir_label_v2"
sett_dat$col_name_case <- "passing"
sett_dat$col_name_am <- "pxx_dti_m_rnd1"

sett_proc <- c()
sett_proc$position <- 5
sett_proc$do_plot <- T

sett_act <- c()
sett_act$glances <- 
  dbGetSrc(dbFindConnObj("Study-1"), 
           "t_glances_dir_id_description_v2") %>% 
  rename(label = glance_dir_label,
         level = glance_dir_level) %>% 
  distinct(level, label)



# Prepare data ------------------------------------------------------------

dat <- 
  get(sett_dat$df_name) %>% 
  filter_(paste(sett_dat$col_name_am, "<= 0")) %>% 
  filter(pxx == sett_proc$position) %>% 
  filter(round_txt == "normal")

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



# Compute glance rates ----------------------------------------------------

glancerates_temp <-
  computeActivityRates(dat,
                       sett_dat$col_name_act_level,
                       sett_dat$col_name_am,
                       sett_dat$col_name_case,
                       unique_level = sett_act$glances$level)


glances_rates1st <- glancerates_temp$dat_full_1st
glances_rates <- glancerates_temp$dat_full

